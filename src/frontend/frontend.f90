module frontend
    ! lazy fortran compiler frontend
    ! Clean coordinator module - delegates to extracted specialized modules
    ! Architecture: Lexer → Parser → Semantic → Codegen with FALLBACK support
    
    use lexer_core, only: token_t, tokenize_core, TK_EOF, TK_KEYWORD
    use parser_core, only: parse_expression, parse_statement, parser_state_t, create_parser_state, &
                           parse_function_definition, parse_do_loop, parse_do_while, parse_select_case
    use ast_core
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    use codegen_core, only: generate_code, generate_code_polymorphic
    use logger, only: log_debug, log_verbose, set_verbose_level
    
    ! FALLBACK modules (temporary until full AST)
    use token_fallback, only: set_current_tokens, generate_use_statements_from_tokens, &
                              generate_executable_statements_from_tokens, &
                              generate_function_definitions_from_tokens
    use declaration_generator, only: generate_declarations
    use debug_utils, only: debug_output_tokens, debug_output_ast, debug_output_semantic, debug_output_codegen
    use json_reader, only: json_read_tokens_from_file, json_read_ast_from_file, json_read_semantic_from_file
    
    implicit none
    private
    
    public :: compile_source, compilation_options_t
    public :: compile_from_tokens_json, compile_from_ast_json, compile_from_semantic_json
    public :: BACKEND_FORTRAN, BACKEND_LLVM, BACKEND_C
    ! Debug functions for unit testing
    public :: find_program_unit_boundary, is_function_start, is_end_function, parse_program_unit
    public :: is_do_loop_start, is_do_while_start, is_select_case_start, is_end_do, is_end_select
    
    ! Backend target enumeration
    integer, parameter :: BACKEND_FORTRAN = 1  ! Standard Fortran (current IR)
    integer, parameter :: BACKEND_LLVM = 2     ! LLVM IR (future)
    integer, parameter :: BACKEND_C = 3        ! C code (future)
    
    ! Compilation options
    type :: compilation_options_t
        integer :: backend = BACKEND_FORTRAN
        logical :: debug_tokens = .false.
        logical :: debug_ast = .false.
        logical :: debug_semantic = .false.
        logical :: debug_codegen = .false.
        logical :: optimize = .false.
        character(len=:), allocatable :: output_file
    end type compilation_options_t
    
contains

    ! Main entry point - clean 4-phase compilation pipeline
    subroutine compile_source(input_file, options, error_msg)
        character(len=*), intent(in) :: input_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg
        
        ! Local variables
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_tree
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: code, source
        integer :: unit, iostat
        
        ! Log compilation start
        call log_verbose("frontend", "compile_source called with: " // trim(input_file))
        
        error_msg = ""
        
        ! Read source file
        open(newunit=unit, file=input_file, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot open input file: " // input_file
            return
        end if
        
        block
            character(len=:), allocatable :: line
            allocate(character(len=0) :: source)
            allocate(character(len=1000) :: line)
            
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                source = source // trim(line) // new_line('a')
            end do
        end block
        close(unit)
        
        ! Phase 1: Lexical Analysis
        call lex_file(source, tokens, error_msg)
        if (error_msg /= "") return
        if (options%debug_tokens) call debug_output_tokens(input_file, tokens)
        
        ! Phase 2: Parsing
        call parse_tokens(tokens, ast_tree, error_msg)
        if (error_msg /= "") return
        if (options%debug_ast) call debug_output_ast(input_file, ast_tree)
        
        ! Phase 3: Semantic Analysis
        sem_ctx = create_semantic_context()
        
        ! STAGE 2 WORKAROUND: Skip semantic analysis for programs with function calls
        ! to avoid Hindley-Milner type system crashes during unification
        if (.not. contains_function_calls(ast_tree)) then
            call analyze_program(sem_ctx, ast_tree)
        end if
        if (options%debug_semantic) call debug_output_semantic(input_file, ast_tree)
        
        ! Phase 4: Code Generation
        call generate_fortran_code(ast_tree, sem_ctx, code)
        if (options%debug_codegen) call debug_output_codegen(input_file, code)
        
        
        ! Write output
        if (allocated(options%output_file)) then
            call write_output_file(options%output_file, code, error_msg)
        end if
        
    end subroutine compile_source

    ! Compile from tokens JSON (skip phase 1)
    subroutine compile_from_tokens_json(tokens_json_file, options, error_msg)
        character(len=*), intent(in) :: tokens_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg
        
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_tree
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: code
        
        error_msg = ""
        
        ! Read tokens from JSON
        tokens = json_read_tokens_from_file(tokens_json_file)
        if (options%debug_tokens) call debug_output_tokens(tokens_json_file, tokens)
        
        ! Phase 2: Parsing
        call parse_tokens(tokens, ast_tree, error_msg)
        if (error_msg /= "") return
        if (options%debug_ast) call debug_output_ast(tokens_json_file, ast_tree)
        
        ! Phase 3: Semantic Analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, ast_tree)
        if (options%debug_semantic) call debug_output_semantic(tokens_json_file, ast_tree)
        
        ! Phase 4: Code Generation
        call generate_fortran_code(ast_tree, sem_ctx, code)
        if (options%debug_codegen) call debug_output_codegen(tokens_json_file, code)
        
        ! Write output
        if (allocated(options%output_file)) then
            call write_output_file(options%output_file, code, error_msg)
        end if
        
    end subroutine compile_from_tokens_json

    ! Compile from AST JSON (skip phases 1-2)
    subroutine compile_from_ast_json(ast_json_file, options, error_msg)
        character(len=*), intent(in) :: ast_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg
        
        class(ast_node), allocatable :: ast_tree
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: code
        
        error_msg = ""
        
        ! Read AST from JSON
        ast_tree = json_read_ast_from_file(ast_json_file)
        if (options%debug_ast) call debug_output_ast(ast_json_file, ast_tree)
        
        ! Phase 3: Semantic Analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, ast_tree)
        if (options%debug_semantic) call debug_output_semantic(ast_json_file, ast_tree)
        
        ! Phase 4: Code Generation
        call generate_fortran_code(ast_tree, sem_ctx, code)
        if (options%debug_codegen) call debug_output_codegen(ast_json_file, code)
        
        ! Write output
        if (allocated(options%output_file)) then
            call write_output_file(options%output_file, code, error_msg)
        end if
        
    end subroutine compile_from_ast_json

    ! Compile from semantic JSON (skip phases 1-3) - ANNOTATED AST TO CODEGEN
    subroutine compile_from_semantic_json(semantic_json_file, options, error_msg)
        character(len=*), intent(in) :: semantic_json_file
        type(compilation_options_t), intent(in) :: options
        character(len=*), intent(out) :: error_msg
        
        class(ast_node), allocatable :: ast_tree
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: code
        
        error_msg = ""
        
        ! Read annotated AST and semantic context from JSON
        call json_read_semantic_from_file(semantic_json_file, ast_tree, sem_ctx)
        if (options%debug_semantic) call debug_output_semantic(semantic_json_file, ast_tree)
        
        ! Phase 4: Code Generation (direct from annotated AST)
        call generate_fortran_code(ast_tree, sem_ctx, code)
        if (options%debug_codegen) call debug_output_codegen(semantic_json_file, code)
        
        ! Write output
        if (allocated(options%output_file)) then
            call write_output_file(options%output_file, code, error_msg)
        end if
        
    end subroutine compile_from_semantic_json

    ! Phase 1: Lexical Analysis
    subroutine lex_file(source, tokens, error_msg)
        character(len=*), intent(in) :: source
        type(token_t), allocatable, intent(out) :: tokens(:)
        character(len=*), intent(out) :: error_msg
        
        error_msg = ""
        call tokenize_core(source, tokens)
        
        ! Store tokens for FALLBACK functions
        call set_current_tokens(tokens)
    end subroutine lex_file

    ! Phase 2: Parsing 
    subroutine parse_tokens(tokens, ast_tree, error_msg)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable, intent(out) :: ast_tree
        character(len=*), intent(out) :: error_msg
        
        ! Local variables for program unit parsing using wrapper pattern
        type(ast_node_wrapper), allocatable :: body_statements(:)
        class(ast_node), allocatable :: stmt
        integer :: i, unit_start, unit_end, stmt_count
        type(token_t), allocatable :: unit_tokens(:)
        
        error_msg = ""
        stmt_count = 0
        
        ! Create program node (dialect-agnostic core)
        allocate(program_node :: ast_tree)
        select type (prog => ast_tree)
        type is (program_node)
            prog%name = "main"
            prog%line = 1
            prog%column = 1
            
            ! Parse program units, not individual lines
            i = 1
            do while (i <= size(tokens))
                if (tokens(i)%kind == TK_EOF) exit
                
                ! Skip empty lines (just EOF tokens)
                if (i < size(tokens) .and. tokens(i)%kind == TK_EOF) then
                    i = i + 1
                    cycle
                end if
                
                ! Find program unit boundary
                call find_program_unit_boundary(tokens, i, unit_start, unit_end)
                
                block
                    character(len=20) :: start_str, end_str
                    write(start_str, '(I0)') unit_start
                    write(end_str, '(I0)') unit_end
                    call log_verbose("parsing", "Found program unit from token " // &
                                    trim(start_str) // " to " // trim(end_str))
                end block
                
                ! Skip empty units, units with just EOF, or single-token keywords that are part of larger constructs
                if (unit_end >= unit_start .and. &
                    .not. (unit_end == unit_start .and. tokens(unit_start)%kind == TK_EOF) .and. &
                    .not. (unit_end == unit_start .and. tokens(unit_start)%kind == TK_KEYWORD .and. &
                           (tokens(unit_start)%text == "real" .or. tokens(unit_start)%text == "integer" .or. &
                            tokens(unit_start)%text == "logical" .or. tokens(unit_start)%text == "character" .or. &
                            tokens(unit_start)%text == "function" .or. tokens(unit_start)%text == "subroutine" .or. &
                            tokens(unit_start)%text == "module"))) then
                    ! Extract unit tokens and add EOF
                    allocate(unit_tokens(unit_end - unit_start + 2))
                    unit_tokens(1:unit_end - unit_start + 1) = tokens(unit_start:unit_end)
                    ! Add EOF token
                    unit_tokens(unit_end - unit_start + 2)%kind = TK_EOF
                    unit_tokens(unit_end - unit_start + 2)%text = ""
                    unit_tokens(unit_end - unit_start + 2)%line = tokens(unit_end)%line
                    unit_tokens(unit_end - unit_start + 2)%column = tokens(unit_end)%column + 1
                    
                    ! Debug: Extracted tokens for do construct
                    
                    call log_verbose("parsing", "Extracted " // trim(adjustl(int_to_str(size(unit_tokens)))) // &
                                    " tokens for unit")
                    
                    ! Parse the program unit
                    stmt = parse_program_unit(unit_tokens)
                    
                    if (allocated(stmt)) then
                        ! Note: Statement added to AST
                        
                        ! Extend wrapper array using [array, new_element] pattern
                        block
                            type(ast_node_wrapper) :: new_wrapper
                            allocate(new_wrapper%node, source=stmt)
                            if (allocated(body_statements)) then
                                body_statements = [body_statements, new_wrapper]
                            else
                                body_statements = [new_wrapper]
                            end if
                            stmt_count = stmt_count + 1
                        end block
                    end if
                    
                    deallocate(unit_tokens)
                end if
                
                i = unit_end + 1
                call log_verbose("parsing", "Next iteration will start at token " // &
                                trim(adjustl(int_to_str(i))))
            end do
            
            ! Create polymorphic array properly using wrapper pattern
            if (stmt_count > 0) then
                ! Create the wrapper array - this works now!
                allocate(prog%body(stmt_count))
                
                ! Copy each wrapper directly
                do i = 1, stmt_count
                    allocate(prog%body(i)%node, source=body_statements(i)%node)
                end do
                
                deallocate(body_statements)
            end if
        end select
    end subroutine parse_tokens

    ! Find program unit boundary (function/subroutine/module spans multiple lines)
    subroutine find_program_unit_boundary(tokens, start_pos, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: unit_start, unit_end
        
        integer :: i, current_line, nesting_level
        logical :: in_function, in_subroutine, in_module, in_do_loop, in_select_case
        
        unit_start = start_pos
        unit_end = start_pos
        in_function = .false.
        in_subroutine = .false.
        in_module = .false.
        in_do_loop = .false.
        in_select_case = .false.
        nesting_level = 0
        
        ! Check if starting token indicates a multi-line construct
        if (start_pos <= size(tokens)) then
            ! Look for function definition patterns
            if (is_function_start(tokens, start_pos)) then
                in_function = .true.
                nesting_level = 1
                call log_verbose("parsing", "Starting function at token " // &
                                trim(adjustl(int_to_str(start_pos))))
            else if (is_subroutine_start(tokens, start_pos)) then
                in_subroutine = .true.
                nesting_level = 1
            else if (is_module_start(tokens, start_pos)) then
                in_module = .true.
                nesting_level = 1
            else if (is_do_while_start(tokens, start_pos)) then
                in_do_loop = .true.
                nesting_level = 1
            else if (is_do_loop_start(tokens, start_pos)) then
                in_do_loop = .true.
                nesting_level = 1
                ! Boundary detection: found do loop at token
            else if (is_select_case_start(tokens, start_pos)) then
                in_select_case = .true.
                nesting_level = 1
            end if
        end if
        
        ! If this is a multi-line construct, find the end
        if (in_function .or. in_subroutine .or. in_module .or. in_do_loop .or. in_select_case) then
            i = start_pos
            do while (i <= size(tokens) .and. nesting_level > 0)
                if (tokens(i)%kind == TK_EOF) exit
                
                ! Check for nested constructs (but skip the first one at start_pos)
                if (i /= start_pos) then
                    if (in_function .and. is_function_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                        call log_verbose("parsing", "Found nested function at token " // &
                                        trim(adjustl(int_to_str(i))) // ", nesting level now: " // &
                                        trim(adjustl(int_to_str(nesting_level))))
                    else if (in_subroutine .and. is_subroutine_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    else if (in_module .and. is_module_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    else if (in_do_loop .and. is_do_loop_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    else if (in_select_case .and. is_select_case_start(tokens, i)) then
                        nesting_level = nesting_level + 1
                    end if
                end if
                
                ! Check for end constructs
                if (in_function .and. is_end_function(tokens, i)) then
                    nesting_level = nesting_level - 1
                    call log_verbose("parsing", "Found END FUNCTION, nesting level now: " // &
                                    trim(adjustl(int_to_str(nesting_level))))
                else if (in_subroutine .and. is_end_subroutine(tokens, i)) then
                    nesting_level = nesting_level - 1
                else if (in_module .and. is_end_module(tokens, i)) then
                    nesting_level = nesting_level - 1
                else if (in_do_loop .and. is_end_do(tokens, i)) then
                    nesting_level = nesting_level - 1
                    unit_end = i + 1  ! Include both "end" and "do" tokens
                    i = i + 2  ! Skip both "end" and "do" tokens
                else if (in_select_case .and. is_end_select(tokens, i)) then
                    nesting_level = nesting_level - 1
                    unit_end = i + 1  ! Include both "end" and "select" tokens
                    i = i + 2  ! Skip both "end" and "select" tokens
                else
                    unit_end = i
                    i = i + 1
                end if
                
                ! Stop when we've closed all nested constructs
                if (nesting_level == 0) exit
            end do
        else
            ! Single line construct - find end of current line
            current_line = tokens(start_pos)%line
            i = start_pos
            do while (i <= size(tokens) .and. tokens(i)%line == current_line)
                unit_end = i
                i = i + 1
            end do
            
            ! Skip empty lines (single EOF token on its own line)
            if (unit_end == unit_start .and. tokens(unit_start)%kind == TK_EOF) then
                unit_end = unit_start - 1  ! Signal to skip this unit
            end if
            
            ! Skip single "real", "integer", etc. that are part of function definitions
            if (unit_end == unit_start .and. start_pos < size(tokens) .and. &
                tokens(start_pos)%kind == TK_KEYWORD .and. &
                (tokens(start_pos)%text == "real" .or. tokens(start_pos)%text == "integer" .or. &
                 tokens(start_pos)%text == "logical" .or. tokens(start_pos)%text == "character")) then
                ! Check if next token is "function"
                if (start_pos + 1 <= size(tokens) .and. &
                    tokens(start_pos + 1)%kind == TK_KEYWORD .and. &
                    tokens(start_pos + 1)%text == "function") then
                    unit_end = unit_start - 1  ! Signal to skip this unit - it's part of a function def
                end if
            end if
        end if
    end subroutine find_program_unit_boundary

    ! Parse a program unit (function, subroutine, module, or statement)
    function parse_program_unit(tokens) result(unit)
        type(token_t), intent(in) :: tokens(:)
        class(ast_node), allocatable :: unit
        type(parser_state_t) :: parser
        
        ! Note: Parsing program unit
        
        ! Check what type of program unit this is
        if (is_function_start(tokens, 1)) then
            ! Multi-line function definition - use proper parser
            parser = create_parser_state(tokens)
            unit = parse_function_definition(parser)
        else if (is_subroutine_start(tokens, 1)) then
            ! Multi-line subroutine definition - fallback to statement parser for now
            unit = parse_statement(tokens)
        else if (is_module_start(tokens, 1)) then
            ! Multi-line module definition - fallback to statement parser for now
            unit = parse_statement(tokens)
        else if (is_do_while_start(tokens, 1)) then
            ! Multi-line do while loop - use proper parser
            parser = create_parser_state(tokens)
            unit = parse_do_while(parser)
        else if (is_do_loop_start(tokens, 1)) then
            ! Multi-line do loop - use proper parser
            parser = create_parser_state(tokens)
            unit = parse_do_loop(parser)
        else if (is_select_case_start(tokens, 1)) then
            ! Multi-line select case - use proper parser
            parser = create_parser_state(tokens)
            unit = parse_select_case(parser)
        else
            ! Single statement
            unit = parse_statement(tokens)
        end if
    end function parse_program_unit

    ! Check if AST contains function calls (to skip semantic analysis temporarily)
    logical function contains_function_calls(ast_tree)
        class(ast_node), intent(in) :: ast_tree
        integer :: i
        
        contains_function_calls = .false.
        
        select type (prog => ast_tree)
        type is (program_node)
            if (allocated(prog%body)) then
                do i = 1, size(prog%body)
                    if (allocated(prog%body(i)%node)) then
                        if (contains_function_calls_in_node(prog%body(i)%node)) then
                            contains_function_calls = .true.
                            return
                        end if
                    end if
                end do
            end if
        end select
    end function contains_function_calls

    ! Recursive helper to check for function calls in any node
    recursive logical function contains_function_calls_in_node(node) result(has_calls)
        class(ast_node), intent(in) :: node
        
        has_calls = .false.
        
        select type (node)
        type is (function_call_node)
            has_calls = .true.
        type is (assignment_node)
            if (allocated(node%value)) then
                has_calls = contains_function_calls_in_node(node%value)
            end if
        type is (binary_op_node)
            if (allocated(node%left)) then
                has_calls = contains_function_calls_in_node(node%left)
            end if
            if (.not. has_calls .and. allocated(node%right)) then
                has_calls = contains_function_calls_in_node(node%right)
            end if
        end select
    end function contains_function_calls_in_node

    ! Helper functions to detect program unit types
    logical function is_function_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_function_start = .false.
        if (pos > size(tokens)) return
        
        ! Only detect function start at the beginning of a line/statement
        ! Check for "type function" pattern first
        if (tokens(pos)%kind == TK_KEYWORD .and. &
            (tokens(pos)%text == "real" .or. tokens(pos)%text == "integer" .or. &
             tokens(pos)%text == "logical" .or. tokens(pos)%text == "character")) then
            if (pos + 1 <= size(tokens) .and. &
                tokens(pos + 1)%kind == TK_KEYWORD .and. &
                tokens(pos + 1)%text == "function") then
                ! Check if this is at the start of a line or after a statement boundary
                if (pos == 1) then
                    is_function_start = .true.
                else if (pos > 1 .and. tokens(pos-1)%line < tokens(pos)%line) then
                    is_function_start = .true.  ! New line
                else if (pos > 2 .and. tokens(pos-2)%text == "end" .and. &
                         tokens(pos-1)%text == "function") then
                    is_function_start = .true.  ! After "end function"
                end if
            end if
        ! Check for standalone "function" keyword (not preceded by a type)
        else if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "function") then
            ! Make sure this isn't the second part of "type function" or "end function"
            if (pos > 1) then
                if (tokens(pos - 1)%kind == TK_KEYWORD .and. &
                    (tokens(pos - 1)%text == "real" .or. tokens(pos - 1)%text == "integer" .or. &
                     tokens(pos - 1)%text == "logical" .or. tokens(pos - 1)%text == "character" .or. &
                     tokens(pos - 1)%text == "end")) then
                    is_function_start = .false.  ! Already counted with the type or it's "end function"
                else
                    is_function_start = .true.
                end if
            else
                is_function_start = .true.
            end if
        end if
    end function is_function_start

    logical function is_subroutine_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_subroutine_start = .false.
        if (pos > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "subroutine") then
            is_subroutine_start = .true.
        end if
    end function is_subroutine_start

    logical function is_module_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_module_start = .false.
        if (pos > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "module") then
            is_module_start = .true.
        end if
    end function is_module_start

    logical function is_end_function(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_end_function = .false.
        if (pos + 1 > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
            tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "function") then
            is_end_function = .true.
        end if
    end function is_end_function

    logical function is_end_subroutine(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_end_subroutine = .false.
        if (pos + 1 > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
            tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "subroutine") then
            is_end_subroutine = .true.
        end if
    end function is_end_subroutine

    logical function is_end_module(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_end_module = .false.
        if (pos + 1 > size(tokens)) return
        
        if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
            tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "module") then
            is_end_module = .true.
        end if
    end function is_end_module

    ! Phase 4: Code Generation (using FALLBACK until full AST)
    subroutine generate_fortran_code(ast_tree, sem_ctx, code)
        class(ast_node), intent(in) :: ast_tree
        type(semantic_context_t), intent(in) :: sem_ctx
        character(len=:), allocatable, intent(out) :: code
        
        select type (prog => ast_tree)
        type is (program_node)
            code = generate_fortran_program(prog, sem_ctx)
        class default
            code = "! Error: Unsupported AST node type"
        end select
    end subroutine generate_fortran_code

    ! Generate Fortran program (FALLBACK approach until full AST)
    function generate_fortran_program(prog, sem_ctx) result(code)
        type(program_node), intent(in) :: prog
        type(semantic_context_t), intent(in) :: sem_ctx
        character(len=:), allocatable :: code
        character(len=:), allocatable :: use_statements, declarations, statements, functions
        
        ! ARCHITECTURE: AST-based code generation ONLY - NO FALLBACK
        code = generate_code(prog)
    end function generate_fortran_program

    ! Write output to file
    subroutine write_output_file(filename, content, error_msg)
        character(len=*), intent(in) :: filename, content
        character(len=*), intent(out) :: error_msg
        
        integer :: unit, iostat
        
        open(newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot create output file: " // filename
            return
        end if
        
        write(unit, '(A)') content
        close(unit)
        error_msg = ""
    end subroutine write_output_file

    ! Helper function to convert integer to string
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=20) :: str
        write(str, '(I0)') num
    end function int_to_str

    ! Check if token sequence starts a do loop
    logical function is_do_loop_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_do_loop_start = .false.
        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "do") then
                ! Regular do loop (not do while)
                if (pos + 1 <= size(tokens)) then
                    if (tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "while") then
                        is_do_loop_start = .false.  ! It's a do while, not a regular do loop
                        ! Found do while, not do loop
                    else
                        is_do_loop_start = .true.
                        ! Found do loop start
                    end if
                else
                    is_do_loop_start = .true.
                    ! Found do loop start (end of tokens)
                end if
            end if
        end if
    end function is_do_loop_start

    ! Check if token sequence starts a do while loop
    logical function is_do_while_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_do_while_start = .false.
        if (pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "do" .and. &
                tokens(pos + 1)%kind == TK_KEYWORD .and. tokens(pos + 1)%text == "while") then
                is_do_while_start = .true.
            end if
        end if
    end function is_do_while_start

    ! Check if token sequence starts a select case
    logical function is_select_case_start(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_select_case_start = .false.
        if (pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "select" .and. &
                tokens(pos+1)%kind == TK_KEYWORD .and. tokens(pos+1)%text == "case") then
                is_select_case_start = .true.
            end if
        end if
    end function is_select_case_start

    ! Check if token sequence ends a do loop
    logical function is_end_do(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_end_do = .false.
        if (pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
                tokens(pos+1)%kind == TK_KEYWORD .and. tokens(pos+1)%text == "do") then
                is_end_do = .true.
            end if
        end if
    end function is_end_do

    ! Check if token sequence ends a select case
    logical function is_end_select(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        
        is_end_select = .false.
        if (pos <= size(tokens) - 1) then
            if (tokens(pos)%kind == TK_KEYWORD .and. tokens(pos)%text == "end" .and. &
                tokens(pos+1)%kind == TK_KEYWORD .and. tokens(pos+1)%text == "select") then
                is_end_select = .true.
            end if
        end if
    end function is_end_select

end module frontend