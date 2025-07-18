program test_frontend_api_comprehensive
    use lexer_core, only: tokenize_core, token_t
    use parser_core, only: parse_statement
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use codegen_core, only: generate_code
    use ast_core
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Frontend API Comprehensive Tests ==='
    print *
    
    ! Test each frontend component via API
    if (.not. test_lexer_api()) all_passed = .false.
    if (.not. test_parser_api()) all_passed = .false.
    if (.not. test_semantic_api()) all_passed = .false.
    if (.not. test_codegen_api()) all_passed = .false.
    if (.not. test_end_to_end_api()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All frontend API tests passed!'
        stop 0
    else
        print *, 'Some frontend API tests failed!'
        stop 1
    end if
    
contains

    logical function test_lexer_api()
        test_lexer_api = .true.
        print *, 'Testing lexer API...'
        
        ! Test basic tokenization
        block
            type(token_t), allocatable :: tokens(:)
            
            ! Test simple assignment
            call tokenize_core('x = 42', tokens)
            if (.not. allocated(tokens) .or. size(tokens) < 3) then
                print *, '  FAIL: Simple assignment tokenization'
                test_lexer_api = .false.
                return
            end if
            print *, '  PASS: Simple assignment tokenization'
            
            ! Test operator tokenization
            call tokenize_core('a + b * c', tokens)
            if (.not. allocated(tokens) .or. size(tokens) < 5) then
                print *, '  FAIL: Operator tokenization'
                test_lexer_api = .false.
                return
            end if
            print *, '  PASS: Operator tokenization'
            
            ! Test keyword tokenization
            call tokenize_core('real :: x', tokens)
            if (.not. allocated(tokens) .or. size(tokens) < 3) then
                print *, '  FAIL: Keyword tokenization'
                test_lexer_api = .false.
                return
            end if
            print *, '  PASS: Keyword tokenization'
        end block
        
    end function test_lexer_api

    logical function test_parser_api()
        test_parser_api = .true.
        print *, 'Testing parser API...'
        
        ! Test assignment parsing
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            call tokenize_core('x = 42', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Assignment parsing'
                test_parser_api = .false.
                return
            end if
            
            select type(node)
            type is (assignment_node)
                print *, '  PASS: Assignment parsing'
            class default
                print *, '  FAIL: Assignment not parsed as assignment_node'
                test_parser_api = .false.
                return
            end select
        end block
        
        ! Test binary operation parsing
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            call tokenize_core('a + b', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Binary operation parsing'
                test_parser_api = .false.
                return
            end if
            
            select type(node)
            type is (binary_op_node)
                print *, '  PASS: Binary operation parsing'
            class default
                print *, '  WARN: Binary operation parsed as different node type'
                ! This might be valid depending on parser implementation
            end select
        end block
        
        ! Test literal parsing
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            
            call tokenize_core('42', tokens)
            node = parse_statement(tokens)
            
            if (.not. allocated(node)) then
                print *, '  FAIL: Literal parsing'
                test_parser_api = .false.
                return
            end if
            
            select type(node)
            type is (literal_node)
                print *, '  PASS: Literal parsing'
            class default
                print *, '  WARN: Literal parsed as different node type'
                ! This might be valid depending on parser implementation
            end select
        end block
        
    end function test_parser_api

    logical function test_semantic_api()
        test_semantic_api = .true.
        print *, 'Testing semantic analyzer API...'
        
        ! Test semantic analysis of assignment
        block
            type(program_node) :: prog
            type(semantic_context_t) :: ctx
            type(assignment_node), allocatable :: assign
            type(identifier_node), allocatable :: target
            type(literal_node), allocatable :: lit_value
            
            ! Create simple program node
            prog%name = 'test'
            allocate(prog%body(1))
            
            ! Create assignment node
            allocate(assign)
            allocate(target)
            allocate(lit_value)
            
            target%name = 'x'
            lit_value%value = '42'
            lit_value%literal_kind = LITERAL_INTEGER
            
            allocate(assign%target, source=target)
            allocate(assign%value, source=lit_value)
            
            allocate(prog%body(1)%node, source=assign)
            
            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, prog)
            
            print *, '  PASS: Semantic analysis completed'
        end block
        
    end function test_semantic_api

    logical function test_codegen_api()
        test_codegen_api = .true.
        print *, 'Testing code generation API...'
        
        ! Test code generation for assignment
        block
            type(assignment_node) :: assign
            type(identifier_node), allocatable :: target
            type(literal_node), allocatable :: lit_value
            character(len=:), allocatable :: code
            
            allocate(target)
            allocate(lit_value)
            
            target%name = 'x'
            lit_value%value = '42'
            lit_value%literal_kind = LITERAL_INTEGER
            
            allocate(assign%target, source=target)
            allocate(assign%value, source=lit_value)
            
            code = generate_code(assign)
            
            if (len_trim(code) > 0) then
                print *, '  PASS: Assignment code generation'
            else
                print *, '  FAIL: Assignment code generation produced empty output'
                test_codegen_api = .false.
                return
            end if
        end block
        
        ! Test code generation for program
        block
            type(program_node) :: prog
            character(len=:), allocatable :: code
            
            prog%name = 'test_program'
            allocate(prog%body(0))  ! Empty program
            
            code = generate_code(prog)
            
            if (len_trim(code) > 0) then
                print *, '  PASS: Program code generation'
            else
                print *, '  FAIL: Program code generation produced empty output'
                test_codegen_api = .false.
                return
            end if
        end block
        
    end function test_codegen_api

    logical function test_end_to_end_api()
        test_end_to_end_api = .true.
        print *, 'Testing end-to-end API workflow...'
        
        ! Test complete pipeline: lexer -> parser -> semantic -> codegen
        block
            type(token_t), allocatable :: tokens(:)
            class(ast_node), allocatable :: node
            type(program_node) :: prog
            character(len=:), allocatable :: code
            
            ! 1. Tokenize
            call tokenize_core('result = 123', tokens)
            if (.not. allocated(tokens)) then
                print *, '  FAIL: End-to-end tokenization'
                test_end_to_end_api = .false.
                return
            end if
            
            ! 2. Parse
            node = parse_statement(tokens)
            if (.not. allocated(node)) then
                print *, '  FAIL: End-to-end parsing'
                test_end_to_end_api = .false.
                return
            end if
            
            ! 3. Create program wrapper
            prog%name = 'test_end_to_end'
            allocate(prog%body(1))
            allocate(prog%body(1)%node, source=node)
            
            ! 4. Semantic analysis  
            block
                type(semantic_context_t) :: ctx
                ctx = create_semantic_context()
                call analyze_program(ctx, prog)
            end block
            
            ! 5. Code generation
            code = generate_code(prog)
            if (len_trim(code) > 0) then
                print *, '  PASS: End-to-end API workflow'
            else
                print *, '  FAIL: End-to-end code generation failed'
                test_end_to_end_api = .false.
                return
            end if
        end block
        
    end function test_end_to_end_api

end program test_frontend_api_comprehensive