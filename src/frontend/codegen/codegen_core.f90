module codegen_core
    use ast_core
    use type_system_hm
    use fpm_strings, only: string_t
    use codegen_indent
    implicit none
    private

    ! Context for function indentation
    logical :: context_has_executable_before_contains = .false.

    ! Public interface for code generation
    public :: generate_code_from_arena, generate_code_polymorphic

contains

    ! Generate code from AST arena
    function generate_code_from_arena(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = ""
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (literal_node)
            code = generate_code_literal(node)
        type is (identifier_node)
            code = generate_code_identifier(node)
        type is (assignment_node)
            code = generate_code_assignment(arena, node, node_index)
        type is (binary_op_node)
            code = generate_code_binary_op(arena, node, node_index)
        type is (program_node)
            code = generate_code_program(arena, node, node_index)
        type is (call_or_subscript_node)
            code = generate_code_call_or_subscript(arena, node, node_index)
        type is (subroutine_call_node)
            code = generate_code_subroutine_call(arena, node, node_index)
        type is (function_def_node)
            code = generate_code_function_def(arena, node, node_index)
        type is (subroutine_def_node)
            code = generate_code_subroutine_def(arena, node, node_index)
        type is (print_statement_node)
            code = generate_code_print_statement(arena, node, node_index)
        type is (declaration_node)
            code = generate_code_declaration(arena, node, node_index)
        type is (if_node)
            code = generate_code_if(arena, node, node_index)
        type is (do_loop_node)
            code = generate_code_do_loop(arena, node, node_index)
        type is (do_while_node)
            code = generate_code_do_while(arena, node, node_index)
        type is (select_case_node)
            code = generate_code_select_case(arena, node, node_index)
        type is (use_statement_node)
            code = generate_code_use_statement(node)
        type is (contains_node)
            code = "contains"
        class default
            code = "! Unknown node type"
        end select
    end function generate_code_from_arena

    ! Generate code for literal node
    function generate_code_literal(node) result(code)
        type(literal_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Return the literal value with proper formatting
        select case (node%literal_kind)
        case (LITERAL_STRING)
            ! Special case for implicit none
            if (node%value == "implicit none") then
                code = "implicit none"
                ! Special case for comments (strings starting with "!")
            else if (len(node%value) > 0 .and. node%value(1:1) == "!") then
                code = node%value  ! Keep comments as-is, no quotes
                ! String literals need quotes if not already present
            else if (len_trim(node%value) == 0) then
                code = ""  ! Skip empty literals (parser placeholders)
            else if (len(node%value) > 0 .and. node%value(1:1) /= '"' .and. &
                     node%value(1:1) /= "'") then
                code = '"'//node%value//'"'
            else
                code = node%value
            end if
        case (LITERAL_REAL)
            ! For real literals, ensure double precision by adding 'd0' suffix if needed
            if (index(node%value, 'd') == 0 .and. index(node%value, 'D') == 0 .and. &
                index(node%value, '_') == 0) then
                code = node%value//"d0"
            else
                code = node%value
            end if
        case default
            ! Handle invalid/empty literals safely
            if (allocated(node%value) .and. len_trim(node%value) > 0) then
                code = node%value
            else
                code = "! Invalid literal node"
            end if
        end select
    end function generate_code_literal

    ! Generate code for identifier node
    function generate_code_identifier(node) result(code)
        type(identifier_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Simply return the identifier name
        code = node%name
    end function generate_code_identifier

    ! Generate code for assignment node
    function generate_code_assignment(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: target_code, value_code

        ! Generate code for target
        if (node%target_index > 0 .and. node%target_index <= arena%size) then
            target_code = generate_code_from_arena(arena, node%target_index)
        else
            target_code = "???"
        end if

        ! Generate code for value
        if (node%value_index > 0 .and. node%value_index <= arena%size) then
            value_code = generate_code_from_arena(arena, node%value_index)
        else
            value_code = "???"
        end if

        ! Combine target and value
        code = target_code//" = "//value_code
    end function generate_code_assignment

    ! Generate code for binary operation node
    function generate_code_binary_op(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code

        ! Generate code for operands
        if (node%left_index > 0 .and. node%left_index <= arena%size) then
            left_code = generate_code_from_arena(arena, node%left_index)
        else
            left_code = "???"
        end if

        if (node%right_index > 0 .and. node%right_index <= arena%size) then
            right_code = generate_code_from_arena(arena, node%right_index)
        else
            right_code = "???"
        end if

        ! Combine with operator - always use spaces for standard Fortran
        code = left_code//" "//node%operator//" "//right_code
    end function generate_code_binary_op

    ! Generate code for program node
    function generate_code_program(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: use_statements, declarations, exec_statements
        integer, allocatable :: child_indices(:)
        integer :: i
        character(len=:), allocatable :: stmt_code
    logical :: has_declarations, has_executable, has_implicit_none, has_var_declarations

        ! Reset indentation for top-level program
        call reset_indent()

        ! Initialize sections
        use_statements = ""
        declarations = ""
        exec_statements = ""
        has_declarations = .false.
        has_executable = .false.
        has_implicit_none = .false.
        has_var_declarations = .false.

        ! Use body_indices from program_node, not get_children
        if (allocated(node%body_indices)) then
            allocate (child_indices(size(node%body_indices)))
            child_indices = node%body_indices
        else
            allocate (child_indices(0))
        end if

        ! Process all statements, separating by type
        do i = 1, size(child_indices)
            if (child_indices(i) > 0 .and. child_indices(i) <= arena%size) then
                if (allocated(arena%entries(child_indices(i))%node)) then
                    select type (child_node => arena%entries(child_indices(i))%node)
                    type is (use_statement_node)
                        if (len(use_statements) > 0) then
                            use_statements = use_statements//new_line('A')
                        end if
                use_statements = use_statements//generate_code_use_statement(child_node)
                    type is (literal_node)
                        ! Check for implicit none
                        if (child_node%value == "implicit none") then
                            if (len(declarations) > 0) then
                                declarations = declarations//new_line('A')
                            end if
                            declarations = declarations//"implicit none"
                            has_declarations = .true.
                            has_implicit_none = .true.
                        else
                            ! Other literals go to executable section
                           stmt_code = generate_code_from_arena(arena, child_indices(i))
                            if (len(stmt_code) > 0) then
                                if (len(exec_statements) > 0) then
                                    exec_statements = exec_statements//new_line('A')
                                end if
                                exec_statements = exec_statements//stmt_code
                                has_executable = .true.
                            end if
                        end if
                    type is (declaration_node)
                        stmt_code = generate_code_from_arena(arena, child_indices(i))
                        if (len(stmt_code) > 0) then
                            if (len(declarations) > 0) then
                                declarations = declarations//new_line('A')
                            end if
                            declarations = declarations//stmt_code
                            has_declarations = .true.
                            has_var_declarations = .true.
                        end if
                    type is (contains_node)
                        ! Handle contains specially - it goes between exec and functions
                        ! We'll process it later, not here
                        continue
                    type is (function_def_node)
                        ! Functions go after contains, not in executable section
                        continue
                    type is (subroutine_def_node)
                        ! Subroutines go after contains, not in executable section
                        continue
                    class default
                        ! All other statements are executable
                        stmt_code = generate_code_from_arena(arena, child_indices(i))
                        if (len(stmt_code) > 0) then
                            if (len(exec_statements) > 0) then
                                exec_statements = exec_statements//new_line('A')
                            end if
                            exec_statements = exec_statements//stmt_code
                            has_executable = .true.
                        end if
                    end select
                end if
            end if
        end do

        ! Combine everything with proper spacing
        code = "program "//node%name//new_line('A')
        call increase_indent()

        if (len(use_statements) > 0) then
            code = code//indent_lines(use_statements)//new_line('A')
        end if

        if (len(declarations) > 0) then
            code = code//indent_lines(declarations)//new_line('A')
            ! Add blank line after declarations only if we have variable declarations and executable statements
            if (has_var_declarations .and. has_executable) then
                code = code//new_line('A')
            end if
        end if

        if (len(exec_statements) > 0) then
            code = code//indent_lines(exec_statements)//new_line('A')
        end if

        ! Check for contains and functions/subroutines
        block
            logical :: has_contains, has_subprograms
            integer :: j

            has_contains = .false.
            has_subprograms = .false.

            ! Check if we have contains or subprograms
            do j = 1, size(child_indices)
                if (child_indices(j) > 0 .and. child_indices(j) <= arena%size) then
                    if (allocated(arena%entries(child_indices(j))%node)) then
                        select type (child_node => arena%entries(child_indices(j))%node)
                        type is (contains_node)
                            has_contains = .true.
                        type is (function_def_node)
                            has_subprograms = .true.
                        type is (subroutine_def_node)
                            has_subprograms = .true.
                        end select
                    end if
                end if
            end do

            ! Generate contains and subprograms
            if (has_contains .or. has_subprograms) then
                code = code//"contains"//new_line('A')

                ! Generate functions and subroutines
                do j = 1, size(child_indices)
                    if (child_indices(j) > 0 .and. child_indices(j) <= arena%size) then
                        if (allocated(arena%entries(child_indices(j))%node)) then
                        select type (child_node => arena%entries(child_indices(j))%node)
                            type is (function_def_node)
                           stmt_code = generate_code_from_arena(arena, child_indices(j))
                                ! Indent function lines
                                code = code//indent_lines(stmt_code)//new_line('A')
                            type is (subroutine_def_node)
                           stmt_code = generate_code_from_arena(arena, child_indices(j))
                                ! Indent subroutine lines
                                code = code//indent_lines(stmt_code)//new_line('A')
                            end select
                        end if
                    end if
                end do
            end if
        end block

        call decrease_indent()
        code = code//"end program "//node%name
    end function generate_code_program

    ! Generate code for call_or_subscript node (handles both function calls and array indexing)
    function generate_code_call_or_subscript(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        ! Generate arguments
        args_code = ""
        if (allocated(node%arg_indices)) then
            do i = 1, size(node%arg_indices)
                if (len(args_code) > 0) then
                    args_code = args_code//", "
                end if
                if (node%arg_indices(i) > 0) then
             args_code = args_code//generate_code_from_arena(arena, node%arg_indices(i))
                end if
            end do
        end if

        ! Combine function name and arguments
        code = node%name//"("//args_code//")"
    end function generate_code_call_or_subscript

    ! Generate code for subroutine call node
    function generate_code_subroutine_call(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_call_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        ! Generate arguments
        args_code = ""
        if (allocated(node%arg_indices)) then
            do i = 1, size(node%arg_indices)
                if (len(args_code) > 0) then
                    args_code = args_code//", "
                end if
                if (node%arg_indices(i) > 0) then
             args_code = args_code//generate_code_from_arena(arena, node%arg_indices(i))
                end if
            end do
        end if

        ! Generate call statement
        if (len(args_code) > 0) then
            code = "call "//node%name//"("//args_code//")"
        else
            code = "call "//node%name
        end if
    end function generate_code_subroutine_call

    ! Polymorphic code generation interface
    function generate_code_polymorphic(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = generate_code_from_arena(arena, node_index)
    end function generate_code_polymorphic

    ! Generate code for function definition
    function generate_code_function_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: return_type_code, params_code, body_code
        integer :: i

        ! Start function definition with return type
        if (allocated(node%return_type) .and. len_trim(node%return_type) > 0) then
            code = node%return_type//" function "//node%name
        else
            code = "function "//node%name
        end if

        ! Generate parameters
        if (allocated(node%param_indices) .and. size(node%param_indices) > 0) then
            code = code//"("
            do i = 1, size(node%param_indices)
                if (i > 1) code = code//", "
           if (node%param_indices(i) > 0 .and. node%param_indices(i) <= arena%size) then
                    params_code = generate_code_from_arena(arena, node%param_indices(i))
                    code = code//params_code
                end if
            end do
            code = code//")"
        else
            code = code//"()"
        end if
        code = code//new_line('a')

        ! Generate body with indentation
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
             if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                    body_code = generate_code_from_arena(arena, node%body_indices(i))
                    ! Add 4 spaces of indentation for function body
                    code = code//"    "//body_code//new_line('a')
                end if
            end do
        end if

        ! End function
        code = code//"end function "//node%name
    end function generate_code_function_def

    ! Generate code for subroutine definition
    function generate_code_subroutine_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: params_code, body_code
        integer :: i

        ! Start subroutine definition
        code = "subroutine "//node%name

        ! Generate parameters
        if (allocated(node%param_indices) .and. size(node%param_indices) > 0) then
            code = code//"("
            do i = 1, size(node%param_indices)
                if (i > 1) code = code//", "
           if (node%param_indices(i) > 0 .and. node%param_indices(i) <= arena%size) then
                    params_code = generate_code_from_arena(arena, node%param_indices(i))
                    code = code//params_code
                end if
            end do
            code = code//")"
        else
            code = code//"()"
        end if
        code = code//new_line('a')
        ! Generate body with indentation
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
             if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                    body_code = generate_code_from_arena(arena, node%body_indices(i))
                    ! Add 4 spaces of indentation for subroutine body
                    code = code//"    "//body_code//new_line('a')
                end if
            end do
        end if

        ! End subroutine
        code = code//"end subroutine "//node%name
    end function generate_code_subroutine_def

    ! Generate code for print statement
    function generate_code_print_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(print_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: arg_code
        integer :: i

        ! Start print statement
        code = "print "//node%format_spec

        ! Generate arguments
        if (allocated(node%arg_indices) .and. size(node%arg_indices) > 0) then
            do i = 1, size(node%arg_indices)
                code = code//", "
               if (node%arg_indices(i) > 0 .and. node%arg_indices(i) <= arena%size) then
                    arg_code = generate_code_from_arena(arena, node%arg_indices(i))
                    code = code//arg_code
                end if
            end do
        end if
    end function generate_code_print_statement

    ! Generate code for declaration
    function generate_code_declaration(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: init_code
        integer :: i

        ! Generate basic declaration
        code = node%type_name

        ! Add kind if present
        if (node%has_kind) then
            code = code//"("//trim(adjustl(int_to_string(node%kind_value)))//")"
        end if

        ! Add intent if present
        if (node%has_intent .and. allocated(node%intent)) then
            code = code//", intent("//node%intent//")"
        end if

        code = code//" :: "//node%var_name

        ! Add array dimensions if present
        if (node%is_array .and. allocated(node%dimension_indices)) then
            ! Generate dimension expressions
            code = code//"("
            do i = 1, size(node%dimension_indices)
                if (i > 1) code = code//","
   if (node%dimension_indices(i) > 0 .and. node%dimension_indices(i) <= arena%size) then
                 code = code//generate_code_from_arena(arena, node%dimension_indices(i))
                else
                    code = code//":"  ! Default for unspecified dimensions
                end if
            end do
            code = code//")"
        end if

        ! Add initializer if present
        if (node%initializer_index > 0 .and. node%initializer_index <= arena%size) then
            init_code = generate_code_from_arena(arena, node%initializer_index)
            code = code//" = "//init_code
        end if
    end function generate_code_declaration

    ! Generate code for if statement
    function generate_code_if(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: cond_code, body_code
        integer :: i

        ! Generate if condition
        if (node%condition_index > 0 .and. node%condition_index <= arena%size) then
            cond_code = generate_code_from_arena(arena, node%condition_index)
        else
            cond_code = ".true."
        end if

        code = with_indent("if ("//cond_code//") then")//new_line('a')
        call increase_indent()

        ! Generate then body
        if (allocated(node%then_body_indices)) then
            do i = 1, size(node%then_body_indices)
   if (node%then_body_indices(i) > 0 .and. node%then_body_indices(i) <= arena%size) then
                  body_code = generate_code_from_arena(arena, node%then_body_indices(i))
                    code = code//with_indent(body_code)//new_line('a')
                end if
            end do
        end if

        ! Generate elseif blocks
        if (allocated(node%elseif_blocks)) then
            do i = 1, size(node%elseif_blocks)
                ! Generate elseif condition
                if (node%elseif_blocks(i)%condition_index > 0 .and. &
                    node%elseif_blocks(i)%condition_index <= arena%size) then
      cond_code = generate_code_from_arena(arena, node%elseif_blocks(i)%condition_index)
                else
                    cond_code = ".true."
                end if

                call decrease_indent()
               code = code//with_indent("else if ("//cond_code//") then")//new_line('a')
                call increase_indent()

                ! Generate elseif body
                if (allocated(node%elseif_blocks(i)%body_indices)) then
                    block
                        integer :: j
                        do j = 1, size(node%elseif_blocks(i)%body_indices)
                            if (node%elseif_blocks(i)%body_indices(j) > 0 .and. &
                               node%elseif_blocks(i)%body_indices(j) <= arena%size) then
      body_code = generate_code_from_arena(arena, node%elseif_blocks(i)%body_indices(j))
                                code = code//with_indent(body_code)//new_line('a')
                            end if
                        end do
                    end block
                end if
            end do
        end if

        ! Generate else block
        if (allocated(node%else_body_indices)) then
            call decrease_indent()
            code = code//with_indent("else")//new_line('a')
            call increase_indent()
            do i = 1, size(node%else_body_indices)
   if (node%else_body_indices(i) > 0 .and. node%else_body_indices(i) <= arena%size) then
                  body_code = generate_code_from_arena(arena, node%else_body_indices(i))
                    code = code//with_indent(body_code)//new_line('a')
                end if
            end do
        end if

        call decrease_indent()
        code = code//with_indent("end if")
    end function generate_code_if

    ! Generate code for do loop
    function generate_code_do_loop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: start_code, end_code, step_code, body_code
        integer :: i

        ! Generate loop variable and bounds
        code = with_indent("do "//node%var_name//" = ")

        if (node%start_expr_index > 0 .and. node%start_expr_index <= arena%size) then
            start_code = generate_code_from_arena(arena, node%start_expr_index)
            code = code//start_code
        else
            code = code//"1"
        end if

        code = code//", "

        if (node%end_expr_index > 0 .and. node%end_expr_index <= arena%size) then
            end_code = generate_code_from_arena(arena, node%end_expr_index)
            code = code//end_code
        else
            code = code//"1"
        end if

        if (node%step_expr_index > 0 .and. node%step_expr_index <= arena%size) then
            step_code = generate_code_from_arena(arena, node%step_expr_index)
            code = code//", "//step_code
        end if

        code = code//new_line('a')
        call increase_indent()

        ! Generate body
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
             if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                    body_code = generate_code_from_arena(arena, node%body_indices(i))
                    code = code//with_indent(body_code)//new_line('a')
                end if
            end do
        end if

        call decrease_indent()
        code = code//"end do"
    end function generate_code_do_loop

    ! Generate code for do while loop
    function generate_code_do_while(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_while_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: cond_code, body_code
        integer :: i

        ! Generate condition
        if (node%condition_index > 0 .and. node%condition_index <= arena%size) then
            cond_code = generate_code_from_arena(arena, node%condition_index)
        else
            cond_code = ".true."
        end if

        code = with_indent("do while ("//cond_code//")")//new_line('a')
        call increase_indent()

        ! Generate body
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
             if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                    body_code = generate_code_from_arena(arena, node%body_indices(i))
                    code = code//with_indent(body_code)//new_line('a')
                end if
            end do
        end if

        call decrease_indent()
        code = code//"end do"
    end function generate_code_do_while

    ! Generate code for select case
    function generate_code_select_case(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(select_case_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, case_code, body_code
        integer :: i, j

        ! Generate select expression
        if (allocated(node%expr)) then
            ! Generate code for the expression
            select type (expr => node%expr)
            type is (identifier_node)
                expr_code = expr%name
            type is (literal_node)
                expr_code = generate_code_literal(expr)
            class default
                expr_code = "???"
            end select
        else
            expr_code = "???"
        end if

        code = with_indent("select case ("//expr_code//")")//new_line('a')
        call increase_indent()

        ! Generate case blocks
        if (allocated(node%cases)) then
            do i = 1, size(node%cases)
                ! Generate case statement
                if (allocated(node%cases(i)%case_type)) then
                    if (node%cases(i)%case_type == "case_default") then
                        code = code//with_indent("case default")//new_line('a')
                    else
                        ! Generate case value
                        if (allocated(node%cases(i)%value)) then
                            select type (value => node%cases(i)%value)
                            type is (identifier_node)
                                case_code = value%name
                            type is (literal_node)
                                case_code = generate_code_literal(value)
                            class default
                                case_code = "???"
                            end select
                        else
                            case_code = "???"
                        end if
                       code = code//with_indent("case ("//case_code//")")//new_line('a')
                    end if
                end if

                call increase_indent()
                ! Generate case body
                if (allocated(node%cases(i)%body)) then
                    do j = 1, size(node%cases(i)%body)
                        if (allocated(node%cases(i)%body(j)%node)) then
                            select type (stmt => node%cases(i)%body(j)%node)
                            type is (identifier_node)
                                body_code = stmt%name
                            type is (literal_node)
                                body_code = generate_code_literal(stmt)
                            type is (assignment_node)
                                ! Find arena index for this assignment node
                                block
                                    integer :: arena_index
                                    arena_index = find_node_index_in_arena(arena, stmt)
                                    if (arena_index > 0) then
                                body_code = generate_code_from_arena(arena, arena_index)
                                    else
                                        body_code = "! unknown assignment"
                                    end if
                                end block
                            class default
                                body_code = "! unknown case body"
                            end select
                            code = code//with_indent(body_code)//new_line('a')
                        end if
                    end do
                end if
                call decrease_indent()
            end do
        end if

        call decrease_indent()
        code = code//"end select"
    end function generate_code_select_case

    ! Generate code for use statement
    function generate_code_use_statement(node) result(code)
        type(use_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        logical :: first_item

        code = "use "//trim(node%module_name)

        if (node%has_only) then
            code = code//", only: "
            first_item = .true.

            ! Add rename list items first
            if (allocated(node%rename_list) .and. size(node%rename_list) > 0) then
                do i = 1, size(node%rename_list)
                    if (allocated(node%rename_list(i)%s)) then
                        if (len_trim(node%rename_list(i)%s) > 0) then
                            if (.not. first_item) code = code//", "
                            code = code//trim(adjustl(node%rename_list(i)%s))
                            first_item = .false.
                        end if
                    end if
                end do
            end if

            ! Add only list items
            if (allocated(node%only_list)) then
                do i = 1, size(node%only_list)
                    if (allocated(node%only_list(i)%s)) then
                        if (len_trim(node%only_list(i)%s) > 0) then
                            if (.not. first_item) code = code//", "
                            code = code//trim(adjustl(node%only_list(i)%s))
                            first_item = .false.
                        end if
                    end if
                end do
            end if
        end if
    end function generate_code_use_statement

    ! Helper function to convert integer to string
    function int_to_string(num) result(str)
        integer, intent(in) :: num
        character(len=20) :: str
        write (str, '(I0)') num
    end function int_to_string

    ! Helper function to find a node's index in the arena
    function find_node_index_in_arena(arena, target_node) result(index)
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: target_node
        integer :: index
        integer :: i

        index = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (same_node(arena%entries(i)%node, target_node)) then
                    index = i
                    return
                end if
            end if
        end do
    end function find_node_index_in_arena

    ! Helper function to compare if two nodes are the same
    function same_node(node1, node2) result(is_same)
        class(ast_node), intent(in) :: node1, node2
        logical :: is_same

        is_same = .false.

        select type (n1 => node1)
        type is (assignment_node)
            select type (n2 => node2)
            type is (assignment_node)
                ! Compare assignment nodes by their target and value indices
                is_same = (n1%target_index == n2%target_index .and. &
                           n1%value_index == n2%value_index)
            end select
        type is (identifier_node)
            select type (n2 => node2)
            type is (identifier_node)
                is_same = (n1%name == n2%name)
            end select
        type is (literal_node)
            select type (n2 => node2)
            type is (literal_node)
               is_same = (n1%value == n2%value .and. n1%literal_kind == n2%literal_kind)
            end select
        end select
    end function same_node

end module codegen_core
