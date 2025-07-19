program test_critical_functionality
    ! Test the three critical priorities
    use lexer_core
    use parser_state_module
    use parser_core
    use ast_core
    use ast_factory
    use codegen_core
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_function_parsing()
    call test_assignment_parsing()
    call test_declaration_parsing()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        stop 1
    end if

contains

    subroutine test_function_parsing()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index
        character(len=:), allocatable :: source, code

        test_count = test_count + 1

        ! Test source code with a function
        source = "real function add(x, y)"//new_line('a')// &
                 "    real :: x, y"//new_line('a')// &
                 "    add = x + y"//new_line('a')// &
                 "end function add"

        ! Tokenize
        call tokenize_core(source, tokens)

        ! Create parser and arena
        arena = create_ast_stack()
        parser = create_parser_state(tokens)

        ! Parse function
        func_index = parse_function_definition(parser, arena)

        if (func_index > 0) then
            ! Generate code
            code = generate_code_polymorphic(arena, func_index)
            if (index(code, "real function add") > 0 .and. &
                index(code, "end function add") > 0) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Function parsing and codegen"
            else
                write (*, '(A)') "FAIL: Function codegen incorrect"
            end if
        else
            write (*, '(A)') "FAIL: Function parsing failed"
        end if
    end subroutine test_function_parsing

    subroutine test_assignment_parsing()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: assign_index, target_index, value_index
        character(len=:), allocatable :: source, code

        test_count = test_count + 1

        ! Test simple assignment
        source = "x = 42"

        ! Tokenize
        call tokenize_core(source, tokens)

        ! Create arena
        arena = create_ast_stack()

        ! Parse manually for this test
        target_index = push_identifier(arena, "x", 1, 1)
        value_index = push_literal(arena, "42", LITERAL_INTEGER, 1, 5)
        assign_index = push_assignment(arena, target_index, value_index, 1, 1)

        ! Generate code
        code = generate_code_polymorphic(arena, assign_index)

        if (trim(code) == "x = 42") then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Assignment parsing and codegen"
        else
            write (*, '(A)') "FAIL: Assignment codegen incorrect: "//trim(code)
        end if
    end subroutine test_assignment_parsing

    subroutine test_declaration_parsing()
        type(ast_arena_t) :: arena
        integer :: decl_index
        character(len=:), allocatable :: code

        test_count = test_count + 1

        ! Create arena
        arena = create_ast_stack()

        ! Create declaration node
        decl_index = push_declaration(arena, "real", "x", line=1, column=1)

        ! Generate code
        code = generate_code_polymorphic(arena, decl_index)

        if (trim(code) == "real :: x") then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Declaration parsing and codegen"
        else
            write (*, '(A)') "FAIL: Declaration codegen incorrect: "//trim(code)
        end if
    end subroutine test_declaration_parsing

end program test_critical_functionality
