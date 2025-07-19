program test_parser_dispatcher
    ! TDD test for parser dispatcher functionality
    use lexer_core
    use ast_core
    use parser_dispatcher_module
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Test the dispatcher handles various statement types
    if (.not. test_dispatcher_assignment()) all_passed = .false.
    if (.not. test_dispatcher_if_statement()) all_passed = .false.
    if (.not. test_dispatcher_do_loop()) all_passed = .false.
    if (.not. test_dispatcher_select_case()) all_passed = .false.
    if (.not. test_dispatcher_use_statement()) all_passed = .false.
    if (.not. test_dispatcher_function_def()) all_passed = .false.
    if (.not. test_dispatcher_declaration()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All parser dispatcher tests passed"
        stop 0
    else
        print '(a)', "Some parser dispatcher tests failed"
        stop 1
    end if

contains

    logical function test_dispatcher_assignment()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_dispatcher_assignment = .true.

        print '(a)', "Testing dispatcher with assignment..."

        call tokenize_statement("x = 42", tokens)
        stmt = parse_statement_dispatcher(tokens)

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: Dispatcher returned no AST node for assignment"
            test_dispatcher_assignment = .false.
        else
            select type (stmt)
            type is (assignment_node)
                print '(a)', "PASS: Assignment dispatched correctly"
            class default
                print '(a)', "FAIL: Assignment dispatched to wrong node type"
                test_dispatcher_assignment = .false.
            end select
        end if

    end function test_dispatcher_assignment

    logical function test_dispatcher_if_statement()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_dispatcher_if_statement = .true.

        print '(a)', "Testing dispatcher with if statement..."

        call tokenize_statement("if (x > 0) then", tokens)
        stmt = parse_statement_dispatcher(tokens)

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: Dispatcher returned no AST node for if statement"
            test_dispatcher_if_statement = .false.
        else
            select type (stmt)
            type is (if_node)
                print '(a)', "PASS: If statement dispatched correctly"
            class default
                print '(a)', "FAIL: If statement dispatched to wrong node type"
                test_dispatcher_if_statement = .false.
            end select
        end if

    end function test_dispatcher_if_statement

    logical function test_dispatcher_do_loop()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_dispatcher_do_loop = .true.

        print '(a)', "Testing dispatcher with do loop..."

        call tokenize_statement("do i = 1, 10", tokens)
        stmt = parse_statement_dispatcher(tokens)

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: Dispatcher returned no AST node for do loop"
            test_dispatcher_do_loop = .false.
        else
            select type (stmt)
            type is (do_loop_node)
                print '(a)', "PASS: Do loop dispatched correctly"
            class default
                print '(a)', "FAIL: Do loop dispatched to wrong node type"
                test_dispatcher_do_loop = .false.
            end select
        end if

    end function test_dispatcher_do_loop

    logical function test_dispatcher_select_case()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_dispatcher_select_case = .true.

        print '(a)', "Testing dispatcher with select case..."

        call tokenize_statement("select case (x)", tokens)
        stmt = parse_statement_dispatcher(tokens)

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: Dispatcher returned no AST node for select case"
            test_dispatcher_select_case = .false.
        else
            select type (stmt)
            type is (select_case_node)
                print '(a)', "PASS: Select case dispatched correctly"
            class default
                print '(a)', "FAIL: Select case dispatched to wrong node type"
                test_dispatcher_select_case = .false.
            end select
        end if

    end function test_dispatcher_select_case

    logical function test_dispatcher_use_statement()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_dispatcher_use_statement = .true.

        print '(a)', "Testing dispatcher with use statement..."

        call tokenize_statement("use mymodule", tokens)
        stmt = parse_statement_dispatcher(tokens)

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: Dispatcher returned no AST node for use statement"
            test_dispatcher_use_statement = .false.
        else
            select type (stmt)
            type is (use_statement_node)
                print '(a)', "PASS: Use statement dispatched correctly"
            class default
                print '(a)', "FAIL: Use statement dispatched to wrong node type"
                test_dispatcher_use_statement = .false.
            end select
        end if

    end function test_dispatcher_use_statement

    logical function test_dispatcher_function_def()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_dispatcher_function_def = .true.

        print '(a)', "Testing dispatcher with function definition..."

        call tokenize_statement("function square(x)", tokens)
        stmt = parse_statement_dispatcher(tokens)

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: Dispatcher returned no AST node for function"
            test_dispatcher_function_def = .false.
        else
            select type (stmt)
            type is (function_def_node)
                print '(a)', "PASS: Function definition dispatched correctly"
            class default
                print '(a)', "FAIL: Function definition dispatched to wrong node type"
                test_dispatcher_function_def = .false.
            end select
        end if

    end function test_dispatcher_function_def

    logical function test_dispatcher_declaration()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_dispatcher_declaration = .true.

        print '(a)', "Testing dispatcher with declaration..."

        call tokenize_statement("real :: x", tokens)
        stmt = parse_statement_dispatcher(tokens)

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: Dispatcher returned no AST node for declaration"
            test_dispatcher_declaration = .false.
        else
            select type (stmt)
            type is (declaration_node)
                print '(a)', "PASS: Declaration dispatched correctly"
            class default
                print '(a)', "FAIL: Declaration dispatched to wrong node type"
                test_dispatcher_declaration = .false.
            end select
        end if

    end function test_dispatcher_declaration

    ! Helper to tokenize a statement and add EOF
    subroutine tokenize_statement(statement, tokens)
        character(len=*), intent(in) :: statement
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(token_t), allocatable :: temp_tokens(:)
        integer :: n

        call tokenize_core(statement, temp_tokens)
        n = size(temp_tokens)
        allocate (tokens(n + 1))
        tokens(1:n) = temp_tokens
        tokens(n + 1)%kind = TK_EOF
        tokens(n + 1)%text = ""
        tokens(n + 1)%line = 1
        tokens(n + 1)%column = len(statement) + 1
    end subroutine tokenize_statement

end program test_parser_dispatcher
