program test_frontend_parser_logical_expressions
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for logical expressions
    if (.not. test_logical_constants()) all_passed = .false.
    if (.not. test_logical_and_operator()) all_passed = .false.
    if (.not. test_logical_or_operator()) all_passed = .false.
    if (.not. test_logical_not_operator()) all_passed = .false.
    if (.not. test_logical_precedence()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All logical expression parser tests passed"
        stop 0
    else
        print '(a)', "Some logical expression parser tests failed"
        stop 1
    end if

contains

    logical function test_logical_constants()
        ! TDD Test 1: Parse logical constants
        ! .true., .false.
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_logical_constants = .true.

        print '(a)', "Testing logical constants..."

        ! Test .true.
        call tokenize_core(".true.", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for .true."
            test_logical_constants = .false.
        else
            select type (expr)
            type is (literal_node)
                if (expr%value == ".true.") then
                    print '(a)', "PASS: .true. parsed correctly"
                else
                    print '(a)', "FAIL: Wrong value for .true."
                    test_logical_constants = .false.
                end if
            class default
                print '(a)', "FAIL: Expected literal_node for .true."
                test_logical_constants = .false.
            end select
        end if

        ! Test .false.
        call tokenize_core(".false.", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (literal_node)
                if (expr%value == ".false.") then
                    print '(a)', "PASS: .false. parsed correctly"
                else
                    print '(a)', "FAIL: Wrong value for .false."
                    test_logical_constants = .false.
                end if
            class default
                print '(a)', "FAIL: Expected literal_node for .false."
                test_logical_constants = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for .false."
            test_logical_constants = .false.
        end if

    end function test_logical_constants

    logical function test_logical_and_operator()
        ! TDD Test 2: Parse logical AND operator
        ! .true. .and. .false.
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_logical_and_operator = .true.

        print '(a)', "Testing logical AND operator..."

        ! Test .true. .and. .false.
        call tokenize_core(".true. .and. .false.", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == ".and.") then
                    print '(a)', "PASS: .and. operator parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for .and."
                    test_logical_and_operator = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for .and."
                test_logical_and_operator = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for .and."
            test_logical_and_operator = .false.
        end if

    end function test_logical_and_operator

    logical function test_logical_or_operator()
        ! TDD Test 3: Parse logical OR operator
        ! .true. .or. .false.
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_logical_or_operator = .true.

        print '(a)', "Testing logical OR operator..."

        ! Test .true. .or. .false.
        call tokenize_core(".true. .or. .false.", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == ".or.") then
                    print '(a)', "PASS: .or. operator parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for .or."
                    test_logical_or_operator = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for .or."
                test_logical_or_operator = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for .or."
            test_logical_or_operator = .false.
        end if

    end function test_logical_or_operator

    logical function test_logical_not_operator()
        ! TDD Test 4: Parse logical NOT operator
        ! .not. .true.
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_logical_not_operator = .true.

        print '(a)', "Testing logical NOT operator..."

        ! Test .not. .true.
        call tokenize_core(".not. .true.", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            ! This could be a unary operator or binary operator depending on implementation
            print '(a)', "PASS: .not. operator parsed"
        else
            print '(a)', "FAIL: No AST node returned for .not."
            test_logical_not_operator = .false.
        end if

    end function test_logical_not_operator

    logical function test_logical_precedence()
        ! TDD Test 5: Parse logical expression with precedence
        ! .true. .or. .false. .and. .true.
        ! Should be parsed as .true. .or. (.false. .and. .true.)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_logical_precedence = .true.

        print '(a)', "Testing logical operator precedence..."

        ! Test .true. .or. .false. .and. .true.
        call tokenize_core(".true. .or. .false. .and. .true.", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == ".or.") then
                    ! Right side should be .and. operation
                    select type (right => expr%right)
                    type is (binary_op_node)
                        if (right%operator == ".and.") then
                            print '(a)', "PASS: Logical precedence parsed correctly"
                        else
                            print '(a)', "FAIL: Wrong operator on right side"
                            test_logical_precedence = .false.
                        end if
                    class default
                        print '(a)', "FAIL: Expected binary_op_node on right side"
                        test_logical_precedence = .false.
                    end select
                else
                    print '(a)', "FAIL: Wrong root operator"
                    test_logical_precedence = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for precedence test"
                test_logical_precedence = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for precedence test"
            test_logical_precedence = .false.
        end if

    end function test_logical_precedence

end program test_frontend_parser_logical_expressions
