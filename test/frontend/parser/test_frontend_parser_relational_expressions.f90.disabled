program test_frontend_parser_relational_expressions
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for relational expressions
    if (.not. test_equality_operators()) all_passed = .false.
    if (.not. test_inequality_operators()) all_passed = .false.
    if (.not. test_comparison_operators()) all_passed = .false.
    if (.not. test_mixed_expressions()) all_passed = .false.
    if (.not. test_relational_precedence()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All relational expression parser tests passed"
        stop 0
    else
        print '(a)', "Some relational expression parser tests failed"
        stop 1
    end if

contains

    logical function test_equality_operators()
        ! TDD Test 1: Parse equality operators
        ! a == b, a /= b
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_equality_operators = .true.

        print '(a)', "Testing equality operators..."

        ! Test equality (==)
        call tokenize_core("a == b", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for equality"
            test_equality_operators = .false.
        else
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "==") then
                    print '(a)', "PASS: Equality operator parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for equality"
                    test_equality_operators = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for equality"
                test_equality_operators = .false.
            end select
        end if

        ! Test not equal (/=)
        call tokenize_core("a /= b", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "/=") then
                    print '(a)', "PASS: Not equal operator parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for not equal"
                    test_equality_operators = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for not equal"
                test_equality_operators = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for not equal"
            test_equality_operators = .false.
        end if

    end function test_equality_operators

    logical function test_inequality_operators()
        ! TDD Test 2: Parse inequality operators
        ! a < b, a > b
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_inequality_operators = .true.

        print '(a)', "Testing inequality operators..."

        ! Test less than (<)
        call tokenize_core("a < b", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for less than"
            test_inequality_operators = .false.
        else
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "<") then
                    print '(a)', "PASS: Less than operator parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for less than"
                    test_inequality_operators = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for less than"
                test_inequality_operators = .false.
            end select
        end if

        ! Test greater than (>)
        call tokenize_core("a > b", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == ">") then
                    print '(a)', "PASS: Greater than operator parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for greater than"
                    test_inequality_operators = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for greater than"
                test_inequality_operators = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for greater than"
            test_inequality_operators = .false.
        end if

    end function test_inequality_operators

    logical function test_comparison_operators()
        ! TDD Test 3: Parse comparison operators
        ! a <= b, a >= b
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_comparison_operators = .true.

        print '(a)', "Testing comparison operators..."

        ! Test less than or equal (<=)
        call tokenize_core("a <= b", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for less than or equal"
            test_comparison_operators = .false.
        else
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "<=") then
                    print '(a)', "PASS: Less than or equal operator parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for less than or equal"
                    test_comparison_operators = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for less than or equal"
                test_comparison_operators = .false.
            end select
        end if

        ! Test greater than or equal (>=)
        call tokenize_core("a >= b", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == ">=") then
                    print '(a)', "PASS: Greater than or equal operator parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for greater than or equal"
                    test_comparison_operators = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for greater than or equal"
                test_comparison_operators = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for greater than or equal"
            test_comparison_operators = .false.
        end if

    end function test_comparison_operators

    logical function test_mixed_expressions()
        ! TDD Test 4: Parse mixed relational and arithmetic expressions
        ! a + b == c * d
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_mixed_expressions = .true.

        print '(a)', "Testing mixed expressions..."

        ! Test arithmetic within relational
        call tokenize_core("a + b == c * d", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for mixed expression"
            test_mixed_expressions = .false.
        else
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "==") then
                    ! Check that left side is addition
                    select type (left => expr%left)
                    type is (binary_op_node)
                        if (left%operator == "+") then
                            print '(a)', "PASS: Mixed expression parsed correctly"
                        else
                            print '(a)', "FAIL: Left side should be addition"
                            test_mixed_expressions = .false.
                        end if
                    class default
                        print '(a)', "FAIL: Expected binary_op_node on left side"
                        test_mixed_expressions = .false.
                    end select
                else
                    print '(a)', "FAIL: Wrong root operator for mixed expression"
                    test_mixed_expressions = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for mixed expression"
                test_mixed_expressions = .false.
            end select
        end if

    end function test_mixed_expressions

    logical function test_relational_precedence()
        ! TDD Test 5: Parse relational expressions with precedence
        ! a .and. b == c should be parsed as a .and. (b == c)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_relational_precedence = .true.

        print '(a)', "Testing relational precedence..."

        ! Test logical AND with relational
        call tokenize_core(".true. .and. a == b", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for precedence test"
            test_relational_precedence = .false.
        else
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == ".and.") then
                    ! Right side should be relational expression
                    select type (right => expr%right)
                    type is (binary_op_node)
                        if (right%operator == "==") then
                            print '(a)', "PASS: Relational precedence parsed correctly"
                        else
                            print '(a)', "FAIL: Right side should be equality"
                            test_relational_precedence = .false.
                        end if
                    class default
                        print '(a)', "FAIL: Expected binary_op_node on right side"
                        test_relational_precedence = .false.
                    end select
                else
                    print '(a)', "FAIL: Wrong root operator for precedence test"
                    test_relational_precedence = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for precedence test"
                test_relational_precedence = .false.
            end select
        end if

    end function test_relational_precedence

end program test_frontend_parser_relational_expressions
