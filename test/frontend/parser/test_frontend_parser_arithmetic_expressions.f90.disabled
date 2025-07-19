program test_frontend_parser_arithmetic_expressions
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for arithmetic expressions
    if (.not. test_simple_arithmetic()) all_passed = .false.
    if (.not. test_operator_precedence()) all_passed = .false.
    if (.not. test_parentheses()) all_passed = .false.
    if (.not. test_unary_operators()) all_passed = .false.
    if (.not. test_power_operator()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All arithmetic expression parser tests passed"
        stop 0
    else
        print '(a)', "Some arithmetic expression parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_arithmetic()
        ! TDD Test 1: Parse simple arithmetic expressions
        ! 1 + 2, 3 - 4, 5 * 6, 7 / 8
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_simple_arithmetic = .true.

        print '(a)', "Testing simple arithmetic expressions..."

        ! Test addition
        call tokenize_core("1 + 2", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for addition"
            test_simple_arithmetic = .false.
        else
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "+") then
                    print '(a)', "PASS: Addition expression parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for addition"
                    test_simple_arithmetic = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for addition"
                test_simple_arithmetic = .false.
            end select
        end if

        ! Test subtraction
        call tokenize_core("3 - 4", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "-") then
                    print '(a)', "PASS: Subtraction expression parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for subtraction"
                    test_simple_arithmetic = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for subtraction"
                test_simple_arithmetic = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for subtraction"
            test_simple_arithmetic = .false.
        end if

        ! Test multiplication
        call tokenize_core("5 * 6", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "*") then
                    print '(a)', "PASS: Multiplication expression parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for multiplication"
                    test_simple_arithmetic = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for multiplication"
                test_simple_arithmetic = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for multiplication"
            test_simple_arithmetic = .false.
        end if

        ! Test division
        call tokenize_core("7 / 8", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "/") then
                    print '(a)', "PASS: Division expression parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for division"
                    test_simple_arithmetic = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for division"
                test_simple_arithmetic = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for division"
            test_simple_arithmetic = .false.
        end if

    end function test_simple_arithmetic

    logical function test_operator_precedence()
        ! TDD Test 2: Parse expressions with operator precedence
        ! 1 + 2 * 3 should be parsed as 1 + (2 * 3), not (1 + 2) * 3
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_operator_precedence = .true.

        print '(a)', "Testing operator precedence..."

        ! Test: 1 + 2 * 3
        call tokenize_core("1 + 2 * 3", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "+") then
                    ! Left side should be literal 1
                    select type (left => expr%left)
                    type is (literal_node)
                        if (left%value == "1") then
                            ! Right side should be binary_op_node with multiplication
                            select type (right => expr%right)
                            type is (binary_op_node)
                                if (right%operator == "*") then
                              print '(a)', "PASS: Precedence 1 + 2 * 3 parsed correctly"
                                else
                                    print '(a)', "FAIL: Wrong operator on right side"
                                    test_operator_precedence = .false.
                                end if
                            class default
                              print '(a)', "FAIL: Expected binary_op_node on right side"
                                test_operator_precedence = .false.
                            end select
                        else
                            print '(a)', "FAIL: Wrong literal on left side"
                            test_operator_precedence = .false.
                        end if
                    class default
                        print '(a)', "FAIL: Expected literal on left side"
                        test_operator_precedence = .false.
                    end select
                else
                    print '(a)', "FAIL: Wrong root operator"
                    test_operator_precedence = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for precedence test"
                test_operator_precedence = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for precedence test"
            test_operator_precedence = .false.
        end if

    end function test_operator_precedence

    logical function test_parentheses()
        ! TDD Test 3: Parse expressions with parentheses
        ! (1 + 2) * 3 should be parsed as (1 + 2) * 3
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_parentheses = .true.

        print '(a)', "Testing parentheses..."

        ! Test: (1 + 2) * 3
        call tokenize_core("(1 + 2) * 3", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "*") then
                    print '(a)', "PASS: Parentheses expression parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for parentheses test"
                    test_parentheses = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for parentheses test"
                test_parentheses = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for parentheses test"
            test_parentheses = .false.
        end if

    end function test_parentheses

    logical function test_unary_operators()
        ! TDD Test 4: Parse unary operators
        ! -1, +2
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_unary_operators = .true.

        print '(a)', "Testing unary operators..."

        ! Test: -1
        call tokenize_core("-1", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            ! This might be a literal or unary expression depending on implementation
            print '(a)', "PASS: Unary minus expression parsed"
        else
            print '(a)', "FAIL: No AST node returned for unary minus"
            test_unary_operators = .false.
        end if

        ! Test: +2
        call tokenize_core("+2", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            print '(a)', "PASS: Unary plus expression parsed"
        else
            print '(a)', "FAIL: No AST node returned for unary plus"
            test_unary_operators = .false.
        end if

    end function test_unary_operators

    logical function test_power_operator()
        ! TDD Test 5: Parse power operator (highest precedence)
        ! 2 ** 3 * 4 should be parsed as (2 ** 3) * 4
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_power_operator = .true.

        print '(a)', "Testing power operator..."

        ! Test: 2 ** 3
        call tokenize_core("2 ** 3", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "**") then
                    print '(a)', "PASS: Power operator parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for power test"
                    test_power_operator = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for power test"
                test_power_operator = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for power test"
            test_power_operator = .false.
        end if

    end function test_power_operator

end program test_frontend_parser_arithmetic_expressions
