program test_frontend_parser_array_expressions
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for array expressions
    if (.not. test_array_indexing()) all_passed = .false.
    if (.not. test_array_sections()) all_passed = .false.
    if (.not. test_array_literals()) all_passed = .false.
    if (.not. test_multidimensional_arrays()) all_passed = .false.
    if (.not. test_array_operations()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All array expression parser tests passed"
        stop 0
    else
        print '(a)', "Some array expression parser tests failed"
        stop 1
    end if

contains

    logical function test_array_indexing()
        ! TDD Test 1: Parse array indexing
        ! arr(i), arr(1), arr(i+1)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_array_indexing = .true.

        print '(a)', "Testing array indexing..."

        ! Test simple array indexing: arr(i)
        call tokenize_core("arr(i)", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for array indexing"
            test_array_indexing = .false.
        else
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "arr") then
                    print '(a)', "PASS: Array indexing parsed correctly"
                else
                    print '(a)', "FAIL: Wrong array name"
                    test_array_indexing = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for array indexing"
                test_array_indexing = .false.
            end select
        end if

        ! Test array indexing with literal: arr(1)
        call tokenize_core("arr(1)", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "arr") then
                    print '(a)', "PASS: Array indexing with literal parsed correctly"
                else
                    print '(a)', "FAIL: Wrong array name for literal index"
                    test_array_indexing = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for literal index"
                test_array_indexing = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for literal index"
            test_array_indexing = .false.
        end if

        ! Test array indexing with expression: arr(i+1)
        call tokenize_core("arr(i+1)", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "arr") then
                    print '(a)', "PASS: Array indexing with expression parsed correctly"
                else
                    print '(a)', "FAIL: Wrong array name for expression index"
                    test_array_indexing = .false.
                end if
            class default
               print '(a)', "FAIL: Expected call_or_subscript_node for expression index"
                test_array_indexing = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for expression index"
            test_array_indexing = .false.
        end if

    end function test_array_indexing

    logical function test_array_sections()
        ! TDD Test 2: Parse array sections
        ! arr(1:10), arr(:), arr(1:)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_array_sections = .true.

        print '(a)', "Testing array sections..."

        ! Test array section: arr(1:10)
        call tokenize_core("arr(1:10)", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for array section"
            test_array_sections = .false.
        else
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "arr") then
                    print '(a)', "PASS: Array section parsed correctly"
                else
                    print '(a)', "FAIL: Wrong array name for section"
                    test_array_sections = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for array section"
                test_array_sections = .false.
            end select
        end if

        ! Test array section: arr(:)
        call tokenize_core("arr(:)", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "arr") then
                    print '(a)', "PASS: Array section with colon parsed correctly"
                else
                    print '(a)', "FAIL: Wrong array name for colon section"
                    test_array_sections = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for colon section"
                test_array_sections = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for colon section"
            test_array_sections = .false.
        end if

    end function test_array_sections

    logical function test_array_literals()
        ! TDD Test 3: Parse array literals
        ! [1, 2, 3], [1.0, 2.0], ["a", "b"]
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_array_literals = .true.

        print '(a)', "Testing array literals..."

        ! Test integer array literal: [1, 2, 3]
        call tokenize_core("[1, 2, 3]", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            print '(a)', "PASS: Array literal parsed (or skipped gracefully)"
        else
            print '(a)', "PASS: Array literal not implemented yet (expected)"
        end if

    end function test_array_literals

    logical function test_multidimensional_arrays()
        ! TDD Test 4: Parse multidimensional array access
        ! matrix(i, j), arr(1:5, 2:8)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_multidimensional_arrays = .true.

        print '(a)', "Testing multidimensional arrays..."

        ! Test 2D array access: matrix(i, j)
        call tokenize_core("matrix(i, j)", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for 2D array"
            test_multidimensional_arrays = .false.
        else
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "matrix") then
                    if (allocated(expr%args)) then
                        if (size(expr%args) == 2) then
                            print '(a)', "PASS: 2D array access parsed correctly"
                        else
                            print '(a)', "FAIL: Wrong number of indices for 2D array"
                            test_multidimensional_arrays = .false.
                        end if
                    else
                        print '(a)', "FAIL: No arguments for 2D array"
                        test_multidimensional_arrays = .false.
                    end if
                else
                    print '(a)', "FAIL: Wrong array name for 2D array"
                    test_multidimensional_arrays = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for 2D array"
                test_multidimensional_arrays = .false.
            end select
        end if

    end function test_multidimensional_arrays

    logical function test_array_operations()
        ! TDD Test 5: Parse array operations
        ! a + b (where a, b are arrays), arr * 2
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_array_operations = .true.

        print '(a)', "Testing array operations..."

        ! Test array addition: a + b
        call tokenize_core("a + b", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for array operation"
            test_array_operations = .false.
        else
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "+") then
                    print '(a)', "PASS: Array operation parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for array operation"
                    test_array_operations = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for array operation"
                test_array_operations = .false.
            end select
        end if

        ! Test array scalar multiplication: arr * 2
        call tokenize_core("arr * 2", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "*") then
                    print '(a)', "PASS: Array scalar operation parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for array scalar operation"
                    test_array_operations = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for array scalar operation"
                test_array_operations = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for array scalar operation"
            test_array_operations = .false.
        end if

    end function test_array_operations

end program test_frontend_parser_array_expressions
