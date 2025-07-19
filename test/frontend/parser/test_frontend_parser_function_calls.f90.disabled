program test_frontend_parser_function_calls
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for function calls
    if (.not. test_intrinsic_functions()) all_passed = .false.
    if (.not. test_user_defined_functions()) all_passed = .false.
    if (.not. test_function_args()) all_passed = .false.
    if (.not. test_nested_function_calls()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All function call parser tests passed"
        stop 0
    else
        print '(a)', "Some function call parser tests failed"
        stop 1
    end if

contains

    logical function test_intrinsic_functions()
        ! TDD Test 1: Parse intrinsic function calls
        ! sin(x), abs(y), sqrt(z)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_intrinsic_functions = .true.

        print '(a)', "Testing intrinsic function calls..."

        ! Test sin(x)
        call tokenize_core("sin(x)", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for sin(x)"
            test_intrinsic_functions = .false.
        else
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "sin") then
                    print '(a)', "PASS: sin(x) parsed correctly"
                else
                    print '(a)', "FAIL: Wrong function name for sin(x)"
                    test_intrinsic_functions = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for sin(x)"
                test_intrinsic_functions = .false.
            end select
        end if

        ! Test abs(y)
        call tokenize_core("abs(y)", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "abs") then
                    print '(a)', "PASS: abs(y) parsed correctly"
                else
                    print '(a)', "FAIL: Wrong function name for abs(y)"
                    test_intrinsic_functions = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for abs(y)"
                test_intrinsic_functions = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for abs(y)"
            test_intrinsic_functions = .false.
        end if

        ! Test sqrt(z)
        call tokenize_core("sqrt(z)", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "sqrt") then
                    if (allocated(expr%args)) then
                        if (size(expr%args) == 1) then
                         print '(a)', "PASS: sqrt(z) parsed with correct argument count"
                        else
                            print '(a)', "FAIL: Wrong argument count for sqrt(z)"
                            test_intrinsic_functions = .false.
                        end if
                    else
                        print '(a)', "FAIL: No arguments for sqrt(z)"
                        test_intrinsic_functions = .false.
                    end if
                else
                    print '(a)', "FAIL: Wrong function name for sqrt(z)"
                    test_intrinsic_functions = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for sqrt(z)"
                test_intrinsic_functions = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for sqrt(z)"
            test_intrinsic_functions = .false.
        end if

    end function test_intrinsic_functions

    logical function test_user_defined_functions()
        ! TDD Test 2: Parse user-defined function calls
        ! myFunc(), calculate(a, b), process_data(x, y, z)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_user_defined_functions = .true.

        print '(a)', "Testing user-defined function calls..."

        ! Test myFunc() - no arguments
        call tokenize_core("myFunc()", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for myFunc()"
            test_user_defined_functions = .false.
        else
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "myFunc") then
                    if (.not. allocated(expr%args) .or. size(expr%args) == 0) then
                        print '(a)', "PASS: myFunc() parsed with no arguments"
                    else
                        print '(a)', "FAIL: myFunc() should have no arguments"
                        test_user_defined_functions = .false.
                    end if
                else
                    print '(a)', "FAIL: Wrong function name for myFunc()"
                    test_user_defined_functions = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for myFunc()"
                test_user_defined_functions = .false.
            end select
        end if

        ! Test calculate(a, b) - two arguments
        call tokenize_core("calculate(a, b)", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "calculate") then
                    if (allocated(expr%args) .and. size(expr%args) == 2) then
                        print '(a)', "PASS: calculate(a, b) parsed with 2 arguments"
                    else
                        print '(a)', "FAIL: Wrong argument count for calculate(a, b)"
                        test_user_defined_functions = .false.
                    end if
                else
                    print '(a)', "FAIL: Wrong function name for calculate(a, b)"
                    test_user_defined_functions = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for calculate(a, b)"
                test_user_defined_functions = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for calculate(a, b)"
            test_user_defined_functions = .false.
        end if

        ! Test process_data(x, y, z) - three arguments
        call tokenize_core("process_data(x, y, z)", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "process_data") then
                    if (allocated(expr%args) .and. size(expr%args) == 3) then
                      print '(a)', "PASS: process_data(x, y, z) parsed with 3 arguments"
                    else
                     print '(a)', "FAIL: Wrong argument count for process_data(x, y, z)"
                        test_user_defined_functions = .false.
                    end if
                else
                    print '(a)', "FAIL: Wrong function name for process_data(x, y, z)"
                    test_user_defined_functions = .false.
                end if
            class default
          print '(a)', "FAIL: Expected call_or_subscript_node for process_data(x, y, z)"
                test_user_defined_functions = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for process_data(x, y, z)"
            test_user_defined_functions = .false.
        end if

    end function test_user_defined_functions

    logical function test_function_args()
        ! TDD Test 3: Parse function calls with various argument types
        ! func(1), func(x+1), func("hello"), func(.true.)
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_function_args = .true.

        print '(a)', "Testing function call arguments..."

        ! Test func(1) - literal argument
        call tokenize_core("func(1)", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for func(1)"
            test_function_args = .false.
        else
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "func") then
                    print '(a)', "PASS: func(1) parsed correctly"
                else
                    print '(a)', "FAIL: Wrong function name for func(1)"
                    test_function_args = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for func(1)"
                test_function_args = .false.
            end select
        end if

        ! Test func(x+1) - expression argument
        call tokenize_core("func(x+1)", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "func") then
                    print '(a)', "PASS: func(x+1) parsed correctly"
                else
                    print '(a)', "FAIL: Wrong function name for func(x+1)"
                    test_function_args = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for func(x+1)"
                test_function_args = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for func(x+1)"
            test_function_args = .false.
        end if

        ! Test func("hello") - string argument
        call tokenize_core('func("hello")', tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "func") then
                    print '(a)', "PASS: func(""hello"") parsed correctly"
                else
                    print '(a)', "FAIL: Wrong function name for func(""hello"")"
                    test_function_args = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for func(""hello"")"
                test_function_args = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for func(""hello"")"
            test_function_args = .false.
        end if

        ! Test func(.true.) - logical argument
        call tokenize_core("func(.true.)", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "func") then
                    print '(a)', "PASS: func(.true.) parsed correctly"
                else
                    print '(a)', "FAIL: Wrong function name for func(.true.)"
                    test_function_args = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for func(.true.)"
                test_function_args = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for func(.true.)"
            test_function_args = .false.
        end if

    end function test_function_args

    logical function test_nested_function_calls()
        ! TDD Test 4: Parse nested function calls
        ! sin(cos(x)), max(abs(a), abs(b))
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_nested_function_calls = .true.

        print '(a)', "Testing nested function calls..."

        ! Test sin(cos(x))
        call tokenize_core("sin(cos(x))", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for sin(cos(x))"
            test_nested_function_calls = .false.
        else
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "sin") then
                    if (allocated(expr%args) .and. size(expr%args) == 1) then
                        ! Check if the argument is also a function call
                        if (allocated(expr%args(1)%node)) then
                            select type (arg => expr%args(1)%node)
                            type is (call_or_subscript_node)
                                if (arg%name == "cos") then
                               print '(a)', "PASS: sin(cos(x)) parsed with nested calls"
                                else
                                    print '(a)', "FAIL: Inner function should be cos"
                                    test_nested_function_calls = .false.
                                end if
                            class default
                           print '(a)', "FAIL: Inner argument should be a function call"
                                test_nested_function_calls = .false.
                            end select
                        else
                            print '(a)', "FAIL: No inner argument for sin(cos(x))"
                            test_nested_function_calls = .false.
                        end if
                    else
                        print '(a)', "FAIL: Wrong argument structure for sin(cos(x))"
                        test_nested_function_calls = .false.
                    end if
                else
                    print '(a)', "FAIL: Outer function should be sin"
                    test_nested_function_calls = .false.
                end if
            class default
                print '(a)', "FAIL: Expected call_or_subscript_node for sin(cos(x))"
                test_nested_function_calls = .false.
            end select
        end if

        ! Test max(abs(a), abs(b))
        call tokenize_core("max(abs(a), abs(b))", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (call_or_subscript_node)
                if (expr%name == "max") then
                    if (allocated(expr%args) .and. size(expr%args) == 2) then
                        print '(a)', "PASS: max(abs(a), abs(b)) parsed with 2 arguments"
                    else
                        print '(a)', "FAIL: max should have 2 arguments"
                        test_nested_function_calls = .false.
                    end if
                else
                    print '(a)', "FAIL: Outer function should be max"
                    test_nested_function_calls = .false.
                end if
            class default
            print '(a)', "FAIL: Expected call_or_subscript_node for max(abs(a), abs(b))"
                test_nested_function_calls = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for max(abs(a), abs(b))"
            test_nested_function_calls = .false.
        end if

    end function test_nested_function_calls

end program test_frontend_parser_function_calls
