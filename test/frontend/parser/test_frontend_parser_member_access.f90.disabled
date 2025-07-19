program test_frontend_parser_member_access
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for structure member access
    if (.not. test_simple_member_access()) all_passed = .false.
    if (.not. test_nested_member_access()) all_passed = .false.
    if (.not. test_array_member_access()) all_passed = .false.
    if (.not. test_member_function_calls()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All member access parser tests passed"
        stop 0
    else
        print '(a)', "Some member access parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_member_access()
        ! TDD Test 1: Parse simple member access
        ! point%x, person%name, config%value
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_simple_member_access = .true.

        print '(a)', "Testing simple member access..."

        ! Test point%x
        call tokenize_core("point%x", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for point%x"
            test_simple_member_access = .false.
        else
            ! For now, we expect it might parse as binary_op with % operator
            ! or we might need a new member_access_node type
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "%") then
                    ! Check left and right operands
                    if (allocated(expr%left) .and. allocated(expr%right)) then
                        select type (left => expr%left)
                        type is (identifier_node)
                            if (left%name == "point") then
                                select type (right => expr%right)
                                type is (identifier_node)
                                    if (right%name == "x") then
                                        print '(a)', "PASS: point%x parsed correctly"
                                    else
                                        print '(a)', "FAIL: Wrong member name"
                                        test_simple_member_access = .false.
                                    end if
                                class default
                                    print '(a)', "FAIL: Member should be identifier"
                                    test_simple_member_access = .false.
                                end select
                            else
                                print '(a)', "FAIL: Wrong structure name"
                                test_simple_member_access = .false.
                            end if
                        class default
                            print '(a)', "FAIL: Structure should be identifier"
                            test_simple_member_access = .false.
                        end select
                    else
                        print '(a)', "FAIL: Missing operands for member access"
                        test_simple_member_access = .false.
                    end if
                else
                    print '(a)', "FAIL: Wrong operator for point%x"
                    test_simple_member_access = .false.
                end if
            type is (identifier_node)
                print '(a)', "FAIL: point%x should not be parsed as identifier"
                test_simple_member_access = .false.
            class default
                ! Debug: print actual type
                select type (expr)
                type is (literal_node)
                    print '(a,a)', "  DEBUG: Got literal_node with value: ", expr%value
                class default
                    print '(a)', "  DEBUG: Got unknown node type"
                end select
                print '(a)', "FAIL: Unexpected node type for point%x"
                test_simple_member_access = .false.
            end select
        end if

        ! Test person%name
        call tokenize_core("person%name", tokens)
        expr = parse_expression(tokens)

        if (allocated(expr)) then
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "%") then
                    print '(a)', "PASS: person%name parsed correctly"
                else
                    print '(a)', "FAIL: Wrong operator for person%name"
                    test_simple_member_access = .false.
                end if
            type is (identifier_node)
                print '(a)', "FAIL: person%name should not be parsed as identifier"
                test_simple_member_access = .false.
            class default
                print '(a)', "FAIL: Unexpected node type for person%name"
                test_simple_member_access = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for person%name"
            test_simple_member_access = .false.
        end if

    end function test_simple_member_access

    logical function test_nested_member_access()
        ! TDD Test 2: Parse nested member access
        ! company%address%street, obj%data%value
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_nested_member_access = .true.

        print '(a)', "Testing nested member access..."

        ! Test company%address%street
        call tokenize_core("company%address%street", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for nested member access"
            test_nested_member_access = .false.
        else
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "%") then
                    ! Check if left side is also a member access
                    select type (left => expr%left)
                    type is (binary_op_node)
                        if (left%operator == "%") then
                            print '(a)', "PASS: Nested member access parsed correctly"
                        else
                            print '(a)', "FAIL: Expected nested % operators"
                            test_nested_member_access = .false.
                        end if
                    class default
                        print '(a)', "FAIL: Expected nested structure"
                        test_nested_member_access = .false.
                    end select
                else
                    print '(a)', "FAIL: Wrong operator for nested member access"
                    test_nested_member_access = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for nested member access"
                test_nested_member_access = .false.
            end select
        end if

    end function test_nested_member_access

    logical function test_array_member_access()
        ! TDD Test 3: Parse array element member access
        ! points(i)%x, objects(1)%data
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_array_member_access = .true.

        print '(a)', "Testing array element member access..."

        ! Test points(i)%x
        call tokenize_core("points(i)%x", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for points(i)%x"
            test_array_member_access = .false.
        else
            select type (expr)
            type is (binary_op_node)
                if (expr%operator == "%") then
                    ! Check if left side is array access
                    select type (left => expr%left)
                    type is (call_or_subscript_node)
                        if (left%name == "points") then
                            print '(a)', "PASS: Array member access parsed correctly"
                        else
                            print '(a)', "FAIL: Wrong array name"
                            test_array_member_access = .false.
                        end if
                    class default
                        print '(a)', "FAIL: Expected array access on left side"
                        test_array_member_access = .false.
                    end select
                else
                    print '(a)', "FAIL: Wrong operator for array member access"
                    test_array_member_access = .false.
                end if
            class default
                print '(a)', "FAIL: Expected binary_op_node for array member access"
                test_array_member_access = .false.
            end select
        end if

    end function test_array_member_access

    logical function test_member_function_calls()
        ! TDD Test 4: Parse member function calls
        ! obj%method(), person%getName()
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: expr

        test_member_function_calls = .true.

        print '(a)', "Testing member function calls..."

        ! Test obj%method()
        call tokenize_core("obj%method()", tokens)
        expr = parse_expression(tokens)

        if (.not. allocated(expr)) then
            print '(a)', "FAIL: No AST node returned for obj%method()"
            test_member_function_calls = .false.
        else
            select type (expr)
            type is (call_or_subscript_node)
                ! The function call should be the outer node
                if (expr%name == "method") then
                 print '(a)', "FAIL: Parser incorrectly parsed obj%method() as method()"
                    test_member_function_calls = .false.
                else
                    print '(a)', "INFO: obj%method() parsed, but structure needs work"
                end if
            type is (binary_op_node)
                if (expr%operator == "%") then
                    ! Check if right side is a function call
                    select type (right => expr%right)
                    type is (call_or_subscript_node)
                        if (right%name == "method") then
                            print '(a)', "PASS: Member function call parsed correctly"
                        else
                            print '(a)', "FAIL: Wrong method name"
                            test_member_function_calls = .false.
                        end if
                    class default
                        print '(a)', "FAIL: Expected function call on right side"
                        test_member_function_calls = .false.
                    end select
                else
                    print '(a)', "FAIL: Wrong operator for member function call"
                    test_member_function_calls = .false.
                end if
            class default
                print '(a)', "FAIL: Unexpected structure for obj%method()"
                test_member_function_calls = .false.
            end select
        end if

    end function test_member_function_calls

end program test_frontend_parser_member_access
