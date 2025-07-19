program test_frontend_parser_use_statements
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for use statements
    if (.not. test_basic_use_statement()) all_passed = .false.
    if (.not. test_use_with_only_clause()) all_passed = .false.
    if (.not. test_use_with_renaming()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All use statement parser tests passed"
        stop 0
    else
        print '(a)', "Some use statement parser tests failed"
        stop 1
    end if

contains

    logical function test_basic_use_statement()
        ! TDD Test 1: Parse basic use statement
        ! use module_name
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_basic_use_statement = .true.

        print '(a)', "Testing basic use statement parsing..."

        ! Test basic use statement
        call tokenize_core("use mymodule", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we get a use_statement_node
        if (.not. allocated(ast_result)) then
            print '(a)', "FAIL: No AST node returned"
            test_basic_use_statement = .false.
        else
            select type (ast_result)
            type is (use_statement_node)
                if (ast_result%module_name == "mymodule") then
                    print '(a)', "PASS: Basic use statement parsed correctly"
                else
                    print '(a)', "FAIL: Wrong module name"
                    test_basic_use_statement = .false.
                end if
            class default
                print '(a)', "FAIL: Expected use_statement_node"
                test_basic_use_statement = .false.
            end select
        end if

    end function test_basic_use_statement

    logical function test_use_with_only_clause()
        ! TDD Test 2: Parse use statement with only clause
        ! use module_name, only: item1, item2
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_use_with_only_clause = .true.

        print '(a)', "Testing use statement with only clause..."

        ! Test use statement with only clause
        call tokenize_core("use mymodule, only: item1, item2", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we get a use_statement_node with only clause
        if (.not. allocated(ast_result)) then
            print '(a)', "FAIL: No AST node returned"
            test_use_with_only_clause = .false.
        else
            select type (ast_result)
            type is (use_statement_node)
                if (ast_result%module_name == "mymodule" .and. ast_result%has_only) then
                    if (allocated(ast_result%only_list)) then
                        if (size(ast_result%only_list) == 2) then
                    print '(a)', "PASS: Use statement with only clause parsed correctly"
                        else
                            print '(a)', "FAIL: Wrong number of only items"
                            test_use_with_only_clause = .false.
                        end if
                    else
                        print '(a)', "FAIL: Only list not allocated"
                        test_use_with_only_clause = .false.
                    end if
                else
                    print '(a)', "FAIL: Wrong module name or has_only flag"
                    test_use_with_only_clause = .false.
                end if
            class default
                print '(a)', "FAIL: Expected use_statement_node"
                test_use_with_only_clause = .false.
            end select
        end if

    end function test_use_with_only_clause

    logical function test_use_with_renaming()
        ! TDD Test 3: Parse use statement with renaming
        ! use module_name, only: new_name => old_name
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_use_with_renaming = .true.

        print '(a)', "Testing use statement with renaming..."

        ! Test use statement with renaming
        call tokenize_core("use mymodule, only: new_name => old_name", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we get a use_statement_node with renaming
        if (.not. allocated(ast_result)) then
            print '(a)', "FAIL: No AST node returned"
            test_use_with_renaming = .false.
        else
            select type (ast_result)
            type is (use_statement_node)
                if (ast_result%module_name == "mymodule" .and. ast_result%has_only) then
                    if (allocated(ast_result%rename_list)) then
                        if (size(ast_result%rename_list) == 1) then
                       print '(a)', "PASS: Use statement with renaming parsed correctly"
                        else
                            print '(a)', "FAIL: Wrong number of rename items"
                            test_use_with_renaming = .false.
                        end if
                    else
                        print '(a)', "FAIL: Rename list not allocated"
                        test_use_with_renaming = .false.
                    end if
                else
                    print '(a)', "FAIL: Wrong module name or has_only flag"
                    test_use_with_renaming = .false.
                end if
            class default
                print '(a)', "FAIL: Expected use_statement_node"
                test_use_with_renaming = .false.
            end select
        end if

    end function test_use_with_renaming

end program test_frontend_parser_use_statements
