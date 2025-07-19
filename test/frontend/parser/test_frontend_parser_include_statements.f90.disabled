program test_frontend_parser_include_statements
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for include statements
    if (.not. test_basic_include_statement()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All include statement parser tests passed"
        stop 0
    else
        print '(a)', "Some include statement parser tests failed"
        stop 1
    end if

contains

    logical function test_basic_include_statement()
        ! TDD Test 1: Parse basic include statement
        ! include 'filename.f90'
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: ast_result

        test_basic_include_statement = .true.

        print '(a)', "Testing basic include statement parsing..."

        ! Test basic include statement
        call tokenize_core("include 'myfile.f90'", tokens)

        ! Parse statement
        ast_result = parse_statement(tokens)

        ! Check that we get an include_statement_node
        if (.not. allocated(ast_result)) then
            print '(a)', "FAIL: No AST node returned"
            test_basic_include_statement = .false.
        else
            select type (ast_result)
            type is (include_statement_node)
                if (ast_result%filename == "'myfile.f90'") then
                    print '(a)', "PASS: Basic include statement parsed correctly"
                else
                    print '(a)', "FAIL: Wrong filename"
                    test_basic_include_statement = .false.
                end if
            class default
                print '(a)', "FAIL: Expected include_statement_node"
                test_basic_include_statement = .false.
            end select
        end if

    end function test_basic_include_statement

end program test_frontend_parser_include_statements
