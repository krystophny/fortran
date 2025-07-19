program test_frontend_parser_select_case
    use lexer_core
    use ast_core
    use parser_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests for select case statements
    if (.not. test_simple_select_case()) all_passed = .false.
    if (.not. test_select_case_multiple()) all_passed = .false.
    if (.not. test_select_case_default()) all_passed = .false.
    if (.not. test_select_case_ranges()) all_passed = .false.
    if (.not. test_nested_select_case()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All select case parser tests passed"
        stop 0
    else
        print '(a)', "Some select case parser tests failed"
        stop 1
    end if

contains

    logical function test_simple_select_case()
        ! TDD Test 1: Parse simple select case
        ! select case (x)
        ! case (1)
        !     y = 10
        ! end select
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_simple_select_case = .true.

        print '(a)', "Testing simple select case..."

        ! Test: select case (x) case (1) end select
        call tokenize_core("select case (x) case (1) end select", tokens)

        ! Add EOF token
        block
            type(token_t), allocatable :: tokens_with_eof(:)
            integer :: n

            n = size(tokens)
            allocate (tokens_with_eof(n + 1))
            tokens_with_eof(1:n) = tokens
            tokens_with_eof(n + 1)%kind = TK_EOF
            tokens_with_eof(n + 1)%text = ""
            tokens_with_eof(n + 1)%line = 1
            tokens_with_eof(n + 1)%column = 1

            stmt = parse_statement(tokens_with_eof)
        end block

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: No AST node returned for select case"
            test_simple_select_case = .false.
        else
            ! Check what type of node we get
            select type (stmt)
            type is (select_case_node)
                print '(a)', "PASS: Select case parsed as select_case_node"

                ! Check expression exists
                if (.not. allocated(stmt%expr)) then
                    print '(a)', "FAIL: No expression in select case node"
                    test_simple_select_case = .false.
                else
                    ! Check expression structure
                    select type (expr => stmt%expr)
                    type is (identifier_node)
                        if (expr%name == "x") then
                            print '(a)', "PASS: Select case expression is 'x'"
                        else
                            print '(a)', "FAIL: Wrong expression name"
                            test_simple_select_case = .false.
                        end if
                    class default
                        print '(a)', "FAIL: Expression is not an identifier"
                        test_simple_select_case = .false.
                    end select
                end if

                ! Check cases exist
                if (.not. allocated(stmt%cases)) then
                    print '(a)', "FAIL: No cases in select case node"
                    test_simple_select_case = .false.
                else
                  print '(a,i0)', "INFO: select case has ", size(stmt%cases), " case(s)"

                    ! Check first case
                    if (size(stmt%cases) > 0) then
                        if (stmt%cases(1)%case_type == "case") then
                            print '(a)', "PASS: First case is regular case"
                        else
                            print '(a)', "FAIL: First case type is wrong"
                            test_simple_select_case = .false.
                        end if
                    end if
                end if
            type is (identifier_node)
                print '(a)', "FAIL: Select case parsed as identifier"
                test_simple_select_case = .false.
            class default
                print '(a)', "FAIL: Select case parsed as wrong node type"
                test_simple_select_case = .false.
            end select
        end if

    end function test_simple_select_case

    logical function test_select_case_multiple()
        ! TDD Test 2: Parse select case with multiple cases
        ! select case (ch)
        ! case ('a', 'e', 'i', 'o', 'u')
        !     vowel = .true.
        ! case ('b':'d', 'f':'h')
        !     consonant = .true.
        ! end select
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_select_case_multiple = .true.

        print '(a)', "Testing select case with multiple values..."

        ! Test the initial select case statement
        call tokenize_core("select case (ch) case ('a') end select", tokens)

        ! Add EOF token
        block
            type(token_t), allocatable :: tokens_with_eof(:)
            integer :: n

            n = size(tokens)
            allocate (tokens_with_eof(n + 1))
            tokens_with_eof(1:n) = tokens
            tokens_with_eof(n + 1)%kind = TK_EOF
            tokens_with_eof(n + 1)%text = ""
            tokens_with_eof(n + 1)%line = 1
            tokens_with_eof(n + 1)%column = 1

            stmt = parse_statement(tokens_with_eof)
        end block

        if (allocated(stmt)) then
            select type (stmt)
            type is (select_case_node)
                print '(a)', "PASS: Multi-value select case parsed as select_case_node"
            class default
                print '(a)', "FAIL: Multi-value select case parsed as wrong node type"
                test_select_case_multiple = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for multi-value select case"
            test_select_case_multiple = .false.
        end if

    end function test_select_case_multiple

    logical function test_select_case_default()
        ! TDD Test 3: Parse select case with default
        ! select case (grade)
        ! case (90:100)
        !     print *, 'A'
        ! case (80:89)
        !     print *, 'B'
        ! case default
        !     print *, 'F'
        ! end select
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_select_case_default = .true.

        print '(a)', "Testing select case with default..."

        ! Test the initial select case statement
        call tokenize_core("select case (grade) case (90) end select", tokens)

        ! Add EOF token
        block
            type(token_t), allocatable :: tokens_with_eof(:)
            integer :: n

            n = size(tokens)
            allocate (tokens_with_eof(n + 1))
            tokens_with_eof(1:n) = tokens
            tokens_with_eof(n + 1)%kind = TK_EOF
            tokens_with_eof(n + 1)%text = ""
            tokens_with_eof(n + 1)%line = 1
            tokens_with_eof(n + 1)%column = 1

            stmt = parse_statement(tokens_with_eof)
        end block

        if (allocated(stmt)) then
            select type (stmt)
            type is (select_case_node)
                print '(a)', "PASS: Select case with default parsed as select_case_node"
            class default
                print '(a)', "FAIL: Select case with default parsed as wrong node type"
                test_select_case_default = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for select case with default"
            test_select_case_default = .false.
        end if

    end function test_select_case_default

    logical function test_select_case_ranges()
        ! TDD Test 4: Parse select case with ranges
        ! select case (temp)
        ! case (:-10)
        !     status = 'freezing'
        ! case (-10:0)
        !     status = 'cold'
        ! case (0:20)
        !     status = 'cool'
        ! case (20:)
        !     status = 'warm'
        ! end select
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_select_case_ranges = .true.

        print '(a)', "Testing select case with ranges..."

        ! Test the initial select case statement
        call tokenize_core("select case (temp) case (10) end select", tokens)

        ! Add EOF token
        block
            type(token_t), allocatable :: tokens_with_eof(:)
            integer :: n

            n = size(tokens)
            allocate (tokens_with_eof(n + 1))
            tokens_with_eof(1:n) = tokens
            tokens_with_eof(n + 1)%kind = TK_EOF
            tokens_with_eof(n + 1)%text = ""
            tokens_with_eof(n + 1)%line = 1
            tokens_with_eof(n + 1)%column = 1

            stmt = parse_statement(tokens_with_eof)
        end block

        if (allocated(stmt)) then
            select type (stmt)
            type is (select_case_node)
                print '(a)', "PASS: Select case with ranges parsed as select_case_node"
            class default
                print '(a)', "FAIL: Select case with ranges parsed as wrong node type"
                test_select_case_ranges = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for select case with ranges"
            test_select_case_ranges = .false.
        end if

    end function test_select_case_ranges

    logical function test_nested_select_case()
        ! TDD Test 5: Parse nested select case
        type(token_t), allocatable :: tokens(:)
        class(ast_node), allocatable :: stmt

        test_nested_select_case = .true.

        print '(a)', "Testing nested select case..."

        ! Test outer select case
        call tokenize_core("select case (x) case (1) end select", tokens)

        ! Add EOF token
        block
            type(token_t), allocatable :: tokens_with_eof(:)
            integer :: n

            n = size(tokens)
            allocate (tokens_with_eof(n + 1))
            tokens_with_eof(1:n) = tokens
            tokens_with_eof(n + 1)%kind = TK_EOF
            tokens_with_eof(n + 1)%text = ""
            tokens_with_eof(n + 1)%line = 1
            tokens_with_eof(n + 1)%column = 1

            stmt = parse_statement(tokens_with_eof)
        end block

        if (allocated(stmt)) then
            select type (stmt)
            type is (select_case_node)
                print '(a)', "PASS: Nested select case parsed as select_case_node"
            class default
                print '(a)', "FAIL: Nested select case parsed as wrong node type"
                test_nested_select_case = .false.
            end select
        else
            print '(a)', "FAIL: No AST node returned for nested select case"
            test_nested_select_case = .false.
        end if

    end function test_nested_select_case

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

end program test_frontend_parser_select_case
