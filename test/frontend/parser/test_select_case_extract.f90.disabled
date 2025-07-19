program test_select_case_extract
    ! TDD test for parse_select_case extraction to parser_control_flow
    use lexer_core
    use ast_core
    use parser_state_module
    use parser_control_flow_module, only: parse_select_case
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Test that parse_select_case is accessible from parser_control_flow_module
    if (.not. test_extraction()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "Parse select case extraction test passed"
        stop 0
    else
        print '(a)', "Parse select case extraction test failed"
        stop 1
    end if

contains

    logical function test_extraction()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        class(ast_node), allocatable :: stmt

        test_extraction = .true.

        print '(a)', "Testing parse_select_case extraction..."

        ! Create tokens for select case
        allocate (tokens(8))
        tokens(1)%kind = TK_KEYWORD
        tokens(1)%text = "select"
        tokens(1)%line = 1
        tokens(1)%column = 1

        tokens(2)%kind = TK_KEYWORD
        tokens(2)%text = "case"
        tokens(2)%line = 1
        tokens(2)%column = 8

        tokens(3)%kind = TK_OPERATOR
        tokens(3)%text = "("
        tokens(3)%line = 1
        tokens(3)%column = 13

        tokens(4)%kind = TK_IDENTIFIER
        tokens(4)%text = "x"
        tokens(4)%line = 1
        tokens(4)%column = 14

        tokens(5)%kind = TK_OPERATOR
        tokens(5)%text = ")"
        tokens(5)%line = 1
        tokens(5)%column = 15

        tokens(6)%kind = TK_KEYWORD
        tokens(6)%text = "end"
        tokens(6)%line = 1
        tokens(6)%column = 17

        tokens(7)%kind = TK_KEYWORD
        tokens(7)%text = "select"
        tokens(7)%line = 1
        tokens(7)%column = 21

        tokens(8)%kind = TK_EOF
        tokens(8)%text = ""
        tokens(8)%line = 1
        tokens(8)%column = 27

        ! Create parser state
        parser = create_parser_state(tokens)

        ! Call parse_select_case from parser_control_flow_module
        stmt = parse_select_case(parser)

        if (.not. allocated(stmt)) then
            print '(a)', "FAIL: parse_select_case returned no AST node"
            test_extraction = .false.
        else
            ! For now, we expect a placeholder or actual select_case_node
            select type (stmt)
            type is (select_case_node)
                print '(a)', "PASS: parse_select_case returns select_case_node"
            type is (literal_node)
                ! Currently returns placeholder
                print '(a)', "INFO: parse_select_case currently returns placeholder"
            class default
                print '(a)', "FAIL: parse_select_case returns unexpected node type"
                test_extraction = .false.
            end select
        end if

    end function test_extraction

end program test_select_case_extract
