! Test do while loop parsing
program test_frontend_parser_do_while
    use parser_core
    use parser_control_flow_module, only: parse_do_while
    use parser_state_module, only: parser_state_t, create_parser_state
use lexer_core, only: token_t, TK_KEYWORD, TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_EOF
    use ast_core
    use iso_fortran_env, only: real64
    implicit none

    integer :: tests_passed = 0
    integer :: tests_failed = 0

    ! Test basic do while loop
    call test_basic_do_while()

    ! Print test summary
    print *, "Do while parser tests:"
    print *, "  Passed:", tests_passed
    print *, "  Failed:", tests_failed
    if (tests_failed == 0) then
        print *, "All tests passed!"
    else
        error stop "Some tests failed"
    end if

contains

    subroutine test_basic_do_while()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        class(ast_node), allocatable :: node
        type(do_while_node), allocatable :: while_node
        integer :: i

        print *, "Testing basic do while loop parsing..."

        ! Create tokens for: do while (i < 10)
        allocate (tokens(8))
        tokens(1) = token_t(TK_KEYWORD, "do", 1, 1)
        tokens(2) = token_t(TK_KEYWORD, "while", 1, 4)
        tokens(3) = token_t(TK_OPERATOR, "(", 1, 10)
        tokens(4) = token_t(TK_IDENTIFIER, "i", 1, 11)
        tokens(5) = token_t(TK_OPERATOR, "<", 1, 13)
        tokens(6) = token_t(TK_NUMBER, "10", 1, 15)
        tokens(7) = token_t(TK_OPERATOR, ")", 1, 17)
        tokens(8) = token_t(TK_EOF, "", 1, 18)

        parser = create_parser_state(tokens)
        node = parse_do_while(parser)

        if (allocated(node)) then
            select type (node)
            type is (do_while_node)
                print *, "  ✓ Parsed as do_while_node"
                tests_passed = tests_passed + 1

                ! Check condition
                if (allocated(node%condition)) then
                    print *, "  ✓ Has condition"
                    tests_passed = tests_passed + 1
                else
                    print *, "  ✗ Missing condition"
                    tests_failed = tests_failed + 1
                end if
            class default
                print *, "  ✗ Wrong node type"
                tests_failed = tests_failed + 1
            end select
        else
            print *, "  ✗ Failed to parse do while loop"
            tests_failed = tests_failed + 1
        end if

        deallocate (tokens)
    end subroutine test_basic_do_while

end program test_frontend_parser_do_while
