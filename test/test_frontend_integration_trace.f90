program test_frontend_integration_trace
    use frontend, only: lex_file, parse_tokens
    use lexer_core, only: token_t
    use ast_core, only: ast_arena_t, create_ast_stack
    implicit none

    logical :: all_passed
    all_passed = .true.

    if (.not. test_frontend_lex_file()) all_passed = .false.

    if (all_passed) then
        print *, "All frontend integration tests passed"
        stop 0
    else
        print *, "Some frontend integration tests failed"
        stop 1
    end if

contains

    logical function test_frontend_lex_file()
        character(len=*), parameter :: source = "x = 1"
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        type(ast_arena_t) :: arena
        integer :: prog_index

        test_frontend_lex_file = .true.
        print *, "Testing frontend lex_file call..."

        ! Allocate error message
        allocate (character(len=256) :: error_msg)
        error_msg = ""

        ! Try lex_file
        call lex_file(source, tokens, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: lex_file returned error:", error_msg
            test_frontend_lex_file = .false.
            return
        end if

        if (allocated(tokens)) then
            print *, "PASS: lex_file completed, got", size(tokens), "tokens"
            if (size(tokens) > 0) then
                print *, "First token:", tokens(1)%text
            end if
        else
            print *, "FAIL: lex_file failed - no tokens allocated"
            test_frontend_lex_file = .false.
            return
        end if

        ! Now try parse_tokens
        print *, "Testing parse_tokens call..."
        arena = create_ast_stack()

        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: parse_tokens returned error:", error_msg
            test_frontend_lex_file = .false.
            return
        end if

        if (prog_index > 0) then
            print *, "PASS: parse_tokens completed, got index", prog_index
        else
            print *, "FAIL: parse_tokens failed - no program index"
            test_frontend_lex_file = .false.
            return
        end if

    end function test_frontend_lex_file

end program test_frontend_integration_trace
