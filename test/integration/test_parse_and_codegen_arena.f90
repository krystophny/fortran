program test_parse_and_codegen_arena
    use lexer_core
    use frontend
    use ast_core
    use codegen_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run integration tests
    if (.not. test_round_trip_literal()) all_passed = .false.
    if (.not. test_round_trip_assignment()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All parse and codegen integration tests passed"
        stop 0
    else
        print '(a)', "Some parse and codegen integration tests failed"
        stop 1
    end if

contains

    logical function test_round_trip_literal()
        character(len=*), parameter :: source = "42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: expr_index
        character(len=:), allocatable :: generated
        character(len=256) :: error_msg

        test_round_trip_literal = .true.
        print '(a)', "Testing literal round trip..."

        ! Tokenize
        call tokenize_core(source, tokens)

        ! Parse
        arena = create_ast_stack()
        call parse_tokens(tokens, arena, expr_index, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Parsing failed: "//error_msg
            test_round_trip_literal = .false.
            return
        end if

        ! Generate code
        generated = generate_code_from_arena(arena, expr_index)

        ! For now, just check that we got some output
        if (len(generated) == 0) then
            print '(a)', "FAIL: No code generated"
            test_round_trip_literal = .false.
        else
            print '(a)', "PASS: Literal round trip - got: "//generated
        end if
    end function test_round_trip_literal

    logical function test_round_trip_assignment()
        character(len=*), parameter :: source = "x = 42"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: generated
        character(len=256) :: error_msg

        test_round_trip_assignment = .true.
        print '(a)', "Testing assignment round trip..."

        ! Tokenize
        call tokenize_core(source, tokens)

        ! Parse
        arena = create_ast_stack()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print '(a)', "FAIL: Parsing failed: "//error_msg
            test_round_trip_assignment = .false.
            return
        end if

        ! Generate code
        generated = generate_code_from_arena(arena, prog_index)

        ! For now, just check that we got some output
        if (len(generated) == 0) then
            print '(a)', "FAIL: No code generated"
            test_round_trip_assignment = .false.
        else
            print '(a)', "PASS: Assignment round trip - got: "//generated
        end if
    end function test_round_trip_assignment

end program test_parse_and_codegen_arena
