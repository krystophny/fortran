program test_cli_stdin
    use cli
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run TDD tests in order
    if (.not. test_stdin_detection()) all_passed = .false.
    if (.not. test_no_args_no_stdin_shows_help()) all_passed = .false.
    if (.not. test_no_args_with_stdin_uses_stdin()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All CLI STDIN tests passed"
        stop 0
    else
        print '(a)', "Some CLI STDIN tests failed"
        stop 1
    end if

contains

    logical function test_stdin_detection()
        ! TDD Test 1: check_stdin_available function exists and works
        logical :: has_stdin

        test_stdin_detection = .true.

        print '(a)', "Testing STDIN detection function..."

        ! This should work without throwing errors
        call check_stdin_available(has_stdin)

        print '(a)', "PASS: STDIN detection function works"

    end function test_stdin_detection

    logical function test_no_args_no_stdin_shows_help()
        ! TDD Test 2: No args + no STDIN → show help
        character(len=256) :: filename
        logical :: show_help
        integer :: verbose_level
        character(len=256) :: custom_cache_dir, custom_config_dir, notebook_output
        integer :: parallel_jobs
        logical :: no_wait, notebook_mode, standardize_only, clear_cache, cache_info
    logical :: debug_tokens, debug_ast, debug_semantic, debug_standardize, debug_codegen
        logical :: from_tokens, from_ast, from_semantic

        test_no_args_no_stdin_shows_help = .true.

        print '(a)', "Testing no args + no STDIN shows help..."

        ! Mock no command line arguments and no STDIN
        ! This test will need to be run in a specific environment
        ! For now, we'll test the logic components

        ! Test that our parsing logic works correctly
        call parse_arguments_with_stdin_check(filename, show_help, verbose_level, &
                                              custom_cache_dir, custom_config_dir, &
                                              parallel_jobs, no_wait, notebook_mode, &
                                              notebook_output, standardize_only, &
                                              clear_cache, cache_info, debug_tokens, &
                          debug_ast, debug_semantic, debug_standardize, debug_codegen, &
                                              from_tokens, from_ast, from_semantic, &
                                              has_stdin=.false., nargs_override=0)

        if (.not. show_help) then
            print '(a)', "FAIL: Should show help when no args and no STDIN"
            test_no_args_no_stdin_shows_help = .false.
        else
            print '(a)', "PASS: Shows help when no args and no STDIN"
        end if

    end function test_no_args_no_stdin_shows_help

    logical function test_no_args_with_stdin_uses_stdin()
        ! TDD Test 3: No args + STDIN available → use STDIN
        character(len=256) :: filename
        logical :: show_help
        integer :: verbose_level
        character(len=256) :: custom_cache_dir, custom_config_dir, notebook_output
        integer :: parallel_jobs
        logical :: no_wait, notebook_mode, standardize_only, clear_cache, cache_info
    logical :: debug_tokens, debug_ast, debug_semantic, debug_standardize, debug_codegen
        logical :: from_tokens, from_ast, from_semantic

        test_no_args_with_stdin_uses_stdin = .true.

        print '(a)', "Testing no args + STDIN uses STDIN..."

        ! Test that our parsing logic works correctly
        call parse_arguments_with_stdin_check(filename, show_help, verbose_level, &
                                              custom_cache_dir, custom_config_dir, &
                                              parallel_jobs, no_wait, notebook_mode, &
                                              notebook_output, standardize_only, &
                                              clear_cache, cache_info, debug_tokens, &
                          debug_ast, debug_semantic, debug_standardize, debug_codegen, &
                                              from_tokens, from_ast, from_semantic, &
                                              has_stdin=.true., nargs_override=0)

        if (show_help) then
            print '(a)', "FAIL: Should not show help when STDIN is available"
            test_no_args_with_stdin_uses_stdin = .false.
        else if (trim(filename) /= '-') then
            print '(a)', "FAIL: Should set filename to '-' for STDIN"
            test_no_args_with_stdin_uses_stdin = .false.
        else
            print '(a)', "PASS: Uses STDIN when no args and STDIN available"
        end if

    end function test_no_args_with_stdin_uses_stdin

end program test_cli_stdin
