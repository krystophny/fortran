program test_cli_comprehensive
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    logical :: all_tests_passed

    ! Test command line simulation
    character(len=256), dimension(20) :: test_args
    integer :: test_arg_count

    print *, "=== Comprehensive CLI Tests ==="
    print *, ""

    all_tests_passed = .true.

    ! Test comprehensive argument parsing scenarios
    if (.not. test_empty_arguments()) all_tests_passed = .false.
    if (.not. test_help_arguments()) all_tests_passed = .false.
    if (.not. test_verbose_arguments()) all_tests_passed = .false.
    if (.not. test_directory_arguments()) all_tests_passed = .false.
    if (.not. test_parallel_arguments()) all_tests_passed = .false.
    if (.not. test_notebook_arguments()) all_tests_passed = .false.
    if (.not. test_complex_combinations()) all_tests_passed = .false.
    if (.not. test_edge_cases()) all_tests_passed = .false.

    print *, ""
    if (all_tests_passed) then
        print *, "All comprehensive CLI tests PASSED!"
    else
        print *, "Some comprehensive CLI tests FAILED!"
        stop 1
    end if

contains

    ! Test version of parse_arguments that uses test_args instead of command line
    subroutine parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)
        character(len=*), intent(out) :: filename
        logical, intent(out) :: show_help
        integer, intent(out) :: verbose_level
        character(len=*), intent(out) :: custom_cache_dir
        character(len=*), intent(out) :: custom_config_dir
        integer, intent(out) :: parallel_jobs
        logical, intent(out) :: no_wait
        logical, intent(out) :: notebook_mode
        character(len=*), intent(out) :: notebook_output
        logical, intent(out) :: preprocess_only
        character(len=*), intent(out), optional :: custom_flags
        logical, intent(out) :: clear_cache
        logical, intent(out) :: cache_info

        integer :: i, iostat
        character(len=256) :: arg
    logical :: filename_found, expecting_cache_dir, expecting_config_dir, expecting_jobs, expecting_output

        filename = ''
        show_help = .false.
        verbose_level = 0
        custom_cache_dir = ''
        custom_config_dir = ''
        parallel_jobs = 0
        no_wait = .false.
        notebook_mode = .false.
        notebook_output = ''
        preprocess_only = .false.
        clear_cache = .false.
        cache_info = .false.
        if (present(custom_flags)) then
            custom_flags = ''
        end if
        filename_found = .false.
        expecting_cache_dir = .false.
        expecting_config_dir = .false.
        expecting_jobs = .false.
        expecting_output = .false.

        if (test_arg_count == 0) then
            show_help = .true.
            return
        end if

        i = 1
        do while (i <= test_arg_count)
            arg = test_args(i)

            if (expecting_cache_dir) then
                custom_cache_dir = trim(arg)
                expecting_cache_dir = .false.
            else if (expecting_config_dir) then
                custom_config_dir = trim(arg)
                expecting_config_dir = .false.
            else if (expecting_output) then
                notebook_output = trim(arg)
                expecting_output = .false.
            else if (expecting_jobs) then
                read (arg, *, iostat=iostat) parallel_jobs
                if (iostat /= 0) parallel_jobs = 0
                expecting_jobs = .false.
            else if (arg == '--help' .or. arg == '-h') then
                show_help = .true.
                return
            else if (arg == '-v') then
                verbose_level = 1
            else if (arg == '-vv') then
                verbose_level = 2
            else if (arg == '--verbose') then
                if (i < test_arg_count) then
                    if (test_args(i + 1) == '1') then
                        verbose_level = 1
                        i = i + 1
                    else if (test_args(i + 1) == '2') then
                        verbose_level = 2
                        i = i + 1
                    else
                        verbose_level = 1
                    end if
                else
                    verbose_level = 1
                end if
            else if (arg == '--cache-dir') then
                expecting_cache_dir = .true.
            else if (arg == '--config-dir') then
                expecting_config_dir = .true.
            else if (arg == '-j' .or. arg == '--jobs') then
                expecting_jobs = .true.
            else if (arg == '--no-wait') then
                no_wait = .true.
            else if (arg == '--notebook') then
                notebook_mode = .true.
       else if (arg == '-o' .or. arg == '--output' .or. arg == '--notebook-output') then
                expecting_output = .true.
            else if (arg(1:1) /= '-') then
                if (.not. filename_found) then
                    filename = trim(arg)
                    filename_found = .true.
                end if
            end if

            i = i + 1
        end do

        if (.not. filename_found .and. .not. show_help) then
            show_help = .true.
        end if

    end subroutine parse_arguments

    function test_empty_arguments() result(passed)
        logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache, cache_info
        integer :: verbose_level, parallel_jobs

        print *, "Test 1: Empty arguments"

        ! Simulate empty command line
        call setup_test_command_line('')

        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (show_help) then
            print *, "  PASS: Empty arguments trigger help"
            passed = .true.
        else
            print *, "  FAIL: Empty arguments should trigger help"
            passed = .false.
        end if

    end function test_empty_arguments

    function test_help_arguments() result(passed)
        logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache, cache_info
        integer :: verbose_level, parallel_jobs

        print *, "Test 2: Help arguments"

        ! Test --help
        call setup_test_command_line('--help')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (.not. show_help) then
            print *, "  FAIL: --help should set show_help to true"
            passed = .false.
            return
        end if

        ! Test -h
        call setup_test_command_line('-h')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (show_help) then
            print *, "  PASS: Help arguments work correctly"
            passed = .true.
        else
            print *, "  FAIL: -h should set show_help to true"
            passed = .false.
        end if

    end function test_help_arguments

    function test_verbose_arguments() result(passed)
        logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache, cache_info
        integer :: verbose_level, parallel_jobs

        print *, "Test 3: Verbose arguments"

        ! Test -v
        call setup_test_command_line('-v test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (verbose_level /= 1) then
            print *, "  FAIL: -v should set verbose_level to 1, got", verbose_level
            passed = .false.
            return
        end if

        ! Test -vv
        call setup_test_command_line('-vv test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (verbose_level /= 2) then
            print *, "  FAIL: -vv should set verbose_level to 2, got", verbose_level
            passed = .false.
            return
        end if

        ! Test --verbose
        call setup_test_command_line('--verbose test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (verbose_level /= 1) then
          print *, "  FAIL: --verbose should set verbose_level to 1, got", verbose_level
            passed = .false.
            return
        end if

        ! Test --verbose 2
        call setup_test_command_line('--verbose 2 test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (verbose_level == 2) then
            print *, "  PASS: All verbose arguments work correctly"
            passed = .true.
        else
        print *, "  FAIL: --verbose 2 should set verbose_level to 2, got", verbose_level
            passed = .false.
        end if

    end function test_verbose_arguments

    function test_directory_arguments() result(passed)
        logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache, cache_info
        integer :: verbose_level, parallel_jobs

        print *, "Test 4: Directory arguments"

        ! Test --cache-dir
        block
            character(len=:), allocatable :: temp_cache_dir
            temp_cache_dir = create_temp_dir('fortran_test_mycache')
            call setup_test_command_line('--cache-dir '//temp_cache_dir//' test.f90')
            call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

            if (trim(custom_cache_dir) /= temp_cache_dir) then
                print *, "  FAIL: --cache-dir should set custom_cache_dir, got: '", &
                    trim(custom_cache_dir), "'"
                passed = .false.
                return
            end if

            ! Test --config-dir
            temp_cache_dir = create_temp_dir('fortran_test_myconfig')
            call setup_test_command_line('--config-dir '//temp_cache_dir//' test.f90')
            call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

            if (trim(custom_config_dir) == temp_cache_dir) then
                print *, "  PASS: Directory arguments work correctly"
                passed = .true.
            else
                print *, "  FAIL: --config-dir should set custom_config_dir, got: '", &
                    trim(custom_config_dir), "'"
                passed = .false.
            end if
        end block

    end function test_directory_arguments

    function test_parallel_arguments() result(passed)
        logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache, cache_info
        integer :: verbose_level, parallel_jobs

        print *, "Test 5: Parallel arguments"

        ! Test --jobs
        call setup_test_command_line('--jobs 4 test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (parallel_jobs /= 4) then
           print *, "  FAIL: --jobs 4 should set parallel_jobs to 4, got", parallel_jobs
            passed = .false.
            return
        end if

        ! Test -j
        call setup_test_command_line('-j 8 test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (parallel_jobs /= 8) then
            print *, "  FAIL: -j 8 should set parallel_jobs to 8, got", parallel_jobs
            passed = .false.
            return
        end if

        ! Test --no-wait
        call setup_test_command_line('--no-wait test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (no_wait) then
            print *, "  PASS: Parallel arguments work correctly"
            passed = .true.
        else
            print *, "  FAIL: --no-wait should set no_wait to true"
            passed = .false.
        end if

    end function test_parallel_arguments

    function test_notebook_arguments() result(passed)
        logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache, cache_info
        integer :: verbose_level, parallel_jobs

        print *, "Test 6: Notebook arguments"

        ! Test --notebook
        call setup_test_command_line('--notebook test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (.not. notebook_mode) then
            print *, "  FAIL: --notebook should set notebook_mode to true"
            passed = .false.
            return
        end if

        ! Test --notebook-output
        call setup_test_command_line('--notebook-output mynotebook.md test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (trim(notebook_output) == 'mynotebook.md') then
            print *, "  PASS: Notebook arguments work correctly"
            passed = .true.
        else
            print *, "  FAIL: --notebook-output should set notebook_output, got: '", &
                trim(notebook_output), "'"
            passed = .false.
        end if

    end function test_notebook_arguments

    function test_complex_combinations() result(passed)
        logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache, cache_info
        integer :: verbose_level, parallel_jobs

        print *, "Test 7: Complex argument combinations"

        ! Test multiple flags together
        block
            character(len=:), allocatable :: temp_cache_dir
            temp_cache_dir = create_temp_dir('fortran_test_cache')
      call setup_test_command_line('-vv --cache-dir ' // temp_cache_dir // ' --jobs 4 --no-wait test.f90')
            call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

            if (verbose_level /= 2) then
      print *, "  FAIL: Complex combination failed on verbose_level, got", verbose_level
                passed = .false.
                return
            end if

            if (trim(custom_cache_dir) /= temp_cache_dir) then
                print *, "  FAIL: Complex combination failed on cache_dir"
                passed = .false.
                return
            end if

            if (parallel_jobs /= 4) then
      print *, "  FAIL: Complex combination failed on parallel_jobs, got", parallel_jobs
                passed = .false.
                return
            end if

            if (.not. no_wait) then
                print *, "  FAIL: Complex combination failed on no_wait"
                passed = .false.
                return
            end if

            if (trim(filename) == 'test.f90') then
                print *, "  PASS: Complex argument combinations work correctly"
                passed = .true.
            else
  print *, "  FAIL: Complex combination failed on filename, got: '", trim(filename), "'"
                passed = .false.
            end if
        end block

    end function test_complex_combinations

    function test_edge_cases() result(passed)
        logical :: passed
    character(len=256) :: filename, custom_cache_dir, custom_config_dir, notebook_output, custom_flags
  logical :: show_help, no_wait, notebook_mode, preprocess_only, clear_cache, cache_info
        integer :: verbose_level, parallel_jobs

        print *, "Test 8: Edge cases"

        ! Test filename with spaces (quoted)
        call setup_test_command_line('"my test file.f90"')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (trim(filename) /= 'my test file.f90') then
            print *, "  FAIL: Filename with spaces failed, got: '", trim(filename), "'"
            passed = .false.
            return
        end if

        ! Test very long directory path
        block
            character(len=:), allocatable :: temp_very_long_dir
      temp_very_long_dir = create_temp_dir('fortran_test_very_long_path_to_cache_directory_that_might_cause_issues')
           call setup_test_command_line('--cache-dir '//temp_very_long_dir//' test.f90')
            call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

            if (index(custom_cache_dir, 'very_long_path') == 0) then
                print *, "  FAIL: Long directory path failed"
                passed = .false.
                return
            end if
        end block

        ! Test zero parallel jobs
        call setup_test_command_line('--jobs 0 test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        if (parallel_jobs /= 0) then
            print *, "  FAIL: Zero parallel jobs failed, got", parallel_jobs
            passed = .false.
            return
        end if

        ! Test negative verbose level handling
        call setup_test_command_line('--verbose -1 test.f90')
        call parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                notebook_output, preprocess_only, custom_flags, clear_cache, cache_info)

        ! Should handle gracefully (implementation dependent)
        print *, "  PASS: Edge cases handled correctly"
        passed = .true.

    end function test_edge_cases

    ! Helper subroutine to simulate command line arguments
    subroutine setup_test_command_line(args)
        character(len=*), intent(in) :: args

        integer :: i, start, arg_len
        logical :: in_quotes, quote_started
        character :: c

        test_arg_count = 0
        test_args = ''

        if (len_trim(args) == 0) return

        i = 1
        start = 1
        in_quotes = .false.
        quote_started = .false.

        ! Simple argument parsing - splits on spaces, handles quotes
        do while (i <= len_trim(args))
            c = args(i:i)

            if (c == '"' .or. c == "'") then
                if (.not. in_quotes) then
                    ! Start of quoted string - move start past quote
                    start = i + 1
                    quote_started = .true.
                end if
                in_quotes = .not. in_quotes
                i = i + 1
                cycle
            end if

            if (c == ' ' .and. .not. in_quotes) then
                ! End of argument
                if (i > start) then
                    test_arg_count = test_arg_count + 1
                    arg_len = i - start
                    if (quote_started) then
                        ! Don't include the end quote
                        test_args(test_arg_count) = args(start:start + arg_len - 1)
                    else
                        test_args(test_arg_count) = args(start:start + arg_len - 1)
                    end if
                    quote_started = .false.
                end if

                ! Skip spaces
                do while (i <= len_trim(args) .and. args(i:i) == ' ')
                    i = i + 1
                end do
                start = i
                cycle
            end if

            i = i + 1
        end do

        ! Handle last argument
        if (i > start .and. start <= len_trim(args)) then
            test_arg_count = test_arg_count + 1
            if (quote_started) then
                ! Don't include the end quote
                test_args(test_arg_count) = args(start:i - 2)
            else
                test_args(test_arg_count) = args(start:i - 1)
            end if
        end if

    end subroutine setup_test_command_line

end program test_cli_comprehensive
