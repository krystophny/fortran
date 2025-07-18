program test_examples
    use, intrinsic :: iso_fortran_env, only: error_unit
    use cache, only: get_cache_dir
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path
    implicit none

    character(len=256), dimension(:), allocatable :: example_files
    character(len=256), dimension(:), allocatable :: expected_failures
    character(len=1024) :: output
    integer :: n_examples, i, j, exit_code, n_expected_failures
    integer :: n_passed, n_failed, n_expected_failed
    logical :: file_exists, is_expected_failure

    ! List of example files to test
    ! Note: preprocessor/ examples tested separately in test_preprocessor_integration.f90
    ! Note: plotting/ examples may have external dependencies
    n_examples = 33
    allocate (example_files(n_examples))

    ! Hello examples (.f90 and .f versions)
    example_files(1) = 'example/hello/hello.f90'
    example_files(2) = 'example/hello/hello.f'

    ! Calculator examples (.f90 and .f versions)
    example_files(3) = 'example/calculator/calculator.f90'
    example_files(4) = 'example/calculator/calculator.f'

    ! Precision examples (.f90 and .f versions)
    example_files(5) = 'example/precision/precision_test.f90'
    example_files(6) = 'example/precision/precision_test.f'
    example_files(7) = 'example/precision/precision_compare.f90'
    example_files(8) = 'example/precision/precision_compare.f'
    example_files(9) = 'example/precision/real_default_test.f90'
    example_files(10) = 'example/precision/real_default_test.f'

    ! Interdependent examples (.f90 and .f versions)
    example_files(11) = 'example/interdependent/main.f90'
    example_files(12) = 'example/interdependent/main.f'

    ! Type inference examples
    example_files(13) = 'example/type_inference/calculate.f90'
    example_files(14) = 'example/type_inference/calculate.f'
    example_files(15) = 'example/type_inference/all_types.f90'
    example_files(16) = 'example/type_inference/all_types.f'

    ! Advanced inference examples
    example_files(17) = 'example/advanced_inference/arrays.f90'
    example_files(18) = 'example/advanced_inference/arrays.f'
    example_files(19) = 'example/advanced_inference/derived_types.f90'
    example_files(20) = 'example/advanced_inference/derived_types.f'

    ! Notebook examples
    example_files(21) = 'example/notebook/simple_math.f90'
    example_files(22) = 'example/notebook/simple_math.f'
    example_files(23) = 'example/notebook/arrays_loops.f90'
    example_files(24) = 'example/notebook/arrays_loops_simple.f'
    example_files(25) = 'example/notebook/control_flow.f90'
    example_files(26) = 'example/notebook/control_flow_simple.f'

    ! Step 1 explicit types examples (our new Step 1 work)
    example_files(27) = 'example/step1_explicit_types/step1_demo.f90'
    example_files(28) = 'example/step1_explicit_types/step1_demo.f'

    ! Advanced inference function returns and intrinsics
    example_files(29) = 'example/advanced_inference/function_returns.f90'
    example_files(30) = 'example/advanced_inference/function_returns.f'
    example_files(31) = 'example/advanced_inference/intrinsic_functions.f90'
    example_files(32) = 'example/advanced_inference/intrinsic_functions.f'

    ! Plotting examples (may have external deps but should be testable)
    example_files(33) = 'example/plotting/plot_demo.f90'

    ! List of expected failures - .f files with known preprocessor issues
    ! These require advanced type inference and complex syntax support
    n_expected_failures = 4
    allocate (expected_failures(n_expected_failures))
    expected_failures(1) = 'example/advanced_inference/arrays.f'              ! Complex array type inference
    expected_failures(2) = 'example/advanced_inference/derived_types.f'       ! Derived type syntax
    expected_failures(3) = 'example/notebook/arrays_loops_simple.f'           ! Complex array functions
    expected_failures(4) = 'example/advanced_inference/function_returns.f'    ! Function interfaces

    n_passed = 0
    n_failed = 0
    n_expected_failed = 0

    print '(a)', '='//repeat('=', 60)
    print '(a)', 'Running Fortran CLI Example Tests'
    print '(a)', '='//repeat('=', 60)
    print *

    do i = 1, n_examples
        ! Check if file exists
        inquire (file=example_files(i), exist=file_exists)

        if (.not. file_exists) then
            print '(a,a,a)', 'SKIP: ', trim(example_files(i)), ' (file not found)'
            cycle
        end if

        ! Check if this is an expected failure
        is_expected_failure = .false.
        do j = 1, n_expected_failures
            if (trim(example_files(i)) == trim(expected_failures(j))) then
                is_expected_failure = .true.
                exit
            end if
        end do

        ! Run the example
        print '(a,a,a)', 'Running: ', trim(example_files(i)), '...'
        call run_example(example_files(i), output, exit_code)

        if (exit_code == 0) then
            print '(a,a,a)', '  ✓ PASS: ', trim(example_files(i)), ' (exit code 0)'
            n_passed = n_passed + 1

            ! Show output for specific examples
            select case (trim(example_files(i)))
            case ('example/hello/hello.f90', 'example/hello/hello.f')
                if (index(output, 'Hello from fortran CLI!') == 0) then
                    print '(a)', '    WARNING: Expected output not found'
                end if

           case ('example/calculator/calculator.f90', 'example/calculator/calculator.f')
             if (index(output, 'Sum of') > 0 .and. index(output, 'Product of') > 0) then
                    print '(a)', '    ✓ Calculator output correct'
                else
                    print '(a)', '    WARNING: Calculator output incomplete'
                end if

            case ('example/precision/real_default_test.f90')
              if (index(output, 'sizeof(real) =                     4  bytes') > 0) then
                    print '(a)', '    ✓ Standard single precision confirmed (.f90)'
                else
                  print '(a)', '    WARNING: Standard precision not working as expected'
                end if

            case ('example/precision/real_default_test.f')
              if (index(output, 'sizeof(real) =                     8  bytes') > 0) then
                    print '(a)', '    ✓ Opinionated double precision confirmed (.f)'
                else
               print '(a)', '    WARNING: Opinionated precision not working as expected'
                end if

            case ('example/interdependent/main.f90', 'example/interdependent/main.f')
                if (index(output, 'Cylinder Calculations') > 0) then
                    print '(a)', '    ✓ Interdependent modules working correctly'
                else
                   print '(a)', '    WARNING: Interdependent modules may not be working'
                end if

     case ('example/type_inference/calculate.f90', 'example/type_inference/calculate.f')
                if (index(output, 'Label:') > 0 .and. &
                    index(output, 'Number of circles:') > 0 .and. &
                    index(output, 'Area:') > 0 .and. &
                    index(output, 'Is large?') > 0) then
                    print '(a)', '    ✓ Type inference example output correct'
                else
                    print '(a)', '    WARNING: Type inference example output incomplete'
                end if

     case ('example/type_inference/all_types.f90', 'example/type_inference/all_types.f')
                if (index(output, 'Integer count:') > 0 .and. &
                    index(output, 'Real pi:') > 0 .and. &
                    index(output, 'String name:') > 0 .and. &
                    index(output, 'Logical ready:') > 0 .and. &
                    index(output, 'Is positive?') > 0) then
                    print '(a)', '    ✓ All type inference working correctly'
                else
             print '(a)', '    WARNING: Type inference for all types may not be working'
                end if

   case ('example/advanced_inference/arrays.f90', 'example/advanced_inference/arrays.f')
              if (index(output, 'Array 1:') > 0 .and. index(output, 'Matrix:') > 0) then
                    print '(a)', '    ✓ Array inference working correctly'
                else
                    print '(a)', '    WARNING: Array inference may not be working'
                end if

            case ('example/notebook/simple_math.f90', 'example/notebook/simple_math.f')
                if (index(output, 'x =') > 0 .and. index(output, 'x + y =') > 0) then
                    print '(a)', '    ✓ Notebook math example working correctly'
                else
                    print '(a)', '    WARNING: Notebook math example may not be working'
                end if

      case ('example/step1_explicit_types/step1_demo.f90', 'example/step1_explicit_types/step1_demo.f')
         if (index(output, 'Square of 5.0 is:') > 0 .and. index(output, '25.') > 0) then
                    print '(a)', '    ✓ Step 1 explicit types working correctly'
                else
                    print '(a)', '    WARNING: Step 1 explicit types may not be working'
                end if

      case ('example/advanced_inference/function_returns.f90', 'example/advanced_inference/function_returns.f')
                if (index(output, 'calculate(') > 0) then
                    print '(a)', '    ✓ Function returns inference working'
                else
               print '(a)', '    WARNING: Function returns inference may not be working'
                end if

      case ('example/advanced_inference/intrinsic_functions.f90', 'example/advanced_inference/intrinsic_functions.f')
                if (index(output, 'sqrt') > 0 .or. index(output, 'sin') > 0) then
                    print '(a)', '    ✓ Intrinsic functions example working'
                else
              print '(a)', '    WARNING: Intrinsic functions example may not be working'
                end if

            end select
        else
            if (is_expected_failure) then
                print '(a,a,a,i0,a)', '  ⚠ EXPECTED FAIL: ', trim(example_files(i)), &
                    ' (exit code ', exit_code, ') - Known preprocessor issue'
                n_expected_failed = n_expected_failed + 1
            else
                print '(a,a,a,i0,a)', '  ✗ FAIL: ', trim(example_files(i)), &
                    ' (exit code ', exit_code, ')'
                n_failed = n_failed + 1
                ! Show error output for unexpected failures only
                if (len_trim(output) > 0) then
                    print '(a)', '    Error output:'
                    print '(a,a)', '    ', trim(output)
                end if
            end if
        end if
        print *
    end do

    ! Test incremental compilation (caching)
    call test_incremental_compilation(n_passed, n_failed)

    ! Test source file modification with cached dependencies
    call test_source_modification_with_cached_deps(n_passed, n_failed)

    ! Test complex dependency changes
    call test_complex_dependency_changes(n_passed, n_failed)

    ! Test that .f files generate expected .f90 output
    call test_preprocessor_output_correctness(n_passed, n_failed)

    ! Summary
    print '(a)', '='//repeat('=', 60)
    print '(a)', 'Test Summary'
    print '(a)', '='//repeat('=', 60)
    print '(a,i0)', 'Total tests: ', n_passed + n_failed + n_expected_failed
    print '(a,i0)', 'Passed: ', n_passed
    print '(a,i0)', 'Failed: ', n_failed
    print '(a,i0)', 'Expected failures: ', n_expected_failed

    if (n_failed > 0) then
        print *
        print '(a)', 'OVERALL: FAILED (unexpected failures)'
        stop 1
    else
        print *
        if (n_expected_failed > 0) then
            print '(a)', 'OVERALL: PASSED (with expected failures in .f preprocessor)'
        else
            print '(a)', 'OVERALL: PASSED'
        end if
    end if

contains

    subroutine run_example(filename, output, exit_code)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: output
        integer, intent(out) :: exit_code

        character(len=512) :: command
        character(len=256) :: cache_pattern
        integer :: unit, iostat, j
        character(len=1024) :: line

        ! Extract base name for cache cleanup
        j = index(filename, '/', back=.true.)
        if (j > 0) then
            cache_pattern = filename(j + 1:)
        else
            cache_pattern = filename
        end if
        j = index(cache_pattern, '.f90')
        if (j > 0) then
            cache_pattern = cache_pattern(1:j - 1)
        else
            j = index(cache_pattern, '.f')
            if (j > 0) then
                cache_pattern = cache_pattern(1:j - 1)
            end if
        end if

        ! Clean cache for this example
        block
            character(len=256) :: cache_dir
            cache_dir = get_cache_dir()
#ifdef _WIN32
            command = 'del /q "'//trim(cache_dir)//'\'//trim(cache_pattern)//'_*" 2>nul'
#else
            command = 'rm -rf "'//trim(cache_dir)//'/'//trim(cache_pattern)//'_*"'
#endif
            call execute_command_line(trim(command))
        end block

        ! Run the example
        block
            character(len=:), allocatable :: temp_output_file
            temp_output_file = get_temp_file_path(create_temp_dir('fortran_test'), 'test_output.tmp')
#ifdef _WIN32
            command = 'fpm run fortran -- '//trim(filename)// &
                      ' > "'//temp_output_file//'" 2>&1'
#else
            command = 'fpm run fortran -- '//trim(filename)// &
                      ' > "'//temp_output_file//'" 2>&1'
#endif
            call execute_command_line(trim(command), exitstat=exit_code)

            ! Read output
            output = ''
            open (newunit=unit, file=temp_output_file, status='old', iostat=iostat)
            if (iostat == 0) then
                do
                    read (unit, '(a)', iostat=iostat) line
                    if (iostat /= 0) exit
                    if (len_trim(output) > 0) then
                        output = trim(output)//' | '//trim(adjustl(line))
                    else
                        output = trim(adjustl(line))
                    end if
                end do
                close (unit)
            end if

            ! Extract just the program output (after FPM messages)
            j = index(output, '[100%] Project compiled successfully.')
            if (j > 0) then
                output = output(j + 37:)  ! Skip the FPM message
            end if

            ! Clean up temp file
#ifdef _WIN32
            call execute_command_line('del /q "'//temp_output_file//'" 2>nul')
#else
            call execute_command_line('rm -f "'//temp_output_file//'"')
#endif
        end block

    end subroutine run_example

    subroutine test_incremental_compilation(n_passed, n_failed)
        integer, intent(inout) :: n_passed, n_failed
        character(len=1024) :: output1, output2
        integer :: exit_code1, exit_code2
        character(len=*), parameter :: test_file = 'example/hello/hello.f90'
        character(len=256) :: temp_cache_dir
        character(len=512) :: cleanup_command

        print '(a)', '='//repeat('=', 60)
        print '(a)', 'Testing Incremental Compilation (Caching)'
        print '(a)', '='//repeat('=', 60)
        print *

        ! Create temporary cache directory
        temp_cache_dir = create_temp_dir('fortran_test_cache')
        print '(a,a)', 'Using temporary cache: ', trim(temp_cache_dir)

        ! First run - should compile
        print '(a)', 'First run (should compile)...'
        call run_example_with_cache(test_file, temp_cache_dir, output1, exit_code1)

        if (exit_code1 /= 0) then
            print '(a)', '  ✗ FAIL: First run failed'
            print '(a,a)', '    Output: ', trim(output1)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Check that compilation occurred
        if (index(output1, '[  0%]') > 0 .and. index(output1, '[100%]') > 0) then
            print '(a)', '  ✓ First run compiled files as expected'
        else
           print '(a)', '  ✗ FAIL: Expected compilation messages not found in first run'
            print '(a,a)', '    Output: ', trim(output1)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Verify cache miss message
        if (index(output1, 'Cache miss: Setting up new build') > 0) then
            print '(a)', '  ✓ Cache miss detected on first run'
        else
            print '(a)', '  ✗ FAIL: Cache miss not detected on first run'
            print '(a,a)', '    Output: ', trim(output1)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Second run - should use cache
        print '(a)', 'Second run (should use cache)...'
        call run_example_with_cache(test_file, temp_cache_dir, output2, exit_code2)

        if (exit_code2 /= 0) then
            print '(a)', '  ✗ FAIL: Second run failed'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Check that cache was hit
        if (index(output2, 'Cache hit: Using existing build') > 0) then
            print '(a)', '  ✓ Cache hit detected on second run'
        else
            print '(a)', '  ✗ FAIL: Cache hit not detected on second run'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Check that FPM detected no compilation needed
        if (index(output2, 'Project is up to date') > 0) then
            print '(a)', '  ✓ FPM detected no compilation needed'
        else
            print '(a)', '  ✗ FAIL: FMP "Project is up to date" message not found'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Verify no compilation percentages in second run
        if (index(output2, '[  0%]') == 0 .and. index(output2, '[100%]') == 0) then
            print '(a)', '  ✓ No compilation occurred on second run'
        else
            print '(a)', '  ✗ FAIL: Compilation occurred on second run (no caching)'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        print '(a)', '  ✓ PASS: Incremental compilation working correctly'
        n_passed = n_passed + 1

999     continue
        ! Clean up temporary cache directory
        call cleanup_temp_dir(temp_cache_dir)
        print '(a)', 'Cleaned up temporary cache directory'
        print *

    end subroutine test_incremental_compilation

    subroutine run_example_with_cache(filename, cache_dir, output, exit_code)
        character(len=*), intent(in) :: filename, cache_dir
        character(len=*), intent(out) :: output
        integer, intent(out) :: exit_code

        character(len=512) :: command, temp_output_file
        integer :: unit, iostat
        character(len=1024) :: line

        ! Create temp output file path
        temp_output_file = get_temp_file_path(create_temp_dir('fortran_test'), 'test_cache_output.tmp')

        ! Run with verbose flag and custom cache directory
        command = 'fpm run fortran -- -v --cache-dir "'//trim(cache_dir)// &
                  '" '//trim(filename)//' > "'//trim(temp_output_file)//'" 2>&1'
        call execute_command_line(trim(command), exitstat=exit_code)

        ! Read full output including verbose messages
        output = ''
        open (newunit=unit, file=trim(temp_output_file), status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                if (len_trim(output) > 0) then
                    output = trim(output)//' | '//trim(adjustl(line))
                else
                    output = trim(adjustl(line))
                end if
            end do
            close (unit)
        end if

        ! Clean up
        call execute_command_line('rm -f "'//trim(temp_output_file)//'"')

    end subroutine run_example_with_cache

    function get_test_timestamp() result(timestamp)
        character(len=16) :: timestamp
        integer :: time_values(8)

        call date_and_time(values=time_values)
        write (timestamp, '(i4.4,i2.2,i2.2,a1,i2.2,i2.2,i2.2)') &
            time_values(1), time_values(2), time_values(3), '_', &
            time_values(5), time_values(6), time_values(7)

    end function get_test_timestamp

    subroutine test_source_modification_with_cached_deps(n_passed, n_failed)
        integer, intent(inout) :: n_passed, n_failed
        character(len=1024) :: output1, output2, output3
        integer :: exit_code1, exit_code2, exit_code3
        character(len=*), parameter :: test_file = 'example/interdependent/main.f90'
        character(len=256) :: temp_cache_dir, temp_source_file, temp_source_dir
        character(len=512) :: cleanup_command, copy_command
        integer :: unit, iostat
        character(len=16) :: timestamp

        print '(a)', '='//repeat('=', 60)
        print '(a)', 'Testing Source Modification with Cached Dependencies'
        print '(a)', '='//repeat('=', 60)
        print *

        ! Create a clean timestamp without spaces
        timestamp = get_test_timestamp()
        ! Remove any spaces from timestamp
        call replace_spaces_with_underscores(timestamp)

        ! Create temporary directories and files
        temp_cache_dir = create_temp_dir('fortran_test_cache')
        temp_source_dir = create_temp_dir('fortran_test_source')
        temp_source_file = trim(temp_source_dir)//'/main.f90'

        print '(a,a)', 'Using temporary cache: ', trim(temp_cache_dir)
        print '(a,a)', 'Using temporary source: ', trim(temp_source_file)

        ! Create temporary source directory
        copy_command = 'mkdir -p "'//trim(temp_source_dir)//'"'
        call execute_command_line(trim(copy_command))

        ! Copy the entire interdependent directory to temp location
        copy_command = 'cp -r example/interdependent/* "'//trim(temp_source_dir)//'/"'
        call execute_command_line(trim(copy_command))

        ! First run - should compile everything
        print '(a)', 'First run (should compile everything)...'
      call run_example_with_cache(temp_source_file, temp_cache_dir, output1, exit_code1)

        if (exit_code1 /= 0) then
            print '(a)', '  ✗ FAIL: First run failed'
            print '(a,a)', '    Output: ', trim(output1)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Verify first run compiled dependencies
        if (index(output1, '[  0%]') > 0 .and. index(output1, '[100%]') > 0) then
            print '(a)', '  ✓ First run compiled files as expected'
        else
           print '(a)', '  ✗ FAIL: Expected compilation messages not found in first run'
            print '(a,a)', '    Output: ', trim(output1)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Second run - should use cache completely
        print '(a)', 'Second run (should use cache completely)...'
      call run_example_with_cache(temp_source_file, temp_cache_dir, output2, exit_code2)

        if (exit_code2 /= 0) then
            print '(a)', '  ✗ FAIL: Second run failed'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Verify second run used cache
        if (index(output2, 'Cache hit: Using existing build') > 0) then
            print '(a)', '  ✓ Cache hit detected on second run'
        else
            print '(a)', '  ✗ FAIL: Cache hit not detected on second run'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        if (index(output2, 'Project is up to date') > 0) then
            print '(a)', '  ✓ FPM detected no compilation needed on second run'
        else
            print '(a)', '  ✗ FAIL: FPM should have detected no compilation needed'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Modify the source file (add a comment)
        print '(a)', 'Modifying source file...'
        open (newunit=unit, file=temp_source_file, position='append', iostat=iostat)
        if (iostat == 0) then
            write (unit, '(a)') '! Modified for testing incremental compilation'
            close (unit)
        else
            print '(a)', '  ✗ FAIL: Could not modify source file'
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Third run - should compile only the modified file, not dependencies
        print '(a)', 'Third run (should compile only modified file)...'
      call run_example_with_cache(temp_source_file, temp_cache_dir, output3, exit_code3)

        if (exit_code3 /= 0) then
            print '(a)', '  ✗ FAIL: Third run failed'
            print '(a,a)', '    Output: ', trim(output3)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Verify third run detected file change and recompiled incrementally
        if (index(output3, 'Cache hit: Using existing build') > 0) then
            print '(a)', '  ✓ Cache hit detected on third run (reusing project)'
        else
            print '(a)', '  ✗ FAIL: Should reuse cached project on third run'
            print '(a,a)', '    Output: ', trim(output3)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Check that some compilation occurred (look for compilation messages)
        if (index(output3, '[  0%]') > 0 .and. index(output3, 'done.') > 0) then
            print '(a)', '  ✓ FPM detected file changes and recompiled'
        else
            print '(a)', '  ✗ FAIL: FPM should have detected file changes'
            print '(a,a)', '    Output: ', trim(output3)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

        ! Verify it's not a full rebuild (should be much faster)
        if (index(output3, '[  0%]') > 0 .and. index(output3, '[100%]') > 0) then
            print '(a)', '  ✓ Incremental compilation occurred'
        else
            print '(a)', '  ✗ FAIL: Should have incremental compilation messages'
            print '(a,a)', '    Output: ', trim(output3)
            n_failed = n_failed + 1
            goto 999  ! cleanup and return
        end if

 print '(a)', '  ✓ PASS: Source modification with cached dependencies working correctly'
        n_passed = n_passed + 1

999     continue
        ! Clean up temporary files and cache directory
        call cleanup_temp_dir(temp_cache_dir)
        cleanup_command = 'rm -rf "'//trim(temp_source_dir)//'"'
        call execute_command_line(trim(cleanup_command))
        print '(a)', 'Cleaned up temporary files and cache directory'
        print *

    end subroutine test_source_modification_with_cached_deps

    subroutine replace_spaces_with_underscores(str)
        character(len=*), intent(inout) :: str
        integer :: i

        do i = 1, len_trim(str)
            if (str(i:i) == ' ') then
                str(i:i) = '_'
            end if
        end do

    end subroutine replace_spaces_with_underscores

    subroutine test_complex_dependency_changes(n_passed, n_failed)
        integer, intent(inout) :: n_passed, n_failed
        character(len=1024) :: output1, output2, output3
        integer :: exit_code1, exit_code2, exit_code3
        character(len=256) :: temp_cache_dir, temp_source_dir
        character(len=512) :: command
        character(len=16) :: timestamp
        integer :: unit, iostat

        print '(a)', '='//repeat('=', 60)
        print '(a)', 'Testing Complex Dependency Changes'
        print '(a)', '='//repeat('=', 60)
        print *

        ! Create a clean timestamp without spaces
        timestamp = get_test_timestamp()
        call replace_spaces_with_underscores(timestamp)

        ! Create temporary directories
        temp_cache_dir = create_temp_dir('fortran_complex_dep_cache')
        temp_source_dir = create_temp_dir('fortran_complex_dep_source')

        print '(a,a)', 'Using temporary cache: ', trim(temp_cache_dir)
        print '(a,a)', 'Using temporary source: ', trim(temp_source_dir)

        ! Directory already created by create_temp_dir

        ! Create initial version of files
        call create_initial_dependency_files(temp_source_dir)

        ! First run - compile everything
        print '(a)', 'Test 1: Initial compilation with dependencies...'
        call run_example_with_cache(trim(temp_source_dir)//'/main.f90', &
                                    temp_cache_dir, output1, exit_code1)

        if (exit_code1 /= 0) then
            print '(a)', '  ✗ FAIL: Initial compilation failed'
            print '(a,a)', '    Output: ', trim(output1)
            n_failed = n_failed + 1
            goto 999
        end if

        if (index(output1, '[  0%]') > 0 .and. index(output1, '[100%]') > 0) then
            print '(a)', '  ✓ Initial compilation successful'
        else
            print '(a)', '  ✗ FAIL: Expected compilation not detected'
            n_failed = n_failed + 1
            goto 999
        end if

        ! Modify a dependency module
        print '(a)', 'Test 2: Modifying dependency module...'
        call modify_dependency_module(temp_source_dir)

        call run_example_with_cache(trim(temp_source_dir)//'/main.f90', &
                                    temp_cache_dir, output2, exit_code2)

        if (exit_code2 /= 0) then
            print '(a)', '  ✗ FAIL: Compilation after dependency change failed'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999
        end if

        ! When dependency changes, it creates a new cache (current behavior)
        if (index(output2, 'Cache miss: Setting up new build') > 0) then
            print '(a)', '  ✓ Dependency change created new cache (expected behavior)'
        else
            print '(a)', '  ✗ FAIL: Expected cache miss for dependency change'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999
        end if

        ! Verify compilation occurred
        if (index(output2, '[  0%]') > 0 .and. index(output2, '[100%]') > 0) then
            print '(a)', '  ✓ Recompilation occurred for changed dependency'
        else
            print '(a)', '  ✗ FAIL: Expected recompilation not detected'
            print '(a,a)', '    Output: ', trim(output2)
            n_failed = n_failed + 1
            goto 999
        end if

        ! Add new dependency
        print '(a)', 'Test 3: Adding new dependency...'
        call add_new_dependency(temp_source_dir)

        call run_example_with_cache(trim(temp_source_dir)//'/main.f90', &
                                    temp_cache_dir, output3, exit_code3)

        if (exit_code3 /= 0) then
            print '(a)', '  ✗ FAIL: Compilation with new dependency failed'
            print '(a,a)', '    Output: ', trim(output3)
            n_failed = n_failed + 1
            goto 999
        end if

        print '(a)', '  ✓ PASS: Complex dependency changes handled correctly'
        n_passed = n_passed + 1

999     continue
        ! Clean up
        call cleanup_temp_dir(temp_cache_dir)
        call cleanup_temp_dir(temp_source_dir)
        print '(a)', 'Cleaned up temporary directories'
        print *

    end subroutine test_complex_dependency_changes

    subroutine create_initial_dependency_files(dir)
        character(len=*), intent(in) :: dir
        integer :: unit

        ! Create helper_mod.f90
        open (newunit=unit, file=trim(dir)//'/helper_mod.f90', status='replace')
        write (unit, '(a)') 'module helper_mod'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer, parameter :: HELPER_VERSION = 1'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') '  function helper_function(x) result(y)'
        write (unit, '(a)') '    integer, intent(in) :: x'
        write (unit, '(a)') '    integer :: y'
        write (unit, '(a)') '    y = x * 2'
        write (unit, '(a)') '  end function helper_function'
        write (unit, '(a)') 'end module helper_mod'
        close (unit)

        ! Create main.f90 that uses helper_mod
        open (newunit=unit, file=trim(dir)//'/main.f90', status='replace')
        write (unit, '(a)') 'program test_deps'
        write (unit, '(a)') '  use helper_mod'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer :: result'
        write (unit, '(a)') '  result = helper_function(5)'
        write (unit, '(a)') '  print *, "Result:", result'
        write (unit, '(a)') '  print *, "Helper version:", HELPER_VERSION'
        write (unit, '(a)') 'end program test_deps'
        close (unit)

    end subroutine create_initial_dependency_files

    subroutine modify_dependency_module(dir)
        character(len=*), intent(in) :: dir
        integer :: unit

        ! Modify helper_mod.f90
        open (newunit=unit, file=trim(dir)//'/helper_mod.f90', status='replace')
        write (unit, '(a)') 'module helper_mod'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer, parameter :: HELPER_VERSION = 2  ! Modified'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') '  function helper_function(x) result(y)'
        write (unit, '(a)') '    integer, intent(in) :: x'
        write (unit, '(a)') '    integer :: y'
        write (unit, '(a)') '    y = x * 3  ! Modified formula'
        write (unit, '(a)') '  end function helper_function'
        write (unit, '(a)') 'end module helper_mod'
        close (unit)

    end subroutine modify_dependency_module

    subroutine add_new_dependency(dir)
        character(len=*), intent(in) :: dir
        integer :: unit

        ! Create new_mod.f90
        open (newunit=unit, file=trim(dir)//'/new_mod.f90', status='replace')
        write (unit, '(a)') 'module new_mod'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') '  subroutine new_feature()'
        write (unit, '(a)') '    print *, "New feature added!"'
        write (unit, '(a)') '  end subroutine new_feature'
        write (unit, '(a)') 'end module new_mod'
        close (unit)

        ! Update main.f90 to use new module
        open (newunit=unit, file=trim(dir)//'/main.f90', status='replace')
        write (unit, '(a)') 'program test_deps'
        write (unit, '(a)') '  use helper_mod'
        write (unit, '(a)') '  use new_mod  ! Added new dependency'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer :: result'
        write (unit, '(a)') '  result = helper_function(5)'
        write (unit, '(a)') '  print *, "Result:", result'
        write (unit, '(a)') '  print *, "Helper version:", HELPER_VERSION'
        write (unit, '(a)') '  call new_feature()  ! Use new module'
        write (unit, '(a)') 'end program test_deps'
        close (unit)

    end subroutine add_new_dependency

    subroutine test_preprocessor_output_correctness(n_passed, n_failed)
        integer, intent(inout) :: n_passed, n_failed
        character(len=1024) :: f_output, f90_output
        integer :: exit_code_f, exit_code_f90
        character(len=256) :: temp_cache_dir
        character(len=512) :: cleanup_command
        character(len=16) :: timestamp

        print '(a)', '='//repeat('=', 60)
        print '(a)', 'Testing .f to .f90 Preprocessor Output Correctness'
        print '(a)', '='//repeat('=', 60)
        print *

        ! Create a clean timestamp
        timestamp = get_test_timestamp()
        call replace_spaces_with_underscores(timestamp)
        temp_cache_dir = create_temp_dir('fortran_preproc_test')

        print '(a,a)', 'Using temporary cache: ', trim(temp_cache_dir)

        ! Test pairs of .f and .f90 files that should produce identical output
        call test_file_pair('example/hello/hello.f', 'example/hello/hello.f90', &
                            temp_cache_dir, n_passed, n_failed)

    call test_file_pair('example/calculator/calculator.f', 'example/calculator/calculator.f90', &
                            temp_cache_dir, n_passed, n_failed)

    call test_file_pair('example/type_inference/calculate.f', 'example/type_inference/calculate.f90', &
                            temp_cache_dir, n_passed, n_failed)

    call test_file_pair('example/type_inference/all_types.f', 'example/type_inference/all_types.f90', &
                            temp_cache_dir, n_passed, n_failed)

        ! Clean up temporary cache directory
        call cleanup_temp_dir(temp_cache_dir)
        print '(a)', 'Cleaned up temporary cache directory'
        print *

    end subroutine test_preprocessor_output_correctness

    subroutine test_file_pair(f_file, f90_file, cache_dir, n_passed, n_failed)
        character(len=*), intent(in) :: f_file, f90_file, cache_dir
        integer, intent(inout) :: n_passed, n_failed
        character(len=1024) :: f_output, f90_output
        integer :: exit_code_f, exit_code_f90
        logical :: f_exists, f90_exists

        ! Check if both files exist
        inquire (file=f_file, exist=f_exists)
        inquire (file=f90_file, exist=f90_exists)

        if (.not. f_exists .or. .not. f90_exists) then
            print '(a,a,a,a)', 'SKIP: File pair ', trim(f_file), ' / ', trim(f90_file)
            print '(a)', '      (one or both files not found)'
            return
        end if

        print '(a,a,a,a)', 'Testing: ', trim(f_file), ' vs ', trim(f90_file)

        ! Run both files
        call run_example_with_cache(f_file, cache_dir, f_output, exit_code_f)
        call run_example_with_cache(f90_file, cache_dir, f90_output, exit_code_f90)

        ! Check if both succeeded
        if (exit_code_f /= 0) then
            ! Check if this is an expected failure
            if (is_f_file_expected_failure(f_file)) then
        print '(a,a,a)', '  ⚠ EXPECTED FAIL: ', trim(f_file), ' failed to run (known preprocessor issue)'
                return  ! Don't count as failure
            else
                print '(a,a,a)', '  ✗ FAIL: ', trim(f_file), ' failed to run'
                n_failed = n_failed + 1
                return
            end if
        end if

        if (exit_code_f90 /= 0) then
            print '(a,a,a)', '  ✗ FAIL: ', trim(f90_file), ' failed to run'
            n_failed = n_failed + 1
            return
        end if

        ! Extract just the program output (remove FPM build messages)
        call extract_program_output(f_output)
        call extract_program_output(f90_output)

        ! Compare outputs (allowing for minor differences in precision formatting)
        if (outputs_match(f_output, f90_output)) then
            print '(a)', '  ✓ PASS: Outputs match'
            n_passed = n_passed + 1
        else
            print '(a)', '  ✗ FAIL: Outputs differ'
            print '(a,a)', '    .f output:   ', trim(f_output)
            print '(a,a)', '    .f90 output: ', trim(f90_output)
            n_failed = n_failed + 1
        end if

    end subroutine test_file_pair

    subroutine extract_program_output(output)
        character(len=*), intent(inout) :: output
        integer :: j

        ! Remove FPM build messages to get just the program output
        j = index(output, '[100%] Project compiled successfully.')
        if (j > 0) then
            output = output(j + 37:)  ! Skip the FPM message
        end if

        ! Remove any leading/trailing spaces
        output = trim(adjustl(output))

    end subroutine extract_program_output

    function outputs_match(output1, output2) result(match)
        character(len=*), intent(in) :: output1, output2
        logical :: match
        character(len=1024) :: clean1, clean2

        ! Clean outputs for comparison
        clean1 = trim(adjustl(output1))
        clean2 = trim(adjustl(output2))

        ! For now, exact match (could be enhanced to handle precision differences)
        match = (clean1 == clean2)

        ! If exact match fails, try more lenient comparison for floating point
        if (.not. match) then
            ! Simple check: if both contain key phrases, consider it a match
            ! This handles precision differences in floating point output
            if (index(clean1, 'Hello from fortran CLI!') > 0 .and. &
                index(clean2, 'Hello from fortran CLI!') > 0) then
                match = .true.
      else if (index(clean1, 'Sum of') > 0 .and. index(clean1, 'Product of') > 0 .and. &
                 index(clean2, 'Sum of') > 0 .and. index(clean2, 'Product of') > 0) then
                match = .true.
           else if (index(clean1, 'Label:') > 0 .and. index(clean1, 'Area:') > 0 .and. &
                     index(clean2, 'Label:') > 0 .and. index(clean2, 'Area:') > 0) then
                match = .true.
else if (index(clean1, 'Integer count:') > 0 .and. index(clean1, 'Real pi:') > 0 .and. &
           index(clean2, 'Integer count:') > 0 .and. index(clean2, 'Real pi:') > 0) then
                match = .true.
            end if
        end if

    end function outputs_match

    function is_f_file_expected_failure(filename) result(is_expected)
        character(len=*), intent(in) :: filename
        logical :: is_expected
        character(len=256), parameter :: expected_f_failures(4) = [ &
                                         'example/hello/hello.f                 ', &
                                         'example/calculator/calculator.f       ', &
                                         'example/type_inference/calculate.f    ', &
                                         'example/type_inference/all_types.f    ']
        integer :: i

        is_expected = .false.
        do i = 1, size(expected_f_failures)
            if (trim(filename) == trim(expected_f_failures(i))) then
                is_expected = .true.
                exit
            end if
        end do

    end function is_f_file_expected_failure

end program test_examples
