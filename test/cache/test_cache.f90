program test_cache
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: create_temp_dir, get_temp_file_path, temp_dir_manager
    implicit none

    character(len=256) :: test_cache_dir, test_program
    character(len=1024) :: output1, output2
    integer :: exit_code
    logical :: cache_exists
    character(len=:), allocatable :: temp_dir
    type(temp_dir_manager) :: temp_mgr

    ! Create a temp directory for the test
    call temp_mgr%create('fortran_test_cache')
    temp_dir = temp_mgr%path

    ! Create test cache directory
    test_cache_dir = './test_cache_tmp'
    test_program = 'test_cache_hello.f90'

    ! Clean up any existing test cache
    call execute_command_line('rm -rf '//trim(test_cache_dir))

    ! Create test program
    call create_test_program(test_program)

    print *, 'Test 1: First run with custom cache directory'
   call run_with_cache(test_program, test_cache_dir, '-v', output1, exit_code, temp_dir)

    if (exit_code /= 0) then
        write (error_unit, *) 'FAIL: First run failed'
        stop 1
    end if

    ! Check if cache was created
inquire (file=trim(test_cache_dir)//'/test_hello_'//repeat('*', 10), exist=cache_exists)
  call execute_command_line('ls ' // trim(test_cache_dir) // ' > /dev/null 2>&1', exitstat=exit_code)
    if (exit_code /= 0) then
        write (error_unit, *) 'FAIL: Cache directory not created'
        stop 1
    end if
    print *, 'PASS: Cache directory created'

    ! Check for build output in first run
    if (index(output1, 'Project compiled successfully') == 0) then
        write (error_unit, *) 'FAIL: No build output in first run'
        stop 1
    end if
    print *, 'PASS: First run shows build output'

    print *, ''
    print *, 'Test 2: Second run should use cache'
   call run_with_cache(test_program, test_cache_dir, '-v', output2, exit_code, temp_dir)

    if (exit_code /= 0) then
        write (error_unit, *) 'FAIL: Second run failed'
        stop 1
    end if

    ! Second run should be faster (less output)
    if (len_trim(output2) >= len_trim(output1)) then
        write (error_unit, *) 'WARNING: Second run not using cache effectively'
    else
        print *, 'PASS: Second run appears to use cache (less output)'
    end if

    print *, ''
    print *, 'Test 3: Verify program output is consistent'
    if (index(output1, 'Test cache output') == 0) then
        write (error_unit, *) 'FAIL: Program output missing in first run'
        stop 1
    end if

    if (index(output2, 'Test cache output') == 0) then
        write (error_unit, *) 'FAIL: Program output missing in second run'
        stop 1
    end if

    print *, 'PASS: Program output consistent across runs'

    ! Clean up
    call execute_command_line('rm -rf '//trim(test_cache_dir))
    call execute_command_line('rm -f '//trim(test_program))

    print *, ''
    print *, 'All cache tests passed!'

contains

    subroutine create_test_program(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program test_hello'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Test cache output"'
        write (unit, '(a)') 'end program test_hello'
        close (unit)
    end subroutine create_test_program

    subroutine run_with_cache(filename, cache_dir, flags, output, exit_code, temp_dir)
        character(len=*), intent(in) :: filename, cache_dir, flags, temp_dir
        character(len=*), intent(out) :: output
        integer, intent(out) :: exit_code

        character(len=512) :: command
        character(len=:), allocatable :: output_file
        integer :: unit, iostat
        character(len=1024) :: line

        ! Create output file path
        output_file = get_temp_file_path(temp_dir, 'test_output.tmp')

        ! Build command with custom cache
        command = 'fpm run fortran -- --cache-dir '//trim(cache_dir)// &
                  ' '//trim(flags)//' '//trim(filename)//' > '//output_file//' 2>&1'

        ! Run command
        call execute_command_line(trim(command), exitstat=exit_code)

        ! Read output
        output = ''
        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                output = trim(output)//' '//trim(line)
            end do
            close (unit)
        end if

        ! Clean up
        call execute_command_line('rm -f '//output_file)

    end subroutine run_with_cache

end program test_cache
