program test_verbose
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    integer :: exit_code
    character(len=1024) :: output
    character(len=256) :: test_program
    logical :: verbose_found, build_found

    ! Create a simple test program
    test_program = 'test_verbose_hello.f90'
    call create_test_program(test_program)

    ! Test 1: Default (quiet) mode - should not show FPM output
    print *, 'Test 1: Default quiet mode'
    call run_fortran(test_program, '', output, exit_code)
    if (exit_code /= 0) then
        write (error_unit, *) 'FAIL: Non-zero exit code in quiet mode'
        stop 1
    end if

    ! Check that FPM build output is suppressed
    build_found = index(output, 'Project compiled') > 0
    if (build_found) then
        write (error_unit, *) 'FAIL: FPM output visible in quiet mode'
        stop 1
    end if
    print *, 'PASS: Quiet mode suppresses FPM output'

    ! Test 2: Verbose mode (-v) - should show FPM output
    print *, ''
    print *, 'Test 2: Verbose mode (-v)'
    call run_fortran(test_program, '-v', output, exit_code)
    if (exit_code /= 0) then
        write (error_unit, *) 'FAIL: Non-zero exit code in verbose mode'
        stop 1
    end if

    build_found = index(output, 'Project compiled') > 0
    if (.not. build_found) then
        write (error_unit, *) 'FAIL: FPM output not visible in verbose mode'
        stop 1
    end if
    print *, 'PASS: Verbose mode shows FPM output'

    ! Test 3: Very verbose mode (-vv) - should show detailed output
    print *, ''
    print *, 'Test 3: Very verbose mode (-vv)'
    call run_fortran(test_program, '-vv', output, exit_code)
    if (exit_code /= 0) then
        write (error_unit, *) 'FAIL: Non-zero exit code in very verbose mode'
        stop 1
    end if

    verbose_found = index(output, '<INFO>') > 0
    if (.not. verbose_found) then
        write (error_unit, *) 'FAIL: Detailed output not visible in very verbose mode'
        stop 1
    end if
    print *, 'PASS: Very verbose mode shows detailed output'

    ! Clean up
    call execute_command_line('rm -f '//trim(test_program))

    print *, ''
    print *, 'All verbose tests passed!'

contains

    subroutine create_test_program(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program test_hello'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Test output"'
        write (unit, '(a)') 'end program test_hello'
        close (unit)
    end subroutine create_test_program

    subroutine run_fortran(filename, flags, output, exit_code)
        character(len=*), intent(in) :: filename, flags
        character(len=*), intent(out) :: output
        integer, intent(out) :: exit_code

        character(len=512) :: command
        character(len=256) :: temp_cache
        character(len=:), allocatable :: temp_dir, temp_output_file
        integer :: unit, iostat
        character(len=1024) :: line

        ! Use temporary cache to ensure fresh builds
        temp_cache = './test_verbose_cache'
        call execute_command_line('rm -rf '//trim(temp_cache))

        ! Create temp directory and file path
        temp_dir = create_temp_dir('fortran_test')
        temp_output_file = get_temp_file_path(temp_dir, 'test_output.tmp')

        ! Build command with custom cache
        command = 'fpm run fortran -- --cache-dir '//trim(temp_cache)// &
                  ' '//trim(flags)//' '// &
                  trim(filename)//' > '//temp_output_file//' 2>&1'

        ! Run command
        call execute_command_line(trim(command), exitstat=exit_code)

        ! Read output
        output = ''
        open (newunit=unit, file=temp_output_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                output = trim(output)//' '//trim(line)
            end do
            close (unit)
        end if

        ! Clean up
        call execute_command_line('rm -f '//temp_output_file)
        call execute_command_line('rm -rf '//trim(temp_cache))

    end subroutine run_fortran

end program test_verbose
