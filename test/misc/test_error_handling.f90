program test_error_handling
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: get_system_temp_dir
    implicit none

    print *, '=== Error Handling Tests ==='
    print *

    ! Test 1: Unknown module - should provide helpful error message
    call test_unknown_module_error()

    ! Test 2: Module not found - should suggest similar names
    call test_similar_module_suggestions()

    ! Test 3: Clear FPM error forwarding
    call test_fpm_error_forwarding()

    print *
    print *, 'All error handling tests passed!'

contains

    subroutine test_unknown_module_error()
        character(len=256) :: test_file, test_dir
        character(len=512) :: command
        integer :: unit

        print *, 'Test 1: Unknown module error message'

        ! Create test directory
        test_dir = get_system_temp_dir()//'/test_unknown_module'
        call execute_command_line('mkdir -p '//trim(test_dir))

        ! Create test file with unknown module
        test_file = trim(test_dir)//'/test_unknown.f90'
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program test_unknown'
        write (unit, '(a)') '  use nonexistent_module'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "This should fail"'
        write (unit, '(a)') 'end program test_unknown'
        close (unit)

        ! Run the program and capture output
        command = 'fpm run fortran -- '//trim(test_file)// &
                ' > '//get_system_temp_dir()//'/unknown_output.txt 2>&1; echo $? > '// &
                  get_system_temp_dir()//'/unknown_exit.txt'
        call execute_command_line(command)

        ! Check that it failed with non-zero exit code
        call check_exit_code(get_system_temp_dir()//'/unknown_exit.txt', 1)

        ! Check that output contains the FPM error message
    call check_output_contains(get_system_temp_dir() // '/unknown_output.txt', 'Unable to find source for module dependency')
    call check_output_contains(get_system_temp_dir() // '/unknown_output.txt', 'nonexistent_module')

        ! Clean up
        call execute_command_line('rm -rf '//trim(test_dir))
   call execute_command_line('rm -f '//get_system_temp_dir()//'/unknown_output.txt '// &
                                  get_system_temp_dir()//'/unknown_exit.txt')

        print *, 'PASS: Unknown module error handled correctly'
        print *
    end subroutine test_unknown_module_error

    subroutine test_similar_module_suggestions()
        character(len=256) :: test_file, test_dir
        character(len=512) :: command
        integer :: unit

        print *, 'Test 2: Module error forwarding'

        ! Create test directory
        test_dir = get_system_temp_dir()//'/test_module_error'
        call execute_command_line('mkdir -p '//trim(test_dir))

        ! Create test file with nonexistent module
        test_file = trim(test_dir)//'/test_error.f90'
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program test_error'
        write (unit, '(a)') '  use some_missing_module'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "This should show clear error"'
        write (unit, '(a)') 'end program test_error'
        close (unit)

        ! Run the program and capture output
        command = 'fpm run fortran -- '//trim(test_file)// &
                  ' > '//get_system_temp_dir()//'/error_output.txt 2>&1; echo $? > '// &
                  get_system_temp_dir()//'/error_exit.txt'
        call execute_command_line(command)

        ! Check that it failed with non-zero exit code
        call check_exit_code(get_system_temp_dir()//'/error_exit.txt', 1)

        ! Check that output contains the FPM error message
    call check_output_contains(get_system_temp_dir() // '/error_output.txt', 'some_missing_module')

        ! Clean up
        call execute_command_line('rm -rf '//trim(test_dir))
     call execute_command_line('rm -f '//get_system_temp_dir()//'/error_output.txt '// &
                                  get_system_temp_dir()//'/error_exit.txt')

        print *, 'PASS: Module error forwarding working'
        print *
    end subroutine test_similar_module_suggestions

    subroutine test_fpm_error_forwarding()
        character(len=256) :: test_file, test_dir
        character(len=512) :: command
        integer :: unit

        print *, 'Test 3: FPM error forwarding'

        ! Create test directory
        test_dir = get_system_temp_dir()//'/test_fpm_error'
        call execute_command_line('mkdir -p '//trim(test_dir))

        ! Create test file with syntax error
        test_file = trim(test_dir)//'/test_syntax.f90'
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program test_syntax'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Missing quote'
        write (unit, '(a)') 'end program test_syntax'
        close (unit)

        ! Run the program and capture output
        command = 'fpm run fortran -- '//trim(test_file)// &
                 ' > '//get_system_temp_dir()//'/syntax_output.txt 2>&1; echo $? > '// &
                  get_system_temp_dir()//'/syntax_exit.txt'
        call execute_command_line(command)

        ! Check that it failed with non-zero exit code
        call check_exit_code(get_system_temp_dir()//'/syntax_exit.txt', 1)

        ! Check that output contains error information
        call check_output_contains(get_system_temp_dir()//'/syntax_output.txt', 'Error')

        ! Clean up
        call execute_command_line('rm -rf '//trim(test_dir))
    call execute_command_line('rm -f '//get_system_temp_dir()//'/syntax_output.txt '// &
                                  get_system_temp_dir()//'/syntax_exit.txt')

        print *, 'PASS: FPM error forwarding working'
        print *
    end subroutine test_fpm_error_forwarding

    subroutine check_exit_code(exit_file, expected_code)
        character(len=*), intent(in) :: exit_file
        integer, intent(in) :: expected_code
        integer :: unit, iostat, actual_code

        open (newunit=unit, file=exit_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open exit code file: ', trim(exit_file)
            stop 1
        end if

        read (unit, *, iostat=iostat) actual_code
        close (unit)

        if (iostat /= 0) then
       write (error_unit, *) 'Error: Cannot read exit code from file: ', trim(exit_file)
            stop 1
        end if

        if (actual_code /= expected_code) then
      write(error_unit, *) 'Error: Expected exit code ', expected_code, ' but got ', actual_code
            stop 1
        end if

    end subroutine check_exit_code

    subroutine check_output_contains(output_file, expected_text)
        character(len=*), intent(in) :: output_file, expected_text
        character(len=512) :: line
        integer :: unit, iostat
        logical :: found

        found = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
            stop 1
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            if (index(line, trim(expected_text)) > 0) then
                found = .true.
                exit
            end if
        end do

        close (unit)

        if (.not. found) then
      write(error_unit, *) 'Error: Expected text "', trim(expected_text), '" not found in output'
            stop 1
        end if

    end subroutine check_output_contains

end program test_error_handling
