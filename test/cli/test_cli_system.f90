program test_cli_system
    use, intrinsic :: iso_fortran_env, only: error_unit
    use cache, only: get_cache_dir
    use temp_utils, only: temp_dir_manager
    implicit none

    character(len=512) :: command, test_file
    character(len=1024) :: output_file, expected_output
    integer :: exit_code, unit, iostat
    logical :: test_passed
    type(temp_dir_manager) :: main_temp_mgr

    print *, '=== System CLI Tests ==='
    print *

    ! Create test file
    call create_test_file()

    ! Test 1: No arguments (should show help)
    call test_no_arguments()

    ! Test 2: --help flag
    call test_help_flag()

    ! Test 3: -h flag
    call test_h_flag()

    ! Test 4: Basic execution
    call test_basic_execution()

    ! Test 5: -v flag
    call test_v_flag()

    ! Test 6: -vv flag
    call test_vv_flag()

    ! Test 7: --verbose flag (no argument)
    call test_verbose_flag()

    ! Test 8: --verbose 1
    call test_verbose_1()

    ! Test 9: --verbose 2
    call test_verbose_2()

    ! Test 10: --cache-dir
    call test_cache_dir()

    ! Test 11: --config-dir
    call test_config_dir()

    ! Test 12: Invalid arguments
    call test_invalid_arguments()

    ! Test 13: .f file execution
    call test_f_file_execution()

    ! Test 14: Invalid file extension
    call test_invalid_extension()

    ! Test 15: --flag option
    call test_flag_option()

    ! Cleanup
    call cleanup_test_files()

    print *
    print *, 'All system CLI tests passed!'

contains

    subroutine create_test_file()
        integer :: unit

        ! Create unique filename in temp directory
        call main_temp_mgr%create('test_cli_sys')
        test_file = main_temp_mgr%get_file_path('test_cli_system.f90')

        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program test_cli_system'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "CLI System Test Output"'
        write (unit, '(a)') 'end program test_cli_system'
        close (unit)

        print *, 'Created test file: ', trim(test_file)
    end subroutine create_test_file

    subroutine test_no_arguments()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 1: No arguments (should show help)'

        call temp_mgr%create('test_no_args')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

  command = 'fpm run fortran -- > "'//output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command)

        call check_help_output(output_file, test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: No arguments test failed'
            stop 1
        end if

        print *, 'PASS: No arguments shows help'
        print *
    end subroutine test_no_arguments

    subroutine test_help_flag()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 2: --help flag'

        call temp_mgr%create('test_help')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

    command = 'fpm run fortran -- --help > "' // output_file // '" 2>&1; echo $? > "' // exit_file // '"'
        call execute_command_line(command)

        call check_help_output(output_file, test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: --help flag test failed'
            stop 1
        end if

        print *, 'PASS: --help flag works'
        print *
    end subroutine test_help_flag

    subroutine test_h_flag()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 3: -h flag'

        call temp_mgr%create('test_h')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

    command = 'fpm run fortran -- -h > "' // output_file // '" 2>&1; echo $? > "' // exit_file // '"'
        call execute_command_line(command)

        call check_help_output(output_file, test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: -h flag test failed'
            stop 1
        end if

        print *, 'PASS: -h flag works'
        print *
    end subroutine test_h_flag

    subroutine test_basic_execution()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 4: Basic execution'

        call temp_mgr%create('test_basic_exec')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

        command = 'fpm run fortran -- "'//trim(test_file)//'" > "'//output_file// &
                  '" 2>&1; echo $? > "'//exit_file//'"'
        print *, 'Executing command: ', trim(command)
        call execute_command_line(command)

        ! Debug: Show what was actually captured
        call debug_output_file(output_file)

        call check_program_output(output_file, 'CLI System Test Output', test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: Basic execution test failed'
            stop 1
        end if

        print *, 'PASS: Basic execution works'
        print *
    end subroutine test_basic_execution

    subroutine test_v_flag()
        type(temp_dir_manager) :: temp_mgr

        print *, 'Test 5: -v flag (cold and warm cache)'

        ! Test 5a: Cold cache (clean build)
        print *, '  Test 5a: Cold cache behavior'
        block
            character(len=256) :: cache_dir, basename
            integer :: last_slash, last_dot

            cache_dir = get_cache_dir()

            ! Extract basename without extension from test_file
            last_slash = index(test_file, '/', back=.true.)
            last_dot = index(test_file, '.', back=.true.)
            if (last_dot > last_slash) then
                basename = test_file(last_slash + 1:last_dot - 1)
            else
                basename = test_file(last_slash + 1:)
            end if

            ! Remove all cache entries for this specific test file
      call execute_command_line('rm -rf "'//trim(cache_dir)//'"/*'//trim(basename)//'*')
        end block

        call temp_mgr%create('test_v_flag')

        command = 'fpm run fortran -- -v '//trim(test_file)// &
   ' > "'//temp_mgr%get_file_path('cli_test_output_cold.txt')//'" 2>&1; echo $? > "'// &
                  temp_mgr%get_file_path('cli_test_exit.txt')//'"'
        call execute_command_line(command)

    call check_program_output(temp_mgr%get_file_path('cli_test_output_cold.txt'), 'CLI System Test Output', test_passed)
    call check_cold_cache_verbose(temp_mgr%get_file_path('cli_test_output_cold.txt'), test_passed)
       call check_exit_code(temp_mgr%get_file_path('cli_test_exit.txt'), 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: -v flag cold cache test failed'
            stop 1
        end if
        print *, '  PASS: Cold cache shows build output'

        ! Test 5b: Warm cache (should use existing build)
        print *, '  Test 5b: Warm cache behavior'
        command = 'fpm run fortran -- -v '//trim(test_file)// &
   ' > "'//temp_mgr%get_file_path('cli_test_output_warm.txt')//'" 2>&1; echo $? > "'// &
                  temp_mgr%get_file_path('cli_test_exit.txt')//'"'
        call execute_command_line(command)

    call check_program_output(temp_mgr%get_file_path('cli_test_output_warm.txt'), 'CLI System Test Output', test_passed)
    call check_warm_cache_verbose(temp_mgr%get_file_path('cli_test_output_warm.txt'), test_passed)
       call check_exit_code(temp_mgr%get_file_path('cli_test_exit.txt'), 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: -v flag warm cache test failed'
            stop 1
        end if
        print *, '  PASS: Warm cache shows cache hit'

        print *, 'PASS: -v flag works for both cache states'
        print *
    end subroutine test_v_flag

    subroutine test_vv_flag()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 6: -vv flag'

        call temp_mgr%create('test_vv')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

        command = 'fpm run fortran -- -vv '//trim(test_file)// &
                  ' > "'//output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command)

        call check_program_output(output_file, 'CLI System Test Output', test_passed)
        call check_verbose_output(output_file, 2, test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: -vv flag test failed'
            stop 1
        end if

        print *, 'PASS: -vv flag works'
        print *
    end subroutine test_vv_flag

    subroutine test_verbose_flag()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 7: --verbose flag (no argument)'

        call temp_mgr%create('test_verbose')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

        command = 'fpm run fortran -- --verbose '//trim(test_file)// &
                  ' > "'//output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command)

        call check_program_output(output_file, 'CLI System Test Output', test_passed)
        call check_verbose_output(output_file, 1, test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: --verbose flag test failed'
            stop 1
        end if

        print *, 'PASS: --verbose flag works'
        print *
    end subroutine test_verbose_flag

    subroutine test_verbose_1()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 8: --verbose 1'

        call temp_mgr%create('test_verbose1')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

        command = 'fpm run fortran -- --verbose 1 '//trim(test_file)// &
                  ' > "'//output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command)

        call check_program_output(output_file, 'CLI System Test Output', test_passed)
        call check_verbose_output(output_file, 1, test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: --verbose 1 test failed'
            stop 1
        end if

        print *, 'PASS: --verbose 1 works'
        print *
    end subroutine test_verbose_1

    subroutine test_verbose_2()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 9: --verbose 2'

        call temp_mgr%create('test_verbose2')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

        command = 'fpm run fortran -- --verbose 2 '//trim(test_file)// &
                  ' > "'//output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command)

        call check_program_output(output_file, 'CLI System Test Output', test_passed)
        call check_verbose_output(output_file, 2, test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: --verbose 2 test failed'
            stop 1
        end if

        print *, 'PASS: --verbose 2 works'
        print *
    end subroutine test_verbose_2

    subroutine test_cache_dir()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file, custom_cache_dir

        print *, 'Test 10: --cache-dir'

        call temp_mgr%create('test_cache_dir')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')
        custom_cache_dir = temp_mgr%get_file_path('custom_cache')

    command = 'fpm run fortran -- --cache-dir "' // custom_cache_dir // '" ' // trim(test_file) // &
                  ' > "'//output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command)

        call check_program_output(output_file, 'CLI System Test Output', test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        ! Check that custom cache directory was created
        call check_directory_exists(custom_cache_dir, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: --cache-dir test failed'
            stop 1
        end if

        print *, 'PASS: --cache-dir works'
        print *
    end subroutine test_cache_dir

    subroutine test_config_dir()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file, custom_config_dir

        print *, 'Test 11: --config-dir'

        call temp_mgr%create('test_config_dir')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')
        custom_config_dir = temp_mgr%get_file_path('custom_config')

        ! Create a custom config directory with registry
        call execute_command_line('mkdir -p "'//custom_config_dir//'"')
        call execute_command_line('cp registry.toml "'//custom_config_dir//'"/')

    command = 'fpm run fortran -- --config-dir "' // custom_config_dir // '" ' // trim(test_file) // &
                  ' > "'//output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command)

        call check_program_output(output_file, 'CLI System Test Output', test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: --config-dir test failed'
            stop 1
        end if

        print *, 'PASS: --config-dir works'
        print *
    end subroutine test_config_dir

    subroutine test_invalid_arguments()
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 12: Invalid arguments'

        call temp_mgr%create('test_invalid_args')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

        ! Test missing cache directory argument
        command = 'fpm run fortran -- --cache-dir > "'//output_file//'" 2>&1; '// &
                  'echo $? > "'//exit_file//'"'
        call execute_command_line(command)

        call check_help_output(output_file, test_passed)
        call check_exit_code(exit_file, 0, test_passed)

        if (.not. test_passed) then
            write (error_unit, *) 'FAIL: Invalid arguments test failed'
            stop 1
        end if

        print *, 'PASS: Invalid arguments handled correctly'
        print *
    end subroutine test_invalid_arguments

    subroutine check_help_output(output_file, passed)
        character(len=*), intent(in) :: output_file
        logical, intent(out) :: passed
        character(len=512) :: line
        integer :: unit, iostat
        logical :: found_usage

        passed = .false.
        found_usage = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
            return
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            if (index(line, 'Usage: fortran') > 0) then
                found_usage = .true.
                exit
            end if
        end do

        close (unit)

        if (found_usage) then
            passed = .true.
        else
            write (error_unit, *) 'Error: Help output not found in ', trim(output_file)
        end if

    end subroutine check_help_output

    subroutine check_program_output(output_file, expected, passed)
        character(len=*), intent(in) :: output_file, expected
        logical, intent(out) :: passed
        character(len=512) :: line
        integer :: unit, iostat
        logical :: found_output

        passed = .false.
        found_output = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
            return
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            if (index(line, trim(expected)) > 0) then
                found_output = .true.
                exit
            end if
        end do

        close (unit)

        if (found_output) then
            passed = .true.
        else
         write (error_unit, *) 'Error: Expected output "', trim(expected), '" not found'
        end if

    end subroutine check_program_output

    subroutine check_verbose_output(output_file, level, passed)
        character(len=*), intent(in) :: output_file
        integer, intent(in) :: level
        logical, intent(out) :: passed
        character(len=512) :: line
        integer :: unit, iostat
        logical :: found_verbose

        passed = .false.
        found_verbose = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
            return
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            if (level == 1) then
                ! Level 1 should show build progress like "[  0%]" or "done." on cache miss
                ! or cache status messages like "Cache hit" or "Cache miss"
                if (index(line, '[') > 0 .and. index(line, '%]') > 0) then
                    found_verbose = .true.
                    exit
                end if
                if (index(line, 'done.') > 0) then
                    found_verbose = .true.
                    exit
                end if
               if (index(line, 'Cache hit') > 0 .or. index(line, 'Cache miss') > 0) then
                    found_verbose = .true.
                    exit
                end if
            else if (level == 2) then
                ! Level 2 should show detailed FPM info like "<INFO>"
                if (index(line, '<INFO>') > 0) then
                    found_verbose = .true.
                    exit
                end if
            end if
        end do

        close (unit)

        if (found_verbose) then
            passed = .true.
        else
            write (error_unit, *) 'Error: Verbose level ', level, ' output not found'
        end if

    end subroutine check_verbose_output

    subroutine check_cold_cache_verbose(output_file, passed)
        character(len=*), intent(in) :: output_file
        logical, intent(out) :: passed
        character(len=512) :: line
        integer :: unit, iostat
        logical :: found_build_output

        passed = .false.
        found_build_output = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
            return
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            ! Cold cache should show either Cache miss or FPM build output
            if (index(line, 'Cache miss') > 0) then
                found_build_output = .true.
                exit
            end if
            if (index(line, '[') > 0 .and. index(line, '%]') > 0) then
                found_build_output = .true.
                exit
            end if
            if (index(line, 'done.') > 0) then
                found_build_output = .true.
                exit
            end if
        end do

        close (unit)

        if (found_build_output) then
            passed = .true.
        else
      write(error_unit, *) 'Error: Cold cache verbose output not found (expected Cache miss or build output)'
        end if

    end subroutine check_cold_cache_verbose

    subroutine check_warm_cache_verbose(output_file, passed)
        character(len=*), intent(in) :: output_file
        logical, intent(out) :: passed
        character(len=512) :: line
        integer :: unit, iostat
        logical :: found_cache_hit

        passed = .false.
        found_cache_hit = .false.

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
            return
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            ! Warm cache should show Cache hit
            if (index(line, 'Cache hit') > 0) then
                found_cache_hit = .true.
                exit
            end if
        end do

        close (unit)

        if (found_cache_hit) then
            passed = .true.
        else
 write (error_unit, *) 'Error: Warm cache verbose output not found (expected Cache hit)'
        end if

    end subroutine check_warm_cache_verbose

    subroutine get_timestamp_suffix(suffix)
        character(len=*), intent(out) :: suffix
        integer :: values(8)

        call date_and_time(values=values)
        write (suffix, '(i2.2,i2.2,i2.2)') values(5), values(6), values(7)

    end subroutine get_timestamp_suffix

    subroutine check_exit_code(exit_file, expected, passed)
        character(len=*), intent(in) :: exit_file
        integer, intent(in) :: expected
        logical, intent(out) :: passed
        integer :: unit, iostat, actual_exit

        passed = .false.

        open (newunit=unit, file=exit_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot open exit code file: ', trim(exit_file)
            return
        end if

        read (unit, *, iostat=iostat) actual_exit
        close (unit)

        if (iostat /= 0) then
            write (error_unit, *) 'Error: Cannot read exit code from ', trim(exit_file)
            return
        end if

        if (actual_exit == expected) then
            passed = .true.
        else
  write (error_unit, *) 'Error: Expected exit code ', expected, ' but got ', actual_exit
        end if

    end subroutine check_exit_code

    subroutine check_directory_exists(directory, passed)
        character(len=*), intent(in) :: directory
        logical, intent(out) :: passed
        logical :: exists

        inquire (file=directory, exist=exists)
        passed = exists

        if (.not. exists) then
            write (error_unit, *) 'Error: Directory does not exist: ', trim(directory)
        end if

    end subroutine check_directory_exists

    subroutine test_f_file_execution()
        character(len=512) :: test_f_file
        logical :: success, exit_ok
        integer :: unit
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 13: .f file execution'

        call temp_mgr%create('test_f_file_exec')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

        ! Create a simple .f test file
        test_f_file = temp_mgr%get_file_path('test_simple.f')
        open (newunit=unit, file=test_f_file, status='replace')
        write (unit, '(a)') "print *, 'Hello from .f file'"
        close (unit)

        command = 'fpm run fortran -- "'//trim(test_f_file)//'" > "'// &
                  output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command, exitstat=exit_code)

        call check_exit_code(exit_file, 0, exit_ok)
        call check_program_output(output_file, 'Hello from .f file', success)

        if (exit_ok .and. success) then
            print *, 'PASS: .f file executed successfully'
        else
            write (error_unit, *) 'FAIL: .f file execution failed'
            stop 1
        end if

        ! Cleanup is automatic via temp_mgr finalizer
    end subroutine test_f_file_execution

    subroutine test_invalid_extension()
        logical :: exit_ok, has_error
        integer :: unit
        character(len=256) :: test_txt_file
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 14: Invalid file extension'

        call temp_mgr%create('test_invalid_ext')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

        ! Create a temporary .txt file
        test_txt_file = temp_mgr%get_file_path('test_invalid.txt')
        open (newunit=unit, file=test_txt_file, status='replace')
        write (unit, '(a)') "This is not a Fortran file"
        close (unit)

        command = 'fpm run fortran -- "'//trim(test_txt_file)//'" > "'// &
                  output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command, exitstat=exit_code)

        call check_exit_code(exit_file, 1, exit_ok)
        call check_program_output(output_file, &
                                 'must have .f90, .F90, .f, or .F extension', has_error)

        if (exit_ok .and. has_error) then
            print *, 'PASS: Invalid extension rejected'
        else
            write (error_unit, *) 'FAIL: Should reject invalid extensions'
            stop 1
        end if

        ! Cleanup is automatic via temp_mgr finalizer
    end subroutine test_invalid_extension

    subroutine test_flag_option()
        logical :: exit_ok, has_output
        integer :: unit
        character(len=256) :: test_flag_file
        type(temp_dir_manager) :: temp_mgr
        character(len=:), allocatable :: output_file, exit_file

        print *, 'Test 15: --flag option functionality'

        call temp_mgr%create('test_flag_option')
        output_file = temp_mgr%get_file_path('cli_test_output.txt')
        exit_file = temp_mgr%get_file_path('cli_test_exit.txt')

        ! Create a test .f90 file to test with custom flags
        test_flag_file = temp_mgr%get_file_path('test_flag_option.f90')
        open (newunit=unit, file=test_flag_file, status='replace')
        write (unit, '(a)') 'program test_flags'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Flag test output"'
        write (unit, '(a)') 'end program test_flags'
        close (unit)

        ! Test with custom optimization flags
        command = 'fpm run fortran -- --flag "-O2" "'//trim(test_flag_file)//'" > "'// &
                  output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command, exitstat=exit_code)

        call check_exit_code(exit_file, 0, exit_ok)
        call check_program_output(output_file, 'Flag test output', has_output)

        if (exit_ok .and. has_output) then
            print *, 'PASS: --flag option works with .f90 files'
        else
            write (error_unit, *) 'FAIL: --flag option failed with .f90 files'
            stop 1
        end if

        ! Test with .f file (should combine with opinionated flags)
        print *, '  Testing .f file with --flag option'

        ! Create a test .f file
        test_flag_file = temp_mgr%get_file_path('test_flag_option.f')
        open (newunit=unit, file=test_flag_file, status='replace')
        write (unit, '(a)') 'print *, "Flag test output with .f file"'
        close (unit)

      command = 'fpm run fortran -- --flag "-Wall" "'//trim(test_flag_file)//'" > "'// &
                  output_file//'" 2>&1; echo $? > "'//exit_file//'"'
        call execute_command_line(command, exitstat=exit_code)

        call check_exit_code(exit_file, 0, exit_ok)
     call check_program_output(output_file, 'Flag test output with .f file', has_output)

        if (exit_ok .and. has_output) then
            print *, 'PASS: --flag option works with .f files'
        else
            write (error_unit, *) 'FAIL: --flag option failed with .f files'
            stop 1
        end if

        ! Cleanup is automatic via temp_mgr finalizer
    end subroutine test_flag_option

    subroutine cleanup_test_files()
        ! Cleanup is now automatic via temp_dir_manager finalizers
        ! The main test file will be cleaned up when main_temp_mgr is finalized
        print *, 'Cleaned up test files'
    end subroutine cleanup_test_files

    subroutine debug_output_file(output_file)
        character(len=*), intent(in) :: output_file
        character(len=1024) :: line
        integer :: unit, iostat

        print *, 'DEBUG: Contents of output file: ', trim(output_file)
        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            print *, 'DEBUG: Could not open output file'
            return
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            print *, 'DEBUG: ', trim(line)
        end do
        close (unit)
        print *, 'DEBUG: End of output file'
    end subroutine debug_output_file

end program test_cli_system
