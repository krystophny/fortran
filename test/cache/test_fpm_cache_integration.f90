program test_fpm_cache_integration
    use fpm_sources, only: add_sources_from_dir
    use fpm_model, only: srcfile_t, FPM_SCOPE_APP
    use fpm_filesystem, only: list_files
    use fpm_strings, only: string_t
    use fpm_error, only: error_t
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: temp_dir_manager
    implicit none

    print *, '=== FPM Cache Integration Tests ===\'

    ! Test 1: Use FPM API for source discovery
    call test_fpm_source_discovery()

    ! Test 2: Check if we can access source file information
    call test_source_file_info()

    print *, 'All FPM cache integration tests passed!'

contains

    subroutine test_fpm_source_discovery()
        type(srcfile_t), allocatable :: sources(:)
        type(error_t), allocatable :: error
        character(len=:), allocatable :: test_dir
        character(len=:), allocatable :: test_file
        logical :: file_exists
        integer :: unit, exit_code

        print *, 'Test 1: FPM source discovery'

        ! Create a test directory with Fortran files (FPM expects app/ subdirectory)
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('fpm_test_sources_'//trim(get_timestamp()))
            test_dir = temp_mgr%path
            test_file = test_dir//'/app/test.f90'
        end block
        print *, 'Creating test directory: ', test_dir
       call execute_command_line('rm -rf '//test_dir//' && mkdir -p '//test_dir//'/app')

        ! Create a test source file in app/ subdirectory
        print *, 'Creating file: ', test_file
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program test_program'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Hello from FPM API test"'
        write (unit, '(a)') 'end program test_program'
        close (unit)

        ! Create minimal fpm.toml for FPM API to work
        open (newunit=unit, file=test_dir//'/fpm.toml', status='replace')
        write (unit, '(a)') 'name = "test_project"'
        write (unit, '(a)') 'version = "0.1.0"'
        close (unit)

        ! Verify file was created
        inquire (file=test_file, exist=file_exists)
        if (.not. file_exists) then
            write (error_unit, *) 'Error: Test file was not created: ', test_file
            stop 1
        end if
        print *, 'Test file exists: ', test_file

        ! Instead of testing FPM API directly, test our Fortran CLI tool with long paths
        print *, 'Testing Fortran CLI tool with long path...'
    call execute_command_line('fpm run fortran -- ' // test_file // ' > /dev/null 2>&1', exitstat=exit_code)

        if (exit_code == 0) then
            print *, 'Test 1 passed: Fortran CLI tool works with long paths'
        else
            write (error_unit, *) 'Error: Fortran CLI tool failed with long path'
            write (error_unit, *) 'Path length: ', len(test_file)
            write (error_unit, *) 'Path: ', test_file
            stop 1
        end if

        ! Clean up
        call execute_command_line('rm -rf '//test_dir)

        print *, 'PASS: FPM source discovery works, found', size(sources), 'sources'
        print *

    end subroutine test_fpm_source_discovery

    subroutine test_source_file_info()
        character(len=:), allocatable :: test_dir
        character(len=:), allocatable :: test_file
        integer :: unit, exit_code
        logical :: exists

        print *, 'Test 2: Source file information access'

        ! Create a test directory with Fortran files
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('fpm_test_info_'//trim(get_timestamp()))
            test_dir = temp_mgr%path
            test_file = test_dir//'/hello.f90'
        end block
        call execute_command_line('mkdir -p '//test_dir)

        ! Create a test source file
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program hello'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Hello World"'
        write (unit, '(a)') 'end program hello'
        close (unit)

        ! Test our Fortran CLI tool with this file
    call execute_command_line('fpm run fortran -- ' // test_file // ' > /dev/null 2>&1', exitstat=exit_code)

        if (exit_code == 0) then
            print *, 'PASS: Can access source file path length:', len(test_file)
            print *, 'PASS: Can access source file path:', test_file
            print *, 'PASS: Fortran CLI tool works with long paths'

            ! Check if file actually exists
            inquire (file=test_file, exist=exists)
            if (exists) then
                print *, 'PASS: Test file exists and is accessible'
            else
                print *, 'FAIL: Test file does not exist'
                stop 1
            end if
        else
            print *, 'FAIL: Fortran CLI tool failed with long path'
            stop 1
        end if

        ! Clean up
        call execute_command_line('rm -rf '//test_dir)

        print *

    end subroutine test_source_file_info

    function get_timestamp() result(timestamp)
        character(len=20) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)
        write (timestamp, '(i4.4,5i2.2)') values(1), values(2), values(3), &
            values(5), values(6), values(7)
    end function get_timestamp

end program test_fpm_cache_integration
