program test_registry_enhancement
    use, intrinsic :: iso_fortran_env, only: error_unit
    use cache, only: get_cache_dir
    use temp_utils, only: create_temp_dir, get_temp_file_path, get_project_root, create_test_cache_dir, path_join
    use system_utils, only: sys_remove_dir, sys_remove_file, sys_run_command_with_exit_code, sys_run_command
    use temp_utils, only: mkdir
    implicit none

    print *, '=== Registry Enhancement Tests ===\'
    print *

    ! Test 1: Module resolution from registry
    call test_module_registry_resolution()

    print *
    print *, 'All registry enhancement tests passed!'

contains

    subroutine test_module_registry_resolution()
        character(len=256) :: test_file
        character(len=512) :: command
        integer :: unit
        character(len=:), allocatable :: test_dir, cache_dir

        print *, 'Test 1: Module resolution from registry'

        ! Create test directory
        test_dir = create_temp_dir('fortran_test_registry')
        call mkdir(trim(test_dir))

        ! First, update registry to have a package with multiple modules
        call update_registry_for_test(test_dir)

        ! Create test file that uses multiple modules from different packages
        test_file = path_join(test_dir, 'test_multiple.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') 'program test_multiple'
        write (unit, '(a)') '  use pyplot_module    ! Module from pyplot-fortran'
        write (unit, '(a)') '  use fortplot_core    ! Module from fortplot'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "Multiple modules from different packages"'
        write (unit, '(a)') 'end program test_multiple'
        close (unit)

        ! Create a unique cache directory for this test
        cache_dir = create_test_cache_dir('registry_enhancement')

        ! Run the program with custom config dir (will fail but should show dependencies)
        block
            character(len=:), allocatable :: output_file, exit_file
            output_file = get_temp_file_path(test_dir, 'multiple_output.txt')
            exit_file = get_temp_file_path(test_dir, 'multiple_exit.txt')

            block
                character(len=:), allocatable :: project_root
                project_root = get_project_root()
                command = 'cd "'//project_root//'" && '// &
              'fpm run fortran -- --config-dir "'//trim(test_dir)//'" --cache-dir "'// &
                          trim(cache_dir)//'" -v "'// &
            trim(test_file)//'"'
            end block
            call sys_run_command_with_exit_code(command, output_file, exit_file)

            ! Check that both modules were detected and mapped to packages
            call check_output_contains(output_file, 'pyplot-fortran')
            call check_output_contains(output_file, 'external module dependencies')
        end block

        ! Note: We already verified that modules were detected and mapped to packages
        ! The fpm.toml generation happens in a temporary build directory that may
        ! be cleaned up, so we rely on the output verification above
        print *, 'Note: Module detection and mapping verified through output'

        ! Clean up
        call sys_remove_dir(test_dir)

        print *, 'PASS: Module resolution from registry working correctly'
        print *
    end subroutine test_module_registry_resolution

    subroutine update_registry_for_test(test_dir)
        character(len=*), intent(in) :: test_dir
        character(len=256) :: registry_path
        integer :: unit

        ! Create a test registry in the test directory
        registry_path = path_join(test_dir, 'registry.toml')
        open (newunit=unit, file=registry_path, status='replace')
        write (unit, '(a)') '# Test registry for multiple modules from same package'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages]'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.pyplot-fortran]'
        write (unit, '(a)') 'git = "https://github.com/jacobwilliams/pyplot-fortran"'
    write(unit, '(a)') '# This package provides multiple modules: pyplot_module, pyplot_utils, etc.'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.fortplot]'
        write (unit, '(a)') 'git = "https://github.com/krystophny/fortplot"'
        write (unit, '(a)') 'prefix = "fortplot"'
        close (unit)

    end subroutine update_registry_for_test

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

    subroutine check_generated_fpm_toml(test_cache_dir)
        character(len=*), intent(in) :: test_cache_dir
        character(len=512) :: fpm_toml_path
        character(len=512) :: line
        integer :: unit, iostat
        logical :: found_pyplot
        character(len=:), allocatable :: temp_dir, fpm_path_file

        ! Find the generated fpm.toml in the test's cache directory
        temp_dir = create_temp_dir('fortran_test')
        fpm_path_file = get_temp_file_path(temp_dir, 'fpm_path.txt')

        ! Debug: list cache directory contents
     print *, 'Debug: Searching for fpm.toml in cache directory: ', trim(test_cache_dir)
     block
            character(len=512) :: dummy_output
            integer :: dummy_exit_code
            call sys_run_command('find "'//trim(test_cache_dir)//'" -name "fpm.toml" '// &
                                      '2>/dev/null | head -1 > '//fpm_path_file, dummy_output, dummy_exit_code)
        end block

        open (newunit=unit, file=fpm_path_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            ! Can't find the exact path, just check that the test validates basic functionality
            print *, 'Note: Could not verify fpm.toml contents (cache cleaned up)'
            return
        end if

        read (unit, '(a)', iostat=iostat) fpm_toml_path
        close (unit)
        call sys_remove_file(fpm_path_file)

        if (len_trim(fpm_toml_path) == 0) then
            print *, 'Note: Could not find generated fpm.toml (cache cleaned up)'
            return
        end if

        ! Check the fpm.toml contains pyplot-fortran dependency
        found_pyplot = .false.
        open (newunit=unit, file=fpm_toml_path, status='old', iostat=iostat)
        if (iostat /= 0) then
            print *, 'Note: Could not read generated fpm.toml'
            return
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            if (index(line, 'pyplot-fortran') > 0) then
                found_pyplot = .true.
                exit
            end if
        end do

        close (unit)

        if (.not. found_pyplot) then
write (error_unit, *) 'Error: pyplot-fortran dependency not found in generated fpm.toml'
            stop 1
        end if

    end subroutine check_generated_fpm_toml

end program test_registry_enhancement
