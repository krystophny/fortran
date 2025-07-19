program test_multiple_modules
    use registry_resolver
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: temp_dir_manager
    implicit none

    character(len=256) :: test_registry_path
    character(len=128) :: package_name
    character(len=256) :: git_url
    logical :: found
    type(temp_dir_manager) :: temp_mgr

    print *, '=== Multiple Modules from Same Package Test ===\'

    ! Create a temporary registry file
    call temp_mgr%create('multiple_modules_test')
    test_registry_path = temp_mgr%get_file_path('test_multiple_registry.toml')
    call create_test_registry(test_registry_path)

    ! Load the test registry
    call load_registry_from_path(test_registry_path)

    ! Test 1: pyplot_module should map to pyplot-fortran
    call resolve_module_to_package('pyplot_module', package_name, git_url, found)
    if (.not. found) then
        write (error_unit, *) 'Error: pyplot_module not found in registry'
        stop 1
    end if
    if (trim(package_name) /= 'pyplot-fortran') then
    write(error_unit, *) 'Error: pyplot_module should map to pyplot-fortran, got: ', trim(package_name)
        stop 1
    end if
    print *, 'PASS: pyplot_module -> pyplot-fortran'

    ! Test 2: pyplot_utils should also map to pyplot-fortran
    call resolve_module_to_package('pyplot_utils', package_name, git_url, found)
    if (.not. found) then
        write (error_unit, *) 'Error: pyplot_utils not found in registry'
        stop 1
    end if
    if (trim(package_name) /= 'pyplot-fortran') then
    write(error_unit, *) 'Error: pyplot_utils should map to pyplot-fortran, got: ', trim(package_name)
        stop 1
    end if
    print *, 'PASS: pyplot_utils -> pyplot-fortran'

    ! Test 3: Both modules should have the same git URL
    call resolve_module_to_package('pyplot_module', package_name, git_url, found)
    if (index(git_url, 'jacobwilliams/pyplot-fortran') == 0) then
       write (error_unit, *) 'Error: Invalid git URL for pyplot_module: ', trim(git_url)
        stop 1
    end if
    print *, 'PASS: Both modules have correct git URL'

    ! Clean up
    call execute_command_line('rm -f '//trim(test_registry_path))

    print *, 'All tests passed!'

contains

    subroutine create_test_registry(registry_path)
        character(len=*), intent(in) :: registry_path
        integer :: unit

        open (newunit=unit, file=registry_path, status='replace')
        write (unit, '(a)') '# Test registry for multiple modules from same package'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages]'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.pyplot-fortran]'
        write (unit, '(a)') 'git = "https://github.com/jacobwilliams/pyplot-fortran"'
      write (unit, '(a)') '# This package provides multiple modules with pyplot_ prefix'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.fortplotlib]'
        write (unit, '(a)') 'git = "https://github.com/krystophny/fortplotlib"'
        write (unit, '(a)') 'prefix = "fortplot"'
        close (unit)

    end subroutine create_test_registry

end program test_multiple_modules
