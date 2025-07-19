program test_version_constraints
    use registry_resolver
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: get_temp_file_path, get_system_temp_dir
    implicit none

    character(len=256) :: test_registry_path
    character(len=128) :: package_name
    character(len=256) :: git_url
    character(len=32) :: version
    logical :: found

    print *, '=== Version Constraints Tests ===\'

    ! Create a temporary registry file with version constraints
  test_registry_path = get_temp_file_path(get_system_temp_dir(), 'test_version_registry.toml')
    call create_test_registry_with_versions(test_registry_path)

    ! Load the test registry
    call load_registry_from_path(test_registry_path)

    ! Test 1: Module with version constraint
call resolve_module_with_version('pyplot_module', package_name, git_url, version, found)
    if (.not. found) then
        write (error_unit, *) 'Error: pyplot_module not found in registry'
        stop 1
    end if
    if (trim(package_name) /= 'pyplot-fortran') then
    write(error_unit, *) 'Error: pyplot_module should map to pyplot-fortran, got: ', trim(package_name)
        stop 1
    end if
    if (trim(version) /= 'v1.0.0') then
    write(error_unit, *) 'Error: pyplot_module should have version v1.0.0, got: ', trim(version)
        stop 1
    end if
    print *, 'PASS: pyplot_module -> pyplot-fortran v1.0.0'

    ! Test 2: Module without version constraint
call resolve_module_with_version('fortplot_test', package_name, git_url, version, found)
    if (.not. found) then
        write (error_unit, *) 'Error: fortplot_test not found in registry'
        stop 1
    end if
    if (trim(package_name) /= 'fortplotlib') then
    write(error_unit, *) 'Error: fortplot_test should map to fortplotlib, got: ', trim(package_name)
        stop 1
    end if
    if (len_trim(version) /= 0) then
write(error_unit, *) 'Error: fortplot_test should have no version, got: ', trim(version)
        stop 1
    end if
    print *, 'PASS: fortplot_test -> fortplotlib (no version)'

    ! Clean up
    call execute_command_line('rm -f '//trim(test_registry_path))

    print *, 'All version constraint tests passed!'

contains

    subroutine create_test_registry_with_versions(registry_path)
        character(len=*), intent(in) :: registry_path
        integer :: unit

        open (newunit=unit, file=registry_path, status='replace')
        write (unit, '(a)') '# Test registry with version constraints'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages]'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.pyplot-fortran]'
        write (unit, '(a)') 'git = "https://github.com/jacobwilliams/pyplot-fortran"'
        write (unit, '(a)') 'version = "v1.0.0"'
  write (unit, '(a)') '# This package provides multiple modules with version constraint'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.fortplotlib]'
        write (unit, '(a)') 'git = "https://github.com/krystophny/fortplotlib"'
        write (unit, '(a)') 'prefix = "fortplot"'
        write (unit, '(a)') '# This package has no version constraint'
        close (unit)

    end subroutine create_test_registry_with_versions

end program test_version_constraints
