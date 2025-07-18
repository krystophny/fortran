program test_conflicting_dependencies
    use registry_resolver
    use fpm_generator
    use module_scanner
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: temp_dir_manager
    implicit none

    character(len=256) :: test_registry_path, project_dir, fpm_toml_path
    character(len=512) :: line
    type(module_info), dimension(2) :: test_modules
    integer :: unit, iostat
    logical :: found_v1, found_v2
    type(temp_dir_manager) :: temp_mgr, temp_mgr2

    print *, '=== Conflicting Dependencies Tests ===\'

    ! Create a temporary registry file with conflicting versions
    call temp_mgr%create('conflicting_deps_test')
    test_registry_path = temp_mgr%get_file_path('registry.toml')
    call create_test_registry_with_conflicts(test_registry_path)

    ! Load the test registry
    call load_registry_from_path(test_registry_path)

    ! Create test modules that would conflict
    test_modules(1)%name = 'pyplot_module'  ! Maps to pyplot-fortran
    test_modules(2)%name = 'pyplot_utils'   ! Also maps to pyplot-fortran

    ! Create temporary project directory
    call temp_mgr2%create('test_conflict_project')
    project_dir = temp_mgr2%path

    ! Generate FPM file - this should handle the conflict
    call generate_fpm_with_deps_from_config(project_dir, 'test_project', test_modules, 2, temp_mgr%path, .false., '')

    ! Check the generated fpm.toml for conflict resolution
    fpm_toml_path = trim(project_dir)//'/fpm.toml'

    found_v1 = .false.
    found_v2 = .false.

    open (newunit=unit, file=fpm_toml_path, status='old', iostat=iostat)
    if (iostat /= 0) then
        write (error_unit, *) 'Error: Cannot open generated fpm.toml'
        stop 1
    end if

    do
        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit

        ! Check for different versions
        if (index(line, 'pyplot-fortran') > 0) then
            if (index(line, 'v1.0.0') > 0) then
                found_v1 = .true.
            end if
            if (index(line, 'v2.0.0') > 0) then
                found_v2 = .true.
            end if
        end if
    end do

    close (unit)

    ! Check that multiple modules from same package get deduplicated correctly
    if (found_v1 .and. found_v2) then
        write (error_unit, *) 'Error: Both v1 and v2 found - this should not happen'
        stop 1
    end if

    if (found_v2) then
        print *, 'PASS: Multiple modules from same package deduplicated correctly'
    else if (found_v1) then
        write (error_unit, *) 'Error: Found v1 instead of expected v2'
        stop 1
    else
        write (error_unit, *) 'Error: No pyplot-fortran dependency found'
        stop 1
    end if

    ! Clean up
    call execute_command_line('rm -rf '//trim(project_dir))
    call execute_command_line('rm -f '//trim(test_registry_path))

    print *, 'All conflicting dependency tests passed!'

contains

    subroutine create_test_registry_with_conflicts(registry_path)
        character(len=*), intent(in) :: registry_path
        integer :: unit

        open (newunit=unit, file=registry_path, status='replace')
        write (unit, '(a)') '# Test registry for conflict detection'
    write(unit, '(a)') '# This test demonstrates that multiple modules from the same package'
        write (unit, '(a)') '# get correctly deduplicated to a single dependency'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages]'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.pyplot-fortran]'
        write (unit, '(a)') 'git = "https://github.com/jacobwilliams/pyplot-fortran"'
        write (unit, '(a)') 'version = "v2.0.0"'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.fortplotlib]'
        write (unit, '(a)') 'git = "https://github.com/krystophny/fortplotlib"'
        write (unit, '(a)') 'prefix = "fortplot"'
        close (unit)

    end subroutine create_test_registry_with_conflicts

end program test_conflicting_dependencies
