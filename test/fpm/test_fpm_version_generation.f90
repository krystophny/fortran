program test_fmp_version_generation
    use fpm_generator
    use registry_resolver
    use module_scanner
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: temp_dir_manager
    implicit none

    character(len=256) :: test_registry_path, project_dir, fpm_toml_path
    character(len=256) :: config_dir
    character(len=512) :: line
    type(module_info), dimension(2) :: test_modules
    integer :: unit, iostat
    logical :: found_with_version, found_without_version
    type(temp_dir_manager) :: temp_mgr1, temp_mgr2

    print *, '=== FPM Version Generation Tests ===\'

    ! Create a temporary registry file with version constraints
    call temp_mgr1%create('fpm_version_test')
    test_registry_path = temp_mgr1%get_file_path('registry.toml')
    call create_test_registry_with_versions(test_registry_path)

    ! Load the test registry
    call load_registry_from_path(test_registry_path)

    ! Create test modules
    test_modules(1)%name = 'pyplot_module'
    test_modules(2)%name = 'fortplot_test'

    ! Create temporary project directory
    call temp_mgr2%create('test_fpm_version_project')
    project_dir = temp_mgr2%path
    config_dir = temp_mgr1%path

    ! Generate FPM file using custom config directory
    call generate_fpm_with_deps_from_config(project_dir, 'test_project', test_modules, 2, config_dir, .false., '')

    ! Check the generated fpm.toml
    fpm_toml_path = trim(project_dir)//'/fpm.toml'

    found_with_version = .false.
    found_without_version = .false.

    open (newunit=unit, file=fpm_toml_path, status='old', iostat=iostat)
    if (iostat /= 0) then
        write (error_unit, *) 'Error: Cannot open generated fpm.toml'
        stop 1
    end if

    do
        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) exit

        ! Check for pyplot-fortran with version
     if (index(line, 'pyplot-fortran') > 0 .and. index(line, 'tag = "v1.0.0"') > 0) then
            found_with_version = .true.
        end if

        ! Check for fortplotlib without version
        if (index(line, 'fortplotlib') > 0 .and. index(line, 'tag =') == 0) then
            found_without_version = .true.
        end if
    end do

    close (unit)

    ! Verify results
    if (.not. found_with_version) then
 write (error_unit, *) 'Error: pyplot-fortran with version v1.0.0 not found in fpm.toml'
        stop 1
    end if
    print *, 'PASS: pyplot-fortran with version v1.0.0 found in fpm.toml'

    if (.not. found_without_version) then
        write (error_unit, *) 'Error: fortplotlib without version not found in fpm.toml'
        stop 1
    end if
    print *, 'PASS: fortplotlib without version found in fpm.toml'

    ! Clean up
    call execute_command_line('rm -rf '//trim(project_dir))
    call execute_command_line('rm -f '//trim(test_registry_path))

    print *, 'All FPM version generation tests passed!'

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
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.fortplotlib]'
        write (unit, '(a)') 'git = "https://github.com/krystophny/fortplotlib"'
        write (unit, '(a)') 'prefix = "fortplot"'
        close (unit)

    end subroutine create_test_registry_with_versions

end program test_fmp_version_generation
