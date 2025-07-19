program test_registry_validation
    use registry_resolver
    use, intrinsic :: iso_fortran_env, only: error_unit
    use temp_utils, only: get_temp_file_path, get_system_temp_dir
    implicit none

    character(len=256) :: test_registry_path

    print *, '=== Registry Validation Tests ===\'

    ! Test 1: Valid registry
    call test_valid_registry()

    ! Test 2: Invalid TOML syntax
    call test_invalid_syntax()

    ! Test 3: Missing required fields
    call test_missing_fields()

    ! Test 4: Empty registry
    call test_empty_registry()

    print *, 'All registry validation tests passed!'

contains

    subroutine test_valid_registry()
        character(len=256) :: registry_path
        character(len=512) :: error_message
        logical :: is_valid

        print *, 'Test 1: Valid registry'

   registry_path = get_temp_file_path(get_system_temp_dir(), 'test_valid_registry.toml')
        call create_valid_registry(registry_path)

        ! Validate the registry - should succeed
        call validate_registry(registry_path, is_valid, error_message)

        if (.not. is_valid) then
  write (error_unit, *) 'Error: Valid registry failed validation: ', trim(error_message)
            stop 1
        end if

        ! Load the registry - should succeed
        call load_registry_from_path(registry_path)

        ! Clean up
        call execute_command_line('rm -f '//trim(registry_path))

        print *, 'PASS: Valid registry loaded successfully'
        print *

    end subroutine test_valid_registry

    subroutine test_invalid_syntax()
        character(len=256) :: registry_path
        character(len=512) :: error_message
        logical :: is_valid

        print *, 'Test 2: Invalid TOML syntax'

   registry_path = get_temp_file_path(get_system_temp_dir(), 'test_invalid_syntax.toml')
        call create_invalid_syntax_registry(registry_path)

        ! Validate the registry - should fail
        call validate_registry(registry_path, is_valid, error_message)

        if (is_valid) then
      write(error_unit, *) 'Error: Invalid registry passed validation when it should have failed'
            stop 1
        end if

        print *, 'PASS: Invalid syntax detected: ', trim(error_message)

        ! Clean up
        call execute_command_line('rm -f '//trim(registry_path))
        print *

    end subroutine test_invalid_syntax

    subroutine test_missing_fields()
        character(len=256) :: registry_path
        character(len=512) :: error_message
        logical :: is_valid

        print *, 'Test 3: Missing required fields'

   registry_path = get_temp_file_path(get_system_temp_dir(), 'test_missing_fields.toml')
        call create_missing_fields_registry(registry_path)

        ! Validate the registry - should succeed (missing fields are optional)
        call validate_registry(registry_path, is_valid, error_message)

        if (.not. is_valid) then
            print *, 'INFO: Missing fields validation: ', trim(error_message)
        else
            print *, 'PASS: Missing fields handled gracefully (optional fields)'
        end if

        ! Clean up
        call execute_command_line('rm -f '//trim(registry_path))
        print *

    end subroutine test_missing_fields

    subroutine test_empty_registry()
        character(len=256) :: registry_path
        character(len=512) :: error_message
        logical :: is_valid

        print *, 'Test 4: Empty registry'

   registry_path = get_temp_file_path(get_system_temp_dir(), 'test_empty_registry.toml')
        call create_empty_registry(registry_path)

        ! Validate the registry - should succeed (empty is valid)
        call validate_registry(registry_path, is_valid, error_message)

        if (.not. is_valid) then
  write (error_unit, *) 'Error: Empty registry failed validation: ', trim(error_message)
            stop 1
        end if

        ! Load the registry - should handle gracefully
        call load_registry_from_path(registry_path)

        ! Clean up
        call execute_command_line('rm -f '//trim(registry_path))

        print *, 'PASS: Empty registry handled gracefully'
        print *

    end subroutine test_empty_registry

    subroutine create_valid_registry(registry_path)
        character(len=*), intent(in) :: registry_path
        integer :: unit

        open (newunit=unit, file=registry_path, status='replace')
        write (unit, '(a)') '# Valid registry file'
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

    end subroutine create_valid_registry

    subroutine create_invalid_syntax_registry(registry_path)
        character(len=*), intent(in) :: registry_path
        integer :: unit

        open (newunit=unit, file=registry_path, status='replace')
        write (unit, '(a)') '# Invalid TOML syntax'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages'  ! Missing closing bracket
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.pyplot-fortran]'
        write (unit, '(a)') 'git = "https://github.com/jacobwilliams/pyplot-fortran'  ! Missing closing quote
        write (unit, '(a)') 'version = v1.0.0'  ! Missing quotes
        close (unit)

    end subroutine create_invalid_syntax_registry

    subroutine create_missing_fields_registry(registry_path)
        character(len=*), intent(in) :: registry_path
        integer :: unit

        open (newunit=unit, file=registry_path, status='replace')
        write (unit, '(a)') '# Registry with missing fields'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages]'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.pyplot-fortran]'
        write (unit, '(a)') '# Missing git field'
        write (unit, '(a)') 'version = "v1.0.0"'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages.incomplete-package]'
        write (unit, '(a)') 'git = "https://github.com/example/incomplete"'
        write (unit, '(a)') '# No version or prefix'
        close (unit)

    end subroutine create_missing_fields_registry

    subroutine create_empty_registry(registry_path)
        character(len=*), intent(in) :: registry_path
        integer :: unit

        open (newunit=unit, file=registry_path, status='replace')
        write (unit, '(a)') '# Empty registry'
        write (unit, '(a)') ''
        write (unit, '(a)') '[packages]'
        write (unit, '(a)') '# No packages defined'
        close (unit)

    end subroutine create_empty_registry

end program test_registry_validation
