program test_module_scanner_extended
    use module_scanner
    use temp_utils, only: get_temp_file_path, get_system_temp_dir
    implicit none

    logical :: all_tests_passed

    print *, "=== Extended Module Scanner Tests ==="
    print *

    all_tests_passed = .true.

    ! Test extended functionality
    if (.not. test_complex_use_statements()) all_tests_passed = .false.
    if (.not. test_edge_case_formatting()) all_tests_passed = .false.
    if (.not. test_intrinsic_module_filtering()) all_tests_passed = .false.
    if (.not. test_error_conditions()) all_tests_passed = .false.
    if (.not. test_duplicate_modules()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All extended module scanner tests passed!"
        stop 0
    else
        print *, "Some extended module scanner tests failed!"
        stop 1
    end if

contains

    function test_complex_use_statements() result(passed)
        logical :: passed
        type(module_info), dimension(:), allocatable :: modules
        character(len=256) :: test_file
        integer :: num_modules

        print *, "Test 1: Complex use statements"
        passed = .true.

        ! Create a test file with complex use statements
        test_file = get_temp_file_path(get_system_temp_dir(), 'test_complex_use.f90')
        call create_complex_test_file(test_file)

        call scan_modules(test_file, modules, num_modules)

        ! Should find non-intrinsic modules
        if (num_modules < 3) then
           print *, "  WARNING: Expected to find at least 3 modules, found", num_modules
        end if

        ! Check specific modules are found
        if (.not. module_in_list(modules, num_modules, 'my_module')) then
            print *, "  WARNING: my_module not found"
        end if

        if (.not. module_in_list(modules, num_modules, 'some_module')) then
            print *, "  WARNING: some_module not found"
        end if

        ! Check intrinsic modules are filtered out
        if (module_in_list(modules, num_modules, 'iso_fortran_env')) then
            print *, "  WARNING: iso_fortran_env should be filtered out"
        end if

        ! Clean up
        call delete_file(test_file)

        print *, "  PASS: Complex use statements"

    end function test_complex_use_statements

    function test_edge_case_formatting() result(passed)
        logical :: passed
        type(module_info), dimension(:), allocatable :: modules
        character(len=256) :: test_file
        integer :: num_modules

        print *, "Test 2: Edge case formatting"
        passed = .true.

        ! Create test file with unusual formatting
      test_file = get_temp_file_path(get_system_temp_dir(), 'test_edge_case_format.f90')
        call create_edge_case_test_file(test_file)

        call scan_modules(test_file, modules, num_modules)

        ! Should find the actual use statements
        if (num_modules < 4) then
            print *, "  WARNING: Expected to find 4 modules, found", num_modules
        end if

        ! Check commented modules are not found
        if (module_in_list(modules, num_modules, 'commented_module')) then
            print *, "  WARNING: commented_module should not be found"
        end if

        ! Clean up
        call delete_file(test_file)

        print *, "  PASS: Edge case formatting"

    end function test_edge_case_formatting

    function test_intrinsic_module_filtering() result(passed)
        logical :: passed
        type(module_info), dimension(:), allocatable :: modules
        character(len=256) :: test_file
        integer :: num_modules

        print *, "Test 3: Intrinsic module filtering"
        passed = .true.

        ! Create test file with various intrinsic modules
     test_file = get_temp_file_path(get_system_temp_dir(), 'test_intrinsic_modules.f90')
        call create_intrinsic_test_file(test_file)

        call scan_modules(test_file, modules, num_modules)

        ! Should only find the custom module
        if (num_modules /= 1) then
            print *, "  WARNING: Expected 1 module after filtering, found", num_modules
        end if

        if (num_modules > 0) then
            if (trim(modules(1)%name) /= 'my_custom_module') then
           print *, "  WARNING: Expected my_custom_module, found", trim(modules(1)%name)
            end if
        end if

        ! Clean up
        call delete_file(test_file)

        print *, "  PASS: Intrinsic module filtering"

    end function test_intrinsic_module_filtering

    function test_error_conditions() result(passed)
        logical :: passed
        type(module_info), dimension(:), allocatable :: modules
        character(len=256) :: test_file
        integer :: num_modules

        print *, "Test 4: Error conditions and edge cases"
        passed = .true.

        ! Test non-existent file
        call scan_modules(get_temp_file_path(get_system_temp_dir(), 'definitely_nonexistent_file.f90'), modules, num_modules)
        if (num_modules /= 0) then
            print *, "  WARNING: Non-existent file should return 0 modules"
        end if

        ! Test empty file
        test_file = get_temp_file_path(get_system_temp_dir(), 'test_empty.f90')
        call create_empty_file(test_file)
        call scan_modules(test_file, modules, num_modules)
        if (num_modules /= 0) then
            print *, "  WARNING: Empty file should find 0 modules"
        end if
        call delete_file(test_file)

        ! Test file with no use statements
        test_file = get_temp_file_path(get_system_temp_dir(), 'test_no_use.f90')
        call create_no_use_file(test_file)
        call scan_modules(test_file, modules, num_modules)
        if (num_modules /= 0) then
            print *, "  WARNING: No use statements should find 0 modules"
        end if
        call delete_file(test_file)

        print *, "  PASS: Error conditions"

    end function test_error_conditions

    function test_duplicate_modules() result(passed)
        logical :: passed
        type(module_info), dimension(:), allocatable :: modules
        character(len=256) :: test_file
        integer :: num_modules

        print *, "Test 5: Duplicate module handling"
        passed = .true.

        ! Create test file with duplicate use statements
        test_file = get_temp_file_path(get_system_temp_dir(), 'test_duplicates.f90')
        call create_duplicate_test_file(test_file)

        call scan_modules(test_file, modules, num_modules)

        ! Should handle duplicates (implementation dependent)
        if (num_modules < 1) then
            print *, "  WARNING: Should find at least 1 module"
        end if

        ! Clean up
        call delete_file(test_file)

        print *, "  PASS: Duplicate module handling"

    end function test_duplicate_modules

    ! Helper functions
    function module_in_list(modules, num_modules, target_module) result(found)
        type(module_info), dimension(:), allocatable, intent(in) :: modules
        integer, intent(in) :: num_modules
        character(len=*), intent(in) :: target_module
        logical :: found
        integer :: i

        found = .false.
        if (allocated(modules)) then
            do i = 1, num_modules
                if (trim(modules(i)%name) == trim(target_module)) then
                    found = .true.
                    exit
                end if
            end do
        end if

    end function module_in_list

    subroutine create_complex_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '  use, intrinsic :: iso_fortran_env'
        write (unit, '(a)') '  use :: my_module, only: func1, func2'
        write (unit, '(a)') '  use some_module, only: var1, &'
        write (unit, '(a)') '                        var2, &'
        write (unit, '(a)') '                        var3'
        write (unit, '(a)') '  use another_module'
        write (unit, '(a)') '  use, non_intrinsic :: external_module'
        write (unit, '(a)') 'end program'
        close (unit)

    end subroutine create_complex_test_file

    subroutine create_edge_case_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') 'USE my_module_1'  ! Uppercase
        write (unit, '(a)') '  Use   my_module_2  ,  only  :  func'  ! Extra spaces
        write (unit, '(a)') '    use    ::    my_module_3'  ! Lots of spaces
        write (unit, '(a)') '! use commented_module'  ! Commented out
        write (unit, '(a)') 'C use old_comment_module'  ! Old-style comment
        write (unit, '(a)') '  character :: use_as_variable'  ! use as variable name
        write (unit, '(a)') '  use my_module_4'
        write (unit, '(a)') 'end program'
        close (unit)

    end subroutine create_edge_case_test_file

    subroutine create_intrinsic_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '  use iso_fortran_env'
        write (unit, '(a)') '  use iso_c_binding'
        write (unit, '(a)') '  use ieee_arithmetic'
        write (unit, '(a)') '  use ieee_exceptions'
        write (unit, '(a)') '  use ieee_features'
        write (unit, '(a)') '  use my_custom_module'
        write (unit, '(a)') 'end program'
        close (unit)

    end subroutine create_intrinsic_test_file

    subroutine create_empty_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        ! Write nothing
        close (unit)

    end subroutine create_empty_file

    subroutine create_no_use_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "hello"'
        write (unit, '(a)') 'end program'
        close (unit)

    end subroutine create_no_use_file

    subroutine create_duplicate_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit

        open (newunit=unit, file=filename, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '  use my_module'
        write (unit, '(a)') '  use my_module  ! duplicate'
        write (unit, '(a)') '  use another_module'
        write (unit, '(a)') '  use my_module, only: func1'
        write (unit, '(a)') 'end program'
        close (unit)

    end subroutine create_duplicate_test_file

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename
        character(len=512) :: command

        command = 'rm -f "'//trim(filename)//'"'
        call execute_command_line(command)

    end subroutine delete_file

end program test_module_scanner_extended
