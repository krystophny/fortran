program test_fortran95_passthrough
    use iso_fortran_env, only: error_unit
    use temp_utils, only: temp_dir_manager
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    write (*, '(a)') '=== Standard Fortran 95 Passthrough Tests ==='
    write (*, '(a)') ''

    call test_simple_program_passthrough(test_count, pass_count)
    call test_module_passthrough(test_count, pass_count)
    call test_complex_program_passthrough(test_count, pass_count)

    write (*, '(a)') ''
    write(*, '(a,i0,a,i0,a)') 'Fortran 95 passthrough tests: ', pass_count, '/', test_count, ' passed'

    if (pass_count /= test_count) then
        error stop 'Some Fortran 95 passthrough tests failed!'
    end if

contains

    subroutine test_simple_program_passthrough(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        character(len=:), allocatable :: temp_input, temp_output
        logical :: files_identical
        integer :: unit
        type(temp_dir_manager) :: temp_dir

        write (*, '(a)') 'Test 1: Simple program passthrough'
        test_count = test_count + 1

        block
            call temp_dir%create('fortran95_test1')
            temp_input = temp_dir%get_file_path('test_simple_prog.f90')
            temp_output = temp_dir%get_file_path('test_simple_prog_out.f90')

            ! Create test file
            open (newunit=unit, file=temp_input, status='replace', action='write')
            write (unit, '(a)') 'program test'
            write (unit, '(a)') '    implicit none'
            write (unit, '(a)') '    integer :: i'
            write (unit, '(a)') '    i = 42'
            write (unit, '(a)') '    print *, "i =", i'
            write (unit, '(a)') 'end program test'
            close (unit)

            ! Process through frontend
            if (compile_standard_fortran(temp_input, temp_output)) then
                call compare_files(temp_input, temp_output, files_identical)
                if (files_identical) then
                    write (*, '(a)') '  ✓ PASS: Simple program preserved'
                    pass_count = pass_count + 1
                else
                    write (*, '(a)') '  ✗ FAIL: Simple program modified'
                    call show_diff(temp_input, temp_output)
                end if
            else
                write (*, '(a)') '  ✗ FAIL: Compilation failed'
            end if

            ! Clean up temporary files
        call execute_command_line('rm -f "' // trim(temp_input) // '" "' // trim(temp_output) // '"', wait=.true.)
        end block

    end subroutine test_simple_program_passthrough

    subroutine test_module_passthrough(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        character(len=:), allocatable :: temp_input, temp_output
        logical :: files_identical
        integer :: unit
        type(temp_dir_manager) :: temp_dir

        write (*, '(a)') 'Test 2: Module passthrough'
        test_count = test_count + 1

        block
            call temp_dir%create('fortran95_test2')
            temp_input = temp_dir%get_file_path('test_module.f90')
            temp_output = temp_dir%get_file_path('test_module_out.f90')

            ! Create test file
            open (newunit=unit, file=temp_input, status='replace', action='write')
            write (unit, '(a)') 'module test_passthrough_mod_xyz'
            write (unit, '(a)') '    implicit none'
            write (unit, '(a)') '    real, parameter :: pi = 3.14159'
            write (unit, '(a)') 'contains'
            write (unit, '(a)') '    function circle_area(r) result(area)'
            write (unit, '(a)') '        real, intent(in) :: r'
            write (unit, '(a)') '        real :: area'
            write (unit, '(a)') '        area = pi * r**2'
            write (unit, '(a)') '    end function circle_area'
            write (unit, '(a)') 'end module test_passthrough_mod_xyz'
            close (unit)

            if (compile_standard_fortran(temp_input, temp_output)) then
                call compare_files(temp_input, temp_output, files_identical)
                if (files_identical) then
                    write (*, '(a)') '  ✓ PASS: Module preserved'
                    pass_count = pass_count + 1
                else
                    write (*, '(a)') '  ✗ FAIL: Module modified'
                    call show_diff(temp_input, temp_output)
                end if
            else
                write (*, '(a)') '  ✗ FAIL: Compilation failed'
            end if

            ! Clean up temporary files
        call execute_command_line('rm -f "' // trim(temp_input) // '" "' // trim(temp_output) // '"', wait=.true.)
        end block

    end subroutine test_module_passthrough

    subroutine test_complex_program_passthrough(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        character(len=:), allocatable :: temp_input, temp_output
        logical :: files_identical
        integer :: unit
        type(temp_dir_manager) :: temp_dir

        write (*, '(a)') 'Test 3: Complex program with arrays and loops'
        test_count = test_count + 1

        block
            call temp_dir%create('fortran95_test3')
            temp_input = temp_dir%get_file_path('test_complex.f90')
            temp_output = temp_dir%get_file_path('test_complex_out.f90')

            ! Create test file
            open (newunit=unit, file=temp_input, status='replace', action='write')
            write (unit, '(a)') 'program matrix_ops'
            write (unit, '(a)') '    implicit none'
            write (unit, '(a)') '    integer, parameter :: n = 3'
            write (unit, '(a)') '    real :: matrix(n,n), trace'
            write (unit, '(a)') '    integer :: i, j'
            write (unit, '(a)') '    '
            write (unit, '(a)') '    ! Initialize matrix'
            write (unit, '(a)') '    do i = 1, n'
            write (unit, '(a)') '        do j = 1, n'
            write (unit, '(a)') '            matrix(i,j) = real(i*j)'
            write (unit, '(a)') '        end do'
            write (unit, '(a)') '    end do'
            write (unit, '(a)') '    '
            write (unit, '(a)') '    ! Calculate trace'
            write (unit, '(a)') '    trace = 0.0'
            write (unit, '(a)') '    do i = 1, n'
            write (unit, '(a)') '        trace = trace + matrix(i,i)'
            write (unit, '(a)') '    end do'
            write (unit, '(a)') '    '
            write (unit, '(a)') '    print *, "Trace =", trace'
            write (unit, '(a)') 'end program matrix_ops'
            close (unit)

            if (compile_standard_fortran(temp_input, temp_output)) then
                call compare_files(temp_input, temp_output, files_identical)
                if (files_identical) then
                    write (*, '(a)') '  ✓ PASS: Complex program preserved'
                    pass_count = pass_count + 1
                else
                    write (*, '(a)') '  ✗ FAIL: Complex program modified'
                    call show_diff(temp_input, temp_output)
                end if
            else
                write (*, '(a)') '  ✗ FAIL: Compilation failed'
            end if

            ! Clean up temporary files
        call execute_command_line('rm -f "' // trim(temp_input) // '" "' // trim(temp_output) // '"', wait=.true.)
        end block

    end subroutine test_complex_program_passthrough

    ! Helper function to compile standard Fortran through frontend
    logical function compile_standard_fortran(input_file, output_file)
        character(len=*), intent(in) :: input_file, output_file
        integer :: exit_code

        call execute_command_line('fpm run fortran -- "'//trim(input_file)//'"'// &
                             ' --standardize > "'//trim(output_file)//'" 2>/dev/null', &
                                  exitstat=exit_code)

        compile_standard_fortran = (exit_code == 0)
    end function compile_standard_fortran

    ! Helper to compare two files
    subroutine compare_files(file1, file2, identical)
        character(len=*), intent(in) :: file1, file2
        logical, intent(out) :: identical
        integer :: unit1, unit2, ios1, ios2
        character(len=1024) :: line1, line2

        identical = .true.

        open (newunit=unit1, file=file1, status='old', action='read', iostat=ios1)
        open (newunit=unit2, file=file2, status='old', action='read', iostat=ios2)

        if (ios1 /= 0 .or. ios2 /= 0) then
            identical = .false.
            return
        end if

        do
            read (unit1, '(a)', iostat=ios1) line1
            read (unit2, '(a)', iostat=ios2) line2

            if (ios1 /= ios2) then
                identical = .false.
                exit
            end if

            if (ios1 /= 0) exit  ! End of both files

            if (trim(line1) /= trim(line2)) then
                identical = .false.
                exit
            end if
        end do

        close (unit1)
        close (unit2)
    end subroutine compare_files

    ! Helper to show file differences
    subroutine show_diff(file1, file2)
        character(len=*), intent(in) :: file1, file2
        write (*, '(a)') '    First 3 lines of input:'
        call execute_command_line('head -3 "'//trim(file1)//'" | sed "s/^/      /"')
        write (*, '(a)') '    First 3 lines of output:'
        call execute_command_line('head -3 "'//trim(file2)//'" | sed "s/^/      /"')
    end subroutine show_diff

end program test_fortran95_passthrough
