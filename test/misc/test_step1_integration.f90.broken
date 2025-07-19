! Test Step 1: Handle explicitly typed functions with opinionated defaults
program test_step1_integration
    use, intrinsic :: iso_fortran_env, only: error_unit
    use standardizer, only: standardize_file
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    call test_explicit_function_with_parameters()
    call test_intent_in_default_behavior()
    call test_real_to_real8_conversion()

    call print_results()

contains

    subroutine test_explicit_function_with_parameters()
        character(len=*), parameter :: test_name = "Explicit function with parameters gets intent(in)"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success

        input_file = 'test_step1_explicit.f'
        output_file = 'test_step1_explicit.f90'

        call create_test_file(input_file, &
            'result = square(5.0)' // new_line('a') // &
            '' // new_line('a') // &
            'real function square(x)' // new_line('a') // &
            '  real :: x' // new_line('a') // &
            '  square = x * x' // new_line('a') // &
            'end function')

        call standardize_file(input_file, output_file, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Check for opinionated defaults applied
            success = check_output_contains(output_file, 'real(8), intent(in) :: x')
            success = success .and. check_output_contains(output_file, 'real(8) :: result')
            success = success .and. check_output_contains(output_file, 'real(8) function square(x)')
        else
            success = .false.
            write(error_unit, '(A, A)') 'Error: ', trim(error_msg)
        end if

        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine

    subroutine test_intent_in_default_behavior()
        character(len=*), parameter :: test_name = "Parameters get intent(in) by default"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success

        input_file = 'test_intent_default.f'
        output_file = 'test_intent_default.f90'

        call create_test_file(input_file, &
            'y = compute(3, 4)' // new_line('a') // &
            '' // new_line('a') // &
            'integer function compute(a, b)' // new_line('a') // &
            '  integer :: a, b' // new_line('a') // &
            '  compute = a + b' // new_line('a') // &
            'end function')

        call standardize_file(input_file, output_file, error_msg)

        if (len_trim(error_msg) == 0) then
            success = check_output_contains(output_file, 'integer, intent(in) :: a, b')
            success = success .and. check_output_contains(output_file, 'integer :: y')
        else
            success = .false.
        end if

        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine

    subroutine test_real_to_real8_conversion()
        character(len=*), parameter :: test_name = "real converts to real(8) for explicitness"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success

        input_file = 'test_real_conversion.f'
        output_file = 'test_real_conversion.f90'

        call create_test_file(input_file, &
            'result = half(10.0)' // new_line('a') // &
            '' // new_line('a') // &
            'real function half(value)' // new_line('a') // &
            '  real :: value' // new_line('a') // &
            '  half = value / 2.0' // new_line('a') // &
            'end function')

        call standardize_file(input_file, output_file, error_msg)

        if (len_trim(error_msg) == 0) then
            success = check_output_contains(output_file, 'real(8), intent(in) :: value')
            success = success .and. check_output_contains(output_file, 'real(8) function half(value)')
        else
            success = .false.
        end if

        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine

    subroutine create_test_file(filename, content)
        character(len=*), intent(in) :: filename, content
        integer :: unit, ios

        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write(unit, '(A)') content
            close(unit)
        end if
    end subroutine

    function check_output_contains(filename, expected_text) result(found)
        character(len=*), intent(in) :: filename, expected_text
        logical :: found
        character(len=1024) :: line
        integer :: unit, ios

        found = .false.
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, trim(expected_text)) > 0) then
                    found = .true.
                    exit
                end if
            end do
            close(unit)
        end if
    end function

    subroutine cleanup_files(file1, file2)
        character(len=*), intent(in) :: file1, file2
        integer :: unit, ios

        open(newunit=unit, file=file1, status='old', iostat=ios)
        if (ios == 0) then
            close(unit, status='delete')
        end if

        open(newunit=unit, file=file2, status='old', iostat=ios)
        if (ios == 0) then
            close(unit, status='delete')
        end if
    end subroutine

    subroutine assert_true(condition, test_name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name

        test_count = test_count + 1
        if (condition) then
            pass_count = pass_count + 1
            write(*, '(A, A)') 'PASS: ', test_name
        else
            write(error_unit, '(A, A)') 'FAIL: ', test_name
        end if
    end subroutine

    subroutine print_results()
        write(*, '(A, I0, A, I0, A)') 'Tests: ', pass_count, '/', test_count, ' passed'
        if (pass_count /= test_count) then
            write(error_unit, '(A, I0, A)') 'FAILED: ', test_count - pass_count, ' tests failed'
            stop 1
        end if
    end subroutine

end program
