! Test Step 1: Single-file type inference and enhancements
program test_step1_single_file
    use, intrinsic :: iso_fortran_env, only: error_unit
    use standardizer, only: standardize_file
    use temp_utils, only: temp_dir_manager
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    call test_function_signature_enhancement()
    call test_parameter_type_enhancement()
    call test_simple_forward_propagation()
    call test_multiple_functions_single_file()
    call test_mixed_explicit_implicit_types()
    call test_nested_function_calls()

    call print_results()

contains

    subroutine test_function_signature_enhancement()
        character(len=*), parameter :: test_name = "Function signature enhancement (real → real(8))"
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        logical :: success
        type(temp_dir_manager) :: temp_mgr

        call temp_mgr%create('test_func_sig')
        input_file = temp_mgr%get_file_path('test_func_sig.f')
        output_file = temp_mgr%get_file_path('test_func_sig.f90')

        call create_test_file(input_file, &
            'result = compute(5.0)' // new_line('a') // &
            '' // new_line('a') // &
            'real function compute(x)' // new_line('a') // &
            '  real :: x' // new_line('a') // &
            '  compute = x * x' // new_line('a') // &
            'end function')

        call standardize_file(input_file, output_file, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Check for function signature enhancement
            success = check_output_contains(output_file, 'real(8) function compute(x)')
            success = success .and. check_output_contains(output_file, 'real(8), intent(in) :: x')
        else
            success = .false.
            write(error_unit, '(A, A)') 'Error: ', trim(error_msg)
        end if

        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine

    subroutine test_parameter_type_enhancement()
        character(len=*), parameter :: test_name = "Parameter type enhancement with intent(in)"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success

        input_file = 'test_param_enhance.f'
        output_file = 'test_param_enhance.f90'

        call create_test_file(input_file, &
            'area = rectangle_area(5.0, 3.0)' // new_line('a') // &
            '' // new_line('a') // &
            'real function rectangle_area(width, height)' // new_line('a') // &
            '  real :: width, height' // new_line('a') // &
            '  rectangle_area = width * height' // new_line('a') // &
            'end function')

        call standardize_file(input_file, output_file, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Check for parameter enhancement (parameters can be on same line)
            success = check_output_contains(output_file, 'real(8), intent(in) :: width, height') .or. &
                      (check_output_contains(output_file, 'real(8), intent(in) :: width') .and. &
                       check_output_contains(output_file, 'real(8), intent(in) :: height'))
            success = success .and. check_output_contains(output_file, 'real(8) function rectangle_area')
        else
            success = .false.
        end if

        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine

    subroutine test_simple_forward_propagation()
        character(len=*), parameter :: test_name = "Forward type propagation (variable gets function return type)"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success

        input_file = 'test_forward_prop.f'
        output_file = 'test_forward_prop.f90'

        call create_test_file(input_file, &
            'pi_value = get_pi()' // new_line('a') // &
            'print *, pi_value' // new_line('a') // &
            '' // new_line('a') // &
            'real function get_pi()' // new_line('a') // &
            '  get_pi = 3.14159265' // new_line('a') // &
            'end function')

        call standardize_file(input_file, output_file, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Check that pi_value gets the right type from function return
            success = check_output_contains(output_file, 'real(8) :: pi_value')
            success = success .and. check_output_contains(output_file, 'real(8) function get_pi')
        else
            success = .false.
        end if

        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine

    subroutine test_multiple_functions_single_file()
        character(len=*), parameter :: test_name = "Multiple functions in single file"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success

        input_file = 'test_multi_func.f'
        output_file = 'test_multi_func.f90'

        call create_test_file(input_file, &
            'x = 5.0' // new_line('a') // &
            'y = square(x)' // new_line('a') // &
            'z = cube(x)' // new_line('a') // &
            'print *, x, y, z' // new_line('a') // &
            '' // new_line('a') // &
            'real function square(val)' // new_line('a') // &
            '  real :: val' // new_line('a') // &
            '  square = val * val' // new_line('a') // &
            'end function' // new_line('a') // &
            '' // new_line('a') // &
            'real function cube(val)' // new_line('a') // &
            '  real :: val' // new_line('a') // &
            '  cube = val * val * val' // new_line('a') // &
            'end function')

        call standardize_file(input_file, output_file, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Check both functions are enhanced and variables get correct types
            success = check_output_contains(output_file, 'real(8) function square')
            success = success .and. check_output_contains(output_file, 'real(8) function cube')
            success = success .and. check_output_contains(output_file, 'real(8) :: x')
            success = success .and. check_output_contains(output_file, 'real(8) :: y')
            success = success .and. check_output_contains(output_file, 'real(8) :: z')
        else
            success = .false.
        end if

        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine

    subroutine test_mixed_explicit_implicit_types()
        character(len=*), parameter :: test_name = "Mixed explicit and implicit types"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success

        input_file = 'test_mixed_types.f'
        output_file = 'test_mixed_types.f90'

        call create_test_file(input_file, &
            'count = 0' // new_line('a') // &
            'rate = get_rate()' // new_line('a') // &
            'total = count * rate' // new_line('a') // &
            '' // new_line('a') // &
            'real function get_rate()' // new_line('a') // &
            '  get_rate = 1.25' // new_line('a') // &
            'end function')

        call standardize_file(input_file, output_file, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Check that integer and real types are properly inferred
            success = check_output_contains(output_file, 'integer :: count')
            success = success .and. check_output_contains(output_file, 'real(8) :: rate')
            success = success .and. check_output_contains(output_file, 'real(8) :: total')
            success = success .and. check_output_contains(output_file, 'real(8) function get_rate')
        else
            success = .false.
        end if

        call assert_true(success, test_name)
        call cleanup_files(input_file, output_file)
    end subroutine

    subroutine test_nested_function_calls()
        character(len=*), parameter :: test_name = "Nested function calls"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success

        input_file = 'test_nested_calls.f'
        output_file = 'test_nested_calls.f90'

        call create_test_file(input_file, &
            'result = double_it(square(3.0))' // new_line('a') // &
            'print *, result' // new_line('a') // &
            '' // new_line('a') // &
            'real function square(x)' // new_line('a') // &
            '  real :: x' // new_line('a') // &
            '  square = x * x' // new_line('a') // &
            'end function' // new_line('a') // &
            '' // new_line('a') // &
            'real function double_it(x)' // new_line('a') // &
            '  real :: x' // new_line('a') // &
            '  double_it = x * 2.0' // new_line('a') // &
            'end function')

        call standardize_file(input_file, output_file, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Check nested calls work correctly
            success = check_output_contains(output_file, 'real(8) :: result')
            success = success .and. check_output_contains(output_file, 'real(8) function square')
            success = success .and. check_output_contains(output_file, 'real(8) function double_it')
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

                ! Skip debug output and other noise
                if (index(line, 'DEBUG:') > 0 .or. &
                    index(line, '!') == 1) cycle

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
