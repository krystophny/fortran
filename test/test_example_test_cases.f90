program test_example_test_cases
    use iso_fortran_env, only: error_unit
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    use temp_utils, only: get_temp_file_path, get_system_temp_dir
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    write (*, '(a)') '=== Example Test Cases ==='
    write (*, '(a)') ''

    call test_case("use_statement", test_count, pass_count)
    call test_case("print_statement", test_count, pass_count)
    call test_case("multi_statement", test_count, pass_count)
    call test_case("if_else_simple", test_count, pass_count)
    call test_case("if_elseif_else", test_count, pass_count)
    call test_case("logical_type_inference", test_count, pass_count)

    write (*, '(a)') ''
write(*, '(a,i0,a,i0,a)') 'Example test cases: ', pass_count, '/', test_count, ' passed'

    if (pass_count /= test_count) then
        error stop 'Some example test cases failed!'
    end if

contains

    subroutine test_case(case_name, test_count, pass_count)
        character(len=*), intent(in) :: case_name
        integer, intent(inout) :: test_count, pass_count

        character(len=256) :: input_file, expected_file, output_file, error_msg
        character(len=4096) :: expected_code, generated_code
        type(compilation_options_t) :: options
        logical :: success

        write (*, '(a,a)') 'Test: ', case_name
        test_count = test_count + 1

        ! Set file paths (updated for reorganized structure)
input_file = "example/frontend_test_cases/"//trim(case_name)//"/"//trim(case_name)//".f"
        expected_file = "example/frontend_test_cases/" // trim(case_name) // "/" // trim(case_name) // ".f90"
    output_file = get_temp_file_path(get_system_temp_dir(), trim(case_name)//"_out.f90")

        ! Read expected output
        call read_file(expected_file, expected_code)

        ! Set up compilation options
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file

        ! Compile
        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a,a)') '  ✗ FAIL: Compilation error: ', trim(error_msg)
            return
        end if

        ! Read generated output
        call read_file(output_file, generated_code)

        ! Compare (normalize whitespace)
        success = compare_normalized(generated_code, expected_code)

        if (success) then
            write (*, '(a)') '  ✓ PASS'
            pass_count = pass_count + 1
        else
            write (*, '(a)') '  ✗ FAIL: Output mismatch'
            write (error_unit, '(a)') '  Expected:'
            write (error_unit, '(a)') trim(expected_code)
            write (error_unit, '(a)') '  Got:'
            write (error_unit, '(a)') trim(generated_code)
        end if

        ! Clean up
        call execute_command_line("rm -f "//trim(output_file), wait=.true.)

    end subroutine test_case

    subroutine read_file(filename, content)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: content

        integer :: unit, ios
        character(len=256) :: line

        content = ""
        open (newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write (error_unit, '(a,a)') '  Error: Cannot read file: ', trim(filename)
            return
        end if

        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(content) > 0) then
                content = trim(content)//new_line('a')
            end if
            content = trim(content)//trim(line)
        end do

        close (unit)
    end subroutine read_file

    function compare_normalized(str1, str2) result(equal)
        character(len=*), intent(in) :: str1, str2
        logical :: equal

        ! For now, just do a simple comparison
        ! TODO: Implement proper whitespace normalization
        equal = (trim(adjustl(str1)) == trim(adjustl(str2)))
    end function compare_normalized

end program test_example_test_cases
