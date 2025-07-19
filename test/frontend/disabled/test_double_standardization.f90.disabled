program test_double_standardization
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_simple_assignment()
    call test_function_definition()
    call test_do_loop()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_simple_assignment()
        character(len=:), allocatable :: input
        logical :: success

        test_count = test_count + 1

        ! Test simple assignment
        input = "x = 42"

        ! Test double standardization
        if (check_double_standardization(input)) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Simple assignment double standardization"
        else
            write (*, '(A)') "FAIL: Simple assignment double standardization"
        end if
    end subroutine test_simple_assignment

    subroutine test_function_definition()
        character(len=:), allocatable :: input
        logical :: success

        test_count = test_count + 1

        ! Test function definition
        input = "function square(x)"//new_line('a')// &
                "square = x * x"//new_line('a')// &
                "end function"

        ! Test double standardization
        if (check_double_standardization(input)) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Function definition double standardization"
        else
            write (*, '(A)') "FAIL: Function definition double standardization"
        end if
    end subroutine test_function_definition

    subroutine test_do_loop()
        character(len=:), allocatable :: input
        logical :: success

        test_count = test_count + 1

        ! Test do loop
        input = "do i = 1, 10"//new_line('a')// &
                "sum = sum + i"//new_line('a')// &
                "end do"

        ! Test double standardization
        if (check_double_standardization(input)) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Do loop double standardization"
        else
            write (*, '(A)') "FAIL: Do loop double standardization"
        end if
    end subroutine test_do_loop

    function check_double_standardization(input_code) result(success)
        character(len=*), intent(in) :: input_code
        logical :: success

   character(len=256) :: temp_input1, temp_input2, temp_output1, temp_output2, error_msg
        character(len=2048) :: output1, output2, line
        type(compilation_options_t) :: options
        integer :: unit, ios

        success = .false.

        ! First standardization
        temp_input1 = "/tmp/test_double_std_1.f"
        temp_output1 = "/tmp/test_double_std_1_out.f90"

        ! Create input file
        open (newunit=unit, file=temp_input1, status='replace', action='write')
        write (unit, '(a)') input_code
        close (unit)

        ! Set up compilation options
        options%backend = BACKEND_FORTRAN
        options%output_file = temp_output1

        ! Compile first time
        call compile_source(temp_input1, options, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a,a)') '    First compilation error: ', trim(error_msg)
            return
        end if

        ! Read first output
        output1 = ""
        open (newunit=unit, file=temp_output1, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read (unit, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (len_trim(output1) > 0) then
                    output1 = trim(output1)//new_line('a')
                end if
                output1 = trim(output1)//trim(line)
            end do
            close (unit)
        else
            write (error_unit, '(a)') '    Failed to read first output file'
            return
        end if

        ! Second standardization (use first output as input)
        temp_input2 = "/tmp/test_double_std_2.f"
        temp_output2 = "/tmp/test_double_std_2_out.f90"

        ! Create input file with first output
        open (newunit=unit, file=temp_input2, status='replace', action='write')
        write (unit, '(a)') trim(output1)
        close (unit)

        ! Set up compilation options
        options%output_file = temp_output2

        ! Compile second time
        call compile_source(temp_input2, options, error_msg)

        if (len_trim(error_msg) > 0) then
           write (error_unit, '(a,a)') '    Second compilation error: ', trim(error_msg)
            return
        end if

        ! Read second output
        output2 = ""
        open (newunit=unit, file=temp_output2, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read (unit, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (len_trim(output2) > 0) then
                    output2 = trim(output2)//new_line('a')
                end if
                output2 = trim(output2)//trim(line)
            end do
            close (unit)
        else
            write (error_unit, '(a)') '    Failed to read second output file'
            return
        end if

        ! Compare outputs (should be identical)
        success = (trim(adjustl(output1)) == trim(adjustl(output2)))

        if (.not. success) then
            write (error_unit, '(a)') '    First output:'
            write (error_unit, '(a)') trim(output1)
            write (error_unit, '(a)') '    Second output:'
            write (error_unit, '(a)') trim(output2)
        end if
    end function check_double_standardization

end program test_double_standardization
