program test_phase6_basic
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_single_assignment()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_single_assignment()
        character(len=:), allocatable :: input
        character(len=256) :: temp_input, temp_output, error_msg
        character(len=2048) :: output, line
        type(compilation_options_t) :: options
        integer :: unit, ios

        test_count = test_count + 1

        ! Test simple assignment with explicit type
        input = "integer :: x"//new_line('a')//"x = 42"

        ! Create temp files
        temp_input = "/tmp/test_phase6_input.f"
        temp_output = "/tmp/test_phase6_output.f90"

        ! Create input file
        open (newunit=unit, file=temp_input, status='replace', action='write')
        write (unit, '(a)') input
        close (unit)

        ! Set up compilation options
        options%backend = BACKEND_FORTRAN
        options%output_file = temp_output

        ! Compile
        call compile_source(temp_input, options, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a,a)') 'Compilation error: ', trim(error_msg)
            return
        end if

        ! Read output
        output = ""
        open (newunit=unit, file=temp_output, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read (unit, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (len_trim(output) > 0) then
                    output = trim(output)//new_line('a')
                end if
                output = trim(output)//trim(line)
            end do
            close (unit)
        else
            write (error_unit, '(a)') 'Failed to read output file'
            return
        end if

        ! Basic check: should contain program wrapper
        if (index(output, 'program') > 0 .and. index(output, 'x = 42') > 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Single assignment basic compilation"
        else
            write (*, '(A)') "FAIL: Single assignment basic compilation"
            write (error_unit, '(A)') "Output:"
            write (error_unit, '(A)') trim(output)
        end if
    end subroutine test_single_assignment

end program test_phase6_basic
