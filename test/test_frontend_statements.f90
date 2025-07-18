program test_frontend_statements
    use iso_fortran_env, only: error_unit
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    write (*, '(a)') '=== Frontend Statement Tests ==='
    write (*, '(a)') ''

    call test_use_statement(test_count, pass_count)
    call test_print_statement(test_count, pass_count)
    call test_multiple_statements(test_count, pass_count)
    call test_do_while_statement(test_count, pass_count)

    write (*, '(a)') ''
    write(*, '(a,i0,a,i0,a)') 'Frontend statement tests: ', pass_count, '/', test_count, ' passed'

    if (pass_count /= test_count) then
        error stop 'Some frontend statement tests failed!'
    end if

contains

    subroutine test_use_statement(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        write (*, '(a)') 'Test 1: Use statement'
        test_count = test_count + 1

        if (compile_and_check("use iso_fortran_env", &
                              'program main'//new_line('a')// &
                              '    use iso_fortran_env'//new_line('a')// &
                              '    implicit none'//new_line('a')// &
                              'end program main')) then
            write (*, '(a)') '  ✓ PASS: Use statement parsed correctly'
            pass_count = pass_count + 1
        else
            write (*, '(a)') '  ✗ FAIL: Use statement parsing failed'
        end if

    end subroutine test_use_statement

    subroutine test_print_statement(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        write (*, '(a)') 'Test 2: Print statement'
        test_count = test_count + 1

        if (compile_and_check('print *, "Hello"', &
                              'program main'//new_line('a')// &
                              '    implicit none'//new_line('a')// &
                              '    print *, "Hello"'//new_line('a')// &
                              'end program main')) then
            write (*, '(a)') '  ✓ PASS: Print statement parsed correctly'
            pass_count = pass_count + 1
        else
            write (*, '(a)') '  ✗ FAIL: Print statement parsing failed'
        end if

    end subroutine test_print_statement

    subroutine test_multiple_statements(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        write (*, '(a)') 'Test 3: Multiple statements'
        test_count = test_count + 1

        if (compile_and_check('x = 42'//new_line('a')//'print *, x', &
                              'program main'//new_line('a')// &
                              '    implicit none'//new_line('a')// &
                              '    integer :: x'//new_line('a')// &
                              ''//new_line('a')// &
                              '    x = 42'//new_line('a')// &
                              '    print *, x'//new_line('a')// &
                              'end program main')) then
            write (*, '(a)') '  ✓ PASS: Multiple statements parsed correctly'
            pass_count = pass_count + 1
        else
            write (*, '(a)') '  ✗ FAIL: Multiple statements parsing failed'
        end if

    end subroutine test_multiple_statements

    subroutine test_do_while_statement(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        write (*, '(a)') 'Test 4: Do while statement'
        test_count = test_count + 1

        if (compile_and_check('i = 0'//new_line('a')// &
                              'do while (i < 3)'//new_line('a')// &
                              '    print *, i'//new_line('a')// &
                              '    i = i + 1'//new_line('a')// &
                              'end do', &
                              'program main'//new_line('a')// &
                              '    implicit none'//new_line('a')// &
                              '    integer :: i'//new_line('a')// &
                              ''//new_line('a')// &
                              '    i = 0'//new_line('a')// &
                              '    do while (i < 3)'//new_line('a')// &
                              '    print *, i'//new_line('a')// &
                              '    i = i + 1'//new_line('a')// &
                              'end do'//new_line('a')// &
                              'end program main')) then
            write (*, '(a)') '  ✓ PASS: Do while statement parsed correctly'
            pass_count = pass_count + 1
        else
            write (*, '(a)') '  ✗ FAIL: Do while statement parsing failed'
        end if

    end subroutine test_do_while_statement

    function compile_and_check(input_code, expected_output) result(success)
        use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
        character(len=*), intent(in) :: input_code, expected_output
        logical :: success

        character(len=256) :: temp_input, temp_output, error_msg
        character(len=1024) :: generated_code, line
        type(compilation_options_t) :: options
        integer :: unit, ios, i

        success = .false.

        ! Create temporary input file
temp_input = get_temp_file_path(create_temp_dir('fortran_test'), 'test_frontend_stmt.f')
        open (newunit=unit, file=temp_input, status='replace', action='write')
        write (unit, '(a)') input_code
        close (unit)

        ! Set up compilation options
        temp_output = get_temp_file_path(create_temp_dir('fortran_test'), 'test_frontend_stmt_out.f90')
        options%backend = BACKEND_FORTRAN
        options%output_file = temp_output

        ! Compile
        call compile_source(temp_input, options, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a,a)') '    Compilation error: ', trim(error_msg)
            return
        end if

        ! Read generated code
        generated_code = ""
        open (newunit=unit, file=temp_output, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read (unit, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (len_trim(generated_code) > 0) then
                    generated_code = trim(generated_code)//new_line('a')
                end if
                generated_code = trim(generated_code)//trim(line)
            end do
            close (unit)
        else
            write (error_unit, '(a)') '    Failed to read output file'
            return
        end if

        ! Compare (normalize whitespace)
        success = (trim(adjustl(generated_code)) == trim(adjustl(expected_output)))

        if (.not. success) then
            write (error_unit, '(a)') '    Expected:'
            write (error_unit, '(a)') trim(expected_output)
            write (error_unit, '(a)') '    Got:'
            write (error_unit, '(a)') trim(generated_code)
        end if

        ! Clean up
        open (newunit=unit, file=temp_input, status='old', iostat=ios)
        if (ios == 0) close (unit, status='delete')
        open (newunit=unit, file=temp_output, status='old', iostat=ios)
        if (ios == 0) close (unit, status='delete')

    end function compile_and_check

end program test_frontend_statements
