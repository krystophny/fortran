program test_control_flow_comprehensive
    use frontend, only: compile_source, compilation_options_t, BACKEND_FORTRAN
    implicit none

    character(len=:), allocatable :: source
    character(len=256) :: output_file, error_msg
    type(compilation_options_t) :: options
    integer :: unit, test_count, passed_count

    test_count = 0
    passed_count = 0

    print *, "Running comprehensive control flow parsing tests..."
    print *, ""

    ! Test 1: Simple sequence (baseline)
    call run_test("Simple sequence", &
                  "print *, 'First statement'"//new_line('a')// &
                  "print *, 'Second statement'", &
                  2) ! Expected minimum lines

    ! Test 2: Simple if-then-endif with following statement
    call run_test("Simple if-then with following statement", &
                  "x = 1"//new_line('a')// &
                  "if (x > 0) then"//new_line('a')// &
                  "    print *, 'positive'"//new_line('a')// &
                  "end if"//new_line('a')// &
                  "print *, 'after if block'", &
                  5) ! Expected minimum lines

    ! Test 3: If-else-endif with following statement (the main bug case)
    call run_test("If-else-endif with following statement", &
                  "x = 1"//new_line('a')// &
                  "if (x < 0) then"//new_line('a')// &
                  "    print *, 'negative'"//new_line('a')// &
                  "else if (x > 0) then"//new_line('a')// &
                  "    print *, 'positive'"//new_line('a')// &
                  "else"//new_line('a')// &
                  "    print *, 'zero'"//new_line('a')// &
                  "end if"//new_line('a')// &
                  "print *, 'after if-else block'", &
                  9) ! Expected minimum lines

    ! Test 4: String with escaped quotes
    call run_test("String with escaped quotes", &
                  "print *, 'It''s freezing!'"//new_line('a')// &
                  "print *, ""It's cold""", &
                  2) ! Expected minimum lines

    ! Test 5: Multiple if blocks in sequence
    call run_test("Multiple if blocks in sequence", &
                  "x = 1"//new_line('a')// &
                  "if (x > 0) then"//new_line('a')// &
                  "    print *, 'first if'"//new_line('a')// &
                  "end if"//new_line('a')// &
                  "if (x < 10) then"//new_line('a')// &
                  "    print *, 'second if'"//new_line('a')// &
                  "end if"//new_line('a')// &
                  "print *, 'final statement'", &
                  8) ! Expected minimum lines

    print *, ""
    print *, "Test Results:", passed_count, "/", test_count, "passed"
    if (passed_count == test_count) then
        print *, "All tests PASSED!"
    else
        print *, "Some tests FAILED"
    end if

contains

    subroutine run_test(test_name, test_source, expected_min_lines)
        character(len=*), intent(in) :: test_name, test_source
        integer, intent(in) :: expected_min_lines
        integer :: generated_lines

        test_count = test_count + 1

        ! Write source to file
        open (newunit=unit, file="/tmp/test_cf.f", status='replace')
        write (unit, '(A)') test_source
        close (unit)

        output_file = "/tmp/test_cf_out.f90"
        options%backend = BACKEND_FORTRAN
        options%output_file = output_file

        call compile_source("/tmp/test_cf.f", options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "❌ FAIL:", trim(test_name), "- Compilation error:", trim(error_msg)
        else
            ! Count lines in generated output
      call execute_command_line("wc -l < "//trim(output_file)//" > /tmp/line_count.txt")
            open (newunit=unit, file="/tmp/line_count.txt", status='old')
            read (unit, *) generated_lines
            close (unit)

            if (generated_lines >= expected_min_lines) then
            print *, "✅ PASS:", trim(test_name), "- Generated", generated_lines, "lines"
                passed_count = passed_count + 1
            else
       print *, "❌ FAIL:", trim(test_name), "- Generated only", generated_lines, "lines"
            end if
        end if
    end subroutine run_test

end program test_control_flow_comprehensive
