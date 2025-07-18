program test_notebook_output_extended
    use notebook_output
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed

    print *, "=== Extended Notebook Output Tests ==="
    print *

    all_tests_passed = .true.

    ! Test extended functionality of notebook output module
    if (.not. test_basic_output_capture()) all_tests_passed = .false.
    if (.not. test_multi_cell_outputs()) all_tests_passed = .false.
    if (.not. test_error_output_handling()) all_tests_passed = .false.
    if (.not. test_output_limits()) all_tests_passed = .false.
    if (.not. test_file_io_operations()) all_tests_passed = .false.
    if (.not. test_edge_cases()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All extended notebook output tests passed!"
        stop 0
    else
        print *, "Some extended notebook output tests failed!"
        stop 1
    end if

contains

    function test_basic_output_capture() result(passed)
        logical :: passed
        type(cell_output_t) :: output

        print *, "Test 1: Basic output capture"
        passed = .true.

        ! Initialize with 3 cells
        call init_output_capture(3)

        ! Test cell 1
        call start_cell_capture(1)
        call notebook_print("Hello from cell 1")
        call notebook_print("Second line")

        output = get_cell_output(1)
        if (output%count /= 2) then
            print *, "  FAILED: Expected 2 outputs, got", output%count
            passed = .false.
        end if

        if (allocated(output%entries(1)%content)) then
            if (output%entries(1)%content /= "Hello from cell 1") then
                print *, "  FAILED: First output mismatch"
                passed = .false.
            end if
        else
            print *, "  FAILED: First output not allocated"
            passed = .false.
        end if

        call finalize_output_capture()

        if (passed) print *, "  PASS: Basic output capture"

    end function test_basic_output_capture

    function test_multi_cell_outputs() result(passed)
        logical :: passed
        type(cell_output_t) :: output1, output3, output5

        print *, "Test 2: Multi-cell output management"
        passed = .true.

        call init_output_capture(5)

        ! Capture output for multiple cells
        call start_cell_capture(1)
        call notebook_print("Cell 1 output")

        call start_cell_capture(3)
        call notebook_print("Cell 3 output")
        call notebook_error("Cell 3 error")

        call start_cell_capture(5)
        call notebook_print("Cell 5 output")

        ! Check outputs
        output1 = get_cell_output(1)
        output3 = get_cell_output(3)
        output5 = get_cell_output(5)

        if (output1%count /= 1) then
            print *, "  FAILED: Cell 1 count wrong"
            passed = .false.
        end if

        if (output3%count /= 2) then
            print *, "  FAILED: Cell 3 count wrong"
            passed = .false.
        end if

        if (.not. output3%has_error) then
            print *, "  FAILED: Cell 3 should have error"
            passed = .false.
        end if

        if (output5%count /= 1) then
            print *, "  FAILED: Cell 5 count wrong"
            passed = .false.
        end if

        call finalize_output_capture()

        if (passed) print *, "  PASS: Multi-cell outputs"

    end function test_multi_cell_outputs

    function test_error_output_handling() result(passed)
        logical :: passed
        type(cell_output_t) :: output

        print *, "Test 3: Error output handling"
        passed = .true.

        call init_output_capture(2)

        ! Test error outputs
        call start_cell_capture(1)
        call notebook_print("Normal output")
        call notebook_error("Error occurred!")
        call notebook_print("After error")

        output = get_cell_output(1)

        if (output%count /= 3) then
            print *, "  FAILED: Expected 3 outputs"
            passed = .false.
        end if

        if (.not. output%has_error) then
            print *, "  FAILED: Cell should have error flag"
            passed = .false.
        end if

        if (allocated(output%entries(2)%content)) then
            if (.not. output%entries(2)%is_error) then
                print *, "  FAILED: Second entry should be error"
                passed = .false.
            end if
        end if

        call finalize_output_capture()

        if (passed) print *, "  PASS: Error output handling"

    end function test_error_output_handling

    function test_output_limits() result(passed)
        logical :: passed
        type(cell_output_t) :: output
        integer :: i
        character(len=50) :: msg

        print *, "Test 4: Output limits and boundaries"
        passed = .true.

        call init_output_capture(1)
        call start_cell_capture(1)

        ! Try to exceed MAX_OUTPUTS (1000)
        do i = 1, 1005
            write (msg, '(a,i0)') "Output line ", i
            call notebook_print(trim(msg))
        end do

        output = get_cell_output(1)

        ! Should be capped at MAX_OUTPUTS
        if (output%count /= 1000) then
           print *, "  FAILED: Output count should be capped at 1000, got", output%count
            passed = .false.
        end if

        call finalize_output_capture()

        if (passed) print *, "  PASS: Output limits"

    end function test_output_limits

    function test_file_io_operations() result(passed)
        logical :: passed
        character(len=:), allocatable :: cell_results(:)
        character(len=256) :: test_file
        type(temp_dir_manager) :: temp_mgr

        print *, "Test 5: File I/O operations"
        passed = .true.

        call temp_mgr%create('notebook_output_test')
        test_file = temp_mgr%get_file_path('test_notebook_output.dat')

        ! Create some outputs
        call init_output_capture(3)

        call start_cell_capture(1)
        call notebook_print("First cell output")
        call notebook_print("Second line of first cell")

        call start_cell_capture(2)
        call notebook_error("Error in cell 2")

        call start_cell_capture(3)
        call notebook_print("Third cell output")

        ! Write to file
        call write_outputs_to_file(test_file)

        ! Read back from file
        call read_outputs_from_file(test_file, cell_results)

        if (.not. allocated(cell_results)) then
            print *, "  FAILED: Results not allocated"
            passed = .false.
        else
            if (size(cell_results) /= 3) then
                print *, "  FAILED: Wrong number of cells read"
                passed = .false.
            end if

            ! Check content (basic check)
            if (index(cell_results(1), "First cell output") == 0) then
                print *, "  FAILED: First cell content not found"
                passed = .false.
            end if

            if (index(cell_results(2), "Error in cell 2") == 0) then
                print *, "  FAILED: Second cell error not found"
                passed = .false.
            end if
        end if

        ! Clean up
        call finalize_output_capture()

        if (passed) print *, "  PASS: File I/O operations"

    end function test_file_io_operations

    function test_edge_cases() result(passed)
        logical :: passed
        type(cell_output_t) :: output
        character(len=2000) :: long_message
        integer :: i

        print *, "Test 6: Edge cases and error conditions"
        passed = .true.

        ! Test invalid cell numbers
        call init_output_capture(5)

        ! Try to capture for cell 0
        call start_cell_capture(0)
        call notebook_print("Should not be captured")

        output = get_cell_output(0)
        if (output%count /= 0) then
            print *, "  FAILED: Cell 0 should have no output"
            passed = .false.
        end if

        ! Try to capture for cell beyond max
        call start_cell_capture(10)
        call notebook_print("Should not be captured either")

        output = get_cell_output(10)
        if (output%count /= 0) then
            print *, "  FAILED: Cell 10 should have no output"
            passed = .false.
        end if

        ! Test very long message
        do i = 1, 200
            long_message(i*10 - 9:i*10) = "1234567890"
        end do

        call start_cell_capture(1)
        call notebook_print(trim(long_message))

        output = get_cell_output(1)
        if (output%count /= 1) then
            print *, "  FAILED: Long message not captured"
            passed = .false.
        end if

        ! Test empty message
        call start_cell_capture(2)
        call notebook_print("")
        call notebook_error("")

        output = get_cell_output(2)
        if (output%count /= 2) then
            print *, "  FAILED: Empty messages not captured"
            passed = .false.
        end if

        call finalize_output_capture()

        if (passed) print *, "  PASS: Edge cases"

    end function test_edge_cases

end program test_notebook_output_extended
