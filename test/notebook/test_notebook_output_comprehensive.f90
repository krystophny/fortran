program test_notebook_output_comprehensive
    use notebook_output
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed

    print *, "=== Comprehensive Notebook Output Tests ==="
    print *, ""

    all_tests_passed = .true.

    ! Test all major functionality of notebook_output module
    if (.not. test_initialization()) all_tests_passed = .false.
    if (.not. test_cell_capture()) all_tests_passed = .false.
    if (.not. test_print_functions()) all_tests_passed = .false.
    if (.not. test_error_handling()) all_tests_passed = .false.
    if (.not. test_output_retrieval()) all_tests_passed = .false.
    if (.not. test_file_operations()) all_tests_passed = .false.
    if (.not. test_edge_cases()) all_tests_passed = .false.
    if (.not. test_memory_management()) all_tests_passed = .false.

    print *, ""
    if (all_tests_passed) then
        print *, "All comprehensive notebook output tests PASSED!"
    else
        print *, "Some comprehensive notebook output tests FAILED!"
        stop 1
    end if

contains

    function test_initialization() result(passed)
        logical :: passed
        type(cell_output_t) :: output

        print *, "Test 1: Initialization"

        ! Test basic initialization
        call init_output_capture(5)

        ! Check that cells are properly initialized
        output = get_cell_output(1)
        if (output%count /= 0) then
            print *, "  FAIL: Cell should be initialized with count 0"
            passed = .false.
            return
        end if

        if (output%has_error) then
            print *, "  FAIL: Cell should be initialized with has_error = false"
            passed = .false.
            return
        end if

        ! Test reinitialization
        call init_output_capture(10)
        output = get_cell_output(7)
        if (output%count /= 0) then
            print *, "  FAIL: Reinitialization failed"
            passed = .false.
            return
        end if

        print *, "  PASS: Initialization works correctly"
        passed = .true.
        call finalize_output_capture()

    end function test_initialization

    function test_cell_capture() result(passed)
        logical :: passed
        type(cell_output_t) :: output

        print *, "Test 2: Cell capture"

        call init_output_capture(3)

        ! Test starting capture for different cells
        call start_cell_capture(1)
        call notebook_print("Cell 1 output")

        call start_cell_capture(2)
        call notebook_print("Cell 2 output")

        call start_cell_capture(3)
        call notebook_print("Cell 3 output")

        ! Verify outputs are captured in correct cells
        output = get_cell_output(1)
        if (output%count /= 1) then
            print *, "  FAIL: Cell 1 should have 1 output, got", output%count
            passed = .false.
            call finalize_output_capture()
            return
        end if

        if (trim(output%entries(1)%content) /= "Cell 1 output") then
    print *, "  FAIL: Cell 1 content incorrect: '", trim(output%entries(1)%content), "'"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        output = get_cell_output(2)
        if (trim(output%entries(1)%content) /= "Cell 2 output") then
            print *, "  FAIL: Cell 2 content incorrect"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        print *, "  PASS: Cell capture works correctly"
        passed = .true.
        call finalize_output_capture()

    end function test_cell_capture

    function test_print_functions() result(passed)
        logical :: passed
        type(cell_output_t) :: output

        print *, "Test 3: Print functions"

        call init_output_capture(1)
        call start_cell_capture(1)

        ! Test different print functions
        call notebook_print("String message")
        call notebook_print("Integer: 42")
        call notebook_print("Real: 3.14159")
        call notebook_print("Logical: T")

        output = get_cell_output(1)
        if (output%count /= 4) then
            print *, "  FAIL: Should have 4 outputs, got", output%count
            passed = .false.
            call finalize_output_capture()
            return
        end if

        ! Check string output
        if (trim(output%entries(1)%content) /= "String message") then
            print *, "  FAIL: String output incorrect"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        ! Check integer output
        if (index(output%entries(2)%content, "Integer: 42") == 0) then
    print *, "  FAIL: Integer output incorrect: '", trim(output%entries(2)%content), "'"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        ! Check real output
        if (index(output%entries(3)%content, "Real:") == 0) then
            print *, "  FAIL: Real output incorrect"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        ! Check logical output
        if (index(output%entries(4)%content, "Logical: T") == 0) then
    print *, "  FAIL: Logical output incorrect: '", trim(output%entries(4)%content), "'"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        print *, "  PASS: All print functions work correctly"
        passed = .true.
        call finalize_output_capture()

    end function test_print_functions

    function test_error_handling() result(passed)
        logical :: passed
        type(cell_output_t) :: output

        print *, "Test 4: Error handling"

        call init_output_capture(1)
        call start_cell_capture(1)

        ! Add normal output and error output
        call notebook_print("Normal message")
        call notebook_error("Error message")
        call notebook_print("Another normal message")

        output = get_cell_output(1)

        ! Check error flag is set
        if (.not. output%has_error) then
            print *, "  FAIL: Cell should have has_error = true"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        ! Check error entry is marked correctly
        if (.not. output%entries(2)%is_error) then
            print *, "  FAIL: Second entry should be marked as error"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        if (output%entries(1)%is_error .or. output%entries(3)%is_error) then
            print *, "  FAIL: Non-error entries should not be marked as errors"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        print *, "  PASS: Error handling works correctly"
        passed = .true.
        call finalize_output_capture()

    end function test_error_handling

    function test_output_retrieval() result(passed)
        logical :: passed
        type(cell_output_t) :: output

        print *, "Test 5: Output retrieval"

        call init_output_capture(5)

        ! Test retrieving from valid cells
        call start_cell_capture(3)
        call notebook_print("Cell 3 message")

        output = get_cell_output(3)
        if (output%count /= 1) then
            print *, "  FAIL: Valid cell retrieval failed"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        ! Test retrieving from invalid cell numbers
        output = get_cell_output(0)  ! Below range
        if (output%count /= 0) then
            print *, "  FAIL: Invalid cell (0) should return empty output"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        output = get_cell_output(10)  ! Above range
        if (output%count /= 0) then
            print *, "  FAIL: Invalid cell (10) should return empty output"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        ! Test retrieving from empty cell
        output = get_cell_output(2)
        if (output%count /= 0) then
            print *, "  FAIL: Empty cell should have count 0"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        print *, "  PASS: Output retrieval works correctly"
        passed = .true.
        call finalize_output_capture()

    end function test_output_retrieval

    function test_file_operations() result(passed)
        logical :: passed
        character(len=:), allocatable :: test_file
        character(len=:), allocatable :: cell_results(:)
        integer :: i

        print *, "Test 6: File operations"

        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create("test_notebook_output")
            test_file = temp_mgr%get_file_path("test_notebook_output.dat")

            call init_output_capture(3)

            ! Create test data
            call start_cell_capture(1)
            call notebook_print("Output 1")
            call notebook_print("Output 2")

            call start_cell_capture(2)
            call notebook_error("Error message")
            call notebook_print("After error")

            call start_cell_capture(3)
            call notebook_print("Cell 3 output")

            ! Write to file
            call write_outputs_to_file(test_file)

            ! Read back from file
            call finalize_output_capture()  ! Clean up first
            call read_outputs_from_file(test_file, cell_results)

            ! Verify read data
            if (size(cell_results) /= 3) then
                print *, "  FAIL: Should read 3 cells, got", size(cell_results)
                passed = .false.
                return
            end if

            ! Check cell 1 content
            if (index(cell_results(1), "Output 1") == 0 .or. &
                index(cell_results(1), "Output 2") == 0) then
          print *, "  FAIL: Cell 1 content not preserved: '", trim(cell_results(1)), "'"
                passed = .false.
                return
            end if

            ! Check cell 2 content
            if (index(cell_results(2), "Error message") == 0 .or. &
                index(cell_results(2), "After error") == 0) then
                print *, "  FAIL: Cell 2 content not preserved"
                passed = .false.
                return
            end if

            ! Clean up
            call execute_command_line('rm -f '//trim(test_file))

            print *, "  PASS: File operations work correctly"
            passed = .true.
        end block

    end function test_file_operations

    function test_edge_cases() result(passed)
        logical :: passed
        type(cell_output_t) :: output
        integer :: i

        print *, "Test 7: Edge cases"

        call init_output_capture(1)
        call start_cell_capture(1)

        ! Test maximum outputs per cell
        do i = 1, 1005  ! Exceed MAX_OUTPUTS (1000)
            call notebook_print("Message")
        end do

        output = get_cell_output(1)
        if (output%count > 1000) then
            print *, "  FAIL: Should not exceed MAX_OUTPUTS, got", output%count
            passed = .false.
            call finalize_output_capture()
            return
        end if

        ! Test very long messages
        call finalize_output_capture()
        call init_output_capture(1)
        call start_cell_capture(1)

        call notebook_print(repeat("X", 500))  ! Very long string
        output = get_cell_output(1)
        if (len(output%entries(1)%content) /= 500) then
            print *, "  FAIL: Long message not preserved correctly"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        ! Test empty messages
        call notebook_print("")
        output = get_cell_output(1)
        if (output%count /= 2) then
            print *, "  FAIL: Empty message should still be counted"
            passed = .false.
            call finalize_output_capture()
            return
        end if

        print *, "  PASS: Edge cases handled correctly"
        passed = .true.
        call finalize_output_capture()

    end function test_edge_cases

    function test_memory_management() result(passed)
        logical :: passed

        print *, "Test 8: Memory management"

        ! Test multiple initialize/finalize cycles
        call init_output_capture(10)
        call start_cell_capture(5)
        call notebook_print("Test message")
        call finalize_output_capture()

        call init_output_capture(5)
        call start_cell_capture(2)
        call notebook_print("Another message")
        call finalize_output_capture()

        ! Test with zero cells
        call init_output_capture(0)
        call finalize_output_capture()

        ! Test finalize without initialize
        call finalize_output_capture()

        print *, "  PASS: Memory management works correctly"
        passed = .true.

    end function test_memory_management

end program test_notebook_output_comprehensive
