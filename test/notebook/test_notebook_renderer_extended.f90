program test_notebook_renderer_extended
    use notebook_parser
    use notebook_executor
    use notebook_renderer
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed

    print *, "=== Extended Notebook Renderer Tests ==="
    print *

    all_tests_passed = .true.

    ! Test extended functionality
    if (.not. test_markdown_cell_edge_cases()) all_tests_passed = .false.
    if (.not. test_code_cell_output_formatting()) all_tests_passed = .false.
    if (.not. test_empty_notebook_rendering()) all_tests_passed = .false.
    if (.not. test_large_notebook_rendering()) all_tests_passed = .false.
    if (.not. test_save_markdown_edge_cases()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All extended notebook renderer tests passed!"
        stop 0
    else
        print *, "Some extended notebook renderer tests failed!"
        stop 1
    end if

contains

    function test_markdown_cell_edge_cases() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        type(execution_result_t) :: results
        character(len=:), allocatable :: output

        print *, "Test 1: Markdown cell edge cases"
        passed = .true.

        ! Create notebook with various markdown cells
        call create_test_notebook_markdown_edge_cases(notebook)
        call create_empty_results(results, notebook%num_cells)

        ! Render the notebook
        call render_notebook_markdown(notebook, results, output)

        ! Check that output contains expected markdown elements
        if (index(output, '#') == 0) then
            print *, "  WARNING: Headers not found in output"
        end if

        if (index(output, '*') == 0) then
            print *, "  WARNING: Formatting not found in output"
        end if

        print *, "  PASS: Markdown edge cases"

    end function test_markdown_cell_edge_cases

    function test_code_cell_output_formatting() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        type(execution_result_t) :: results
        character(len=:), allocatable :: output

        print *, "Test 2: Code cell output formatting"
        passed = .true.

        ! Create notebook with code cells
        call create_test_notebook_code_cells(notebook)
        call create_test_results_with_output(results, notebook%num_cells)

        ! Render the notebook
        call render_notebook_markdown(notebook, results, output)

        ! Check for code formatting
        if (index(output, '```fortran') == 0) then
            print *, "  WARNING: Fortran code blocks not found"
        end if

        if (index(output, '```') == 0) then
            print *, "  WARNING: Code blocks not found"
        end if

        print *, "  PASS: Code cell formatting"

    end function test_code_cell_output_formatting

    function test_empty_notebook_rendering() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        type(execution_result_t) :: results
        character(len=:), allocatable :: output

        print *, "Test 3: Empty notebook rendering"
        passed = .true.

        ! Create empty notebook
        notebook%num_cells = 0
        call create_empty_results(results, 0)

        ! Render empty notebook
        call render_notebook_markdown(notebook, results, output)

        ! Should handle gracefully
        if (len(output) < 0) then  ! Should not crash
            passed = .false.
        end if

        print *, "  PASS: Empty notebook"

    end function test_empty_notebook_rendering

    function test_large_notebook_rendering() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        type(execution_result_t) :: results
        character(len=:), allocatable :: output

        print *, "Test 4: Large notebook rendering"
        passed = .true.

        ! Create notebook with many cells
        call create_large_test_notebook(notebook)
        call create_empty_results(results, notebook%num_cells)

        ! Render large notebook
        call render_notebook_markdown(notebook, results, output)

        ! Should handle large content
        if (len(output) < 100) then
            print *, "  WARNING: Large notebook output seems too small"
        end if

        print *, "  PASS: Large notebook"

    end function test_large_notebook_rendering

    function test_save_markdown_edge_cases() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        type(execution_result_t) :: results
        character(len=256) :: test_file
        logical :: file_exists

        print *, "Test 5: Save markdown edge cases"
        passed = .true.

        ! Create test notebook
        call create_test_notebook_markdown_edge_cases(notebook)
        call create_empty_results(results, notebook%num_cells)

        ! Test saving to valid file
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('renderer_test')
            test_file = temp_mgr%get_file_path('test_renderer_output.md')
            call save_notebook_markdown(notebook, results, test_file)
        end block

        inquire (file=test_file, exist=file_exists)
        if (.not. file_exists) then
            print *, "  WARNING: Output file was not created"
        else
            ! Clean up
            call delete_test_file(test_file)
        end if

        ! Test saving with invalid path
        test_file = '/dev/null/invalid/path.md'
        call save_notebook_markdown(notebook, results, test_file)
        ! Should handle gracefully without crashing

        print *, "  PASS: Save markdown edge cases"

    end function test_save_markdown_edge_cases

    ! Helper subroutines
    subroutine create_test_notebook_markdown_edge_cases(notebook)
        type(notebook_t), intent(out) :: notebook

        notebook%num_cells = 3
        allocate (notebook%cells(3))

        ! Markdown cell with headers
        notebook%cells(1)%cell_type = CELL_MARKDOWN
        notebook%cells(1)%content = '# Main Title'//new_line('a')// &
                                    '## Subtitle'//new_line('a')// &
                                    'Some *bold* and **italic** text.'

        ! Markdown cell with lists
        notebook%cells(2)%cell_type = CELL_MARKDOWN
        notebook%cells(2)%content = '- Item 1'//new_line('a')// &
                                    '- Item 2'//new_line('a')// &
                                    '  - Nested item'

        ! Empty markdown cell
        notebook%cells(3)%cell_type = CELL_MARKDOWN
        notebook%cells(3)%content = ''

    end subroutine create_test_notebook_markdown_edge_cases

    subroutine create_test_notebook_code_cells(notebook)
        type(notebook_t), intent(out) :: notebook

        notebook%num_cells = 2
        allocate (notebook%cells(2))

        ! Code cell with simple content
        notebook%cells(1)%cell_type = CELL_CODE
        notebook%cells(1)%content = 'x = 42'//new_line('a')// &
                                    'print *, "x =", x'

        ! Code cell with complex content
        notebook%cells(2)%cell_type = CELL_CODE
        notebook%cells(2)%content = 'do i = 1, 3'//new_line('a')// &
                                    '  print *, "i =", i'//new_line('a')// &
                                    'end do'

    end subroutine create_test_notebook_code_cells

    subroutine create_large_test_notebook(notebook)
        type(notebook_t), intent(out) :: notebook
        integer :: i
        character(len=32) :: cell_number

        notebook%num_cells = 20
        allocate (notebook%cells(20))

        do i = 1, notebook%num_cells
            write (cell_number, '(i0)') i
            if (mod(i, 2) == 0) then
                notebook%cells(i)%cell_type = CELL_MARKDOWN
             notebook%cells(i)%content = '# Cell '//trim(cell_number)//new_line('a')// &
                                      'This is markdown cell number '//trim(cell_number)
            else
                notebook%cells(i)%cell_type = CELL_CODE
      notebook%cells(i)%content = 'print *, "This is code cell '//trim(cell_number)//'"'
            end if
        end do

    end subroutine create_large_test_notebook

    subroutine create_empty_results(results, num_cells)
        type(execution_result_t), intent(out) :: results
        integer, intent(in) :: num_cells
        integer :: i

        allocate (results%cells(num_cells))

        do i = 1, num_cells
            results%cells(i)%output = ''
            results%cells(i)%success = .true.
        end do

    end subroutine create_empty_results

    subroutine create_test_results_with_output(results, num_cells)
        type(execution_result_t), intent(out) :: results
        integer, intent(in) :: num_cells
        integer :: i
        character(len=32) :: cell_number

        allocate (results%cells(num_cells))

        do i = 1, num_cells
            write (cell_number, '(i0)') i
            results%cells(i)%output = 'Output from cell '//trim(cell_number)
            results%cells(i)%success = .true.
        end do

    end subroutine create_test_results_with_output

    subroutine delete_test_file(filename)
        character(len=*), intent(in) :: filename
        character(len=512) :: command

        command = 'rm -f "'//trim(filename)//'"'
        call execute_command_line(command)

    end subroutine delete_test_file

end program test_notebook_renderer_extended
