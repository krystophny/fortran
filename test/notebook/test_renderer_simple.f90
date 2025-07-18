program test_renderer_simple
    use notebook_renderer
    use notebook_parser
    use notebook_executor
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed

    print *, "=== Renderer Simple Tests ==="
    print *

    all_tests_passed = .true.

    if (.not. test_basic_render()) all_tests_passed = .false.
    if (.not. test_save_markdown()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All renderer simple tests passed!"
        stop 0
    else
        print *, "Some renderer simple tests failed!"
        stop 1
    end if

contains

    function test_basic_render() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        type(execution_result_t) :: results
        character(len=:), allocatable :: rendered
        character(len=:), allocatable :: content

        print *, "Test 1: Basic rendering"
        passed = .true.

        ! Create simple notebook
        content = "print *, 'Hello from renderer test'"
        call parse_notebook(content, notebook)

        ! Execute
        call execute_notebook(notebook, results)

        ! Render
        call render_notebook_markdown(notebook, results, rendered)

        if (.not. allocated(rendered)) then
            print *, "  FAILED: Rendered output not allocated"
            passed = .false.
        end if

        call free_notebook(notebook)

        if (passed) print *, "  PASS: Basic rendering"

    end function test_basic_render

    function test_save_markdown() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        type(execution_result_t) :: results
        character(len=:), allocatable :: rendered
        character(len=:), allocatable :: content
        character(len=256) :: test_file
        logical :: file_exists
        type(temp_dir_manager) :: temp_mgr

        print *, "Test 2: Save markdown"
        passed = .true.

        ! Create notebook
        content = "x = 42"
        call parse_notebook(content, notebook)
        call execute_notebook(notebook, results)
        call render_notebook_markdown(notebook, results, rendered)

        ! Save
        call temp_mgr%create('renderer_simple_test')
        test_file = temp_mgr%get_file_path('test_renderer_save.md')
        if (allocated(rendered)) then
            call save_notebook_markdown(notebook, results, test_file)

            inquire (file=test_file, exist=file_exists)
            if (.not. file_exists) then
                print *, "  FAILED: Saved file does not exist"
                passed = .false.
            end if

            call execute_command_line("rm -f "//trim(test_file))
        end if

        call free_notebook(notebook)

        if (passed) print *, "  PASS: Save markdown"

    end function test_save_markdown

end program test_renderer_simple
