program test_notebook_integration
    use notebook_parser
    use notebook_executor
    use notebook_renderer
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, "Testing notebook integration..."

    ! Test 1: Simple notebook with output
    call test_simple_notebook_execution()

    ! Test 2: Notebook with markdown rendering
    call test_markdown_rendering()

    ! Test 3: Notebook with multiple cells
    call test_multi_cell_execution()

    ! Test 4: Notebook with fortplotlib (mock)
    call test_plot_capture()

    if (all_tests_passed) then
        print *, "All notebook integration tests passed!"
        stop 0
    else
        print *, "Some notebook integration tests failed!"
        stop 1
    end if

contains

    subroutine test_simple_notebook_execution()
        character(len=:), allocatable :: content, output
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=256) :: test_cache_dir
        integer :: i

        print *, "  Test 1: Simple notebook execution..."

        ! Create a simple notebook
        content = "! %% [markdown]"//new_line('a')// &
                  "! # Simple Calculation"//new_line('a')// &
                  "! %%"//new_line('a')// &
                  "x = 5"//new_line('a')// &
                  "y = x * 2"//new_line('a')// &
                  "print *, 'Result:', y"

        ! Parse notebook
        call parse_notebook(content, nb)

        ! Set up test cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('test_notebook_cache')
            test_cache_dir = temp_mgr%path
        end block

        ! Execute notebook
        call execute_notebook(nb, results, test_cache_dir)

        ! Check execution results
        if (.not. results%success) then
            print *, "    FAILED: Execution failed"
            all_tests_passed = .false.
            return
        end if

        ! Check that output was captured
        if (index(results%cells(2)%output, "Result:") == 0) then
            print *, "    FAILED: Output not captured"
            all_tests_passed = .false.
            return
        end if

        print *, "    PASSED"

        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_simple_notebook_execution

    subroutine test_markdown_rendering()
        character(len=:), allocatable :: content, markdown_output
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=256) :: test_cache_dir

        print *, "  Test 2: Markdown rendering..."

        content = "! %% [markdown]"//new_line('a')// &
                  "! # Test Notebook"//new_line('a')// &
                  "! This is a test."//new_line('a')// &
                  "! %%"//new_line('a')// &
                  "print *, 'Hello, World!'"

        call parse_notebook(content, nb)

        ! Set up test cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('test_notebook_cache')
            test_cache_dir = temp_mgr%path
        end block

        call execute_notebook(nb, results, test_cache_dir)
        call render_notebook_markdown(nb, results, markdown_output)

        ! Check markdown output contains expected elements
        if (index(markdown_output, "# Test Notebook") == 0) then
            print *, "    FAILED: Markdown title not found"
            all_tests_passed = .false.
            return
        end if

        if (index(markdown_output, "```fortran") == 0) then
            print *, "    FAILED: Code block not found"
            all_tests_passed = .false.
            return
        end if

        if (index(markdown_output, "Hello, World!") == 0) then
            print *, "    FAILED: Output not found"
            all_tests_passed = .false.
            return
        end if

        print *, "    PASSED"

        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_markdown_rendering

    subroutine test_multi_cell_execution()
        character(len=:), allocatable :: content, markdown_output
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=256) :: test_cache_dir
        integer :: i

        print *, "  Test 3: Multi-cell execution..."

        content = "! %%"//new_line('a')// &
                  "x = 10"//new_line('a')// &
                  "! %%"//new_line('a')// &
                  "y = x * 2"//new_line('a')// &
                  "print *, 'y =', y"//new_line('a')// &
                  "! %%"//new_line('a')// &
                  "z = x + y"//new_line('a')// &
                  "print *, 'z =', z"

        call parse_notebook(content, nb)

        ! Set up test cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('test_notebook_cache')
            test_cache_dir = temp_mgr%path
        end block

        call execute_notebook(nb, results, test_cache_dir)

        ! Check that variables persist across cells
        if (index(results%cells(2)%output, "y =") == 0 .or. &
            index(results%cells(2)%output, "20") == 0) then
            print *, "    FAILED: Second cell output incorrect"
            all_tests_passed = .false.
            return
        end if

        if (index(results%cells(3)%output, "z =") == 0 .or. &
            index(results%cells(3)%output, "30") == 0) then
            print *, "    FAILED: Third cell output incorrect"
            all_tests_passed = .false.
            return
        end if

        print *, "    PASSED"

        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_multi_cell_execution

    subroutine test_plot_capture()
        character(len=:), allocatable :: content, markdown_output
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=256) :: test_cache_dir

        print *, "  Test 4: Plot capture (mock)..."

        content = "! %%"//new_line('a')// &
                  "! Mock plot generation"//new_line('a')// &
                  "! call plot([1, 2, 3], [1, 4, 9])"//new_line('a')// &
                  "! call show()"//new_line('a')// &
                  "print *, 'Plot would be shown here'"

        call parse_notebook(content, nb)

        ! Set up test cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('test_notebook_cache')
            test_cache_dir = temp_mgr%path
        end block

        call execute_notebook(nb, results, test_cache_dir)
        call render_notebook_markdown(nb, results, markdown_output)

        ! For now, just check basic rendering works
        if (index(markdown_output, "Plot would be shown here") == 0) then
            print *, "    FAILED: Mock output not found"
            all_tests_passed = .false.
            return
        end if

        print *, "    PASSED"

        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_plot_capture

end program test_notebook_integration
