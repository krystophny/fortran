program test_notebook_examples
    use notebook_parser
    use notebook_executor
    use notebook_renderer
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed

    print *, '=== Notebook Examples Tests ==='
    print *

    all_tests_passed = .true.

    ! Test 1: simple_math.f example
    call test_simple_math_example(all_tests_passed)

    ! Test 2: arrays_loops.f example
    call test_arrays_loops_example(all_tests_passed)

    ! Test 3: control_flow.f example
    call test_control_flow_example(all_tests_passed)

    if (all_tests_passed) then
        print *
        print *, 'All notebook example tests passed!'
        stop 0
    else
        print *
        print *, 'Some example tests failed!'
        stop 1
    end if

contains

    subroutine test_simple_math_example(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: content
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: markdown_output
        character(len=:), allocatable :: test_cache_dir

        print *, 'Test 1: simple_math.f example'

        ! Set up test cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            temp_mgr = temp_dir_manager("test_notebook_examples")
            test_cache_dir = temp_mgr%get_path()
            call execute_command_line("mkdir -p "//trim(test_cache_dir))

            ! Read the simple_math.f example
            call read_example_file("example/scientific/notebook/simple_math.f", content)

            if (len(content) == 0) then
                print *, '  FAIL: Could not read simple_math.f'
                passed = .false.
                return
            end if

            ! Parse the notebook
            call parse_notebook(content, nb)

            ! Check parsing worked
            if (nb%num_cells == 0) then
                print *, '  FAIL: No cells parsed from simple_math.f'
                passed = .false.
                goto 99
            end if

            ! Execute the notebook
            call execute_notebook(nb, results, test_cache_dir)

            ! Check execution completed
            if (.not. allocated(results%cells)) then
                print *, '  FAIL: No execution results for simple_math.f'
                passed = .false.
                goto 99
            end if

            if (size(results%cells) /= nb%num_cells) then
                print *, '  FAIL: Result cells mismatch for simple_math.f'
                passed = .false.
                goto 99
            end if

            ! Generate markdown output
            call render_notebook_markdown(nb, results, markdown_output)

            ! Check markdown was generated
            if (len(markdown_output) == 0) then
                print *, '  FAIL: No markdown output generated'
                passed = .false.
                goto 99
            end if

            ! Check for expected content
            if (index(markdown_output, "# Simple Mathematical Computations") == 0) then
                print *, '  FAIL: Title not found in markdown'
                passed = .false.
                goto 99
            end if

            if (index(markdown_output, "```fortran") == 0) then
                print *, '  FAIL: Code blocks not found in markdown'
                passed = .false.
                goto 99
            end if

            print *, '  PASS'

99          continue
            ! Cleanup
            call execute_command_line("rm -rf "//trim(test_cache_dir))
            call free_notebook(nb)
            call free_execution_results(results)
        end block

    end subroutine test_simple_math_example

    subroutine test_arrays_loops_example(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: content
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: markdown_output
        character(len=:), allocatable :: test_cache_dir

        print *, 'Test 2: arrays_loops.f example'

        ! Set up test cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            temp_mgr = temp_dir_manager("test_notebook_examples_arrays")
            test_cache_dir = temp_mgr%get_path()
            call execute_command_line("mkdir -p "//trim(test_cache_dir))

            ! Read the arrays_loops.f example
           call read_example_file("example/scientific/notebook/arrays_loops.f", content)

            if (len(content) == 0) then
                print *, '  FAIL: Could not read arrays_loops.f'
                passed = .false.
                return
            end if

            ! Parse the notebook
            call parse_notebook(content, nb)

            ! Check parsing worked
            if (nb%num_cells == 0) then
                print *, '  FAIL: No cells parsed from arrays_loops.f'
                passed = .false.
                goto 99
            end if

            ! Execute the notebook
            call execute_notebook(nb, results, test_cache_dir)

            ! Check execution completed
            if (.not. allocated(results%cells)) then
                print *, '  FAIL: No execution results for arrays_loops.f'
                passed = .false.
                goto 99
            end if

            if (size(results%cells) /= nb%num_cells) then
                print *, '  FAIL: Result cells mismatch for arrays_loops.f'
                passed = .false.
                goto 99
            end if

            ! Generate markdown output
            call render_notebook_markdown(nb, results, markdown_output)

            ! Check markdown was generated
            if (len(markdown_output) == 0) then
                print *, '  FAIL: No markdown output generated'
                passed = .false.
                goto 99
            end if

            ! Check for expected content
            if (index(markdown_output, "# Arrays and Loops in Fortran") == 0) then
                print *, '  FAIL: Title not found in markdown'
                passed = .false.
                goto 99
            end if

            if (index(markdown_output, "Array of squares") == 0) then
                print *, '  FAIL: Array content not found in markdown'
                passed = .false.
                goto 99
            end if

            print *, '  PASS'

99          continue
            ! Cleanup
            call execute_command_line("rm -rf "//trim(test_cache_dir))
            call free_notebook(nb)
            call free_execution_results(results)
        end block

    end subroutine test_arrays_loops_example

    subroutine test_control_flow_example(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: content
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: markdown_output
        character(len=:), allocatable :: test_cache_dir

        print *, 'Test 3: control_flow.f example'

        ! Set up test cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            temp_mgr = temp_dir_manager("test_notebook_examples_control")
            test_cache_dir = temp_mgr%get_path()
            call execute_command_line("mkdir -p "//trim(test_cache_dir))

            ! Read the control_flow.f example
           call read_example_file("example/scientific/notebook/control_flow.f", content)

            if (len(content) == 0) then
                print *, '  FAIL: Could not read control_flow.f'
                passed = .false.
                return
            end if

            ! Parse the notebook
            call parse_notebook(content, nb)

            ! Check parsing worked
            if (nb%num_cells == 0) then
                print *, '  FAIL: No cells parsed from control_flow.f'
                passed = .false.
                goto 99
            end if

            ! Execute the notebook
            call execute_notebook(nb, results, test_cache_dir)

            ! Check execution completed
            if (.not. allocated(results%cells)) then
                print *, '  FAIL: No execution results for control_flow.f'
                passed = .false.
                goto 99
            end if

            if (size(results%cells) /= nb%num_cells) then
                print *, '  FAIL: Result cells mismatch for control_flow.f'
                passed = .false.
                goto 99
            end if

            ! Generate markdown output
            call render_notebook_markdown(nb, results, markdown_output)

            ! Check markdown was generated
            if (len(markdown_output) == 0) then
                print *, '  FAIL: No markdown output generated'
                passed = .false.
                goto 99
            end if

            ! Check for expected content
            if (index(markdown_output, "# Control Flow in Fortran") == 0) then
                print *, '  FAIL: Title not found in markdown'
                passed = .false.
                goto 99
            end if

            if (index(markdown_output, "Conditional Statements") == 0) then
                print *, '  FAIL: Conditional content not found in markdown'
                passed = .false.
                goto 99
            end if

            print *, '  PASS'

99          continue
            ! Cleanup
            call execute_command_line("rm -rf "//trim(test_cache_dir))
            call free_notebook(nb)
            call free_execution_results(results)
        end block

    end subroutine test_control_flow_example

    subroutine read_example_file(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content

        integer :: unit, iostat, file_size
        logical :: file_exists

        inquire (file=filepath, exist=file_exists, size=file_size)

        if (.not. file_exists) then
            content = ""
            return
        end if

        if (file_size <= 0) then
            content = ""
            return
        end if

        open (newunit=unit, file=filepath, status='old', &
              access='stream', form='unformatted', iostat=iostat)

        if (iostat /= 0) then
            content = ""
            return
        end if

        allocate (character(len=file_size) :: content)
        read (unit, iostat=iostat) content
        close (unit)

        if (iostat /= 0) then
            content = ""
        end if

    end subroutine read_example_file

end program test_notebook_examples
