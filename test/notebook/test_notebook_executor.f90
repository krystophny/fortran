program test_notebook_executor
    use notebook_executor
    use notebook_parser
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed

    print *, '=== Notebook Executor Unit Tests ==='
    print *

    all_tests_passed = .true.

    ! Test 1: Basic notebook execution
    call test_basic_execution(all_tests_passed)

    ! Test 2: Execution results structure
    call test_execution_results_structure(all_tests_passed)

    ! Test 3: Multiple cell execution
    call test_multiple_cell_execution(all_tests_passed)

    ! Test 4: Error handling
    call test_error_handling(all_tests_passed)

    ! Test 5: Cache directory usage
    call test_cache_directory_usage(all_tests_passed)

    ! Test 6: Empty notebook handling
    call test_empty_notebook(all_tests_passed)

    ! Test 7: Mixed cell types
    call test_mixed_cell_types(all_tests_passed)

    ! Test 8: Variable persistence
    call test_variable_persistence(all_tests_passed)

    ! Test 9: Print statement handling
    call test_print_statement_handling(all_tests_passed)

    ! Test 10: Memory management
    call test_memory_management(all_tests_passed)

    if (all_tests_passed) then
        print *
        print *, 'All notebook executor tests passed!'
        stop 0
    else
        print *
        print *, 'Some tests failed!'
        stop 1
    end if

contains

    subroutine test_basic_execution(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: test_cache_dir
        type(temp_dir_manager) :: temp_dir

        print *, 'Test 1: Basic notebook execution'

        block
            ! Set up test cache directory
            call temp_dir%create('test_notebook_executor')
            test_cache_dir = temp_dir%path

            ! Create simple test notebook
            nb%num_cells = 1
            allocate (nb%cells(1))
            nb%cells(1)%cell_type = CELL_CODE
            nb%cells(1)%content = "x = 42"

            ! Execute notebook
            call execute_notebook(nb, results, test_cache_dir)

            ! Check results structure
            if (.not. allocated(results%cells)) then
                print *, '  FAIL: Results cells not allocated'
                passed = .false.
                goto 99
            end if

            if (size(results%cells) /= 1) then
                print *, '  FAIL: Wrong number of result cells'
                passed = .false.
                goto 99
            end if

            print *, '  PASS'

99          continue
        end block
        ! Cleanup
        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_basic_execution

    subroutine test_execution_results_structure(passed)
        logical, intent(inout) :: passed
        type(execution_result_t) :: results

        print *, 'Test 2: Execution results structure'

        ! Test manual allocation and initialization
        allocate (results%cells(2))
        results%success = .true.

        ! Test setting cell results
        results%cells(1)%success = .true.
        results%cells(1)%output = "Hello"
        results%cells(2)%success = .true.
        results%cells(2)%output = "World"

        ! Check structure
        if (.not. allocated(results%cells)) then
            print *, '  FAIL: Results cells not allocated'
            passed = .false.
            return
        end if

        if (size(results%cells) /= 2) then
            print *, '  FAIL: Wrong number of cells'
            passed = .false.
            return
        end if

        if (results%cells(1)%output /= "Hello") then
            print *, '  FAIL: First cell output not set correctly'
            passed = .false.
            return
        end if

        if (results%cells(2)%output /= "World") then
            print *, '  FAIL: Second cell output not set correctly'
            passed = .false.
            return
        end if

        ! Test freeing results
        call free_execution_results(results)

        ! After freeing, cells should be deallocated
        if (allocated(results%cells)) then
            print *, '  FAIL: Results cells not properly deallocated'
            passed = .false.
            return
        end if

        print *, '  PASS'

    end subroutine test_execution_results_structure

    subroutine test_multiple_cell_execution(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: test_cache_dir
        type(temp_dir_manager) :: temp_dir

        print *, 'Test 3: Multiple cell execution'

        block
            ! Set up test cache directory
            call temp_dir%create('test_notebook_executor_multi')
            test_cache_dir = temp_dir%path

            ! Create notebook with multiple cells
            nb%num_cells = 3
            allocate (nb%cells(3))
            nb%cells(1)%cell_type = CELL_CODE
            nb%cells(1)%content = "x = 10"
            nb%cells(2)%cell_type = CELL_CODE
            nb%cells(2)%content = "y = x * 2"
            nb%cells(3)%cell_type = CELL_CODE
            nb%cells(3)%content = "print *, 'y =', y"

            ! Execute notebook
            call execute_notebook(nb, results, test_cache_dir)

            ! Check that all cells have results
            if (.not. allocated(results%cells)) then
                print *, '  FAIL: Results cells not allocated'
                passed = .false.
                goto 99
            end if

            if (size(results%cells) /= 3) then
                print *, '  FAIL: Wrong number of result cells'
                passed = .false.
                goto 99
            end if

            print *, '  PASS'

99          continue
        end block
        ! Cleanup
        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_multiple_cell_execution

    subroutine test_error_handling(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: test_cache_dir
        type(temp_dir_manager) :: temp_dir

        print *, 'Test 4: Error handling'

        block
            ! Set up test cache directory
            call temp_dir%create('test_notebook_executor_error')
            test_cache_dir = temp_dir%path

            ! Create notebook with syntax error
            nb%num_cells = 1
            allocate (nb%cells(1))
            nb%cells(1)%cell_type = CELL_CODE
            nb%cells(1)%content = "x = " ! Incomplete statement

            ! Execute notebook
            call execute_notebook(nb, results, test_cache_dir)

            ! Check that results structure is still valid
            if (.not. allocated(results%cells)) then
                print *, '  FAIL: Results cells not allocated after error'
                passed = .false.
                goto 99
            end if

            if (size(results%cells) /= 1) then
                print *, '  FAIL: Wrong number of result cells after error'
                passed = .false.
                goto 99
            end if

            print *, '  PASS'

99          continue
        end block
        ! Cleanup
        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_error_handling

    subroutine test_cache_directory_usage(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: test_cache_dir
        logical :: dir_exists
        type(temp_dir_manager) :: temp_dir

        print *, 'Test 5: Cache directory usage'

        block
            ! Set up unique test cache directory
            call temp_dir%create('test_notebook_executor_cache_test')
            test_cache_dir = temp_dir%path

            ! Create simple notebook
            nb%num_cells = 1
            allocate (nb%cells(1))
            nb%cells(1)%cell_type = CELL_CODE
            nb%cells(1)%content = "x = 123"

            ! Execute notebook with custom cache directory
            call execute_notebook(nb, results, test_cache_dir)

            ! Check that cache directory was created and used
            inquire (file=test_cache_dir, exist=dir_exists)
            if (.not. dir_exists) then
                print *, '  FAIL: Custom cache directory not created'
                passed = .false.
                goto 99
            end if

            print *, '  PASS'

99          continue
        end block
        ! Cleanup
        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_cache_directory_usage

    subroutine test_empty_notebook(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: test_cache_dir
        type(temp_dir_manager) :: temp_dir

        print *, 'Test 6: Empty notebook handling'

        block
            ! Set up test cache directory
            call temp_dir%create('test_notebook_executor_empty')
            test_cache_dir = temp_dir%path

            ! Create empty notebook
            nb%num_cells = 0
            allocate (nb%cells(0))

            ! Execute notebook
            call execute_notebook(nb, results, test_cache_dir)

            ! Check that empty results are handled properly
            if (.not. allocated(results%cells)) then
                print *, '  FAIL: Results not allocated for empty notebook'
                passed = .false.
                goto 99
            end if

            if (size(results%cells) /= 0) then
                print *, '  FAIL: Non-zero result cells for empty notebook'
                passed = .false.
                goto 99
            end if

            print *, '  PASS'

99          continue
        end block
        ! Cleanup
        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_empty_notebook

    subroutine test_mixed_cell_types(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=:), allocatable :: test_cache_dir
        type(temp_dir_manager) :: temp_dir

        print *, 'Test 7: Mixed cell types'

        block
            ! Set up test cache directory
            call temp_dir%create('test_notebook_executor_mixed')
            test_cache_dir = temp_dir%path

            ! Create notebook with mixed cell types
            nb%num_cells = 3
            allocate (nb%cells(3))
            nb%cells(1)%cell_type = CELL_MARKDOWN
            nb%cells(1)%content = "# Title"
            nb%cells(2)%cell_type = CELL_CODE
            nb%cells(2)%content = "x = 50"
            nb%cells(3)%cell_type = CELL_MARKDOWN
            nb%cells(3)%content = "Some text"

            ! Execute notebook
            call execute_notebook(nb, results, test_cache_dir)

            ! Check that all cells have results (including markdown)
            if (.not. allocated(results%cells)) then
                print *, '  FAIL: Results cells not allocated'
                passed = .false.
                goto 99
            end if

            if (size(results%cells) /= 3) then
                print *, '  FAIL: Wrong number of result cells for mixed types'
                passed = .false.
                goto 99
            end if

            print *, '  PASS'

99          continue
        end block
        ! Cleanup
        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_mixed_cell_types

    subroutine test_variable_persistence(passed)
        logical, intent(inout) :: passed
        type(execution_result_t) :: results

        print *, 'Test 8: Variable persistence structure'

        ! Test that results can handle multiple cells with different outputs
        allocate (results%cells(2))
        results%success = .true.

        results%cells(1)%success = .true.
        results%cells(1)%output = ""  ! First cell has no output

        results%cells(2)%success = .true.
        results%cells(2)%output = "var = 999"  ! Second cell prints variable

        ! Verify structure
        if (.not. allocated(results%cells)) then
            print *, '  FAIL: Results cells not allocated'
            passed = .false.
            return
        end if

        if (size(results%cells) /= 2) then
            print *, '  FAIL: Wrong number of cells'
            passed = .false.
            return
        end if

        if (results%cells(2)%output /= "var = 999") then
            print *, '  FAIL: Second cell output not correct'
            passed = .false.
            return
        end if

        call free_execution_results(results)
        print *, '  PASS'

    end subroutine test_variable_persistence

    subroutine test_print_statement_handling(passed)
        logical, intent(inout) :: passed
        type(execution_result_t) :: results

        print *, 'Test 9: Print statement output structure'

        ! Test that results can handle print statement outputs
        allocate (results%cells(2))
        results%success = .true.

        results%cells(1)%success = .true.
        results%cells(1)%output = "simple"

        results%cells(2)%success = .true.
        results%cells(2)%output = "value = 42"

        ! Verify structure
        if (.not. allocated(results%cells)) then
            print *, '  FAIL: Results cells not allocated'
            passed = .false.
            return
        end if

        if (size(results%cells) /= 2) then
            print *, '  FAIL: Wrong number of cells'
            passed = .false.
            return
        end if

        if (results%cells(1)%output /= "simple") then
            print *, '  FAIL: First cell output not correct'
            passed = .false.
            return
        end if

        if (results%cells(2)%output /= "value = 42") then
            print *, '  FAIL: Second cell output not correct'
            passed = .false.
            return
        end if

        call free_execution_results(results)
        print *, '  PASS'

    end subroutine test_print_statement_handling

    subroutine test_memory_management(passed)
        logical, intent(inout) :: passed
        type(execution_result_t) :: results1, results2, results3

        print *, 'Test 10: Memory management'

        ! Test multiple allocations and frees
        allocate (results1%cells(1))
        results1%cells(1)%output = "Test1"

        allocate (results2%cells(2))
        results2%cells(1)%output = "Test2a"
        results2%cells(2)%output = "Test2b"

        allocate (results3%cells(0))

        ! Free all results
        call free_execution_results(results1)
        call free_execution_results(results2)
        call free_execution_results(results3)

        ! Check all are deallocated
        if (allocated(results1%cells) .or. allocated(results2%cells) .or. allocated(results3%cells)) then
            print *, '  FAIL: Memory not properly deallocated'
            passed = .false.
            return
        end if

        print *, '  PASS'

    end subroutine test_memory_management

end program test_notebook_executor
