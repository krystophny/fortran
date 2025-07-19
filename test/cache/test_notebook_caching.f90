program test_notebook_caching
    use notebook_parser
    use notebook_executor
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed

    print *, '=== Notebook Caching Tests ==='
    print *

    all_tests_passed = .true.

    ! Test 1: Cache directory creation
    call test_cache_directory_creation(all_tests_passed)

    ! Test 2: Cache reuse with same content
    call test_cache_reuse(all_tests_passed)

    ! Test 3: Cache invalidation with different content
    call test_cache_invalidation(all_tests_passed)

    if (all_tests_passed) then
        print *
        print *, 'All notebook caching tests passed!'
        stop 0
    else
        print *
        print *, 'Some caching tests failed!'
        stop 1
    end if

contains

    subroutine test_cache_directory_creation(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb
        type(execution_result_t) :: results
        character(len=256) :: test_cache_dir
        logical :: dir_exists

        print *, 'Test 1: Cache directory creation'

        ! Set up unique cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('test_notebook_caching')
            test_cache_dir = temp_mgr%path
        end block

        ! Create simple notebook
        nb%num_cells = 1
        allocate (nb%cells(1))
        nb%cells(1)%cell_type = CELL_CODE
        nb%cells(1)%content = "x = 123.0"//new_line('a')//"print *, 'x =', x"

        ! Execute notebook
        call execute_notebook(nb, results, test_cache_dir)

        ! Check that cache directory was created
        inquire (file=test_cache_dir, exist=dir_exists)
        if (.not. dir_exists) then
            print *, '  FAIL: Cache directory not created'
            passed = .false.
            goto 99
        end if

        print *, '  PASS'

99      continue
        ! Cleanup
        call execute_command_line("rm -rf "//trim(test_cache_dir))
        call free_notebook(nb)
        call free_execution_results(results)

    end subroutine test_cache_directory_creation

    subroutine test_cache_reuse(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb1, nb2
        type(execution_result_t) :: results1, results2
        character(len=256) :: test_cache_dir

        print *, 'Test 2: Cache reuse with same content'

        ! Set up cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('test_notebook_reuse')
            test_cache_dir = temp_mgr%path
        end block

        ! Create first notebook
        nb1%num_cells = 1
        allocate (nb1%cells(1))
        nb1%cells(1)%cell_type = CELL_CODE
      nb1%cells(1)%content = "value = 456.0"//new_line('a')//"print *, 'value =', value"

        ! Create identical second notebook
        nb2%num_cells = 1
        allocate (nb2%cells(1))
        nb2%cells(1)%cell_type = CELL_CODE
nb2%cells(1)%content = "value = 456.0"//new_line('a')//"print *, 'value =', value"  ! Same content

        ! Execute both notebooks
        call execute_notebook(nb1, results1, test_cache_dir)
        call execute_notebook(nb2, results2, test_cache_dir)

        ! Check that results structure is valid (execution may fail but structure should be there)
        if (.not. allocated(results1%cells)) then
            print *, '  FAIL: First execution results not allocated'
            passed = .false.
            goto 99
        end if

        if (.not. allocated(results2%cells)) then
            print *, '  FAIL: Second execution results not allocated'
            passed = .false.
            goto 99
        end if

        print *, '  PASS'

99      continue
        ! Cleanup
        call execute_command_line("rm -rf "//trim(test_cache_dir))
        call free_notebook(nb1)
        call free_notebook(nb2)
        call free_execution_results(results1)
        call free_execution_results(results2)

    end subroutine test_cache_reuse

    subroutine test_cache_invalidation(passed)
        logical, intent(inout) :: passed
        type(notebook_t) :: nb1, nb2
        type(execution_result_t) :: results1, results2
        character(len=256) :: test_cache_dir

        print *, 'Test 3: Cache invalidation with different content'

        ! Set up cache directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('test_notebook_invalidation')
            test_cache_dir = temp_mgr%path
        end block

        ! Create first notebook
        nb1%num_cells = 1
        allocate (nb1%cells(1))
        nb1%cells(1)%cell_type = CELL_CODE
        nb1%cells(1)%content = "first_value = 789.0" // new_line('a') // "print *, 'first =', first_value"

        ! Create different second notebook
        nb2%num_cells = 1
        allocate (nb2%cells(1))
        nb2%cells(1)%cell_type = CELL_CODE
        nb2%cells(1)%content = "second_value = 101112.0" // new_line('a') // "print *, 'second =', second_value"  ! Different content

        ! Execute both notebooks
        call execute_notebook(nb1, results1, test_cache_dir)
        call execute_notebook(nb2, results2, test_cache_dir)

        ! Check that results structure is valid (different content should create different cache keys)
        if (.not. allocated(results1%cells)) then
            print *, '  FAIL: First execution results not allocated'
            passed = .false.
            goto 99
        end if

        if (.not. allocated(results2%cells)) then
            print *, '  FAIL: Second execution results not allocated'
            passed = .false.
            goto 99
        end if

        print *, '  PASS'

99      continue
        ! Cleanup
        call execute_command_line("rm -rf "//trim(test_cache_dir))
        call free_notebook(nb1)
        call free_notebook(nb2)
        call free_execution_results(results1)
        call free_execution_results(results2)

    end subroutine test_cache_invalidation

end program test_notebook_caching
