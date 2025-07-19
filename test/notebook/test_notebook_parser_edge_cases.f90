program test_notebook_parser_edge_cases
    use notebook_parser
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_tests_passed

    print *, "=== Notebook Parser Edge Case Tests ==="
    print *

    all_tests_passed = .true.

    ! Test edge cases in notebook parsing
    if (.not. test_empty_notebook()) all_tests_passed = .false.
    if (.not. test_malformed_markers()) all_tests_passed = .false.
    if (.not. test_nested_markers()) all_tests_passed = .false.
    if (.not. test_large_content()) all_tests_passed = .false.
    if (.not. test_special_characters()) all_tests_passed = .false.
    if (.not. test_mixed_content()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All notebook parser edge case tests passed!"
        stop 0
    else
        print *, "Some notebook parser edge case tests failed!"
        stop 1
    end if

contains

    function test_empty_notebook() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        character(len=:), allocatable :: content

        print *, "Test 1: Empty notebook parsing"
        passed = .true.

        ! Test completely empty content
        content = ""
        call parse_notebook(content, notebook)

        ! Empty content might create 0 or 1 cell depending on implementation
        if (notebook%num_cells > 1) then
     print *, "  FAILED: Empty content should have 0 or 1 cell, got", notebook%num_cells
            passed = .false.
        end if

        call free_notebook(notebook)

        ! Test only whitespace
        content = "   "//new_line('a')//"  "//new_line('a')//"   "
        call parse_notebook(content, notebook)

        if (notebook%num_cells /= 1) then
            print *, "  FAILED: Whitespace should create 1 cell"
            passed = .false.
        end if

        call free_notebook(notebook)

        ! Test only comments
        content = "! This is just a comment"//new_line('a')//"! Another comment"
        call parse_notebook(content, notebook)

        if (notebook%num_cells /= 1) then
            print *, "  FAILED: Comments should create 1 cell"
            passed = .false.
        end if

        call free_notebook(notebook)

        if (passed) print *, "  PASS: Empty notebook parsing"

    end function test_empty_notebook

    function test_malformed_markers() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        character(len=:), allocatable :: content

        print *, "Test 2: Malformed cell markers"
        passed = .true.

        ! Test incomplete markers
        content = "! %"//new_line('a')// &
                  "print *, 'test'"//new_line('a')// &
                  "! %%"//new_line('a')// &
                  "x = 42"

        call parse_notebook(content, notebook)

        ! Should handle gracefully
        if (notebook%num_cells < 1) then
            print *, "  WARNING: Should create at least 1 cell"
        end if

        call free_notebook(notebook)

        ! Test markers with extra characters
        content = "! %%% [markdown] extra stuff"//new_line('a')// &
                  "# Header"//new_line('a')// &
                  "! %%%% code"//new_line('a')// &
                  "y = 100"

        call parse_notebook(content, notebook)

        call free_notebook(notebook)

        ! Test markers with no space
        content = "!%% [markdown]"//new_line('a')// &
                  "Some text"//new_line('a')// &
                  "!%%[code]"//new_line('a')// &
                  "z = 200"

        call parse_notebook(content, notebook)

        call free_notebook(notebook)

        if (passed) print *, "  PASS: Malformed markers"

    end function test_malformed_markers

    function test_nested_markers() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        character(len=:), allocatable :: content
        integer :: i

        print *, "Test 3: Nested and consecutive markers"
        passed = .true.

        ! Test consecutive markers
        content = "! %% [markdown]"//new_line('a')// &
                  "! %% [code]"//new_line('a')// &
                  "! %% [markdown]"//new_line('a')// &
                  "Final content"

        call parse_notebook(content, notebook)

        if (notebook%num_cells < 1) then
            print *, "  WARNING: Consecutive markers handling"
        end if

        ! Check last cell has content
        if (notebook%num_cells > 0) then
            if (.not. allocated(notebook%cells(notebook%num_cells)%content)) then
                print *, "  FAILED: Last cell should have content"
                passed = .false.
            end if
        end if

        call free_notebook(notebook)

        ! Test marker inside code
        content = "! %% [code]"//new_line('a')// &
                  "character(len=*) :: marker = '! %%'"//new_line('a')// &
                  "print *, marker"

        call parse_notebook(content, notebook)

        if (notebook%num_cells /= 1) then
            print *, "  WARNING: Marker in string should not split cell"
        end if

        call free_notebook(notebook)

        if (passed) print *, "  PASS: Nested markers"

    end function test_nested_markers

    function test_large_content() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        character(len=:), allocatable :: content
        character(len=100) :: line
        integer :: i

        print *, "Test 4: Large content handling"
        passed = .true.

        ! Create a large notebook with many cells
        content = ""
        do i = 1, 100
            if (mod(i, 2) == 0) then
                content = content//"! %% [markdown]"//new_line('a')
                write (line, '(a,i0)') "# Section ", i
                content = content//trim(line)//new_line('a')
            else
                content = content//"! %% [code]"//new_line('a')
                write (line, '(a,i0)') "x = ", i
                content = content//trim(line)//new_line('a')
            end if
        end do

        call parse_notebook(content, notebook)

        if (notebook%num_cells < 50) then
            print *, "  FAILED: Should have many cells, got", notebook%num_cells
            passed = .false.
        end if

        call free_notebook(notebook)

        ! Test very long lines
        content = "! %% [code]"//new_line('a')
        line = "x = '"
        do i = 1, 500
            line = line//"A"
        end do
        line = line//"'"
        content = content//line

        call parse_notebook(content, notebook)

        if (notebook%num_cells /= 1) then
            print *, "  FAILED: Long line test failed"
            passed = .false.
        end if

        call free_notebook(notebook)

        if (passed) print *, "  PASS: Large content"

    end function test_large_content

    function test_special_characters() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        character(len=:), allocatable :: content

        print *, "Test 5: Special characters and encodings"
        passed = .true.

        ! Test with various special characters
        content = "! %% [markdown]"//new_line('a')// &
                  "# Special: ñ, é, ü, ∑, π"//new_line('a')// &
                  "! %% [code]"//new_line('a')// &
                  "print *, 'τ = 2π'"//new_line('a')// &
                  "! Fortran comment with © symbol"

        call parse_notebook(content, notebook)

        if (notebook%num_cells < 2) then
            print *, "  WARNING: Special characters may affect parsing"
        end if

        call free_notebook(notebook)

        ! Test with tabs and mixed whitespace
        content = "! %%"//char(9)//"[markdown]"//new_line('a')// &
                  char(9)//"Tabbed content"//new_line('a')// &
                  "!  %%  "//char(9)//"  [code]"//new_line('a')// &
                  "    x = 42  "//char(9)//"! comment"

        call parse_notebook(content, notebook)

        call free_notebook(notebook)

        if (passed) print *, "  PASS: Special characters"

    end function test_special_characters

    function test_mixed_content() result(passed)
        logical :: passed
        type(notebook_t) :: notebook
        character(len=:), allocatable :: content
        character(len=256) :: test_file
        character(len=:), allocatable :: temp_dir
        integer :: unit, iostat
        type(temp_dir_manager) :: temp_mgr

        print *, "Test 6: Mixed content and file operations"
        passed = .true.

        ! Test mixed valid and invalid content
        content = "Some initial code"//new_line('a')// &
                  "x = 1"//new_line('a')// &
                  "! %% [unknown]"//new_line('a')// &
                  "Unknown cell type"//new_line('a')// &
                  "! %% "//new_line('a')// &
                  "No type specified"//new_line('a')// &
                  "! %% [MaRkDoWn]"//new_line('a')// &
                  "Case insensitive?"

        call parse_notebook(content, notebook)

        if (notebook%num_cells == 0) then
            print *, "  FAILED: Should parse some cells"
            passed = .false.
        end if

        call free_notebook(notebook)

        ! Create a temp directory for file tests
        call temp_mgr%create('parser_edge_test')
        temp_dir = temp_mgr%get_path()

        ! Test file parsing with non-existent file
        test_file = temp_mgr%get_file_path('definitely_does_not_exist_notebook.f90')
        call parse_notebook_file(test_file, notebook)

        ! Should handle gracefully (likely empty notebook)
        if (notebook%num_cells /= 0) then
            print *, "  WARNING: Non-existent file produced cells"
        end if

        call free_notebook(notebook)

        ! Test with actual file
        test_file = temp_mgr%get_file_path('test_notebook_parser_edge.f90')
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') "! %% [markdown]"
        write (unit, '(a)') "# Test"
        write (unit, '(a)') "! %%"
        write (unit, '(a)') "print *, 'Hello'"
        close (unit)

        call parse_notebook_file(test_file, notebook)

        if (notebook%num_cells < 2) then
            print *, "  WARNING: File parsing issue"
        end if

        if (allocated(notebook%source_file)) then
            if (notebook%source_file /= test_file) then
                print *, "  FAILED: Source file not set correctly"
                passed = .false.
            end if
        end if

        call free_notebook(notebook)

        if (passed) print *, "  PASS: Mixed content"

    end function test_mixed_content

end program test_notebook_parser_edge_cases
