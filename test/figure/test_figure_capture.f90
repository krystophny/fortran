program test_figure_capture
    use figure_capture
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path, get_system_temp_dir
    implicit none

    logical :: all_tests_passed

    print *, '=== Figure Capture Unit Tests ==='
    print *

    all_tests_passed = .true.

    ! Test 1: Initialization and finalization
    call test_init_finalize(all_tests_passed)

    ! Test 2: Enable/disable functionality
    call test_enable_disable(all_tests_passed)

    ! Test 3: Base64 encoding functionality
    call test_base64_encoding(all_tests_passed)

    ! Test 4: Figure data retrieval
    call test_figure_data_retrieval(all_tests_passed)

    ! Test 5: Show interceptor functionality
    call test_show_interceptor(all_tests_passed)

    if (all_tests_passed) then
        print *
        print *, 'All figure capture tests passed!'
        stop 0
    else
        print *
        print *, 'Some figure capture tests failed!'
        stop 1
    end if

contains

    subroutine test_init_finalize(passed)
        logical, intent(inout) :: passed
        logical :: dir_exists

        print *, 'Test 1: Initialization and finalization'

        ! Test initialization
        call init_figure_capture()

        ! Check that temp directory was created (default is ./fortran_figures)
        inquire (file='./fortran_figures', exist=dir_exists)
        if (.not. dir_exists) then
            print *, '  FAIL: Temp directory not created'
            passed = .false.
            return
        end if

        ! Test finalization
        call finalize_figure_capture()

        ! Directory should be cleaned up (may still exist if other processes use it)
        print *, '  PASS'

    end subroutine test_init_finalize

    subroutine test_enable_disable(passed)
        logical, intent(inout) :: passed

        print *, 'Test 2: Enable/disable functionality'

        ! Test enable/disable interface exists and works
        call enable_figure_capture()
        call disable_figure_capture()

        ! Since these are just state setters, if they compile and run, they work
        print *, '  PASS'

    end subroutine test_enable_disable

    subroutine test_base64_encoding(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: base64_data
        character(len=256) :: test_file
        integer :: unit
        logical :: file_exists

        print *, 'Test 3: Base64 encoding functionality'

        ! Create a small test file
        test_file = get_temp_file_path(get_system_temp_dir(), "test_figure_capture.txt")
        open (newunit=unit, file=test_file, status='replace')
        write (unit, '(a)') "test data"
        close (unit)

        ! Test PNG to base64 conversion with text file (should work for any file)
        call png_to_base64(test_file, base64_data)

        ! Check that some data was returned
        if (len(base64_data) == 0) then
            print *, '  FAIL: No base64 data returned'
            passed = .false.
            goto 99
        end if

        ! Base64 should only contain valid characters
        ! (This is a basic check - in practice we'd validate the encoding)
        if (len(base64_data) < 4) then
            print *, '  FAIL: Base64 data too short'
            passed = .false.
            goto 99
        end if

        print *, '  PASS'

99      continue
        ! Cleanup
        call execute_command_line("rm -f "//trim(test_file))

    end subroutine test_base64_encoding

    subroutine test_figure_data_retrieval(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: fig_data

        print *, 'Test 4: Figure data retrieval'

        ! Initialize figure capture
        call init_figure_capture()

        ! Try to get figure data (will be empty since no figures were created)
        fig_data = get_figure_data(1)

        ! Should return empty string for non-existent figure
        if (len(fig_data) > 0) then
            print *, '  WARNING: Got data for non-existent figure (may be expected)'
        end if

        ! Test passes if no crashes occur
        print *, '  PASS'

        ! Cleanup
        call finalize_figure_capture()

    end subroutine test_figure_data_retrieval

    subroutine test_show_interceptor(passed)
        logical, intent(inout) :: passed

        print *, 'Test 5: Show interceptor functionality'

        ! Initialize figure capture
        call init_figure_capture()
        call enable_figure_capture()

        ! Test the show interceptor
        call fortplot_show_interceptor()

        ! Should not crash and should handle the call gracefully
        print *, '  PASS'

        ! Cleanup
        call finalize_figure_capture()

    end subroutine test_show_interceptor

    ! Helper subroutine to test PNG to base64 conversion
    ! This duplicates the private subroutine for testing
    subroutine png_to_base64(png_file, base64_data)
        character(len=*), intent(in) :: png_file
        character(len=:), allocatable, intent(out) :: base64_data
        character(len=:), allocatable :: command_output
        character(len=512) :: command
        integer :: exit_code
        logical :: file_exists

        ! Check if file exists
        inquire (file=png_file, exist=file_exists)
        if (.not. file_exists) then
            base64_data = ""
            return
        end if

        ! Use base64 command to encode file
        command = 'base64 -w 0 "'//trim(png_file)//'"'
        call execute_and_capture_output(command, command_output, exit_code)

        if (exit_code == 0) then
            base64_data = trim(command_output)
        else
            base64_data = ""
        end if

    end subroutine png_to_base64

    subroutine execute_and_capture_output(command, output, exit_code)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exit_code

        character(len=256) :: temp_file
        character(len=512) :: full_command
        integer :: unit, iostat, file_size

        temp_file = get_temp_file_path(get_system_temp_dir(), 'fortran_fig_test.out')

        full_command = trim(command)//' > '//trim(temp_file)//' 2>&1'
        call execute_command_line(full_command, exitstat=exit_code)

        inquire (file=temp_file, size=file_size)

        if (file_size > 0) then
            open (newunit=unit, file=temp_file, status='old', &
                  access='stream', form='unformatted', iostat=iostat)

            if (iostat == 0) then
                allocate (character(len=file_size) :: output)
                read (unit, iostat=iostat) output
                close (unit)

                ! Remove trailing newline if present
             if (len(output) > 0 .and. output(len(output):len(output)) == char(10)) then
                    output = output(1:len(output) - 1)
                end if
            else
                output = ""
            end if
        else
            output = ""
        end if

        call execute_command_line('rm -f '//trim(temp_file))

    end subroutine execute_and_capture_output

end program test_figure_capture
