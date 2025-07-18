module figure_capture
    implicit none
    private

    ! Figure capture configuration
    character(len=256), save :: temp_figure_dir = ""
    integer, save :: figure_counter = 0
    logical, save :: capture_enabled = .false.

    ! Public interface
    public :: init_figure_capture
    public :: finalize_figure_capture
    public :: get_figure_data
    public :: enable_figure_capture
    public :: disable_figure_capture
    public :: fortplot_show_interceptor
    public :: get_next_figure_path
    public :: increment_figure_counter
    public :: cleanup_figure_capture
    public :: get_figure_directory
    public :: convert_to_base64
    public :: read_base64_file
    public :: intercept_show
    public :: get_figure_counter

contains

    subroutine init_figure_capture(custom_dir)
        character(len=*), intent(in), optional :: custom_dir
        character(len=256) :: command

        ! Set figure directory (use custom if provided)
        if (present(custom_dir)) then
            temp_figure_dir = trim(custom_dir)
        else
            temp_figure_dir = './fortran_figures'
            call execute_command_line('mkdir -p '//trim(temp_figure_dir))
        end if

        ! Create temporary directory for figures (if not already created)
        if (.not. present(custom_dir)) then
            ! Directory already created by create_temp_dir
        else
            command = 'mkdir -p "'//trim(temp_figure_dir)//'"'
            call execute_command_line(command)
        end if

        ! Reset figure counter
        figure_counter = 0

        ! Enable capture
        capture_enabled = .true.

    end subroutine init_figure_capture

    subroutine finalize_figure_capture()
        character(len=256) :: command

        ! Clean up temporary directory
        command = 'rm -rf "'//trim(temp_figure_dir)//'"'
        call execute_command_line(command)

        ! Disable capture
        capture_enabled = .false.

    end subroutine finalize_figure_capture

    subroutine enable_figure_capture()
        capture_enabled = .true.
    end subroutine enable_figure_capture

    subroutine disable_figure_capture()
        capture_enabled = .false.
    end subroutine disable_figure_capture

    function get_next_figure_filename() result(filename)
        character(len=:), allocatable :: filename
        character(len=32) :: counter_str

        figure_counter = figure_counter + 1
        write (counter_str, '(i0)') figure_counter

        filename = trim(temp_figure_dir)//'/figure_'//trim(counter_str)//'.png'

    end function get_next_figure_filename

    subroutine fortplot_show_interceptor()
        character(len=:), allocatable :: figure_file

        if (.not. capture_enabled) then
            ! If capture is disabled, call original show
            call fortplot_show_original()
            return
        end if

        ! Generate unique filename for this figure
        figure_file = get_next_figure_filename()

        ! Save figure instead of showing
        call fortplot_savefig(figure_file)

        ! Store figure data for later retrieval
        call store_figure_data(figure_counter, figure_file)

    end subroutine fortplot_show_interceptor

    subroutine store_figure_data(fig_num, filename)
        integer, intent(in) :: fig_num
        character(len=*), intent(in) :: filename

        ! For now, just ensure the file exists
        ! In a full implementation, this would store metadata
        ! about which cell generated which figure

    end subroutine store_figure_data

    function get_figure_data(fig_num) result(base64_data)
        integer, intent(in) :: fig_num
        character(len=:), allocatable :: base64_data
        character(len=:), allocatable :: figure_file
        character(len=32) :: counter_str

        write (counter_str, '(i0)') fig_num
        figure_file = trim(temp_figure_dir)//'/figure_'//trim(counter_str)//'.png'

        ! Convert PNG to base64
        call png_to_base64(figure_file, base64_data)

    end function get_figure_data

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

        ! Use base64 command to encode PNG file
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

        temp_file = './fortran_fig_cmd.out'

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

    ! Placeholder procedures for fortplotlib interface
    ! These would be replaced by actual fortplotlib calls
    subroutine fortplot_show_original()
        ! Original show() implementation would go here
        ! For now, just print a message
        print *, "Figure would be displayed here"
    end subroutine fortplot_show_original

    subroutine fortplot_savefig(filename)
        character(len=*), intent(in) :: filename

        ! This would call the actual fortplotlib savefig
        ! For now, create a placeholder PNG file
        call create_placeholder_png(filename)

    end subroutine fortplot_savefig

    subroutine create_placeholder_png(filename)
        character(len=*), intent(in) :: filename
        character(len=512) :: command

        ! Create a simple placeholder PNG using convert (ImageMagick)
        ! In a real implementation, this would be handled by fortplotlib
        command = 'convert -size 800x600 xc:lightblue -pointsize 24 '// &
                 '-fill black -gravity center -annotate +0+0 "Figure Placeholder" "'// &
                  trim(filename)//'"'
        call execute_command_line(command)

        ! If ImageMagick is not available, create an empty file
        ! (base64 encoding will handle this gracefully)

    end subroutine create_placeholder_png

    ! Additional subroutines needed by tests
    subroutine get_next_figure_path(fig_path)
        character(len=*), intent(out) :: fig_path
        character(len=:), allocatable :: filename

        filename = get_next_figure_filename()
        fig_path = filename

    end subroutine get_next_figure_path

    subroutine increment_figure_counter()
        figure_counter = figure_counter + 1
    end subroutine increment_figure_counter

    subroutine cleanup_figure_capture()
        ! Alias for finalize_figure_capture for test compatibility
        call finalize_figure_capture()
    end subroutine cleanup_figure_capture

    subroutine get_figure_directory(fig_dir)
        character(len=*), intent(out) :: fig_dir
        fig_dir = trim(temp_figure_dir)
    end subroutine get_figure_directory

    subroutine convert_to_base64(input_file, output_file, success)
        character(len=*), intent(in) :: input_file, output_file
        logical, intent(out) :: success
        character(len=:), allocatable :: base64_data
        integer :: unit, iostat

        ! Convert file to base64 using existing function
        call png_to_base64(input_file, base64_data)

        success = (len(base64_data) > 0)

        if (success) then
            ! Write base64 data to output file
            open (newunit=unit, file=output_file, status='replace', iostat=iostat)
            if (iostat == 0) then
                write (unit, '(a)', iostat=iostat) base64_data
                close (unit)
                success = (iostat == 0)
            else
                success = .false.
            end if
        end if

    end subroutine convert_to_base64

    subroutine read_base64_file(filename, base64_data)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: base64_data
        integer :: unit, iostat, file_size
        logical :: file_exists

        inquire (file=filename, exist=file_exists, size=file_size)

        if (.not. file_exists .or. file_size <= 0) then
            base64_data = ""
            return
        end if

        open (newunit=unit, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) then
            base64_data = ""
            return
        end if

        allocate (character(len=file_size) :: base64_data)
        read (unit, '(a)', iostat=iostat) base64_data
        close (unit)

        if (iostat /= 0) then
            base64_data = ""
        end if

    end subroutine read_base64_file

    subroutine intercept_show()
        ! Alias for fortplot_show_interceptor for test compatibility
        call fortplot_show_interceptor()
    end subroutine intercept_show

    function get_figure_counter() result(counter)
        integer :: counter
        counter = figure_counter
    end function get_figure_counter

end module figure_capture
