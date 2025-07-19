module temp_utils
    implicit none
    private
    public :: create_temp_dir, cleanup_temp_dir, get_temp_file_path, temp_dir_manager, &
              get_system_temp_dir

    ! Type for managing temporary directories with automatic cleanup
    type :: temp_dir_manager
        character(len=:), allocatable :: path
        logical :: cleanup_on_destroy = .true.
    contains
        procedure :: create => temp_dir_create
        procedure :: get_path => temp_dir_get_path
        procedure :: get_file_path => temp_dir_get_file_path
        procedure :: cleanup => temp_dir_cleanup
        final :: temp_dir_destroy
    end type temp_dir_manager

contains

    function create_temp_dir(prefix) result(temp_dir)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: temp_dir
        character(len=32) :: random_suffix
        character(len=256) :: base_temp_dir
        integer :: ios

        ! Get system temp directory (cross-platform)
        call get_environment_variable('TMPDIR', base_temp_dir, status=ios)
        if (ios /= 0) then
            call get_environment_variable('TMP', base_temp_dir, status=ios)
            if (ios /= 0) then
                call get_environment_variable('TEMP', base_temp_dir, status=ios)
                if (ios /= 0) then
                    base_temp_dir = get_system_temp_dir()
                end if
            end if
        end if

        ! Generate random suffix using system time and process ID
        call generate_random_suffix(random_suffix)

        ! Create unique temp directory path (cross-platform path separator)
#ifdef _WIN32
        temp_dir = trim(base_temp_dir)//'\'//trim(prefix)//'_'//trim(random_suffix)

        ! Create the directory (Windows)
        call execute_command_line('mkdir "'//temp_dir//'" 2>nul', exitstat=ios)
#else
        temp_dir = trim(base_temp_dir)//'/'//trim(prefix)//'_'//trim(random_suffix)

        ! Create the directory (Unix)
        call execute_command_line('mkdir -p "'//temp_dir//'"', exitstat=ios)
#endif
        if (ios /= 0) then
            error stop 'Failed to create temporary directory: '//temp_dir
        end if

    end function create_temp_dir

    subroutine cleanup_temp_dir(temp_dir)
        character(len=*), intent(in) :: temp_dir
        integer :: ios

        if (len_trim(temp_dir) > 0) then
#ifdef _WIN32
     call execute_command_line('rmdir /s /q "'//trim(temp_dir)//'" 2>nul', exitstat=ios)
#else
            call execute_command_line('rm -rf "'//trim(temp_dir)//'"', exitstat=ios)
#endif
            ! Don't error on cleanup failure - just warn
            if (ios /= 0) then
             print *, 'Warning: Failed to cleanup temporary directory: '//trim(temp_dir)
            end if
        end if

    end subroutine cleanup_temp_dir

    function get_temp_file_path(temp_dir, filename) result(file_path)
        character(len=*), intent(in) :: temp_dir, filename
        character(len=:), allocatable :: file_path

#ifdef _WIN32
        file_path = trim(temp_dir)//'\'//trim(filename)
#else
        file_path = trim(temp_dir)//'/'//trim(filename)
#endif

    end function get_temp_file_path

    subroutine generate_random_suffix(suffix)
        character(len=*), intent(out) :: suffix
        integer :: time_vals(8)
        integer :: pid_estimate
        integer :: random_num

        ! Get current time
        call date_and_time(values=time_vals)

        ! Create pseudo-random number from time and a simple PID estimate
        pid_estimate = time_vals(8)*1000 + time_vals(7)*100 + time_vals(6)
        random_num = time_vals(6)*1000000 + time_vals(7)*1000 + time_vals(8)
        random_num = random_num + pid_estimate

        ! Convert to hex string for shorter suffix
        write (suffix, '(z0)') abs(random_num)

    end subroutine generate_random_suffix

    ! Type-bound procedures for temp_dir_manager

    subroutine temp_dir_create(this, prefix)
        class(temp_dir_manager), intent(inout) :: this
        character(len=*), intent(in) :: prefix

        this%path = create_temp_dir(prefix)

    end subroutine temp_dir_create

    function temp_dir_get_path(this) result(path)
        class(temp_dir_manager), intent(in) :: this
        character(len=:), allocatable :: path

        if (allocated(this%path)) then
            path = this%path
        else
            error stop 'temp_dir_manager: directory not created'
        end if

    end function temp_dir_get_path

    function temp_dir_get_file_path(this, filename) result(file_path)
        class(temp_dir_manager), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: file_path

        if (allocated(this%path)) then
            file_path = get_temp_file_path(this%path, filename)
        else
            error stop 'temp_dir_manager: directory not created'
        end if

    end function temp_dir_get_file_path

    subroutine temp_dir_cleanup(this)
        class(temp_dir_manager), intent(inout) :: this

        if (allocated(this%path)) then
            call cleanup_temp_dir(this%path)
            deallocate (this%path)
        end if

    end subroutine temp_dir_cleanup

    subroutine temp_dir_destroy(this)
        type(temp_dir_manager), intent(inout) :: this

        if (this%cleanup_on_destroy) then
            call this%cleanup()
        end if

    end subroutine temp_dir_destroy

    function get_system_temp_dir() result(temp_dir)
        character(len=:), allocatable :: temp_dir

#ifdef _WIN32
        temp_dir = 'C:\Windows\Temp'
#else
        temp_dir = '/tmp'
#endif

    end function get_system_temp_dir

end module temp_utils
