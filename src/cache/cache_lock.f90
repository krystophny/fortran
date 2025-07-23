module cache_lock
    use iso_c_binding, only: c_int
    use iso_fortran_env, only: error_unit
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path, mkdir
    use fpm_environment, only: get_os_type, OS_WINDOWS
    use fpm_filesystem, only: join_path
    use system_utils, only: sys_remove_file, sys_move_file, sys_find_files, &
                            sys_create_symlink, sys_process_exists, sys_sleep, sys_file_exists
    use logger_utils, only: debug_print, print_info, print_warning, print_error
    use string_utils, only: int_to_char
    implicit none
    private
    public :: acquire_lock, release_lock, is_locked, cleanup_stale_locks

    integer, parameter :: MAX_WAIT_TIME = 60  ! seconds (increased for slower CI environments)
    integer, parameter :: STALE_LOCK_TIME = 300  ! 5 minutes

contains


    function acquire_lock(cache_dir, project_name, wait) result(success)
        character(len=*), intent(in) :: cache_dir, project_name
        logical, intent(in), optional :: wait
        logical :: success, locked
        character(len=512) :: lock_file
        logical :: should_wait
        integer :: wait_time, unit, iostat, pid
        character(len=32) :: pid_str, timestamp

        should_wait = .true.
        if (present(wait)) should_wait = wait

        lock_file = get_lock_file_path(cache_dir, project_name)
        wait_time = 0
        success = .false.

        ! Ensure cache directory exists
        call mkdir(cache_dir)

        do
            ! First check if lock already exists
            inquire (file=lock_file, exist=locked)
            ! DEBUG
            ! print *, 'DEBUG acquire_lock: lock exists=', locked, ' for ', trim(lock_file)
            if (.not. locked) then
                ! Try to create lock file atomically
                if (try_create_lock(lock_file)) then
                    success = .true.
                    exit
                else
                    ! Failed to create, but file doesn't exist - race condition?
                    success = .false.
                    if (.not. should_wait) then
                        exit
                    end if
                end if
            else
                ! Lock exists, check if it's stale
                if (is_lock_stale(lock_file)) then
                    call remove_lock(lock_file)
                    cycle
                end if

                ! Lock exists and is not stale
                success = .false.
                ! DEBUG
                ! print *, 'DEBUG: Lock exists and not stale, setting success=F, should_wait=', should_wait

                ! If not waiting, fail immediately
                if (.not. should_wait) then
                    exit
                end if
            end if

            ! Wait and retry
            call sys_sleep(1)
            wait_time = wait_time + 1

            if (wait_time >= MAX_WAIT_TIME) then
                call debug_print('Cache lock timeout after ' // int_to_char(MAX_WAIT_TIME) // ' seconds for ' // trim(project_name))
                exit
            end if
        end do

    end function acquire_lock

    subroutine release_lock(cache_dir, project_name)
        character(len=*), intent(in) :: cache_dir, project_name
        character(len=512) :: lock_file

        lock_file = get_lock_file_path(cache_dir, project_name)
        call remove_lock(lock_file)

    end subroutine release_lock

    function is_locked(cache_dir, project_name) result(locked)
        character(len=*), intent(in) :: cache_dir, project_name
        logical :: locked
        character(len=512) :: lock_file

        lock_file = get_lock_file_path(cache_dir, project_name)

        ! Use system utilities for cross-platform lock detection
        ! This uses a platform-agnostic file check instead of shell commands
        locked = sys_file_exists(lock_file)

        ! Only check for stale locks when explicitly requested, not during normal checks
        ! This prevents race conditions where a fresh lock is incorrectly considered stale

    end function is_locked

    ! Helper function to check lock file existence using system utilities
    ! This ensures consistency across the module and avoids creating 'nul' files
    function check_lock_file_exists(lock_file) result(exists)
        character(len=*), intent(in) :: lock_file
        logical :: exists
        
        call debug_print('check_lock_file_exists called for: ' // trim(lock_file))
        
        ! Use system utility for file existence check
        exists = sys_file_exists(lock_file)
        
        call debug_print('File exists: ' // merge('T', 'F', exists))
    end function check_lock_file_exists

    subroutine cleanup_stale_locks(cache_dir)
        character(len=*), intent(in) :: cache_dir
        character(len=512) :: lock_files(1000)
        integer :: num_files, i

        ! Find all lock files in cache directory
        call sys_find_files(cache_dir, '*.lock', lock_files, num_files, .false., 1)

        ! Check each lock file
        do i = 1, num_files
            if (is_lock_stale(trim(lock_files(i)))) then
                call remove_lock(trim(lock_files(i)))
            end if
        end do

    end subroutine cleanup_stale_locks

    ! Private helper functions

    function get_lock_file_path(cache_dir, project_name) result(lock_file)
        character(len=*), intent(in) :: cache_dir, project_name
        character(len=512) :: lock_file

        lock_file = join_path(trim(cache_dir), trim(project_name)//'.lock')

    end function get_lock_file_path

    function try_create_lock(lock_file) result(success)
        character(len=*), intent(in) :: lock_file
        logical :: success
        character(len=512) :: temp_file, command
        integer :: unit, iostat, pid
        character(len=32) :: pid_str, timestamp
        logical :: file_exists

        success = .false.

        ! Create temporary lock file with PID and timestamp
        call get_pid(pid)
        write (pid_str, '(i0)') pid
        call get_timestamp(timestamp)

        temp_file = trim(lock_file)//'.tmp.'//trim(pid_str)

        open (newunit=unit, file=temp_file, status='new', iostat=iostat)
        if (iostat == 0) then
            write (unit, '(a)') trim(pid_str)
            write (unit, '(a)') trim(timestamp)
            close (unit)

            ! Try to atomically move temp file to lock file
            ! First check if lock file already exists using system command (consistent with is_locked)
            file_exists = check_lock_file_exists(lock_file)
            if (file_exists) then
                ! Lock already exists, can't create
                ! Clean up temp file since lock already exists
                call debug_print('Deleting temp file: ' // trim(temp_file))
                call sys_remove_file(temp_file)
                success = .false.
            else
                ! Try atomic move/link operation
                call debug_print('Moving temp file to lock file')
                call debug_print('From: ' // trim(temp_file))
                call debug_print('To: ' // trim(lock_file))

                ! Use move for atomic operation on all platforms
                ! This avoids symlink issues and is atomic within same filesystem
                call sys_move_file(temp_file, lock_file, success)

                if (.not. success) then
                    call debug_print('Atomic lock creation failed')
                    ! Clean up temp file if it still exists
                    call sys_remove_file(temp_file)
                end if
            end if
        end if

    end function try_create_lock

    function is_lock_stale(lock_file) result(stale)
        character(len=*), intent(in) :: lock_file
        logical :: stale
        integer :: unit, iostat, lock_pid, current_time, lock_time
        character(len=32) :: pid_str, timestamp_str
        integer :: lock_year, lock_month, lock_day, lock_hour, lock_min, lock_sec

        stale = .false.
        call debug_print('is_lock_stale checking: ' // trim(lock_file))

        open (newunit=unit, file=lock_file, status='old', iostat=iostat)
        if (iostat == 0) then
            read (unit, '(a)', iostat=iostat) pid_str
            read (unit, '(a)', iostat=iostat) timestamp_str
            close (unit)

            if (iostat == 0) then
                ! Parse timestamp YYYYMMDDHHMMSS
                if (len_trim(timestamp_str) >= 14) then
                    read (timestamp_str(1:4), *, iostat=iostat) lock_year
                 if (iostat == 0) read (timestamp_str(5:6), *, iostat=iostat) lock_month
                   if (iostat == 0) read (timestamp_str(7:8), *, iostat=iostat) lock_day
                 if (iostat == 0) read (timestamp_str(9:10), *, iostat=iostat) lock_hour
                 if (iostat == 0) read (timestamp_str(11:12), *, iostat=iostat) lock_min
                 if (iostat == 0) read (timestamp_str(13:14), *, iostat=iostat) lock_sec

                    if (iostat == 0) then
                        ! Convert to seconds since 2000
               lock_time = ((lock_year - 2000)*365 + lock_month*30 + lock_day)*86400 + &
                                    lock_hour*3600 + lock_min*60 + lock_sec
                        current_time = get_current_timestamp()

                        ! Check if lock is older than stale threshold
                        if (current_time - lock_time > STALE_LOCK_TIME) then
                            call debug_print('Lock is stale due to age')
                            stale = .true.
                        else
                            ! Check if process is still running (only on Unix where we have real PIDs)
                            if (get_os_type() == OS_WINDOWS) then
                                ! On Windows we use random numbers as PIDs, so skip process check
                                call debug_print('Windows detected, skipping process check - lock is fresh')
                            else
                                read (pid_str, *, iostat=iostat) lock_pid
                                if (iostat == 0) then
                                    call debug_print('Checking if process ' // int_to_char(lock_pid) // ' is running')
                                    if (.not. is_process_running(lock_pid)) then
                                        call debug_print('Process not running, lock is stale')
                                        stale = .true.
                                    else
                                        call debug_print('Process still running, lock is fresh')
                                    end if
                                else
                                    call debug_print('Failed to parse PID, considering stale')
                                    stale = .true.
                                end if
                            end if
                        end if
                    end if
                else
                    ! Invalid timestamp format, consider it stale
                    call debug_print('Invalid timestamp format, considering stale: ' // trim(timestamp_str))
                    stale = .true.
                end if
            end if
        end if

    end function is_lock_stale

    subroutine remove_lock(lock_file)
        character(len=*), intent(in) :: lock_file
        logical :: success

        call debug_print('Removing lock file: ' // trim(lock_file))
        call sys_remove_file(lock_file, success)
        if (.not. success) then
            call debug_print('Failed to remove lock file: ' // trim(lock_file))
        else
            call debug_print('Lock file removed successfully')
        end if

    end subroutine remove_lock

    subroutine get_pid(pid)
        integer, intent(out) :: pid
        real :: r

        ! Interface declaration must be outside if block
        interface
            function getpid() bind(c, name='getpid')
                import :: c_int
                integer(c_int) :: getpid
            end function getpid
        end interface

        if (get_os_type() == OS_WINDOWS) then
            ! Simple fallback for Windows - just use a random number
            call random_number(r)
            pid = int(r*99999)
        else
            pid = getpid()
        end if

    end subroutine get_pid

    subroutine get_timestamp(timestamp)
        character(len=*), intent(out) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)
        ! Format: YYYYMMDDHHMMSS
        write (timestamp, '(i4.4,5i2.2)') values(1), values(2), values(3), &
            values(5), values(6), values(7)

    end subroutine get_timestamp

    function get_current_timestamp() result(timestamp)
        integer :: timestamp
        character(len=32) :: timestamp_str
        integer :: values(8)

        call date_and_time(values=values)
        ! Use unix timestamp approximation (seconds since 2000)
        timestamp = ((values(1) - 2000)*365 + values(2)*30 + values(3))*86400 + &
                    values(5)*3600 + values(6)*60 + values(7)

    end function get_current_timestamp

    function is_process_running(pid) result(running)
        integer, intent(in) :: pid
        logical :: running

        running = sys_process_exists(pid)

    end function is_process_running

end module cache_lock
