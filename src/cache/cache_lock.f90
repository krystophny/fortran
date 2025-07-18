module cache_lock
    use iso_c_binding, only: c_int
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path
    implicit none
    private
    public :: acquire_lock, release_lock, is_locked, cleanup_stale_locks

    integer, parameter :: MAX_WAIT_TIME = 30  ! seconds
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
            call sleep(1)
            wait_time = wait_time + 1

            if (wait_time >= MAX_WAIT_TIME) then
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
        inquire (file=lock_file, exist=locked)

        ! Only check for stale locks when explicitly requested, not during normal checks
        ! This prevents race conditions where a fresh lock is incorrectly considered stale

    end function is_locked

    subroutine cleanup_stale_locks(cache_dir)
        character(len=*), intent(in) :: cache_dir
        character(len=512) :: command, temp_locks_file
        integer :: unit, iostat
        character(len=512) :: lock_file

        ! Find all lock files in cache directory
        temp_locks_file = get_temp_file_path(create_temp_dir('fortran_locks'), &
                                             'fortran_locks.tmp')
        command = 'find "'//trim(cache_dir)//'" -name "*.lock" -type f > "'// &
                  trim(temp_locks_file)//'" 2>/dev/null'
        call execute_command_line(command)

        open (newunit=unit, file=trim(temp_locks_file), status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(a)', iostat=iostat) lock_file
                if (iostat /= 0) exit

                if (is_lock_stale(trim(lock_file))) then
                    call remove_lock(trim(lock_file))
                end if
            end do
            close (unit)
        end if

        ! Cleanup handled by temp_utils

    end subroutine cleanup_stale_locks

    ! Private helper functions

    function get_lock_file_path(cache_dir, project_name) result(lock_file)
        character(len=*), intent(in) :: cache_dir, project_name
        character(len=512) :: lock_file

        lock_file = trim(cache_dir)//'/'//trim(project_name)//'.lock'

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
            ! First check if lock file already exists
            inquire (file=lock_file, exist=file_exists)
            if (file_exists) then
                ! Lock already exists, can't create
                call execute_command_line('rm -f "'//trim(temp_file)//'"')
                success = .false.
            else
                ! Use ln to create hard link atomically, then remove temp
              command = 'ln "'//trim(temp_file)//'" "'//trim(lock_file)//'" 2>/dev/null'
                call execute_command_line(command, exitstat=iostat)

                if (iostat == 0) then
                    ! Double-check that we really created the lock
                    inquire (file=lock_file, exist=file_exists)
                    success = file_exists
                else
                    success = .false.
                end if
                ! Always clean up temp file
                call execute_command_line('rm -f "'//trim(temp_file)//'"')
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
                            stale = .true.
                        else
                            ! Check if process is still running
                            read (pid_str, *, iostat=iostat) lock_pid
                            if (iostat == 0) then
                                if (.not. is_process_running(lock_pid)) then
                                    stale = .true.
                                end if
                            end if
                        end if
                    end if
                else
                    ! Invalid timestamp format, consider it stale
                    stale = .true.
                end if
            end if
        end if

    end function is_lock_stale

    subroutine remove_lock(lock_file)
        character(len=*), intent(in) :: lock_file
        character(len=512) :: command

        command = 'rm -f "'//trim(lock_file)//'"'
        call execute_command_line(command)

    end subroutine remove_lock

    subroutine get_pid(pid)
        integer, intent(out) :: pid

        interface
            function getpid() bind(c, name='getpid')
                import :: c_int
                integer(c_int) :: getpid
            end function getpid
        end interface

        ! Use C getpid function for portability
        pid = getpid()

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
        character(len=128) :: command
        integer :: exitstat

        write (command, '(a,i0,a)') 'kill -0 ', pid, ' 2>/dev/null'
        call execute_command_line(command, exitstat=exitstat)

        running = (exitstat == 0)

    end function is_process_running

    subroutine sleep(seconds)
        integer, intent(in) :: seconds
        character(len=32) :: command

        write (command, '(a,i0)') 'sleep ', seconds
        call execute_command_line(command)

    end subroutine sleep

end module cache_lock
