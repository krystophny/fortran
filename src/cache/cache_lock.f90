module cache_lock
    use iso_c_binding, only: c_int
    use iso_fortran_env, only: error_unit
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path, mkdir
    use fpm_environment, only: get_os_type, OS_WINDOWS, get_env
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

        ! Ensure cache directory exists (only create if it doesn't exist)
        block
            logical :: dir_exists
            inquire(file=trim(cache_dir), exist=dir_exists)
            if (.not. dir_exists) then
                call mkdir(cache_dir)
            end if
        end block

        do
            ! First check if lock already exists
            inquire (file=lock_file, exist=locked)
            ! DEBUG - Enable on debug branch
            if (len_trim(get_env('FORTRAN_DEBUG_CACHE', '')) > 0) then
                print *, 'DEBUG acquire_lock: lock exists=', locked, ' for ', trim(lock_file)
            end if
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
                ! DEBUG - Enable on debug branch
                if (len_trim(get_env('FORTRAN_DEBUG_CACHE', '')) > 0) then
                    print *, 'DEBUG: Lock exists and not stale, setting success=F, should_wait=', should_wait
                end if

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
        integer :: unit, iostat, pid, exitstat
        character(len=32) :: pid_str, timestamp
        logical :: file_exists, temp_exists

        success = .false.
        
        ! DEBUG - Enable on debug branch
        if (len_trim(get_env('FORTRAN_DEBUG_CACHE', '')) > 0) then
            print *, 'DEBUG try_create_lock: attempting to create ', trim(lock_file)
        end if

        ! Create temporary lock file with PID and timestamp
        call get_pid(pid)
        write (pid_str, '(i0)') pid
        call get_timestamp(timestamp)

        temp_file = trim(lock_file)//'.tmp.'//trim(pid_str)

        ! For Windows, we'll use a different approach - try to create the lock file directly
        if (get_os_type() == OS_WINDOWS) then
            ! Check if we're in CI environment
            block
                character(len=256) :: ci_env
                logical :: is_ci
                
                call get_environment_variable('CI', ci_env)
                is_ci = (len_trim(ci_env) > 0)
                
                if (is_ci) then
                    ! In CI, use a simpler approach with retries
                    call debug_print('CI environment detected, using simple locking')
                    
                    ! DEBUG - Enable on debug branch
                    if (len_trim(get_env('FORTRAN_DEBUG_CACHE', '')) > 0) then
                        print *, 'DEBUG: Windows CI path in try_create_lock'
                    end if
                    
                    ! Check if lock exists
                    file_exists = check_lock_file_exists(lock_file)
                    if (.not. file_exists) then
                        ! Try to create it using echo
                        command = 'echo '//trim(pid_str)//' > "'//trim(escape_quotes(lock_file))//'" 2>nul'
                        
                        ! DEBUG - Enable on debug branch
                        if (len_trim(get_env('FORTRAN_DEBUG_CACHE', '')) > 0) then
                            print *, 'DEBUG: Executing command: ', trim(command)
                        end if
                        
                        call execute_command_line(trim(command), exitstat=exitstat)
                        
                        if (exitstat == 0) then
                            ! Append timestamp
                            command = 'echo '//trim(timestamp)//' >> "'//trim(escape_quotes(lock_file))//'" 2>nul'
                            call execute_command_line(trim(command), exitstat=exitstat)
                            success = (exitstat == 0)
                        else
                            success = .false.
                        end if
                    else
                        success = .false.
                        call debug_print('Lock already exists in CI')
                    end if
                else
                    ! Non-CI Windows: use Fortran's exclusive file opening
                    call debug_print('Attempting exclusive file creation on Windows')
                    
                    ! Try to open the lock file with 'new' status (fails if exists)
                    open(newunit=unit, file=lock_file, status='new', iostat=iostat)
                    if (iostat == 0) then
                        ! We successfully created the lock file
                        write (unit, '(a)') trim(pid_str)
                        write (unit, '(a)') trim(timestamp)
                        close (unit)
                        success = .true.
                        call debug_print('Lock created successfully with exclusive open')
                    else
                        ! File already exists or other error
                        success = .false.
                        call debug_print('Lock file already exists or cannot be created')
                    end if
                end if
            end block
        else
            ! Unix approach - create temp file then use atomic link
            open (newunit=unit, file=temp_file, status='new', iostat=iostat)
            if (iostat == 0) then
                write (unit, '(a)') trim(pid_str)
                write (unit, '(a)') trim(timestamp)
                close (unit)

                ! Use link for true atomicity
                call debug_print('Attempting atomic link on Unix')
                command = 'ln "'//trim(escape_quotes(temp_file))//'" "'// &
                          trim(escape_quotes(lock_file))//'" 2>/dev/null'
                call execute_command_line(trim(command), exitstat=exitstat)
                success = (exitstat == 0)
                
                if (.not. success) then
                    ! If link failed, check if it's because lock already exists
                    file_exists = check_lock_file_exists(lock_file)
                    if (.not. file_exists) then
                        ! Lock doesn't exist but link failed - try mv as fallback
                        call debug_print('Link failed, trying mv')
                        call sys_move_file(temp_file, lock_file, success)
                    end if
                end if

                ! Always clean up temp file
                temp_exists = check_lock_file_exists(temp_file)
                if (temp_exists) then
                    call sys_remove_file(temp_file)
                end if
            end if
        end if

        if (success) then
            call debug_print('Lock created successfully')
        else
            call debug_print('Failed to create lock')
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
                            ! Check if process is still running
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
        
        ! DEBUG - Enable on debug branch
        if (len_trim(get_env('FORTRAN_DEBUG_CACHE', '')) > 0) then
            print *, 'DEBUG remove_lock: removing ', trim(lock_file)
        end if
        
        call sys_remove_file(lock_file, success)
        if (.not. success) then
            call debug_print('Failed to remove lock file: ' // trim(lock_file))
            ! DEBUG - Enable on debug branch
            if (len_trim(get_env('FORTRAN_DEBUG_CACHE', '')) > 0) then
                print *, 'DEBUG: Failed to remove lock file!'
            end if
        else
            call debug_print('Lock file removed successfully')
            ! DEBUG - Enable on debug branch
            if (len_trim(get_env('FORTRAN_DEBUG_CACHE', '')) > 0) then
                print *, 'DEBUG: Lock file removed successfully'
            end if
        end if

    end subroutine remove_lock

    function get_filename_only(full_path) result(filename)
        character(len=*), intent(in) :: full_path
        character(len=:), allocatable :: filename
        integer :: last_sep, i
        
        ! Find last path separator
        last_sep = 0
        do i = len_trim(full_path), 1, -1
            if (full_path(i:i) == '/' .or. full_path(i:i) == '\') then
                last_sep = i
                exit
            end if
        end do
        
        if (last_sep > 0) then
            filename = full_path(last_sep+1:)
        else
            filename = full_path
        end if
    end function get_filename_only

    function escape_quotes(str) result(escaped)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: escaped
        integer :: i, n, len_str
        
        len_str = len_trim(str)
        ! Count characters needed
        n = 0
        do i = 1, len_str
            if (str(i:i) == '"') then
                n = n + 2
            else
                n = n + 1
            end if
        end do
        
        ! Allocate and build escaped string
        allocate(character(len=n) :: escaped)
        n = 0
        do i = 1, len_str
            if (str(i:i) == '"') then
                n = n + 1
                escaped(n:n) = '\'
                n = n + 1
                escaped(n:n) = '"'
            else
                n = n + 1
                escaped(n:n) = str(i:i)
            end if
        end do
    end function escape_quotes

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
            ! On Windows, try to get real PID
            ! Note: getpid() works on Windows with MinGW/MSYS2
            pid = getpid()
            ! If it returns -1, fall back to random number
            if (pid < 0) then
                call random_number(r)
                pid = int(r*99999)
            end if
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
