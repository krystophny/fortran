program test_cache_lock
    use cache_lock
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    character(len=256) :: temp_cache_dir
    logical :: success, locked
    integer :: i, unit

    ! Create temporary directory for testing
    temp_cache_dir = create_temp_dir('fortran_cache_lock_test')
    call system('mkdir -p '//trim(temp_cache_dir))

    print '(a)', 'Testing cache lock functionality...'

    ! Test 1: Basic lock acquisition and release
    print '(a)', ''
    print '(a)', 'Test 1: Basic lock acquisition and release'
    print '(a,a)', 'Using cache dir: ', trim(temp_cache_dir)
    success = acquire_lock(trim(temp_cache_dir), 'test_project', .false.)
    if (success) then
        print '(a)', '  ✓ Lock acquired successfully'
        ! List directory contents right after acquiring lock
        call system('ls -la '//trim(temp_cache_dir)//'/*.lock 2>&1 | head -10')
    else
        print '(a)', '  ✗ Failed to acquire lock'
        stop 1
    end if

    ! Check if locked
    locked = is_locked(trim(temp_cache_dir), 'test_project')
    if (locked) then
        print '(a)', '  ✓ Lock is detected as locked'
    else
        print '(a)', '  ✗ Lock not detected'
        stop 1
    end if

    ! Release lock
    call release_lock(trim(temp_cache_dir), 'test_project')
    locked = is_locked(trim(temp_cache_dir), 'test_project')
    if (.not. locked) then
        print '(a)', '  ✓ Lock released successfully'
    else
        print '(a)', '  ✗ Failed to release lock'
        stop 1
    end if

    ! Test 2: Lock conflict with no-wait
    print '(a)', ''
    print '(a)', 'Test 2: Lock conflict with no-wait'
    success = acquire_lock(trim(temp_cache_dir), 'test_project2', .false.)
    if (success) then
        print '(a)', '  ✓ First lock acquired'

        ! Try to acquire same lock with no-wait
        success = acquire_lock(trim(temp_cache_dir), 'test_project2', .false.)
        if (.not. success) then
            print '(a)', '  ✓ Second lock correctly failed with no-wait'
        else
            print '(a)', '  ✗ Second lock should have failed'
            ! Debug: check if lock file exists
            inquire (file=trim(temp_cache_dir)//'/test_project2.lock', exist=locked)
            print '(a,l)', '  Debug: Lock file exists = ', locked
            call system('ls -la '//trim(temp_cache_dir)//'/*.lock')
            stop 1
        end if

        call release_lock(trim(temp_cache_dir), 'test_project2')
        print '(a)', '  ✓ Lock released'
    else
        print '(a)', '  ✗ Failed to acquire first lock'
        stop 1
    end if

    ! Test 3: Multiple different locks
    print '(a)', ''
    print '(a)', 'Test 3: Multiple different project locks'
    success = acquire_lock(trim(temp_cache_dir), 'project_a', .false.)
    if (.not. success) then
        print '(a)', '  ✗ Failed to acquire lock for project_a'
        stop 1
    end if

    success = acquire_lock(trim(temp_cache_dir), 'project_b', .false.)
    if (.not. success) then
        print '(a)', '  ✗ Failed to acquire lock for project_b'
        stop 1
    end if

    print '(a)', '  ✓ Multiple project locks acquired successfully'

    call release_lock(trim(temp_cache_dir), 'project_a')
    call release_lock(trim(temp_cache_dir), 'project_b')
    print '(a)', '  ✓ All locks released'

    ! Test 4: Stale lock cleanup
    print '(a)', ''
    print '(a)', 'Test 4: Stale lock cleanup'
    call create_stale_lock(trim(temp_cache_dir), 'stale_project')

    ! Cleanup should remove stale locks
    call cleanup_stale_locks(trim(temp_cache_dir))

    ! Now we should be able to acquire the lock
    success = acquire_lock(trim(temp_cache_dir), 'stale_project', .false.)
    if (success) then
        print '(a)', '  ✓ Stale lock was cleaned up and new lock acquired'
        call release_lock(trim(temp_cache_dir), 'stale_project')
    else
        print '(a)', '  ✗ Failed to acquire lock after stale cleanup'
        stop 1
    end if

    ! Cleanup
    call system('rm -rf '//trim(temp_cache_dir))

    print '(a)', ''
    print '(a)', 'All cache lock tests passed!'

contains

    subroutine get_temp_dir(dir)
        character(len=*), intent(out) :: dir
        integer :: unit, iostat, last_char

        block
            character(len=256) :: temp_file
 temp_file = get_temp_file_path(create_temp_dir('fortran_test'), 'fortran_test_dir.tmp')
            call system('mktemp -d > '//trim(temp_file))
            open (newunit=unit, file=temp_file, status='old', iostat=iostat)
            if (iostat == 0) then
                read (unit, '(a)') dir
                close (unit)
                ! Cleanup handled by temp_utils

                ! Remove trailing newline if present
                last_char = len_trim(dir)
                if (last_char > 0) then
                    if (iachar(dir(last_char:last_char)) == 10 .or. &
                        iachar(dir(last_char:last_char)) == 13) then
                        dir = dir(1:last_char - 1)
                    end if
                end if
            else
                dir = create_temp_dir('fortran_cache_lock_test')
            end if
        end block
    end subroutine get_temp_dir

    subroutine create_stale_lock(cache_dir, project_name)
        character(len=*), intent(in) :: cache_dir, project_name
        character(len=512) :: lock_file
        integer :: unit

        lock_file = trim(cache_dir)//'/'//trim(project_name)//'.lock'

        ! Create a lock file with old timestamp
        open (newunit=unit, file=lock_file, status='new')
        write (unit, '(a)') '99999'  ! Non-existent PID
        write (unit, '(a)') '20200101000000'  ! Old timestamp
        close (unit)

    end subroutine create_stale_lock

    function get_timestamp_str() result(timestamp)
        character(len=16) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)
        write (timestamp, '(i0,5i2.2)') values(1), values(2), values(3), &
            values(5), values(6), values(7)
    end function get_timestamp_str

end program test_cache_lock
