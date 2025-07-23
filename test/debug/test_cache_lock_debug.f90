program test_cache_lock_debug
    use temp_utils, only: create_test_cache_dir, get_temp_file_path, get_project_root
    use system_utils, only: escape_shell_arg, sys_sleep
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none

    character(len=512) :: command, temp_cache_dir, temp_output_file
    character(len=1024) :: output
    integer :: exit_code, unit, iostat, i
    character(len=1024) :: line
    character(len=*), parameter :: test_file = 'example/basic/hello/hello.f90'

    print '(a)', '='//repeat('=', 60)
    print '(a)', 'DEBUG: Windows Cache Lock Test'
    print '(a)', '='//repeat('=', 60)
    print *

    ! Create temporary cache directory
    temp_cache_dir = create_test_cache_dir('debug_cache_lock')
    print '(a,a)', 'Cache directory: ', trim(temp_cache_dir)

    ! Test multiple runs with same cache
    do i = 1, 3
        print *
        print '(a,i0,a)', 'Run ', i, '...'
        
        ! Create temp output file
        temp_output_file = get_temp_file_path(temp_cache_dir, 'output.tmp')
        
        ! Run fortran CLI with cache
        command = 'cd "'//trim(escape_shell_arg(get_project_root()))//'" && '// &
                  'fpm run fortran -- -vv --cache-dir "'//trim(escape_shell_arg(temp_cache_dir))//'" '// &
                  trim(escape_shell_arg(test_file))//' > "'//trim(escape_shell_arg(temp_output_file))//'" 2>&1'
        
        print '(a)', 'Command: '//trim(command)
        call execute_command_line(trim(command), exitstat=exit_code)
        
        ! Read output
        output = ''
        open(newunit=unit, file=trim(temp_output_file), status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                if (len_trim(output) > 0) then
                    output = trim(output)//' | '//trim(adjustl(line))
                else
                    output = trim(adjustl(line))
                end if
            end do
            close(unit)
        end if
        
        print '(a,i0)', 'Exit code: ', exit_code
        print '(a)', 'Output: '//trim(output)
        
        if (exit_code /= 0) then
            print '(a)', 'ERROR: Run failed!'
            if (index(output, 'cache lock') > 0) then
                print '(a)', 'FOUND: Cache lock error detected'
            end if
        end if
        
        ! Add delays between runs
        if (i < 3) then
            print '(a)', 'Waiting 3 seconds before next run...'
            call sys_sleep(3)
        end if
    end do

    print *
    print '(a)', 'Test completed'

end program test_cache_lock_debug