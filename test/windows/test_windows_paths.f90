program test_windows_paths
    use fpm_environment, only: get_os_type, OS_WINDOWS
    use temp_utils, only: get_project_root, create_temp_dir, get_temp_file_path
    implicit none
    
    character(len=512) :: command, temp_output_file, cache_dir
    character(len=256) :: filename
    integer :: exit_code
    logical :: file_exists
    
    print '(a)', 'Windows Path Debug Test'
    print '(a)', '======================'
    
    ! Test file
    filename = 'example/basic/hello/hello.f'
    
    ! Create cache dir
    cache_dir = create_temp_dir('test_cache_debug')
    
    ! Create temp output file path
    temp_output_file = get_temp_file_path(create_temp_dir('fortran_test'), 'test_output.tmp')
    
    print '(a)', 'Environment:'
    print '(a,a)', '  OS Type: ', merge('Windows', 'Unix   ', get_os_type() == OS_WINDOWS)
    print '(a,a)', '  Project root: ', trim(get_project_root())
    print '(a,a)', '  Test file: ', trim(filename)
    print '(a,a)', '  Cache dir: ', trim(cache_dir)
    print '(a,a)', '  Temp output: ', trim(temp_output_file)
    
    ! Check if test file exists
    inquire(file=trim(filename), exist=file_exists)
    print '(a,l1)', '  Test file exists: ', file_exists
    
    ! Build command
    if (get_os_type() == OS_WINDOWS) then
        ! Try different command variations
        print '(a)', ''
        print '(a)', 'Testing Windows command variations:'
        
        ! Version 1: Simple cd
        command = 'cd /d "'//trim(get_project_root())//'" && dir example\basic\hello'
        print '(a)', '  Command 1: '//trim(command)
        call execute_command_line(trim(command), exitstat=exit_code)
        print '(a,i0)', '  Exit code: ', exit_code
        
        ! Version 2: With escaped paths
        command = 'cd /d "'//trim(get_project_root())//'" && fpm run fortran -- --version'
        print '(a)', '  Command 2: '//trim(command)  
        call execute_command_line(trim(command), exitstat=exit_code)
        print '(a,i0)', '  Exit code: ', exit_code
        
        ! Version 3: Full command
        command = 'cd /d "'//trim(get_project_root())//'" && '// &
                  'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" "'// &
                  trim(filename)//'" > "'//trim(temp_output_file)//'" 2>nul'
        print '(a)', '  Command 3: '//trim(command)
        call execute_command_line(trim(command), exitstat=exit_code)
        print '(a,i0)', '  Exit code: ', exit_code
        
        ! Check if output file was created
        inquire(file=trim(temp_output_file), exist=file_exists)
        print '(a,l1)', '  Output file created: ', file_exists
    else
        print '(a)', 'Not running on Windows, skipping tests'
    end if
    
    print '(a)', ''
    print '(a)', 'Test completed'
    
end program test_windows_paths