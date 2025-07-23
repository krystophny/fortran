program test_windows_paths_simple
    implicit none
    
    character(len=512) :: command, cwd
    integer :: exit_code, unit
    logical :: file_exists
    
    print '(a)', 'Windows Path Debug Test (Simple)'
    print '(a)', '================================'
    
    ! Get current directory
    call getcwd(cwd)
    print '(a,a)', 'Current directory: ', trim(cwd)
    
    ! Test file
    print '(a)', ''
    print '(a)', 'Checking test files:'
    inquire(file='example\basic\hello\hello.f', exist=file_exists)
    print '(a,l1)', '  example\basic\hello\hello.f exists: ', file_exists
    
    inquire(file='example\basic\hello\hello.f90', exist=file_exists)
    print '(a,l1)', '  example\basic\hello\hello.f90 exists: ', file_exists
    
    ! Test different path formats
    print '(a)', ''
    print '(a)', 'Testing path formats:'
    
    ! Forward slashes
    inquire(file='example/basic/hello/hello.f', exist=file_exists)
    print '(a,l1)', '  example/basic/hello/hello.f exists: ', file_exists
    
    ! Test commands
    print '(a)', ''
    print '(a)', 'Testing commands:'
    
    ! Simple dir command
    command = 'dir example\basic\hello'
    print '(a,a)', '  Command: ', trim(command)
    call execute_command_line(trim(command), exitstat=exit_code)
    print '(a,i0)', '  Exit code: ', exit_code
    
    ! Test with quotes
    command = 'dir "example\basic\hello"'
    print '(a,a)', '  Command: ', trim(command)
    call execute_command_line(trim(command), exitstat=exit_code)
    print '(a,i0)', '  Exit code: ', exit_code
    
    ! Test fpm run
    command = 'fpm run fortran -- --version'
    print '(a,a)', '  Command: ', trim(command)
    call execute_command_line(trim(command), exitstat=exit_code)
    print '(a,i0)', '  Exit code: ', exit_code
    
    ! Test with cache dir and file
    command = 'fpm run fortran -- --cache-dir "C:\temp\cache" "example\basic\hello\hello.f"'
    print '(a,a)', '  Command: ', trim(command)
    call execute_command_line(trim(command), exitstat=exit_code)
    print '(a,i0)', '  Exit code: ', exit_code
    
    print '(a)', ''
    print '(a)', 'Test completed'
    
end program test_windows_paths_simple