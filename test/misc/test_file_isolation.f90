program test_file_isolation
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path
    implicit none

    integer :: unit
    character(len=:), allocatable :: temp_dir
    character(len=:), allocatable :: file1, file2, file3

    ! Create temporary directory
    temp_dir = create_temp_dir('test_file_isolation')
    file1 = get_temp_file_path(temp_dir, 'file1.txt')
    file2 = get_temp_file_path(temp_dir, 'file2.txt')
    file3 = get_temp_file_path(temp_dir, 'file3.txt')

    ! Test 1: Write to file1
    open (newunit=unit, file=file1, status='replace')
    write (unit, '(a)') 'Content from test 1'
    close (unit)

    ! Test 2: Write to file2
    open (newunit=unit, file=file2, status='replace')
    write (unit, '(a)') 'Content from test 2'
    close (unit)

    ! Test 3: Write to file3
    open (newunit=unit, file=file3, status='replace')
    write (unit, '(a)') 'Content from test 3'
    close (unit)

    ! Check contents
    print '(a)', trim(file1)//':'
    call execute_command_line('cat '//file1)

    print '(a)', trim(file2)//':'
    call execute_command_line('cat '//file2)

    print '(a)', trim(file3)//':'
    call execute_command_line('cat '//file3)

    ! Cleanup
    call cleanup_temp_dir(temp_dir)

end program test_file_isolation
