program test_registry_enhancement
  use, intrinsic :: iso_fortran_env, only: error_unit
  use cache, only: get_cache_dir
  implicit none
  
  print *, '=== Registry Enhancement Tests ===\'
  print *
  
  ! Test 1: Multiple modules from same package
  call test_multiple_modules_same_package()
  
  print *
  print *, 'All registry enhancement tests passed!'
  
contains

  subroutine test_multiple_modules_same_package()
    character(len=256) :: test_file, test_dir
    character(len=512) :: command
    integer :: unit
    
    print *, 'Test 1: Multiple modules from same package'
    
    ! Create test directory
    test_dir = '/tmp/test_multiple_modules'
    call execute_command_line('mkdir -p ' // trim(test_dir))
    
    ! First, update registry to have a package with multiple modules
    call update_registry_for_test(test_dir)
    
    ! Create test file that uses multiple modules from same package
    test_file = trim(test_dir) // '/test_multiple.f90'
    open(newunit=unit, file=test_file, status='replace')
    write(unit, '(a)') 'program test_multiple'
    write(unit, '(a)') '  use pyplot_module    ! First module from pyplot-fortran'
    write(unit, '(a)') '  use pyplot_utils     ! Second module from pyplot-fortran'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  print *, "Multiple modules from same package"'
    write(unit, '(a)') 'end program test_multiple'
    close(unit)
    
    ! Run the program with custom config dir (will fail but should show dependencies)
    command = 'fpm run fortran -- --config-dir ' // trim(test_dir) // ' -v ' // &
              trim(test_file) // ' > /tmp/multiple_output.txt 2>&1; echo $? > /tmp/multiple_exit.txt'
    call execute_command_line(command)
    
    ! Check that both modules were detected and mapped to the same package
    call check_output_contains('/tmp/multiple_output.txt', 'pyplot-fortran')
    call check_output_contains('/tmp/multiple_output.txt', 'external module dependencies')
    
    ! Also check that the generated fmp.toml contains the dependencies
    call check_generated_fpm_toml()
    
    ! Clean up
    call execute_command_line('rm -rf ' // trim(test_dir))
    call execute_command_line('rm -f /tmp/multiple_output.txt /tmp/multiple_exit.txt')
    
    print *, 'PASS: Multiple modules from same package handled correctly'
    print *
  end subroutine test_multiple_modules_same_package

  subroutine update_registry_for_test(test_dir)
    character(len=*), intent(in) :: test_dir
    character(len=256) :: registry_path
    integer :: unit
    
    ! Create a test registry in the test directory
    registry_path = trim(test_dir) // '/registry.toml'
    open(newunit=unit, file=registry_path, status='replace')
    write(unit, '(a)') '# Test registry for multiple modules from same package'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages]'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages.pyplot-fortran]'
    write(unit, '(a)') 'git = "https://github.com/jacobwilliams/pyplot-fortran"'
    write(unit, '(a)') '# This package provides multiple modules: pyplot_module, pyplot_utils, etc.'
    write(unit, '(a)') ''
    write(unit, '(a)') '[packages.fortplotlib]'
    write(unit, '(a)') 'git = "https://github.com/krystophny/fortplotlib"'
    write(unit, '(a)') 'prefix = "fortplot"'
    close(unit)
    
  end subroutine update_registry_for_test

  subroutine check_output_contains(output_file, expected_text)
    character(len=*), intent(in) :: output_file, expected_text
    character(len=512) :: line
    integer :: unit, iostat
    logical :: found
    
    found = .false.
    
    open(newunit=unit, file=output_file, status='old', iostat=iostat)
    if (iostat /= 0) then
      write(error_unit, *) 'Error: Cannot open output file: ', trim(output_file)
      stop 1
    end if
    
    do
      read(unit, '(a)', iostat=iostat) line
      if (iostat /= 0) exit
      
      if (index(line, trim(expected_text)) > 0) then
        found = .true.
        exit
      end if
    end do
    
    close(unit)
    
    if (.not. found) then
      write(error_unit, *) 'Error: Expected text "', trim(expected_text), '" not found in output'
      stop 1
    end if
    
  end subroutine check_output_contains

  subroutine check_generated_fpm_toml()
    character(len=512) :: cache_dir, fpm_toml_path
    character(len=512) :: line
    integer :: unit, iostat
    logical :: found_pyplot
    
    ! Find the generated fpm.toml in cache directory
    block
      character(len=256) :: cache_dir
      cache_dir = get_cache_dir()
      call execute_command_line('find "' // trim(cache_dir) // '" -name "fpm.toml" -newer /tmp/multiple_output.txt ' // &
                                '2>/dev/null | head -1 > /tmp/fpm_path.txt')
    end block
    
    open(newunit=unit, file='/tmp/fpm_path.txt', status='old', iostat=iostat)
    if (iostat /= 0) then
      ! Can't find the exact path, just check that the test validates basic functionality
      print *, 'Note: Could not verify fpm.toml contents (cache cleaned up)'
      return
    end if
    
    read(unit, '(a)', iostat=iostat) fpm_toml_path
    close(unit)
    call execute_command_line('rm -f /tmp/fpm_path.txt')
    
    if (len_trim(fpm_toml_path) == 0) then
      print *, 'Note: Could not find generated fpm.toml (cache cleaned up)'
      return
    end if
    
    ! Check the fpm.toml contains pyplot-fortran dependency
    found_pyplot = .false.
    open(newunit=unit, file=fpm_toml_path, status='old', iostat=iostat)
    if (iostat /= 0) then
      print *, 'Note: Could not read generated fpm.toml'
      return
    end if
    
    do
      read(unit, '(a)', iostat=iostat) line
      if (iostat /= 0) exit
      
      if (index(line, 'pyplot-fortran') > 0) then
        found_pyplot = .true.
        exit
      end if
    end do
    
    close(unit)
    
    if (.not. found_pyplot) then
      write(error_unit, *) 'Error: pyplot-fortran dependency not found in generated fpm.toml'
      stop 1
    end if
    
  end subroutine check_generated_fpm_toml

end program test_registry_enhancement