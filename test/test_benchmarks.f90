program test_benchmarks
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  
  integer :: n_passed, n_failed
  
  n_passed = 0
  n_failed = 0
  
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Fortran CLI Performance Benchmarks'
  print '(a)', '='//repeat('=', 60)
  print *
  
  ! Run benchmark tests
  call benchmark_simple_program(n_passed, n_failed)
  call benchmark_local_modules(n_passed, n_failed)
  call benchmark_incremental_compilation(n_passed, n_failed)
  
  ! Summary
  print *
  print '(a)', '='//repeat('=', 60)
  print '(a)', 'Benchmark Summary'
  print '(a)', '='//repeat('=', 60)
  print '(a,i0)', 'Tests passed: ', n_passed
  print '(a,i0)', 'Tests failed: ', n_failed
  
  if (n_failed > 0) then
    stop 1
  end if
  
contains

  subroutine benchmark_simple_program(n_passed, n_failed)
    integer, intent(inout) :: n_passed, n_failed
    real :: time_cold, time_warm, speedup
    character(len=256) :: test_file, cache_dir
    
    print '(a)', 'Benchmark 1: Simple Program Performance'
    print '(a)', '---------------------------------------'
    
    ! Create test file
    test_file = '/tmp/bench_simple.f90'
    call create_simple_test_file(test_file)
    
    ! Use temporary cache
    cache_dir = '/tmp/bench_cache_simple'
    
    ! Measure cold cache
    time_cold = measure_execution_time(test_file, cache_dir, .true.)
    print '(a,f6.3,a)', 'Cold cache time: ', time_cold, ' seconds'
    
    ! Measure warm cache
    time_warm = measure_execution_time(test_file, cache_dir, .false.)
    print '(a,f6.3,a)', 'Warm cache time: ', time_warm, ' seconds'
    
    ! Calculate speedup
    if (time_warm > 0.0) then
      speedup = time_cold / time_warm
      print '(a,f4.1,a)', 'Speedup: ', speedup, 'x'
      
      if (speedup > 1.5) then
        print '(a)', '  ✓ PASS: Significant speedup achieved'
        n_passed = n_passed + 1
      else
        print '(a)', '  ✗ FAIL: Insufficient speedup'
        n_failed = n_failed + 1
      end if
    else
      print '(a)', '  ✗ FAIL: Could not measure times'
      n_failed = n_failed + 1
    end if
    
    ! Cleanup
    call execute_command_line('rm -rf ' // trim(test_file) // ' ' // trim(cache_dir))
    print *
    
  end subroutine benchmark_simple_program
  
  subroutine benchmark_local_modules(n_passed, n_failed)
    integer, intent(inout) :: n_passed, n_failed
    real :: time_cold, time_warm, speedup
    character(len=256) :: test_dir, cache_dir
    
    print '(a)', 'Benchmark 2: Local Modules Performance'
    print '(a)', '--------------------------------------'
    
    ! Create test directory with modules
    test_dir = '/tmp/bench_modules'
    call create_modules_test_files(test_dir)
    
    ! Use temporary cache
    cache_dir = '/tmp/bench_cache_modules'
    
    ! Measure cold cache
    time_cold = measure_execution_time(trim(test_dir) // '/main.f90', cache_dir, .true.)
    print '(a,f6.3,a)', 'Cold cache time: ', time_cold, ' seconds'
    
    ! Measure warm cache
    time_warm = measure_execution_time(trim(test_dir) // '/main.f90', cache_dir, .false.)
    print '(a,f6.3,a)', 'Warm cache time: ', time_warm, ' seconds'
    
    ! Calculate speedup
    if (time_warm > 0.0) then
      speedup = time_cold / time_warm
      print '(a,f4.1,a)', 'Speedup: ', speedup, 'x'
      
      if (speedup > 2.0) then
        print '(a)', '  ✓ PASS: Excellent speedup for modules'
        n_passed = n_passed + 1
      else
        print '(a)', '  ✗ FAIL: Insufficient speedup for modules'
        n_failed = n_failed + 1
      end if
    else
      print '(a)', '  ✗ FAIL: Could not measure times'
      n_failed = n_failed + 1
    end if
    
    ! Cleanup
    call execute_command_line('rm -rf ' // trim(test_dir) // ' ' // trim(cache_dir))
    print *
    
  end subroutine benchmark_local_modules
  
  subroutine benchmark_incremental_compilation(n_passed, n_failed)
    integer, intent(inout) :: n_passed, n_failed
    real :: time_full, time_incremental, speedup
    character(len=256) :: test_dir, cache_dir
    
    print '(a)', 'Benchmark 3: Incremental Compilation Performance'
    print '(a)', '-----------------------------------------------'
    
    ! Create test directory
    test_dir = '/tmp/bench_incremental'
    call create_modules_test_files(test_dir)
    
    ! Use temporary cache
    cache_dir = '/tmp/bench_cache_incremental'
    
    ! Initial build (establish cache)
    time_full = measure_execution_time(trim(test_dir) // '/main.f90', cache_dir, .true.)
    print '(a,f6.3,a)', 'Initial build time: ', time_full, ' seconds'
    
    ! Modify main file only
    call modify_main_file(trim(test_dir) // '/main.f90')
    
    ! Measure incremental build
    time_incremental = measure_execution_time(trim(test_dir) // '/main.f90', cache_dir, .false.)
    print '(a,f6.3,a)', 'Incremental build: ', time_incremental, ' seconds'
    
    ! Calculate speedup
    if (time_incremental > 0.0) then
      speedup = time_full / time_incremental
      print '(a,f4.1,a)', 'Speedup: ', speedup, 'x'
      
      if (speedup > 1.5 .and. time_incremental < time_full * 0.7) then
        print '(a)', '  ✓ PASS: Incremental compilation working'
        n_passed = n_passed + 1
      else
        print '(a)', '  ✗ FAIL: Incremental compilation not effective'
        n_failed = n_failed + 1
      end if
    else
      print '(a)', '  ✗ FAIL: Could not measure times'
      n_failed = n_failed + 1
    end if
    
    ! Cleanup
    call execute_command_line('rm -rf ' // trim(test_dir) // ' ' // trim(cache_dir))
    print *
    
  end subroutine benchmark_incremental_compilation
  
  function measure_execution_time(file_path, cache_dir, clear_cache) result(elapsed_time)
    character(len=*), intent(in) :: file_path, cache_dir
    logical, intent(in) :: clear_cache
    real :: elapsed_time
    integer(int64) :: count_start, count_end, count_rate
    character(len=512) :: command
    integer :: exit_code
    
    ! Clear cache if requested
    if (clear_cache) then
      call execute_command_line('rm -rf ' // trim(cache_dir))
    end if
    
    ! Build command
    command = './build/gfortran_*/app/fortran --cache-dir "' // &
              trim(cache_dir) // '" "' // trim(file_path) // '" > /dev/null 2>&1'
    
    ! Measure execution time
    call system_clock(count_start, count_rate)
    call execute_command_line(trim(command), exitstat=exit_code)
    call system_clock(count_end)
    
    if (exit_code == 0) then
      elapsed_time = real(count_end - count_start) / real(count_rate)
    else
      elapsed_time = -1.0  ! Error indicator
    end if
    
  end function measure_execution_time
  
  subroutine create_simple_test_file(file_path)
    character(len=*), intent(in) :: file_path
    integer :: unit
    
    open(newunit=unit, file=file_path, status='replace')
    write(unit, '(a)') 'program bench_simple'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: i, sum'
    write(unit, '(a)') '  sum = 0'
    write(unit, '(a)') '  do i = 1, 100'
    write(unit, '(a)') '    sum = sum + i'
    write(unit, '(a)') '  end do'
    write(unit, '(a)') '  print *, "Sum:", sum'
    write(unit, '(a)') 'end program bench_simple'
    close(unit)
    
  end subroutine create_simple_test_file
  
  subroutine create_modules_test_files(dir_path)
    character(len=*), intent(in) :: dir_path
    integer :: unit
    character(len=512) :: command
    
    ! Create directory
    command = 'mkdir -p "' // trim(dir_path) // '"'
    call execute_command_line(command)
    
    ! Create module1.f90
    open(newunit=unit, file=trim(dir_path)//'/module1.f90', status='replace')
    write(unit, '(a)') 'module module1'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer, parameter :: MOD1_VALUE = 42'
    write(unit, '(a)') 'contains'
    write(unit, '(a)') '  function mod1_func(x) result(y)'
    write(unit, '(a)') '    integer, intent(in) :: x'
    write(unit, '(a)') '    integer :: y'
    write(unit, '(a)') '    y = x + MOD1_VALUE'
    write(unit, '(a)') '  end function mod1_func'
    write(unit, '(a)') 'end module module1'
    close(unit)
    
    ! Create module2.f90
    open(newunit=unit, file=trim(dir_path)//'/module2.f90', status='replace')
    write(unit, '(a)') 'module module2'
    write(unit, '(a)') '  use module1'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') 'contains'
    write(unit, '(a)') '  function mod2_func(x) result(y)'
    write(unit, '(a)') '    integer, intent(in) :: x'
    write(unit, '(a)') '    integer :: y'
    write(unit, '(a)') '    y = mod1_func(x) * 2'
    write(unit, '(a)') '  end function mod2_func'
    write(unit, '(a)') 'end module module2'
    close(unit)
    
    ! Create main.f90
    open(newunit=unit, file=trim(dir_path)//'/main.f90', status='replace')
    write(unit, '(a)') 'program bench_modules'
    write(unit, '(a)') '  use module2'
    write(unit, '(a)') '  implicit none'
    write(unit, '(a)') '  integer :: result'
    write(unit, '(a)') '  result = mod2_func(10)'
    write(unit, '(a)') '  print *, "Result:", result'
    write(unit, '(a)') 'end program bench_modules'
    close(unit)
    
  end subroutine create_modules_test_files
  
  subroutine modify_main_file(file_path)
    character(len=*), intent(in) :: file_path
    integer :: unit
    
    ! Append a comment to trigger recompilation
    open(newunit=unit, file=file_path, position='append')
    write(unit, '(a)') '! Modified for incremental build test'
    close(unit)
    
  end subroutine modify_main_file
  
end program test_benchmarks