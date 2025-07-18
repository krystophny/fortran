program test_benchmarks
    use, intrinsic :: iso_fortran_env, only: int64
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    integer :: n_passed, n_failed

    n_passed = 0
    n_failed = 0

    print '(a)', '='//repeat('=', 60)
    print '(a)', 'Fortran CLI Cache Behavior Tests'
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
        character(len=256) :: test_file, cache_dir, command
        character(len=1024) :: output1, output2
        integer :: exit_code
        logical :: first_compiled, second_cached

        print '(a)', 'Test 1: Simple Program Compilation Behavior'
        print '(a)', '-------------------------------------------'

        ! Create test file
     test_file = get_temp_file_path(create_temp_dir('fortran_test'), 'bench_simple.f90')
        call create_simple_test_file(test_file)

        ! Use temporary cache
        cache_dir = create_temp_dir('fortran_test_bench_cache_simple')
        call execute_command_line('rm -rf "'//trim(cache_dir)//'"')

        ! First run - should compile
        block
            character(len=:), allocatable :: temp_dir, bench1_output
            temp_dir = create_temp_dir('fortran_test')
            bench1_output = get_temp_file_path(temp_dir, 'bench1_output.txt')

            command = 'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" -v "'// &
                      trim(test_file)//'" > '//bench1_output//' 2>&1'
            call execute_command_line(command, exitstat=exit_code)

            call read_file_content(bench1_output, output1)
first_compiled = index(output1, 'Cache miss') > 0 .or. index(output1, '.f90  done.') > 0

            print '(a)', 'First run (cold cache):'
            if (first_compiled) then
                print '(a)', '  ✓ Files compiled as expected'
            else
                print '(a)', '  ✗ Expected compilation but none detected'
            end if

            ! Second run - should use cache
            bench1_output = get_temp_file_path(temp_dir, 'bench2_output.txt')
            command = 'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" -v "'// &
                      trim(test_file)//'" > '//bench1_output//' 2>&1'
            call execute_command_line(command, exitstat=exit_code)

            call read_file_content(bench1_output, output2)
        end block
    second_cached = index(output2, 'Cache hit') > 0 .or. index(output2, 'Project is up to date') > 0

        print '(a)', 'Second run (warm cache):'
        if (second_cached) then
            print '(a)', '  ✓ Cache used as expected'
        else
            print '(a)', '  ✗ Expected cache hit but got recompilation'
        end if

        if (first_compiled .and. second_cached) then
            print '(a)', '  ✓ PASS: Caching behavior correct'
            n_passed = n_passed + 1
        else
            print '(a)', '  ✗ FAIL: Caching behavior incorrect'
            n_failed = n_failed + 1
        end if

        ! Cleanup
        call execute_command_line('rm -rf '//trim(test_file)//' '//trim(cache_dir))
        print *

    end subroutine benchmark_simple_program

    subroutine benchmark_local_modules(n_passed, n_failed)
        integer, intent(inout) :: n_passed, n_failed
        character(len=256) :: test_dir, cache_dir, command
        character(len=1024) :: output1, output2
        integer :: exit_code
        logical :: modules_compiled, cached_properly

        print '(a)', 'Test 2: Local Modules Compilation Behavior'
        print '(a)', '------------------------------------------'

        ! Create test directory with modules
        test_dir = create_temp_dir('fortran_test_bench_modules')
        call create_modules_test_files(test_dir)

        ! Use temporary cache
        cache_dir = create_temp_dir('fortran_test_bench_cache_modules')
        call execute_command_line('rm -rf "'//trim(cache_dir)//'"')

        ! First run - should compile modules
        block
            character(len=:), allocatable :: temp_dir, bench_mod1_output
            temp_dir = create_temp_dir('fortran_test')
            bench_mod1_output = get_temp_file_path(temp_dir, 'bench_mod1_output.txt')

            command = 'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" -v "'// &
                      trim(test_dir)//'/main.f90" > '//bench_mod1_output//' 2>&1'
            call execute_command_line(command, exitstat=exit_code)

            call read_file_content(bench_mod1_output, output1)
            modules_compiled = index(output1, 'Cache miss') > 0 .and. &
                (index(output1, '.f90  done.') > 0 .or. index(output1, 'libmain.a') > 0)

            print '(a)', 'First run (should compile modules):'
            if (modules_compiled) then
                print '(a)', '  ✓ Modules compiled as expected'
            else
                print '(a)', '  ✗ Expected module compilation but none detected'
            end if

            ! Second run - should use cache
            bench_mod1_output = get_temp_file_path(temp_dir, 'bench_mod2_output.txt')
            command = 'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" -v "'// &
                      trim(test_dir)//'/main.f90" > '//bench_mod1_output//' 2>&1'
            call execute_command_line(command, exitstat=exit_code)

            call read_file_content(bench_mod1_output, output2)
        end block
    cached_properly = index(output2, 'Cache hit') > 0 .or. index(output2, 'Project is up to date') > 0

        print '(a)', 'Second run (should use cache):'
        if (cached_properly) then
            print '(a)', '  ✓ Module cache used as expected'
        else
            print '(a)', '  ✗ Expected cache hit but got recompilation'
        end if

        if (modules_compiled .and. cached_properly) then
            print '(a)', '  ✓ PASS: Module caching behavior correct'
            n_passed = n_passed + 1
        else
            print '(a)', '  ✗ FAIL: Module caching behavior incorrect'
            n_failed = n_failed + 1
        end if

        ! Cleanup
        call execute_command_line('rm -rf '//trim(test_dir)//' '//trim(cache_dir))
        print *

    end subroutine benchmark_local_modules

    subroutine benchmark_incremental_compilation(n_passed, n_failed)
        integer, intent(inout) :: n_passed, n_failed
        character(len=256) :: test_dir, cache_dir, command
        character(len=1024) :: output1, output2
        integer :: exit_code
        logical :: initial_compiled, incremental_cached

        print '(a)', 'Test 3: Incremental Compilation Behavior'
        print '(a)', '----------------------------------------'

        ! Create test directory
        test_dir = create_temp_dir('fortran_test_bench_incremental')
        call create_modules_test_files(test_dir)

        ! Use temporary cache
        cache_dir = create_temp_dir('fortran_test_bench_cache_incremental')
        call execute_command_line('rm -rf "'//trim(cache_dir)//'"')

        ! Initial build (establish cache)
        block
            character(len=:), allocatable :: temp_dir, bench_inc1_output
            temp_dir = create_temp_dir('fortran_test')
            bench_inc1_output = get_temp_file_path(temp_dir, 'bench_inc1_output.txt')

            command = 'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" -v "'// &
                      trim(test_dir)//'/main.f90" > '//bench_inc1_output//' 2>&1'
            call execute_command_line(command, exitstat=exit_code)

            call read_file_content(bench_inc1_output, output1)
    initial_compiled = index(output1, 'Cache miss') > 0 .and. index(output1, '.f90  done.') > 0

            print '(a)', 'Initial build:'
            if (initial_compiled) then
                print '(a)', '  ✓ Full compilation as expected'
            else
                print '(a)', '  ✗ Expected compilation but none detected'
            end if

            ! Modify main file only
            call modify_main_file(trim(test_dir)//'/main.f90')

            ! Incremental build - should recompile main but cache modules
            bench_inc1_output = get_temp_file_path(temp_dir, 'bench_inc2_output.txt')
            command = 'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" -v "'// &
                      trim(test_dir)//'/main.f90" > '//bench_inc1_output//' 2>&1'
            call execute_command_line(command, exitstat=exit_code)

            call read_file_content(bench_inc1_output, output2)
        end block
    incremental_cached = index(output2, 'Cache hit') > 0 .and. index(output2, 'main.f90  done.') > 0

        print '(a)', 'Incremental build:'
        if (incremental_cached) then
            print '(a)', '  ✓ Cache used for unchanged modules'
        else
            print '(a)', '  ✗ Expected incremental build behavior'
        end if

        if (initial_compiled .and. incremental_cached) then
            print '(a)', '  ✓ PASS: Incremental compilation working'
            n_passed = n_passed + 1
        else
            print '(a)', '  ✗ FAIL: Incremental compilation behavior incorrect'
            n_failed = n_failed + 1
        end if

        ! Cleanup
        call execute_command_line('rm -rf '//trim(test_dir)//' '//trim(cache_dir))
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
            call execute_command_line('rm -rf '//trim(cache_dir))
        end if

        ! Build command
        command = 'fpm run fortran -- --cache-dir "'// &
                  trim(cache_dir)//'" "'//trim(file_path)//'" > /dev/null 2>&1'

        ! Measure execution time
        call system_clock(count_start, count_rate)
        call execute_command_line(trim(command), exitstat=exit_code)
        call system_clock(count_end)

        if (exit_code == 0) then
            elapsed_time = real(count_end - count_start)/real(count_rate)
        else
            elapsed_time = -1.0  ! Error indicator
        end if

    end function measure_execution_time

    subroutine create_simple_test_file(file_path)
        character(len=*), intent(in) :: file_path
        integer :: unit

        open (newunit=unit, file=file_path, status='replace')
        write (unit, '(a)') 'program bench_simple'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer :: i, sum'
        write (unit, '(a)') '  sum = 0'
        write (unit, '(a)') '  do i = 1, 100'
        write (unit, '(a)') '    sum = sum + i'
        write (unit, '(a)') '  end do'
        write (unit, '(a)') '  print *, "Sum:", sum'
        write (unit, '(a)') 'end program bench_simple'
        close (unit)

    end subroutine create_simple_test_file

    subroutine create_modules_test_files(dir_path)
        character(len=*), intent(in) :: dir_path
        integer :: unit
        character(len=512) :: command

        ! Create directory
        command = 'mkdir -p "'//trim(dir_path)//'"'
        call execute_command_line(command)

        ! Create module1.f90
        open (newunit=unit, file=trim(dir_path)//'/module1.f90', status='replace')
        write (unit, '(a)') 'module module1'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer, parameter :: MOD1_VALUE = 42'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') '  function mod1_func(x) result(y)'
        write (unit, '(a)') '    integer, intent(in) :: x'
        write (unit, '(a)') '    integer :: y'
        write (unit, '(a)') '    y = x + MOD1_VALUE'
        write (unit, '(a)') '  end function mod1_func'
        write (unit, '(a)') 'end module module1'
        close (unit)

        ! Create module2.f90
        open (newunit=unit, file=trim(dir_path)//'/module2.f90', status='replace')
        write (unit, '(a)') 'module module2'
        write (unit, '(a)') '  use module1'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') '  function mod2_func(x) result(y)'
        write (unit, '(a)') '    integer, intent(in) :: x'
        write (unit, '(a)') '    integer :: y'
        write (unit, '(a)') '    y = mod1_func(x) * 2'
        write (unit, '(a)') '  end function mod2_func'
        write (unit, '(a)') 'end module module2'
        close (unit)

        ! Create main.f90
        open (newunit=unit, file=trim(dir_path)//'/main.f90', status='replace')
        write (unit, '(a)') 'program bench_modules'
        write (unit, '(a)') '  use module2'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer :: result'
        write (unit, '(a)') '  result = mod2_func(10)'
        write (unit, '(a)') '  print *, "Result:", result'
        write (unit, '(a)') 'end program bench_modules'
        close (unit)

    end subroutine create_modules_test_files

    subroutine modify_main_file(file_path)
        character(len=*), intent(in) :: file_path
        integer :: unit

        ! Append a comment to trigger recompilation
        open (newunit=unit, file=file_path, position='append')
        write (unit, '(a)') '! Modified for incremental build test'
        close (unit)

    end subroutine modify_main_file

    subroutine read_file_content(filename, content)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: content
        integer :: unit, iostat
        character(len=256) :: line

        content = ''
        open (newunit=unit, file=filename, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                content = trim(content)//' '//trim(line)
            end do
            close (unit)
        end if
    end subroutine read_file_content

end program test_benchmarks
