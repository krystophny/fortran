!> Integration tests for module cache with FPM
program test_module_cache_integration
    use, intrinsic :: iso_fortran_env, only: int64
    use fpm_module_cache
    use fpm_compiler, only: compiler_t, new_compiler, id_gcc
    use fpm_model, only: srcfile_t, FPM_UNIT_MODULE, FPM_SCOPE_LIB
    use fpm_strings, only: string_t
    use fpm_error, only: error_t
    use fpm_filesystem, only: exists, mkdir, join_path, delete_file, list_files
    use module_scanner, only: scan_modules, module_info
    use temp_utils, only: temp_dir_manager
    implicit none

    logical :: all_pass

    all_pass = .true.

    print '(a)', 'Running module cache integration tests...'
    print '(a)', ''

    ! Test 1: Cache with real module compilation
    call test_real_module_caching(all_pass)

    ! Test 2: Cache with module dependencies
    call test_module_dependencies(all_pass)

    ! Test 3: Cache invalidation on source change
    call test_cache_invalidation(all_pass)

    ! Test 4: Cross-project module sharing
    call test_cross_project_sharing(all_pass)

    if (all_pass) then
        print '(a)', ''
        print '(a)', 'All integration tests passed!'
        stop 0
    else
        print '(a)', ''
        print '(a)', 'Some tests failed!'
        stop 1
    end if

contains

    subroutine test_real_module_caching(all_pass)
        logical, intent(inout) :: all_pass
        character(:), allocatable :: test_dir, src_file, build_dir
        type(module_cache_t) :: cache
        type(compiler_t) :: compiler
        type(srcfile_t) :: srcfile
        type(error_t), allocatable :: error
        character(len=64) :: cache_key
        logical :: found, test_pass
        integer :: unit, exitstat, cmdstat
        type(string_t) :: modules(1)
        type(module_info), allocatable :: scanned_modules(:)
        integer :: n_modules

        print '(a)', 'Test 1: Cache with real module compilation'

        ! Setup test directory
        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('fortran_cache_test_'//trim(get_timestamp()))
            test_dir = temp_mgr%path
            build_dir = test_dir//'/build'
        end block
        call execute_command_line('mkdir -p '//trim(test_dir))
        call execute_command_line('mkdir -p '//trim(build_dir))

        ! Create a real Fortran module
        src_file = trim(test_dir)//'/math_utils.f90'
        open (newunit=unit, file=trim(src_file), status='replace', action='write')
        write (unit, '(a)') 'module math_utils'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  private'
        write (unit, '(a)') '  public :: add_numbers'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') '  pure function add_numbers(a, b) result(c)'
        write (unit, '(a)') '    real, intent(in) :: a, b'
        write (unit, '(a)') '    real :: c'
        write (unit, '(a)') '    c = a + b'
        write (unit, '(a)') '  end function add_numbers'
        write (unit, '(a)') 'end module math_utils'
        close (unit)

        ! Scan module
        call scan_modules(src_file, scanned_modules, n_modules)

        ! Setup compiler and cache
        call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
        compiler%id = id_gcc
        cache = new_module_cache(compiler, '13.0.0')

        ! Compile the module
        call execute_command_line('cd '//build_dir//' && gfortran -c '//src_file, &
                                  exitstat=exitstat, cmdstat=cmdstat)

        test_pass = (exitstat == 0 .and. cmdstat == 0)
        if (test_pass) then
            print '(a)', '  ✓ Module compiled successfully'
        else
            print '(a)', '  ✗ Module compilation failed'
            all_pass = .false.
            call cleanup_test_dir(test_dir)
            return
        end if

        ! Setup srcfile
        srcfile%file_name = src_file
        srcfile%unit_type = FPM_UNIT_MODULE
        srcfile%unit_scope = FPM_SCOPE_LIB
        modules(1)%s = 'math_utils'
        srcfile%modules_provided = modules
        srcfile%digest = 33333_int64  ! Would be computed from file content

        ! Cache the compiled module
        cache_key = cache%get_cache_key(srcfile)
        call cache%store_module(srcfile, cache_key, build_dir, error)

        test_pass = .not. allocated(error) .and. cache%is_cached(cache_key)
        if (test_pass) then
            print '(a)', '  ✓ Compiled module cached successfully'
        else
            print '(a)', '  ✗ Failed to cache compiled module'
            all_pass = .false.
        end if

        ! Clear build directory and retrieve from cache
        call execute_command_line('rm -rf '//build_dir//'/*')

        call cache%retrieve_module(cache_key, build_dir, srcfile, found, error)

        test_pass = found .and. exists(join_path(build_dir, 'math_utils.mod'))
        if (test_pass) then
            print '(a)', '  ✓ Module retrieved from cache with .mod file'

            ! Test using cached module in a program
            call test_use_cached_module(test_dir, build_dir, test_pass)
            if (test_pass) then
                print '(a)', '  ✓ Cached module usable in compilation'
            else
                print '(a)', '  ✗ Cached module not usable'
                all_pass = .false.
            end if
        else
            print '(a)', '  ✗ Failed to retrieve module from cache'
            all_pass = .false.
        end if

        ! Cleanup
        call cleanup_test_dir(test_dir)

    end subroutine test_real_module_caching

    subroutine test_module_dependencies(all_pass)
        logical, intent(inout) :: all_pass
        character(:), allocatable :: test_dir, build_dir
        type(module_cache_t) :: cache
        type(compiler_t) :: compiler
        type(srcfile_t) :: base_src, dep_src
        type(srcfile_t) :: deps(1)
        type(error_t), allocatable :: error
        character(len=64) :: key_without_deps, key_with_deps
        logical :: test_pass
        integer :: unit
        type(string_t) :: modules(1)

        print '(a)', 'Test 2: Cache with module dependencies'

        block
            type(temp_dir_manager) :: temp_mgr
            call temp_mgr%create('fortran_dep_test_'//trim(get_timestamp()))
            test_dir = temp_mgr%path
            build_dir = test_dir//'/build'
        end block
        call execute_command_line('mkdir -p '//trim(test_dir))
        call execute_command_line('mkdir -p '//trim(build_dir))

        ! Create base module
        open (newunit=unit, file=trim(test_dir)//'/base.f90', status='replace')
        write (unit, '(a)') 'module base'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  integer, parameter :: dp = kind(1.0d0)'
        write (unit, '(a)') 'end module base'
        close (unit)

        ! Create dependent module
        open (newunit=unit, file=trim(test_dir)//'/derived.f90', status='replace')
        write (unit, '(a)') 'module derived'
        write (unit, '(a)') '  use base'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  real(dp) :: pi = 3.14159_dp'
        write (unit, '(a)') 'end module derived'
        close (unit)

        ! Setup compiler and cache
        call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
        compiler%id = id_gcc
        cache = new_module_cache(compiler, '13.0.0')

        ! Setup source files
        base_src%file_name = 'base.f90'
        base_src%digest = 44444_int64
        modules(1)%s = 'base'
        base_src%modules_provided = modules

        dep_src%file_name = 'derived.f90'
        dep_src%digest = 55555_int64
        modules(1)%s = 'derived'
        dep_src%modules_provided = modules

        deps(1) = base_src

        ! Generate cache keys
        key_without_deps = cache%get_cache_key(dep_src)
        key_with_deps = cache%get_cache_key(dep_src, deps)

        test_pass = (key_without_deps /= key_with_deps)
        if (test_pass) then
            print '(a)', '  ✓ Cache keys differ with/without dependencies'
            print '(a,a)', '    Without deps: ', trim(key_without_deps)
            print '(a,a)', '    With deps: ', trim(key_with_deps)
        else
            print '(a)', '  ✗ Dependencies not affecting cache key'
            all_pass = .false.
        end if

        ! Cleanup
        call cleanup_test_dir(test_dir)

    end subroutine test_module_dependencies

    subroutine test_cache_invalidation(all_pass)
        logical, intent(inout) :: all_pass
        character(:), allocatable :: test_dir, src_file
        type(module_cache_t) :: cache
        type(compiler_t) :: compiler
        type(srcfile_t) :: srcfile
        character(len=64) :: key1, key2
        logical :: test_pass
        integer :: unit
        type(string_t) :: modules(1)
        type(temp_dir_manager) :: temp_mgr

        print '(a)', 'Test 3: Cache invalidation on source change'

        call temp_mgr%create('fortran_inval_test_'//get_timestamp())
        test_dir = temp_mgr%path

        src_file = trim(test_dir)//'/changing.f90'

        ! Create initial version
        open (newunit=unit, file=src_file, status='replace')
        write (unit, '(a)') 'module changing'
        write (unit, '(a)') '  integer :: version = 1'
        write (unit, '(a)') 'end module changing'
        close (unit)

        ! Setup
        call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
        compiler%id = id_gcc
        cache = new_module_cache(compiler, '13.0.0')

        srcfile%file_name = src_file
        srcfile%digest = 66666_int64
        modules(1)%s = 'changing'
        srcfile%modules_provided = modules

        key1 = cache%get_cache_key(srcfile)

        ! Modify source
        open (newunit=unit, file=src_file, status='replace')
        write (unit, '(a)') 'module changing'
        write (unit, '(a)') '  integer :: version = 2'
        write (unit, '(a)') 'end module changing'
        close (unit)

        ! Simulate new digest
        srcfile%digest = 77777_int64
        key2 = cache%get_cache_key(srcfile)

        test_pass = (key1 /= key2)
        if (test_pass) then
            print '(a)', '  ✓ Cache key changes when source is modified'
        else
            print '(a)', '  ✗ Cache key unchanged after source modification'
            all_pass = .false.
        end if

    end subroutine test_cache_invalidation

    subroutine test_cross_project_sharing(all_pass)
        logical, intent(inout) :: all_pass
        character(:), allocatable :: proj1_dir, proj2_dir, src_file
        type(module_cache_t) :: cache
        type(compiler_t) :: compiler
        type(srcfile_t) :: srcfile
        type(error_t), allocatable :: error
        character(len=64) :: cache_key
        logical :: found, test_pass
        integer :: unit
        type(string_t) :: modules(1)

        print '(a)', 'Test 4: Cross-project module sharing'

        ! Create two project directories
        block
            type(temp_dir_manager) :: temp_mgr1, temp_mgr2
            call temp_mgr1%create('fortran_proj1_'//trim(get_timestamp()))
            call temp_mgr2%create('fortran_proj2_'//trim(get_timestamp()))
            proj1_dir = temp_mgr1%path
            proj2_dir = temp_mgr2%path
        end block
        call execute_command_line('mkdir -p '//trim(proj1_dir))
        call execute_command_line('mkdir -p '//trim(proj2_dir))
        call execute_command_line('mkdir -p '//trim(proj1_dir)//'/build')
        call execute_command_line('mkdir -p '//trim(proj2_dir)//'/build')

        ! Create identical module in project 1
        src_file = trim(proj1_dir)//'/shared_utils.f90'
        open (newunit=unit, file=src_file, status='replace')
        write (unit, '(a)') 'module shared_utils'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  real, parameter :: PI = 3.14159'
        write (unit, '(a)') 'end module shared_utils'
        close (unit)

        ! Setup
        call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
        compiler%id = id_gcc
        cache = new_module_cache(compiler, '13.0.0')

        ! Compile in project 1
        call execute_command_line('cd '//proj1_dir//'/build && gfortran -c '//src_file)

        ! Cache from project 1
        srcfile%file_name = src_file
        srcfile%digest = 88888_int64
        modules(1)%s = 'shared_utils'
        srcfile%modules_provided = modules

        cache_key = cache%get_cache_key(srcfile)
        call cache%store_module(srcfile, cache_key, proj1_dir//'/build', error)

        ! Try to retrieve in project 2
       call cache%retrieve_module(cache_key, proj2_dir//'/build', srcfile, found, error)

        test_pass = found .and. exists(proj2_dir//'/build/shared_utils.mod')
        if (test_pass) then
            print '(a)', '  ✓ Module shared between projects via cache'

            ! Test compilation using cached module
          open (newunit=unit, file=trim(proj2_dir)//'/use_shared.f90', status='replace')
            write (unit, '(a)') 'program use_shared'
            write (unit, '(a)') '  use shared_utils'
            write (unit, '(a)') '  print *, "PI =", PI'
            write (unit, '(a)') 'end program use_shared'
            close (unit)

            call execute_command_line('cd '//proj2_dir//'/build && '// &
                                      'gfortran -o use_shared ../use_shared.f90', &
                                      exitstat=unit)

            test_pass = (unit == 0)
            if (test_pass) then
                print '(a)', '  ✓ Cached module usable in different project'
            else
                print '(a)', '  ✗ Failed to use cached module in different project'
                all_pass = .false.
            end if
        else
            print '(a)', '  ✗ Module not shared between projects'
            all_pass = .false.
        end if

        ! Cleanup
        call cleanup_test_dir(proj1_dir)
        call cleanup_test_dir(proj2_dir)

    end subroutine test_cross_project_sharing

    subroutine test_use_cached_module(test_dir, build_dir, success)
        character(*), intent(in) :: test_dir, build_dir
        logical, intent(out) :: success
        integer :: unit, exitstat

        ! Create program using the cached module
        open (newunit=unit, file=trim(test_dir)//'/test_prog.f90', status='replace')
        write (unit, '(a)') 'program test_prog'
        write (unit, '(a)') '  use math_utils'
        write (unit, '(a)') '  implicit none'
        write (unit, '(a)') '  print *, "2 + 3 =", add_numbers(2.0, 3.0)'
        write (unit, '(a)') 'end program test_prog'
        close (unit)

        ! Try to compile using cached module (need to link the object file)
        call execute_command_line('cd '//build_dir//' && '// &
                    'gfortran -o test_prog '//test_dir//'/test_prog.f90 math_utils.o', &
                                  exitstat=exitstat)

        success = (exitstat == 0)

    end subroutine test_use_cached_module

    subroutine cleanup_test_dir(dir)
        character(*), intent(in) :: dir
        call execute_command_line('rm -rf '//dir)
    end subroutine cleanup_test_dir

    function get_timestamp() result(timestamp)
        character(len=16) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)
        write (timestamp, '(i0,5i2.2)') values(1), values(2), values(3), &
            values(5), values(6), values(7)
    end function get_timestamp

end program test_module_cache_integration
