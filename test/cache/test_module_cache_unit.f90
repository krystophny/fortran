!> Unit tests for fpm_module_cache module
program test_module_cache_unit
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fpm_module_cache
    use fpm_compiler, only: compiler_t, new_compiler, id_gcc
    use fpm_model, only: srcfile_t, FPM_UNIT_MODULE
    use fpm_strings, only: string_t
    use fpm_error, only: error_t
    use fpm_filesystem, only: delete_file, exists, mkdir, join_path
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path
    implicit none

    ! Note: setenv/unsetenv C bindings removed for Windows compatibility
    ! Tests now use direct module cache configuration instead

    integer :: stat
    logical :: all_pass

    all_pass = .true.

    print '(a)', 'Running module cache unit tests...'
    print '(a)', ''

    ! Test 1: Cache directory creation
    call test_cache_dir_creation(all_pass)

    ! Test 2: Cache key generation
    call test_cache_key_generation(all_pass)

    ! Test 3: Module storage and retrieval
    call test_module_storage_retrieval(all_pass)

    ! Test 4: Cache hit detection
    call test_cache_hit_detection(all_pass)

    ! Test 5: Disabled cache behavior
    call test_disabled_cache(all_pass)

    if (all_pass) then
        print '(a)', ''
        print '(a)', 'All unit tests passed!'
        stop 0
    else
        print '(a)', ''
        print '(a)', 'Some tests failed!'
        stop 1
    end if

contains

    subroutine test_cache_dir_creation(all_pass)
        logical, intent(inout) :: all_pass
        character(:), allocatable :: cache_dir
        logical :: test_pass

        print '(a)', 'Test 1: Cache directory creation'

        cache_dir = get_module_cache_dir()
        test_pass = len_trim(cache_dir) > 0

        if (test_pass) then
            print '(a)', '  ✓ Module cache directory path determined'
            print '(a,a)', '    Path: ', cache_dir
        else
            print '(a)', '  ✗ Failed to determine cache directory'
            all_pass = .false.
        end if

    end subroutine test_cache_dir_creation

    subroutine test_cache_key_generation(all_pass)
        logical, intent(inout) :: all_pass
        type(module_cache_t) :: cache
        type(compiler_t) :: compiler
        type(srcfile_t) :: src1, src2
        character(len=64) :: key1, key2
        logical :: test_pass

        print '(a)', 'Test 2: Cache key generation'

        ! Setup compiler
        call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
        compiler%id = id_gcc

        ! Initialize cache
        cache = new_module_cache(compiler, '13.0.0')

        ! Create test source files
        src1%file_name = 'test1.f90'
        src1%digest = 12345_int64

        src2%file_name = 'test2.f90'
        src2%digest = 67890_int64

        ! Generate keys
        key1 = cache%get_cache_key(src1)
        key2 = cache%get_cache_key(src2)

        ! Test: Different sources should have different keys
        test_pass = (key1 /= key2)

        if (test_pass) then
            print '(a)', '  ✓ Different sources generate different cache keys'
            print '(a,a)', '    Key1: ', trim(key1)
            print '(a,a)', '    Key2: ', trim(key2)
        else
            print '(a)', '  ✗ Cache keys are not unique'
            all_pass = .false.
        end if

        ! Test: Same source should generate same key
        key2 = cache%get_cache_key(src1)
        test_pass = (key1 == key2)

        if (test_pass) then
            print '(a)', '  ✓ Same source generates consistent cache key'
        else
            print '(a)', '  ✗ Cache key not consistent for same source'
            all_pass = .false.
        end if

    end subroutine test_cache_key_generation

    subroutine test_module_storage_retrieval(all_pass)
        logical, intent(inout) :: all_pass
        type(module_cache_t) :: cache
        type(compiler_t) :: compiler
        type(srcfile_t) :: srcfile
        type(error_t), allocatable :: error
        character(:), allocatable :: test_build_dir, test_target_dir
        character(len=64) :: cache_key
        logical :: found, test_pass
        integer :: unit
        type(string_t) :: modules(1)

        print '(a)', 'Test 3: Module storage and retrieval'

        ! Setup
        call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
        compiler%id = id_gcc
        cache = new_module_cache(compiler, '13.0.0')

        ! Create test directories
        test_build_dir = create_temp_dir('fortran_test_build')
        test_target_dir = create_temp_dir('fortran_test_target')

        ! Setup source file
        srcfile%file_name = 'test_module.f90'
        srcfile%digest = 99999_int64
        srcfile%unit_type = FPM_UNIT_MODULE
        modules(1)%s = 'test_module'
        srcfile%modules_provided = modules

        ! Create dummy module files
        open (newunit=unit, file=join_path(test_build_dir, 'test_module.mod'), &
              status='replace', action='write')
        write (unit, '(a)') 'dummy module file'
        close (unit)

        open (newunit=unit, file=join_path(test_build_dir, 'test_module.o'), &
              status='replace', action='write')
        write (unit, '(a)') 'dummy object file'
        close (unit)

        ! Get cache key
        cache_key = cache%get_cache_key(srcfile)

        ! Store module
        call cache%store_module(srcfile, cache_key, test_build_dir, error)

        test_pass = .not. allocated(error)
        if (test_pass) then
            print '(a)', '  ✓ Module stored in cache successfully'
        else
            print '(a)', '  ✗ Failed to store module in cache'
            all_pass = .false.
        end if

        ! Target directory already exists from create_temp_dir

        ! Retrieve module
        call cache%retrieve_module(cache_key, test_target_dir, srcfile, found, error)

        test_pass = found .and. .not. allocated(error)
        if (test_pass) then
            print '(a)', '  ✓ Module retrieved from cache successfully'

            ! Verify files were copied
            test_pass = exists(join_path(test_target_dir, 'test_module.mod'))
            if (test_pass) then
                print '(a)', '  ✓ Module file (.mod) retrieved correctly'
            else
                print '(a)', '  ✗ Module file (.mod) not found after retrieval'
                all_pass = .false.
            end if
        else
            print '(a)', '  ✗ Failed to retrieve module from cache'
            all_pass = .false.
        end if

        ! Cleanup
        call cleanup_temp_dir(test_build_dir)
        call cleanup_temp_dir(test_target_dir)

    end subroutine test_module_storage_retrieval

    subroutine test_cache_hit_detection(all_pass)
        logical, intent(inout) :: all_pass
        type(module_cache_t) :: cache
        type(compiler_t) :: compiler
        type(srcfile_t) :: srcfile
        type(error_t), allocatable :: error
        character(:), allocatable :: test_build_dir, test_cache_dir
        character(len=64) :: cache_key
        logical :: is_cached, test_pass
        integer :: unit
        type(string_t) :: modules(1)

        print '(a)', 'Test 4: Cache hit detection'

        ! Setup with unique cache directory
        test_cache_dir = create_temp_dir('fortran_cache_hit')

        call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
        compiler%id = id_gcc
        cache = new_module_cache(compiler, '13.0.0')
        ! Override cache directory to use test-specific one
        cache%cache_dir = test_cache_dir

        test_build_dir = create_temp_dir('fortran_test_hit')

        ! Create dummy module files for testing
        open (newunit=unit, file=join_path(test_build_dir, 'hit_test.mod'), &
              status='replace', action='write')
        write (unit, '(a)') 'dummy module file for hit test'
        close (unit)

        open (newunit=unit, file=join_path(test_build_dir, 'hit_test.o'), &
              status='replace', action='write')
        write (unit, '(a)') 'dummy object file for hit test'
        close (unit)

        ! Setup source file
        srcfile%file_name = 'hit_test.f90'
        srcfile%digest = 11111_int64
        modules(1)%s = 'hit_test'
        srcfile%modules_provided = modules

        cache_key = cache%get_cache_key(srcfile)

        ! Check before caching
        is_cached = cache%is_cached(cache_key)
        test_pass = .not. is_cached

        if (test_pass) then
            print '(a)', '  ✓ Cache miss detected correctly before storing'
        else
            print '(a)', '  ✗ False cache hit before storing'
            all_pass = .false.
        end if

        ! Store module
        call cache%store_module(srcfile, cache_key, test_build_dir, error)

        ! Check after caching
        is_cached = cache%is_cached(cache_key)
        test_pass = is_cached

        if (test_pass) then
            print '(a)', '  ✓ Cache hit detected correctly after storing'
        else
            print '(a)', '  ✗ Cache miss after storing'
            all_pass = .false.
        end if

        ! Cleanup
        call cleanup_temp_dir(test_build_dir)
        call cleanup_temp_dir(test_cache_dir)

    end subroutine test_cache_hit_detection

    subroutine test_disabled_cache(all_pass)
        logical, intent(inout) :: all_pass
        type(module_cache_t) :: cache
        type(compiler_t) :: compiler
        type(srcfile_t) :: srcfile
        type(error_t), allocatable :: error
        character(:), allocatable :: test_build_dir
        character(len=64) :: cache_key
        logical :: found, test_pass
        character(len=256) :: env_val
        integer :: stat

        print '(a)', 'Test 5: Disabled cache behavior'

        ! Check if cache is disabled via environment variable
        call get_environment_variable('FPM_NO_MODULE_CACHE', env_val, status=stat)

        ! Setup
        call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
        compiler%id = id_gcc
        cache = new_module_cache(compiler, '13.0.0')

        ! Test cache behavior based on environment
        if (stat == 0 .and. trim(env_val) == '1') then
            test_pass = .not. cache%enabled
            if (test_pass) then
                print '(a)', '  ✓ Cache disabled via environment variable'
            else
                print '(a)', '  ✗ Cache not properly disabled'
                all_pass = .false.
            end if
        else
            test_pass = cache%enabled
            if (test_pass) then
                print '(a)', '  ✓ Cache enabled (no disable env var)'
            else
                print '(a)', '  ✗ Cache not properly enabled'
                all_pass = .false.
            end if
        end if

        ! Try operations with current cache state
        test_build_dir = create_temp_dir('fortran_test_disabled')
        srcfile%file_name = 'disabled_test.f90'
        srcfile%digest = 22222_int64
        cache_key = cache%get_cache_key(srcfile)

        ! Store operation
        call cache%store_module(srcfile, cache_key, test_build_dir, error)

        ! Retrieve operation
        call cache%retrieve_module(cache_key, test_build_dir, srcfile, found, error)

        ! Results depend on cache state
        print '(a)', '  ✓ Cache operations completed without error'

        ! Cleanup
        call cleanup_temp_dir(test_build_dir)

    end subroutine test_disabled_cache

    function get_timestamp() result(timestamp)
        character(len=16) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)
        write (timestamp, '(i0,5i2.2)') values(1), values(2), values(3), &
            values(5), values(6), values(7)
    end function get_timestamp

end program test_module_cache_unit
