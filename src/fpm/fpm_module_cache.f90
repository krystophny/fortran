!> Module-level caching for FPM builds
!>
!> This module implements a shared cache for compiled Fortran modules (.mod and .o files)
!> that can be reused across different projects. It follows FPM conventions and integrates
!> with the FPM source model.
!>
!> The cache structure is:
!>   ~/.cache/fortran/modules/
!>     <compiler_id>/
!>       <compiler_version>/
!>         <module_hash>/
!>           module.mod
!>           module.o
!>           metadata.json
!>
module fpm_module_cache
    use, intrinsic :: iso_fortran_env, only: int64
    use fpm_filesystem, only: exists, mkdir, join_path, list_files
    use fpm_strings, only: string_t, str
    use fpm_environment, only: get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
    use fpm_compiler, only: compiler_t, id_gcc, id_intel_classic_nix, &
                            id_intel_classic_mac, id_intel_classic_windows, &
                            id_intel_llvm_nix, id_intel_llvm_windows, &
                            id_nvhpc, id_nag, id_lfortran
    use fpm_model, only: srcfile_t
    use fpm_error, only: error_t
    implicit none
    private

    public :: module_cache_t
    public :: new_module_cache
    public :: get_module_cache_dir

    !> Type for managing module-level caching
    type :: module_cache_t
        !> Base cache directory
        character(:), allocatable :: cache_dir

        !> Compiler identifier for cache segregation
        character(:), allocatable :: compiler_id

        !> Compiler version for cache segregation
        character(:), allocatable :: compiler_version

        !> Whether caching is enabled
        logical :: enabled = .true.

    contains
        procedure :: init => cache_init
        procedure :: get_cache_key => cache_get_key
        procedure :: store_module => cache_store_module
        procedure :: retrieve_module => cache_retrieve_module
        procedure :: is_cached => cache_is_cached
        procedure :: get_module_dir => cache_get_module_dir
        procedure :: clean_stale => cache_clean_stale
        procedure :: deep_copy => cache_deep_copy
        procedure :: assign => cache_assign
        generic :: assignment(=) => assign
    end type module_cache_t

contains

    !> Create a new module cache instance
    function new_module_cache(compiler, compiler_version) result(cache)
        type(compiler_t), intent(in) :: compiler
        character(*), intent(in), optional :: compiler_version
        type(module_cache_t) :: cache

        call cache%init(compiler, compiler_version)

    end function new_module_cache

    !> Get the default module cache directory
    function get_module_cache_dir() result(dir)
        character(:), allocatable :: dir
        character(len=256) :: home_dir
        integer :: stat

        ! Try XDG_CACHE_HOME first
        call get_environment_variable('XDG_CACHE_HOME', home_dir, status=stat)

        if (stat == 0 .and. len_trim(home_dir) > 0) then
            dir = trim(home_dir)//'/fortran/modules'
        else
            ! Fallback to HOME
            call get_environment_variable('HOME', home_dir, status=stat)

            if (stat == 0) then
                select case (get_os_type())
                case (OS_LINUX, OS_MACOS)
                    dir = trim(home_dir)//'/.cache/fortran/modules'
                case (OS_WINDOWS)
                    call get_environment_variable('LOCALAPPDATA', home_dir, status=stat)
                    if (stat == 0) then
                        dir = trim(home_dir)//'\fortran\cache\modules'
                    else
                        dir = '.fortran-module-cache'
                    end if
                case default
                    dir = '.fortran-module-cache'
                end select
            else
                dir = '.fortran-module-cache'
            end if
        end if

    end function get_module_cache_dir

    !> Initialize the module cache
    subroutine cache_init(this, compiler, compiler_version)
        class(module_cache_t), intent(inout) :: this
        type(compiler_t), intent(in) :: compiler
        character(*), intent(in), optional :: compiler_version
        character(len=256) :: env_val
        integer :: stat

        ! Set cache directory
        this%cache_dir = get_module_cache_dir()

        ! Extract compiler ID
        select case (compiler%id)
        case (id_gcc)
            this%compiler_id = 'gfortran'
        case (id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows)
            this%compiler_id = 'ifort'
        case (id_intel_llvm_nix, id_intel_llvm_windows)
            this%compiler_id = 'ifx'
        case (id_nvhpc)
            this%compiler_id = 'nvfortran'
        case (id_nag)
            this%compiler_id = 'nagfor'
        case (id_lfortran)
            this%compiler_id = 'lfortran'
        case default
            this%compiler_id = 'unknown'
        end select

        ! Set compiler version
        if (present(compiler_version)) then
            this%compiler_version = compiler_version
        else
            this%compiler_version = 'default'
        end if

        ! Check if caching is disabled via environment
        call get_environment_variable('FPM_NO_MODULE_CACHE', env_val, status=stat)
        if (stat == 0 .and. len_trim(env_val) > 0) then
            this%enabled = .false.
        end if

        ! Create cache directory structure if enabled
        if (this%enabled) then
            call ensure_cache_dirs(this)
        end if

    end subroutine cache_init

    !> Generate cache key for a source file
    function cache_get_key(this, srcfile, dependencies) result(key)
        class(module_cache_t), intent(in) :: this
        type(srcfile_t), intent(in) :: srcfile
        type(srcfile_t), intent(in), optional :: dependencies(:)
        character(len=64) :: key
        integer(int64) :: hash_val
        character(len=16) :: hex_str
        integer :: i

        ! Start with source file digest
        hash_val = srcfile%digest

        ! Include dependency digests if provided
        if (present(dependencies)) then
            do i = 1, size(dependencies)
                hash_val = ieor(hash_val, dependencies(i)%digest)
            end do
        end if

        ! Include compiler flags in hash (simplified for now)
        ! In a full implementation, we'd hash relevant compiler flags

        ! Convert to hex string
        write (hex_str, '(z16.16)') hash_val
        key = hex_str

    end function cache_get_key

    !> Store a compiled module in the cache
    subroutine cache_store_module(this, srcfile, cache_key, build_dir, error)
        class(module_cache_t), intent(in) :: this
        type(srcfile_t), intent(in) :: srcfile
        character(*), intent(in) :: cache_key
        character(*), intent(in) :: build_dir
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: module_dir, src_file, dst_file
        type(string_t), allocatable :: module_files(:)
        integer :: i, unit, iostat
        logical :: found_files

        if (.not. this%enabled) return

        ! Get cache directory for this module
        module_dir = this%get_module_dir(cache_key)

        ! Create directory
        call mkdir(module_dir)

        found_files = .false.

        ! Copy .mod files for each provided module
        if (allocated(srcfile%modules_provided)) then
            do i = 1, size(srcfile%modules_provided)
                ! Look for .mod file
                src_file = join_path(build_dir, srcfile%modules_provided(i)%s//'.mod')
                if (exists(src_file)) then
                 dst_file = join_path(module_dir, srcfile%modules_provided(i)%s//'.mod')
                    call copy_file(src_file, dst_file, iostat)
                    if (iostat == 0) found_files = .true.
                end if

                ! Also look for .smod file (submodules)
                src_file = join_path(build_dir, srcfile%modules_provided(i)%s//'.smod')
                if (exists(src_file)) then
                dst_file = join_path(module_dir, srcfile%modules_provided(i)%s//'.smod')
                    call copy_file(src_file, dst_file, iostat)
                    if (iostat == 0) found_files = .true.
                end if
            end do
        end if

        ! Copy object file
        src_file = get_object_name(srcfile%file_name, build_dir)
        if (exists(src_file)) then
            dst_file = join_path(module_dir, 'module.o')
            call copy_file(src_file, dst_file, iostat)
            if (iostat == 0) found_files = .true.
        end if

        ! Write metadata
        if (found_files) then
            call write_cache_metadata(module_dir, srcfile, cache_key)
        end if

    end subroutine cache_store_module

    !> Retrieve a cached module
    subroutine cache_retrieve_module(this, cache_key, target_dir, srcfile, found, error)
        class(module_cache_t), intent(in) :: this
        character(*), intent(in) :: cache_key
        character(*), intent(in) :: target_dir
        type(srcfile_t), intent(in) :: srcfile
        logical, intent(out) :: found
        type(error_t), allocatable, intent(out) :: error

        character(:), allocatable :: module_dir, src_file, dst_file
        type(string_t), allocatable :: cached_files(:)
        integer :: i, iostat

        found = .false.
        if (.not. this%enabled) return

        module_dir = this%get_module_dir(cache_key)

        ! Check if cache directory exists
        if (.not. exists(module_dir)) return

        ! Copy module files
        if (allocated(srcfile%modules_provided)) then
            do i = 1, size(srcfile%modules_provided)
                ! Copy .mod file
                src_file = join_path(module_dir, srcfile%modules_provided(i)%s//'.mod')
                if (exists(src_file)) then
                 dst_file = join_path(target_dir, srcfile%modules_provided(i)%s//'.mod')
                    call copy_file(src_file, dst_file, iostat)
                    if (iostat == 0) found = .true.
                end if

                ! Copy .smod file if exists
                src_file = join_path(module_dir, srcfile%modules_provided(i)%s//'.smod')
                if (exists(src_file)) then
                dst_file = join_path(target_dir, srcfile%modules_provided(i)%s//'.smod')
                    call copy_file(src_file, dst_file, iostat)
                end if
            end do
        end if

        ! Copy object file
        src_file = join_path(module_dir, 'module.o')
        if (exists(src_file)) then
            dst_file = get_object_name(srcfile%file_name, target_dir)
            call copy_file(src_file, dst_file, iostat)
            if (iostat == 0) found = .true.
        end if

    end subroutine cache_retrieve_module

    !> Check if a module is cached
    function cache_is_cached(this, cache_key) result(is_cached)
        class(module_cache_t), intent(in) :: this
        character(*), intent(in) :: cache_key
        logical :: is_cached
        character(:), allocatable :: module_dir, metadata_file

        is_cached = .false.
        if (.not. this%enabled) return

        module_dir = this%get_module_dir(cache_key)

        ! Check if directory exists AND contains metadata file
        if (exists(module_dir)) then
            metadata_file = join_path(module_dir, 'metadata.txt')
            is_cached = exists(metadata_file)
        end if

    end function cache_is_cached

    !> Get the cache directory for a specific module
    function cache_get_module_dir(this, cache_key) result(dir)
        class(module_cache_t), intent(in) :: this
        character(*), intent(in) :: cache_key
        character(:), allocatable :: dir

        dir = join_path(this%cache_dir, this%compiler_id)
        dir = join_path(dir, this%compiler_version)
        dir = join_path(dir, trim(cache_key))

    end function cache_get_module_dir

    !> Clean stale cache entries
    subroutine cache_clean_stale(this, max_age_days, error)
        class(module_cache_t), intent(in) :: this
        integer, intent(in) :: max_age_days
        type(error_t), allocatable, intent(out) :: error

        ! Implementation: Check metadata timestamps and remove entries older than max_age_days
        character(len=:), allocatable :: cache_dir, metadata_file
        character(len=256) :: line
        integer :: unit, iostat, entry_count, i
        logical :: file_exists
        character(len=:), allocatable :: entry_lines(:)
        integer :: current_timestamp, entry_timestamp, max_age_seconds

        ! Get cache directory
        cache_dir = this%cache_dir
        metadata_file = cache_dir//'/metadata.txt'

        ! Check if metadata file exists
        inquire (file=metadata_file, exist=file_exists)
        if (.not. file_exists) return

        ! Calculate max age in seconds
        max_age_seconds = max_age_days*24*3600

        ! Get current timestamp (approximate)
        current_timestamp = get_current_timestamp()

        ! Read all entries from metadata file
        entry_count = 0
        open (newunit=unit, file=metadata_file, status='old', iostat=iostat)
        if (iostat /= 0) return

        ! Count entries first
        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            entry_count = entry_count + 1
        end do
        close (unit)

        if (entry_count == 0) return

        ! Read all entries
        allocate (character(len=256) :: entry_lines(entry_count))
        open (newunit=unit, file=metadata_file, status='old', iostat=iostat)
        if (iostat /= 0) return

        do i = 1, entry_count
            read (unit, '(a)', iostat=iostat) entry_lines(i)
            if (iostat /= 0) exit
        end do
        close (unit)

        ! Process entries and remove stale ones
        open (newunit=unit, file=metadata_file, status='replace', iostat=iostat)
        if (iostat /= 0) return

        do i = 1, entry_count
            ! Parse timestamp from entry line (format: "hash timestamp size")
            block
                integer :: space_pos
                character(len=:), allocatable :: timestamp_str

                space_pos = index(trim(entry_lines(i)), ' ')
                if (space_pos > 0) then
                    timestamp_str = trim(entry_lines(i) (space_pos + 1:))
                    space_pos = index(timestamp_str, ' ')
                    if (space_pos > 0) then
                        timestamp_str = timestamp_str(1:space_pos - 1)
                        read (timestamp_str, *, iostat=iostat) entry_timestamp
                        if (iostat == 0) then
                            ! Keep entry if it's newer than max_age
                         if (current_timestamp - entry_timestamp < max_age_seconds) then
                                write (unit, '(a)') trim(entry_lines(i))
                            else
                                ! Remove corresponding cache files
                                call remove_cache_entry(this, entry_lines(i))
                            end if
                        end if
                    end if
                end if
            end block
        end do
        close (unit)

    end subroutine cache_clean_stale

    !> Ensure cache directory structure exists
    subroutine ensure_cache_dirs(cache)
        type(module_cache_t), intent(in) :: cache
        character(:), allocatable :: dir

        ! Create base cache directory
        call mkdir(cache%cache_dir)

        ! Create compiler-specific directory
        dir = join_path(cache%cache_dir, cache%compiler_id)
        call mkdir(dir)

        ! Create version-specific directory
        dir = join_path(dir, cache%compiler_version)
        call mkdir(dir)

    end subroutine ensure_cache_dirs

    !> Copy a file
    subroutine copy_file(src, dst, iostat)
        character(*), intent(in) :: src, dst
        integer, intent(out) :: iostat
        character(len=512) :: command

        ! Use system copy command
        select case (get_os_type())
        case (OS_WINDOWS)
            write (command, '(a,a,a,a,a)') 'copy "', src, '" "', dst, '" >nul 2>&1'
        case default
            write (command, '(a,a,a,a,a)') 'cp "', src, '" "', dst, '" 2>/dev/null'
        end select

        call execute_command_line(command, exitstat=iostat)

    end subroutine copy_file

    !> Get object file name from source file name
    function get_object_name(source_file, build_dir) result(object_file)
        character(*), intent(in) :: source_file, build_dir
        character(:), allocatable :: object_file
        character(:), allocatable :: base_name
        integer :: i, last_slash

        ! Extract just the base filename from source_file (remove any path)
        last_slash = 0
        do i = len_trim(source_file), 1, -1
            if (source_file(i:i) == '/' .or. source_file(i:i) == '\') then
                last_slash = i
                exit
            end if
        end do

        if (last_slash > 0) then
            base_name = source_file(last_slash + 1:)
        else
            base_name = source_file
        end if

        ! Change extension to .o
        i = index(base_name, '.', back=.true.)
        if (i > 0) then
            object_file = join_path(build_dir, base_name(1:i - 1)//'.o')
        else
            object_file = join_path(build_dir, base_name//'.o')
        end if

    end function get_object_name

    !> Write cache metadata file
    subroutine write_cache_metadata(cache_dir, srcfile, cache_key)
        character(*), intent(in) :: cache_dir
        type(srcfile_t), intent(in) :: srcfile
        character(*), intent(in) :: cache_key

        character(:), allocatable :: metadata_file
        integer :: unit, i
        integer :: timestamp(8)

        metadata_file = join_path(cache_dir, 'metadata.txt')

        open (newunit=unit, file=metadata_file, status='replace', action='write')

        ! Write timestamp
        call date_and_time(values=timestamp)
      write (unit, '(a,i0,5(a,i2.2))') 'timestamp: ', timestamp(1), '-', timestamp(2), &
            '-', timestamp(3), ' ', timestamp(5), ':', &
            timestamp(6), ':', timestamp(7)

        ! Write cache key
        write (unit, '(a,a)') 'cache_key: ', cache_key

        ! Write source file
        write (unit, '(a,a)') 'source_file: ', srcfile%file_name

        ! Write digest
        write (unit, '(a,z16.16)') 'digest: ', srcfile%digest

        ! Write provided modules
        if (allocated(srcfile%modules_provided)) then
            write (unit, '(a)') 'modules_provided:'
            do i = 1, size(srcfile%modules_provided)
                write (unit, '(a,a)') '  - ', srcfile%modules_provided(i)%s
            end do
        end if

        close (unit)

    end subroutine write_cache_metadata

    !> Get current timestamp (approximate using system time)
    function get_current_timestamp() result(timestamp)
        integer :: timestamp
        integer :: values(8)

        ! Get current date and time
        call date_and_time(values=values)

        ! Convert to approximate timestamp (days since year 2000)
        timestamp = (values(1) - 2000)*365 + values(2)*30 + values(3)
        timestamp = timestamp*24*3600 + values(5)*3600 + values(6)*60 + values(7)

    end function get_current_timestamp

    !> Remove cache entry files
    subroutine remove_cache_entry(cache, entry_line)
        class(module_cache_t), intent(in) :: cache
        character(len=*), intent(in) :: entry_line

        character(len=:), allocatable :: hash_str, cache_file
        integer :: space_pos

        ! Extract hash from entry line (format: "hash timestamp size")
        space_pos = index(trim(entry_line), ' ')
        if (space_pos > 0) then
            hash_str = trim(entry_line(1:space_pos - 1))

            ! Construct cache file path
            cache_file = cache%cache_dir//'/'//hash_str//'.mod'

            ! Remove cache file
            call execute_command_line('rm -f "'//cache_file//'"')

            ! Also remove any .o files
            cache_file = cache%cache_dir//'/'//hash_str//'.o'
            call execute_command_line('rm -f "'//cache_file//'"')
        end if

    end subroutine remove_cache_entry

    !> Deep copy procedures for module_cache_t
    function cache_deep_copy(this) result(copy)
        class(module_cache_t), intent(in) :: this
        type(module_cache_t) :: copy

        copy%enabled = this%enabled

        if (allocated(this%cache_dir)) then
            copy%cache_dir = this%cache_dir
        end if

        if (allocated(this%compiler_id)) then
            copy%compiler_id = this%compiler_id
        end if

        if (allocated(this%compiler_version)) then
            copy%compiler_version = this%compiler_version
        end if
    end function cache_deep_copy

    subroutine cache_assign(lhs, rhs)
        class(module_cache_t), intent(out) :: lhs
        type(module_cache_t), intent(in) :: rhs

        lhs%enabled = rhs%enabled

        if (allocated(rhs%cache_dir)) then
            lhs%cache_dir = rhs%cache_dir
        end if

        if (allocated(rhs%compiler_id)) then
            lhs%compiler_id = rhs%compiler_id
        end if

        if (allocated(rhs%compiler_version)) then
            lhs%compiler_version = rhs%compiler_version
        end if
    end subroutine cache_assign

end module fpm_module_cache
