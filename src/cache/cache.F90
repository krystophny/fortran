module cache
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int64
    use fpm_sources, only: add_sources_from_dir
    use fpm_model, only: srcfile_t, FPM_SCOPE_APP
    use fpm_error, only: error_t
    use fpm_filesystem, only: list_files, read_lines, join_path, exists, run
    use fpm_environment, only: get_os_type, OS_WINDOWS
    use fpm_strings, only: string_t, fnv_1a
    use temp_utils, only: create_temp_dir, get_temp_file_path, create_temp_file
    use temp_utils, only: mkdir
    use system_utils, only: escape_shell_arg
    implicit none
    private
  public :: get_cache_dir, ensure_cache_dir, ensure_cache_structure, get_cache_subdir, &
            store_module_cache, store_executable_cache, get_cache_key, get_fpm_digest, &
      store_build_artifacts, retrieve_build_artifacts, cache_exists, invalidate_cache, &
             get_content_hash, get_single_file_content_hash, clear_cache, get_cache_info

contains

    function get_cache_dir() result(cache_dir)
        character(len=:), allocatable :: cache_dir
        character(len=:), allocatable :: temp_dir, home_dir
        character(len=256) :: temp_buffer, home_buffer
        integer :: status

        ! Try to get XDG_CACHE_HOME first (Linux standard)
        call get_environment_variable('XDG_CACHE_HOME', temp_buffer, status=status)

        if (status == 0 .and. len_trim(temp_buffer) > 0) then
            temp_dir = trim(temp_buffer)
            cache_dir = join_path(temp_dir, 'fortran')
        else
            ! Fallback to HOME directory
            call get_environment_variable('HOME', home_buffer, status=status)

            if (status == 0) then
                home_dir = trim(home_buffer)
                ! Linux/macOS: ~/.cache/fortran
                cache_dir = join_path(home_dir, '.cache', 'fortran')
            else
                ! Windows fallback: try LOCALAPPDATA
                call get_environment_variable('LOCALAPPDATA', temp_buffer, status=status)
                if (status == 0) then
                    temp_dir = trim(temp_buffer)
                    cache_dir = join_path(temp_dir, 'fortran', 'cache')
                else
                    ! Last resort - use system temp directory
                    block
                        use system_utils, only: sys_get_temp_dir
                        character(len=:), allocatable :: temp_path
                        
                        temp_path = sys_get_temp_dir()
                        if (len_trim(temp_path) > 0) then
                            cache_dir = join_path(temp_path, 'fortran-cache')
                        else
                            ! Ultimate fallback - use current directory
                            cache_dir = '.fortran-cache'
                        end if
                    end block
                end if
            end if
        end if

        ! Ensure result is trimmed to avoid issues with fixed-length strings
        cache_dir = trim(cache_dir)

    end function get_cache_dir

    subroutine ensure_cache_dir(cache_dir, success)
        character(len=*), intent(in) :: cache_dir
        logical, intent(out) :: success

        ! Check if directory already exists
        if (exists(trim(cache_dir))) then
            success = .true.
            return
        end if

        ! Create directory using our safe mkdir
        call mkdir(trim(cache_dir))

        ! Check if directory was created successfully
        success = exists(trim(cache_dir))

    end subroutine ensure_cache_dir

    subroutine ensure_cache_structure(cache_dir, success)
        character(len=*), intent(in) :: cache_dir
        logical, intent(out) :: success
 character(len=:), allocatable :: builds_dir, modules_dir, executables_dir, metadata_dir

        ! Create main cache directory first
        call ensure_cache_dir(cache_dir, success)
        if (.not. success) return

        ! Create subdirectories using cross-platform paths
        builds_dir = join_path(trim(cache_dir), 'builds')
        modules_dir = join_path(trim(cache_dir), 'modules')
        executables_dir = join_path(trim(cache_dir), 'executables')
        metadata_dir = join_path(trim(cache_dir), 'metadata')

        call mkdir(trim(builds_dir))
        call mkdir(trim(modules_dir))
        call mkdir(trim(executables_dir))
        call mkdir(trim(metadata_dir))

        ! Check if all directories were created successfully
        success = exists(trim(builds_dir)) .and. exists(trim(modules_dir)) .and. &
                  exists(trim(executables_dir)) .and. exists(trim(metadata_dir))

    end subroutine ensure_cache_structure

    function get_cache_subdir(subdir_name) result(subdir_path)
        character(len=*), intent(in) :: subdir_name
        character(len=:), allocatable :: subdir_path
        character(len=:), allocatable :: cache_dir

        cache_dir = get_cache_dir()
        subdir_path = join_path(trim(cache_dir), trim(subdir_name))

    end function get_cache_subdir

    subroutine store_module_cache(cache_key, module_files, success)
        character(len=*), intent(in) :: cache_key
        character(len=*), intent(in) :: module_files(:)
        logical, intent(out) :: success
        character(len=:), allocatable :: modules_dir, dest_file, command
        integer :: i, exitstat

        ! Get modules cache directory
        modules_dir = get_cache_subdir('modules')

        ! Create cache key subdirectory
        modules_dir = join_path(trim(modules_dir), trim(cache_key))
        call ensure_cache_dir(modules_dir, success)
        if (.not. success) return

        ! Copy each module file using cross-platform commands
        success = .true.
        do i = 1, size(module_files)
            if (len_trim(module_files(i)) > 0) then
       dest_file = join_path(trim(modules_dir), trim(extract_filename(module_files(i))))

                ! Use cross-platform copy command
                if (get_os_type() == OS_WINDOWS) then
                    command = 'copy "'//trim(escape_shell_arg(module_files(i)))//'" "'// &
                              trim(escape_shell_arg(dest_file))//'" >nul 2>&1'
                else
                    command = 'cp "'//trim(escape_shell_arg(module_files(i)))//'" "'// &
                              trim(escape_shell_arg(dest_file))//'" >/dev/null 2>&1'
                end if

                call run(command, exitstat=exitstat)
                if (exitstat /= 0) then
                    success = .false.
                    return
                end if
            end if
        end do

    end subroutine store_module_cache

    subroutine store_executable_cache(cache_key, executable_path, success)
        character(len=*), intent(in) :: cache_key, executable_path
        logical, intent(out) :: success
        character(len=:), allocatable :: executables_dir, dest_file, command
        integer :: exitstat

        ! Get executables cache directory
        executables_dir = get_cache_subdir('executables')

        ! Create cache key subdirectory
        executables_dir = join_path(trim(executables_dir), trim(cache_key))
        call ensure_cache_dir(executables_dir, success)
        if (.not. success) return

        ! Copy executable using cross-platform commands
   dest_file = join_path(trim(executables_dir), trim(extract_filename(executable_path)))

        if (get_os_type() == OS_WINDOWS) then
            command = 'copy "'//trim(escape_shell_arg(executable_path))//'" "'// &
                      trim(escape_shell_arg(dest_file))//'" >nul 2>&1'
        else
            command = 'cp "'//trim(escape_shell_arg(executable_path))//'" "'// &
                      trim(escape_shell_arg(dest_file))//'" >/dev/null 2>&1'
        end if

        call run(command, exitstat=exitstat)
        success = (exitstat == 0)

        if (success .and. get_os_type() /= OS_WINDOWS) then
            ! Make executable (not needed on Windows)
            command = 'chmod +x "'//trim(escape_shell_arg(dest_file))//'"'
            call run(command, exitstat=exitstat)
            success = (exitstat == 0)
        end if

    end subroutine store_executable_cache

    function get_cache_key(source_files, dependencies) result(cache_key)
        character(len=*), intent(in) :: source_files(:)
        character(len=*), intent(in) :: dependencies(:)
        character(len=64) :: cache_key
        character(len=32) :: content_hash, deps_hash
        character(len=256), allocatable :: all_files(:)
        integer :: i, total_size

        ! Use content-based hashing for proper cache key generation
        ! This ensures modules with same name but different content get different cache keys

        ! Get content hash of source files
        content_hash = get_content_hash(source_files)

        ! Include dependencies in the hash if any
        if (size(dependencies) > 0) then
            ! Combine source files and dependencies for hashing
            total_size = size(source_files) + size(dependencies)
            allocate (all_files(total_size))

            ! Copy source files
            do i = 1, size(source_files)
                all_files(i) = source_files(i)
            end do

            ! Copy dependencies
            do i = 1, size(dependencies)
                all_files(size(source_files) + i) = dependencies(i)
            end do

            deps_hash = get_content_hash(all_files)
            deallocate (all_files)
        else
            deps_hash = content_hash
        end if

        ! Use the content hash as the cache key
        cache_key = trim(deps_hash)

    end function get_cache_key

    function get_fpm_digest(source_dir) result(digest_key)
        character(len=*), intent(in) :: source_dir
        character(len=32) :: digest_key
        type(srcfile_t), allocatable :: sources(:)
        type(error_t), allocatable :: error
        integer :: i
        character(len=16) :: hex_digest

        ! Use FPM API to discover sources and get their digests
        call add_sources_from_dir(sources, source_dir, FPM_SCOPE_APP, error=error)

        if (allocated(error)) then
            ! Fallback to simple naming if FPM fails
            digest_key = 'fallback_'//adjustl(extract_filename(source_dir))
            return
        end if

        if (.not. allocated(sources) .or. size(sources) == 0) then
            digest_key = 'empty_'//adjustl(extract_filename(source_dir))
            return
        end if

        ! Combine all source file digests into a single cache key
        ! Use the first source file's digest as the primary key
        write (hex_digest, '(z0)') sources(1)%digest
        digest_key = 'fpm_'//trim(hex_digest)

        ! For multiple sources, XOR their digests together
        do i = 2, size(sources)
            write (hex_digest, '(z0)') ieor(sources(1)%digest, sources(i)%digest)
            digest_key = 'fpm_'//trim(hex_digest)
        end do

    end function get_fpm_digest

    function extract_filename(filepath) result(filename)
        character(len=*), intent(in) :: filepath
        character(len=256) :: filename
        integer :: last_slash, last_backslash, last_separator

        ! Handle both Unix (/) and Windows (\) path separators
        last_slash = index(filepath, '/', back=.true.)
        last_backslash = index(filepath, '\', back=.true.)
        last_separator = max(last_slash, last_backslash)

        if (last_separator > 0) then
            filename = filepath(last_separator + 1:)
        else
            filename = filepath
        end if

    end function extract_filename

    subroutine store_build_artifacts(hash_key, build_dir, success)
        !> Store compiled modules and executables in cache
        character(len=*), intent(in) :: hash_key, build_dir
        logical, intent(out) :: success
        character(len=:), allocatable :: cache_path, command
        integer :: exitstat

        ! Create cache directory for this hash
        cache_path = join_path(trim(get_cache_subdir('builds')), trim(hash_key))
        call ensure_cache_dir(cache_path, success)
        if (.not. success) return

        ! Copy build artifacts to cache using cross-platform commands
        if (get_os_type() == OS_WINDOWS) then
            command = 'xcopy /E /I /Y "'//trim(escape_shell_arg(build_dir))//'" "'// &
                      trim(escape_shell_arg(cache_path))//'" >nul 2>&1'
        else
            command = 'cp -r "'//trim(escape_shell_arg(build_dir))//'/." "'// &
                      trim(escape_shell_arg(cache_path))//'/" >/dev/null 2>&1'
        end if

        call run(command, exitstat=exitstat)
        success = (exitstat == 0)

    end subroutine store_build_artifacts

    subroutine retrieve_build_artifacts(hash_key, target_dir, success)
        !> Retrieve cached build artifacts
        character(len=*), intent(in) :: hash_key, target_dir
        logical, intent(out) :: success
        character(len=:), allocatable :: cache_path, command
        integer :: exitstat

        ! Check if cache exists
        cache_path = join_path(trim(get_cache_subdir('builds')), trim(hash_key))
        if (.not. exists(trim(cache_path))) then
            success = .false.
            return
        end if

        ! Create target directory
        call ensure_cache_dir(target_dir, success)
        if (.not. success) return

        ! Copy cached artifacts to target using cross-platform commands
        if (get_os_type() == OS_WINDOWS) then
            command = 'xcopy /E /I /Y "'//trim(escape_shell_arg(cache_path))//'" "'// &
                      trim(escape_shell_arg(target_dir))//'" >nul 2>&1'
        else
            command = 'cp -r "'//trim(escape_shell_arg(cache_path))//'/." "'// &
                      trim(escape_shell_arg(target_dir))//'/" >/dev/null 2>&1'
        end if

        call run(command, exitstat=exitstat)
        success = (exitstat == 0)

    end subroutine retrieve_build_artifacts

    function cache_exists(hash_key) result(cache_found)
        !> Check if cache entry exists
        character(len=*), intent(in) :: hash_key
        logical :: cache_found
        character(len=:), allocatable :: cache_path

        cache_path = join_path(trim(get_cache_subdir('builds')), trim(hash_key))
        cache_found = exists(trim(cache_path))

    end function cache_exists

    subroutine invalidate_cache(hash_key, success)
        !> Remove cache entry
        character(len=*), intent(in) :: hash_key
        logical, intent(out) :: success
        character(len=:), allocatable :: cache_path, command
        integer :: exitstat

        cache_path = join_path(trim(get_cache_subdir('builds')), trim(hash_key))

        ! Use cross-platform directory removal commands
        if (get_os_type() == OS_WINDOWS) then
            command = 'rmdir /S /Q "'//trim(escape_shell_arg(cache_path))//'" >nul 2>&1'
        else
            command = 'rm -rf "'//trim(escape_shell_arg(cache_path))//'" >/dev/null 2>&1'
        end if

        call run(command, exitstat=exitstat)
        success = (exitstat == 0)

    end subroutine invalidate_cache

    function get_content_hash(source_files) result(hash_key)
        !> Generate content-based hash using FPM's fnv_1a algorithm
        character(len=*), intent(in) :: source_files(:)
        character(len=32) :: hash_key
        type(string_t), allocatable :: file_contents(:)
        character(len=16) :: hex_digest
        integer(kind=8) :: combined_digest
        integer :: i

        ! Read all source files and combine their content using FPM's read_lines
        combined_digest = 0_int64

        do i = 1, size(source_files)
            if (len_trim(source_files(i)) == 0) cycle

            ! Use FPM's read_lines function like fpm_source_parsing.f90 does
            file_contents = read_lines(trim(source_files(i)))

            ! Use FPM's fnv_1a hash function like fmp_source_parsing.f90 does
            if (size(file_contents) > 0) then
                combined_digest = ieor(combined_digest, fnv_1a(file_contents))
            end if
        end do

        ! Convert to hex string
        if (combined_digest /= 0) then
            write (hex_digest, '(z0)') combined_digest
            hash_key = 'fpm_'//trim(hex_digest)
        else
            hash_key = 'fallback_unknown'
        end if

    end function get_content_hash

    function get_single_file_content_hash(file_path) result(hash_key)
        !> Generate content-based hash for a single file using FPM's fnv_1a algorithm
        character(len=*), intent(in) :: file_path
        character(len=32) :: hash_key
        character(len=256), dimension(1) :: single_file_array

        ! Convert single file to array and use existing function
        single_file_array(1) = file_path
        hash_key = get_content_hash(single_file_array)

    end function get_single_file_content_hash

    subroutine clear_cache(custom_cache_dir, success)
        character(len=*), intent(in) :: custom_cache_dir
        logical, intent(out) :: success
        character(len=256) :: cache_dir
        character(len=:), allocatable :: command
        integer :: exitstat, cmdstat

        success = .false.

        ! Get cache directory
        if (len_trim(custom_cache_dir) > 0) then
            cache_dir = trim(custom_cache_dir)
        else
            cache_dir = get_cache_dir()
        end if

        ! Check if cache directory exists
        inquire (file=trim(cache_dir), exist=success)
        if (.not. success) then
            ! No cache directory, nothing to clear
            success = .true.
            return
        end if

        ! Clear cache directory contents
        ! Use platform-specific commands
        if (get_os_type() == OS_WINDOWS) then
            command = 'rmdir /S /Q "'//trim(escape_shell_arg(cache_dir))//'"'
        else
            command = 'rm -rf "'//trim(escape_shell_arg(cache_dir))//'"'
        end if

        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)

        if (cmdstat == 0 .and. exitstat == 0) then
            success = .true.
        end if

    end subroutine clear_cache

    subroutine get_cache_info(custom_cache_dir, info)
        character(len=*), intent(in) :: custom_cache_dir
        character(len=*), intent(out) :: info
        character(len=256) :: cache_dir
        character(len=:), allocatable :: command, size_output
        integer :: unit, ios, exitstat, cmdstat
        integer :: num_files, num_dirs
        logical :: exists

        ! Get cache directory
        if (len_trim(custom_cache_dir) > 0) then
            cache_dir = trim(custom_cache_dir)
        else
            cache_dir = get_cache_dir()
        end if

        ! Check if cache directory exists
        inquire (file=trim(cache_dir), exist=exists)
        if (.not. exists) then
            info = "Cache directory does not exist: "//trim(cache_dir)
            return
        end if

        ! Get cache size and file count
        if (get_os_type() == OS_WINDOWS) then
            ! Windows: Use dir command
            command = 'dir /s "'//trim(escape_shell_arg(cache_dir))//'" 2>nul | find "File(s)"'
        else
            ! Unix-like: Use du and find commands
            command = 'du -sh "'//trim(escape_shell_arg(cache_dir))//'" 2>/dev/null | cut -f1'
        end if

        ! Execute command and capture output
        block
            character(len=256) :: temp_file
      temp_file = create_temp_file('fortran_cache_cache_size', '.tmp')
            call execute_command_line(command//' > '//trim(escape_shell_arg(temp_file)), &
                                      exitstat=exitstat, cmdstat=cmdstat)

            size_output = "unknown"
            if (cmdstat == 0 .and. exitstat == 0) then
            open (newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
                if (ios == 0) then
                    read (unit, '(A)', iostat=ios) size_output
                    close (unit)
                end if
                ! Clean up temp file is handled by temp_utils
            end if
        end block

        ! Count files and directories
        if (get_os_type() == OS_WINDOWS) then
            command = 'dir /b /s "'//trim(escape_shell_arg(cache_dir))//'" 2>nul | find /c /v ""'
        else
            command = 'find "'//trim(escape_shell_arg(cache_dir))//'" -type f 2>/dev/null | wc -l'
        end if

        block
            character(len=256) :: temp_file
     temp_file = create_temp_file('fortran_cache_cache_count', '.tmp')
            call execute_command_line(command//' > '//trim(escape_shell_arg(temp_file)), &
                                      exitstat=exitstat, cmdstat=cmdstat)

            num_files = 0
            if (cmdstat == 0 .and. exitstat == 0) then
            open (newunit=unit, file=temp_file, status='old', action='read', iostat=ios)
                if (ios == 0) then
                    read (unit, *, iostat=ios) num_files
                    close (unit)
                end if
                ! Clean up temp file is handled by temp_utils
            end if
        end block

        ! Build info string
        write (info, '(A)') "Fortran Cache Information:"
    write(info, '(A,A,A)') trim(info), new_line('a'), "  Cache directory: " // trim(cache_dir)
    write(info, '(A,A,A,I0,A)') trim(info), new_line('a'), "  Number of files: ", num_files, " files"
    write(info, '(A,A,A,A)') trim(info), new_line('a'), "  Total size: ", trim(adjustl(size_output))

    end subroutine get_cache_info

end module cache
