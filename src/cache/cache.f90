module cache
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int64
  use fpm_sources, only: add_sources_from_dir
  use fpm_model, only: srcfile_t, FPM_SCOPE_APP
  use fpm_error, only: error_t
  use fpm_filesystem, only: list_files, read_lines
  use fpm_strings, only: string_t, fnv_1a
  implicit none
  private
  public :: get_cache_dir, ensure_cache_dir, ensure_cache_structure, get_cache_subdir, &
            store_module_cache, store_executable_cache, get_cache_key, get_fpm_digest, &
            store_build_artifacts, retrieve_build_artifacts, cache_exists, invalidate_cache, &
            get_content_hash, get_single_file_content_hash, clear_cache, get_cache_info
  
contains

  function get_cache_dir() result(cache_dir)
    character(len=256) :: cache_dir
    character(len=256) :: home_dir
    integer :: status
    
    ! Try to get XDG_CACHE_HOME first (Linux standard)
    call get_environment_variable('XDG_CACHE_HOME', cache_dir, status=status)
    
    if (status == 0 .and. len_trim(cache_dir) > 0) then
      cache_dir = trim(cache_dir) // '/fortran'
    else
      ! Fallback to HOME directory
      call get_environment_variable('HOME', home_dir, status=status)
      
      if (status == 0) then
        ! Linux/macOS: ~/.cache/fortran
        cache_dir = trim(home_dir) // '/.cache/fortran'
      else
        ! Windows fallback: try LOCALAPPDATA
        call get_environment_variable('LOCALAPPDATA', cache_dir, status=status)
        if (status == 0) then
          cache_dir = trim(cache_dir) // '/fortran/cache'
        else
          ! Last resort - use current directory
          cache_dir = './.fortran-cache'
        end if
      end if
    end if
    
  end function get_cache_dir
  
  subroutine ensure_cache_dir(cache_dir, success)
    character(len=*), intent(in) :: cache_dir
    logical, intent(out) :: success
    character(len=512) :: command
    integer :: exitstat, cmdstat
    
    ! Create directory with parents (-p flag)
    command = 'mkdir -p "' // trim(cache_dir) // '"'
    
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    
    success = (cmdstat == 0 .and. exitstat == 0)
    
  end subroutine ensure_cache_dir
  
  subroutine ensure_cache_structure(cache_dir, success)
    character(len=*), intent(in) :: cache_dir
    logical, intent(out) :: success
    character(len=512) :: command
    integer :: exitstat, cmdstat
    logical :: builds_ok, modules_ok, executables_ok, metadata_ok
    
    ! Create main cache directory first
    call ensure_cache_dir(cache_dir, success)
    if (.not. success) return
    
    ! Create subdirectories
    command = 'mkdir -p "' // trim(cache_dir) // '/builds"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    builds_ok = (cmdstat == 0 .and. exitstat == 0)
    
    command = 'mkdir -p "' // trim(cache_dir) // '/modules"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    modules_ok = (cmdstat == 0 .and. exitstat == 0)
    
    command = 'mkdir -p "' // trim(cache_dir) // '/executables"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    executables_ok = (cmdstat == 0 .and. exitstat == 0)
    
    command = 'mkdir -p "' // trim(cache_dir) // '/metadata"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    metadata_ok = (cmdstat == 0 .and. exitstat == 0)
    
    success = builds_ok .and. modules_ok .and. executables_ok .and. metadata_ok
    
  end subroutine ensure_cache_structure
  
  function get_cache_subdir(subdir_name) result(subdir_path)
    character(len=*), intent(in) :: subdir_name
    character(len=512) :: subdir_path
    character(len=256) :: cache_dir
    
    cache_dir = get_cache_dir()
    subdir_path = trim(cache_dir) // '/' // trim(subdir_name)
    
  end function get_cache_subdir
  
  subroutine store_module_cache(cache_key, module_files, success)
    character(len=*), intent(in) :: cache_key
    character(len=*), intent(in) :: module_files(:)
    logical, intent(out) :: success
    character(len=512) :: modules_dir, dest_file, command
    integer :: i, exitstat, cmdstat
    
    ! Get modules cache directory
    modules_dir = get_cache_subdir('modules')
    
    ! Create cache key subdirectory
    modules_dir = trim(modules_dir) // '/' // trim(cache_key)
    call ensure_cache_dir(modules_dir, success)
    if (.not. success) return
    
    ! Copy each module file
    success = .true.
    do i = 1, size(module_files)
      if (len_trim(module_files(i)) > 0) then
        dest_file = trim(modules_dir) // '/' // extract_filename(module_files(i))
        command = 'cp "' // trim(module_files(i)) // '" "' // trim(dest_file) // '"'
        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
        if (cmdstat /= 0 .or. exitstat /= 0) then
          success = .false.
          return
        end if
      end if
    end do
    
  end subroutine store_module_cache
  
  subroutine store_executable_cache(cache_key, executable_path, success)
    character(len=*), intent(in) :: cache_key, executable_path
    logical, intent(out) :: success
    character(len=512) :: executables_dir, dest_file, command
    integer :: exitstat, cmdstat
    
    ! Get executables cache directory
    executables_dir = get_cache_subdir('executables')
    
    ! Create cache key subdirectory
    executables_dir = trim(executables_dir) // '/' // trim(cache_key)
    call ensure_cache_dir(executables_dir, success)
    if (.not. success) return
    
    ! Copy executable
    dest_file = trim(executables_dir) // '/' // extract_filename(executable_path)
    command = 'cp "' // trim(executable_path) // '" "' // trim(dest_file) // '"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    success = (cmdstat == 0 .and. exitstat == 0)
    
    if (success) then
      ! Make executable
      command = 'chmod +x "' // trim(dest_file) // '"'
      call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
      success = (cmdstat == 0 .and. exitstat == 0)
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
      allocate(all_files(total_size))
      
      ! Copy source files
      do i = 1, size(source_files)
        all_files(i) = source_files(i)
      end do
      
      ! Copy dependencies
      do i = 1, size(dependencies)
        all_files(size(source_files) + i) = dependencies(i)
      end do
      
      deps_hash = get_content_hash(all_files)
      deallocate(all_files)
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
      digest_key = 'fallback_' // adjustl(extract_filename(source_dir))
      return
    end if
    
    if (.not. allocated(sources) .or. size(sources) == 0) then
      digest_key = 'empty_' // adjustl(extract_filename(source_dir))
      return
    end if
    
    ! Combine all source file digests into a single cache key
    ! Use the first source file's digest as the primary key
    write(hex_digest, '(z0)') sources(1)%digest
    digest_key = 'fpm_' // trim(hex_digest)
    
    ! For multiple sources, XOR their digests together
    do i = 2, size(sources)
      write(hex_digest, '(z0)') ieor(sources(1)%digest, sources(i)%digest)
      digest_key = 'fpm_' // trim(hex_digest)
    end do
    
  end function get_fpm_digest
  
  function extract_filename(filepath) result(filename)
    character(len=*), intent(in) :: filepath
    character(len=256) :: filename
    integer :: last_slash
    
    last_slash = index(filepath, '/', back=.true.)
    if (last_slash > 0) then
      filename = filepath(last_slash+1:)
    else
      filename = filepath
    end if
    
  end function extract_filename
  
  subroutine store_build_artifacts(hash_key, build_dir, success)
    !> Store compiled modules and executables in cache
    character(len=*), intent(in) :: hash_key, build_dir
    logical, intent(out) :: success
    character(len=512) :: cache_path, command
    integer :: exitstat, cmdstat
    
    ! Create cache directory for this hash
    cache_path = get_cache_subdir('builds') // '/' // trim(hash_key)
    call ensure_cache_dir(cache_path, success)
    if (.not. success) return
    
    ! Copy build artifacts to cache
    command = 'cp -r "' // trim(build_dir) // '"/* "' // trim(cache_path) // '/"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    success = (cmdstat == 0 .and. exitstat == 0)
    
  end subroutine store_build_artifacts
  
  subroutine retrieve_build_artifacts(hash_key, target_dir, success)
    !> Retrieve cached build artifacts
    character(len=*), intent(in) :: hash_key, target_dir
    logical, intent(out) :: success
    character(len=512) :: cache_path, command
    integer :: exitstat, cmdstat
    logical :: exists
    
    ! Check if cache exists
    cache_path = get_cache_subdir('builds') // '/' // trim(hash_key)
    ! Check if directory exists by checking for a marker file
    inquire(file=trim(cache_path) // '/.', exist=exists)
    if (.not. exists) then
      success = .false.
      return
    end if
    
    ! Create target directory
    call ensure_cache_dir(target_dir, success)
    if (.not. success) return
    
    ! Copy cached artifacts to target
    command = 'cp -r "' // trim(cache_path) // '"/* "' // trim(target_dir) // '/"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    success = (cmdstat == 0 .and. exitstat == 0)
    
  end subroutine retrieve_build_artifacts
  
  function cache_exists(hash_key) result(exists)
    !> Check if cache entry exists
    character(len=*), intent(in) :: hash_key
    logical :: exists
    character(len=512) :: cache_path
    
    cache_path = get_cache_subdir('builds') // '/' // trim(hash_key)
    ! Check if directory exists by checking for current directory marker
    inquire(file=trim(cache_path) // '/.', exist=exists)
    
  end function cache_exists
  
  subroutine invalidate_cache(hash_key, success)
    !> Remove cache entry
    character(len=*), intent(in) :: hash_key
    logical, intent(out) :: success
    character(len=512) :: cache_path, command
    integer :: exitstat, cmdstat
    
    cache_path = get_cache_subdir('builds') // '/' // trim(hash_key)
    command = 'rm -rf "' // trim(cache_path) // '"'
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    success = (cmdstat == 0 .and. exitstat == 0)
    
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
      write(hex_digest, '(z0)') combined_digest
      hash_key = 'fpm_' // trim(hex_digest)
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
    character(len=512) :: command
    integer :: exitstat, cmdstat
    
    success = .false.
    
    ! Get cache directory
    if (len_trim(custom_cache_dir) > 0) then
      cache_dir = trim(custom_cache_dir)
    else
      cache_dir = get_cache_dir()
    end if
    
    ! Check if cache directory exists
    inquire(file=trim(cache_dir), exist=success)
    if (.not. success) then
      ! No cache directory, nothing to clear
      success = .true.
      return
    end if
    
    ! Clear cache directory contents
    ! Use platform-specific commands
#ifdef _WIN32
    command = 'rmdir /S /Q "' // trim(cache_dir) // '"'
#else
    command = 'rm -rf "' // trim(cache_dir) // '"/*'
#endif
    
    call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
    
    if (cmdstat == 0 .and. exitstat == 0) then
      success = .true.
    end if
    
  end subroutine clear_cache
  
  subroutine get_cache_info(custom_cache_dir, info)
    character(len=*), intent(in) :: custom_cache_dir
    character(len=*), intent(out) :: info
    character(len=256) :: cache_dir
    character(len=512) :: command, size_output
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
    inquire(file=trim(cache_dir), exist=exists)
    if (.not. exists) then
      info = "Cache directory does not exist: " // trim(cache_dir)
      return
    end if
    
    ! Get cache size and file count
#ifdef _WIN32
    ! Windows: Use dir command
    command = 'dir /s "' // trim(cache_dir) // '" 2>nul | find "File(s)"'
#else
    ! Unix-like: Use du and find commands
    command = 'du -sh "' // trim(cache_dir) // '" 2>/dev/null | cut -f1'
#endif
    
    ! Execute command and capture output
    call execute_command_line(command // ' > cache_size.tmp', exitstat=exitstat, cmdstat=cmdstat)
    
    size_output = "unknown"
    if (cmdstat == 0 .and. exitstat == 0) then
      open(newunit=unit, file='cache_size.tmp', status='old', action='read', iostat=ios)
      if (ios == 0) then
        read(unit, '(A)', iostat=ios) size_output
        close(unit)
      end if
      ! Clean up temp file
      open(newunit=unit, file='cache_size.tmp', status='old', iostat=ios)
      if (ios == 0) close(unit, status='delete')
    end if
    
    ! Count files and directories
#ifdef _WIN32
    command = 'dir /b /s "' // trim(cache_dir) // '" 2>nul | find /c /v ""'
#else
    command = 'find "' // trim(cache_dir) // '" -type f 2>/dev/null | wc -l'
#endif
    
    call execute_command_line(command // ' > cache_count.tmp', exitstat=exitstat, cmdstat=cmdstat)
    
    num_files = 0
    if (cmdstat == 0 .and. exitstat == 0) then
      open(newunit=unit, file='cache_count.tmp', status='old', action='read', iostat=ios)
      if (ios == 0) then
        read(unit, *, iostat=ios) num_files
        close(unit)
      end if
      ! Clean up temp file
      open(newunit=unit, file='cache_count.tmp', status='old', iostat=ios)
      if (ios == 0) close(unit, status='delete')
    end if
    
    ! Build info string
    write(info, '(A)') "Fortran Cache Information:"
    write(info, '(A,A,A)') trim(info), new_line('a'), "  Cache directory: " // trim(cache_dir)
    write(info, '(A,A,A,I0,A)') trim(info), new_line('a'), "  Number of files: ", num_files, " files"
    write(info, '(A,A,A,A)') trim(info), new_line('a'), "  Total size: ", trim(adjustl(size_output))
    
  end subroutine get_cache_info
  
end module cache