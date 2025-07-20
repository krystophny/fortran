module runner
  use cache, only: get_cache_dir, ensure_cache_dir, get_content_hash, get_single_file_content_hash, cache_exists
    use cache_lock, only: acquire_lock, release_lock, cleanup_stale_locks
    use module_scanner, only: scan_modules, module_info
    use fpm_generator, only: generate_fpm_with_deps, generate_fpm_with_deps_from_config
    use registry_resolver, only: ensure_registry_exists, ensure_registry_exists_in_dir
    use fpm_module_cache, only: module_cache_t, new_module_cache
    use fpm_compiler, only: compiler_t, new_compiler, id_gcc
    use fpm_model, only: srcfile_t
    use fpm_strings, only: string_t
    use fpm_error, only: error_t
  use frontend_integration, only: compile_with_frontend, compile_with_frontend_debug, is_simple_fortran_file
    use debug_state, only: get_debug_flags
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path
    use fpm_filesystem, only: mkdir
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private
    public :: run_fortran_file

contains

    subroutine run_fortran_file(filename, exit_code, verbose_level, custom_cache_dir, &
                                custom_config_dir, parallel_jobs, no_wait, custom_flags)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: exit_code
        integer, intent(in) :: verbose_level
        character(len=*), intent(in) :: custom_cache_dir
        character(len=*), intent(in) :: custom_config_dir
        integer, intent(in) :: parallel_jobs
        logical, intent(in) :: no_wait
        character(len=*), intent(in), optional :: custom_flags

        logical :: file_exists, success, file_exists_flag
        character(len=256) :: cache_dir, project_dir, basename
        character(len=512) :: command, flag_string
        character(len=256) :: absolute_path, preprocessed_file, working_file
        character(len=32) :: jobs_flag, content_hash
        character(len=1024) :: preprocess_error
        integer :: exitstat, cmdstat
        integer :: i, last_slash
        logical :: was_preprocessed

        exit_code = 0

        ! Check if file exists
        inquire (file=filename, exist=file_exists)
        if (.not. file_exists) then
            print '(a,a)', 'Error: File not found: ', trim(filename)
            exit_code = 1
            return
        end if

        ! Check file extension
        if (index(filename, '.f90') == 0 .and. index(filename, '.F90') == 0 .and. &
            index(filename, '.f') == 0 .and. index(filename, '.F') == 0) then
            print '(a)', 'Error: Input file must have .f90, .F90, .f, or .F extension'
            exit_code = 1
            return
        end if

        ! Get absolute path
        call get_absolute_path(filename, absolute_path)

        ! Extract basename without extension
        call get_basename(filename, basename)

        ! Check if file needs preprocessing
        was_preprocessed = .false.
        working_file = absolute_path

        if (is_simple_fortran_file(filename)) then
            if (verbose_level >= 1) then
                print '(a)', 'Processing Lowercase Fortran file with frontend...'
            end if

            ! Get content-based cache directory first
            if (len_trim(custom_cache_dir) > 0) then
                cache_dir = custom_cache_dir
            else
                cache_dir = get_cache_dir()
            end if
            call ensure_cache_dir(cache_dir, success)
            if (.not. success) then
                print '(a)', 'Error: Failed to create cache directory'
                exit_code = 1
                return
            end if

            ! Generate content-based filename for preprocessed .f file
            content_hash = get_single_file_content_hash(absolute_path)
            if (content_hash == 'fallback_unknown') then
                ! Fallback to basename if hashing fails
           preprocessed_file = trim(cache_dir)//'/preprocessed_'//trim(basename)//'.f90'
            else
       preprocessed_file = trim(cache_dir)//'/preprocessed_'//trim(content_hash)//'.f90'
            end if

            ! Check if already cached
            inquire (file=preprocessed_file, exist=file_exists_flag)

            if (.not. file_exists_flag) then
                if (verbose_level >= 2) then
                  print '(a,a)', 'Creating preprocessed file: ', trim(preprocessed_file)
                end if

                ! Preprocess the file
                ! Use AST-based preprocessor by default for .f files
                block
                    logical :: use_ast_preprocessor
                    character(len=256) :: env_var

                    ! Check environment variable to allow switching preprocessors
                  call get_environment_variable("FORTRAN_USE_AST_PREPROCESSOR", env_var)
                    use_ast_preprocessor = .true.  ! Default to AST preprocessor
                    if (len_trim(env_var) > 0) then
            use_ast_preprocessor = (trim(env_var) /= "0" .and. trim(env_var) /= "false")
                    end if
                    if (use_ast_preprocessor) then
                        if (verbose_level >= 2) then
                            print '(a)', 'Using AST-based preprocessor'
                        end if

                        ! Check for debug flags from global state
                        block
    logical :: debug_tokens, debug_ast, debug_semantic, debug_standardize, debug_codegen

            call get_debug_flags(debug_tokens, debug_ast, debug_semantic, debug_standardize, debug_codegen)

            if (debug_tokens .or. debug_ast .or. debug_semantic .or. debug_standardize .or. debug_codegen) then
  call compile_with_frontend_debug(absolute_path, preprocessed_file, preprocess_error, &
              debug_tokens, debug_ast, debug_semantic, debug_standardize, debug_codegen)
                            else
          call compile_with_frontend(absolute_path, preprocessed_file, preprocess_error)
                            end if
                        end block
                    else
                        if (verbose_level >= 2) then
                            print '(a)', 'Using legacy preprocessor'
                        end if
          call compile_with_frontend(absolute_path, preprocessed_file, preprocess_error)
                    end if
                end block

                if (len_trim(preprocess_error) > 0) then
                   print '(a,a)', 'Error during preprocessing: ', trim(preprocess_error)
                    exit_code = 1
                    return
                end if
            else
                if (verbose_level >= 2) then
              print '(a,a)', 'Using cached preprocessed file: ', trim(preprocessed_file)
                end if
            end if

            ! Keep track of preprocessed file
            working_file = preprocessed_file
            was_preprocessed = .true.

            if (verbose_level >= 2) then
                print '(a,a)', 'Preprocessed file ready: ', trim(preprocessed_file)
            end if
        end if

        ! Get cache directory (use custom if provided) - for .f90 files or if not set above
        if (.not. is_simple_fortran_file(filename)) then
            if (len_trim(custom_cache_dir) > 0) then
                cache_dir = custom_cache_dir
            else
                cache_dir = get_cache_dir()
            end if
        end if
        call ensure_cache_dir(cache_dir, success)
        if (.not. success) then
            print '(a)', 'Error: Failed to create cache directory'
            exit_code = 1
            return
        end if

        ! Generate content-based hash for cache key
    call get_project_hash_and_directory(absolute_path, basename, cache_dir, project_dir, verbose_level)

        ! Clean up stale locks on startup
        call cleanup_stale_locks(cache_dir)

        ! Try to acquire lock for this project
        if (.not. acquire_lock(cache_dir, basename,.not. no_wait)) then
            if (no_wait) then
print '(a)', 'Error: Cache is locked by another process. Use without --no-wait to wait.'
            else
                print '(a)', 'Error: Timeout waiting for cache lock.'
            end if
            exit_code = 1
            return
        end if

        ! Check if project already exists (cache hit)
        if (directory_exists(trim(project_dir)//'/build')) then
            if (verbose_level >= 1) then
                print '(a)', 'Cache hit: Using existing build'
            end if

            ! Always update source files to allow incremental compilation
            call update_source_files(working_file, project_dir)
        else
            ! Cache miss: need to set up project
            if (verbose_level >= 1) then
                print '(a)', 'Cache miss: Setting up new build'
            end if

            call mkdir(trim(project_dir)//'/app')

            ! Copy source files and generate FPM project only on cache miss
            if (present(custom_flags)) then
          call setup_project_files(working_file, project_dir, basename, verbose_level, &
                                      custom_config_dir, was_preprocessed, custom_flags)
            else
          call setup_project_files(working_file, project_dir, basename, verbose_level, &
                                         custom_config_dir, was_preprocessed, '')
            end if
        end if

        ! Generate flag string based on file type and user input
        if (present(custom_flags)) then
            call generate_flag_string(was_preprocessed, custom_flags, flag_string)
        else
            call generate_flag_string(was_preprocessed, '', flag_string)
        end if

        ! Build first
        ! Prepare parallel jobs flag if specified
        ! NOTE: Current FPM version (0.12.0) doesn't support -j flag
        ! This is prepared for future versions that will support it
        if (parallel_jobs > 0) then
            write (jobs_flag, '(a,i0,a)') ' --jobs ', parallel_jobs, ' '
            if (verbose_level >= 1) then
                print '(a,i0,a)', 'Note: Parallel builds requested (', parallel_jobs, &
                    ' jobs) but current FPM version does not support --jobs flag'
            end if
        else
            jobs_flag = ' '
        end if

        if (verbose_level == 0) then
            ! Quiet mode: capture errors for helpful messages
            block
                character(len=256) :: output_file
                output_file = get_temp_file_path(create_temp_dir('fortran_build'), 'fpm_build_output.txt')
                if (len_trim(flag_string) > 0) then
                    command = 'cd "'//trim(project_dir)//'" && '// &
           'fpm build --flag "'//trim(flag_string)//'" > "'//trim(output_file)//'" 2>&1'
                else
                    command = 'cd "'//trim(project_dir)//'" && '// &
                              'fpm build > "'//trim(output_file)//'" 2>&1'
                end if
            end block
        else if (verbose_level >= 2) then
            ! Very verbose: show detailed build output
            if (len_trim(flag_string) > 0) then
                command = 'cd "'//trim(project_dir)//'" && '// &
                          'fpm build --verbose --flag "'//trim(flag_string)//'"'
            else
                command = 'cd "'//trim(project_dir)//'" && '// &
                          'fpm build --verbose'
            end if
        else
            ! Normal verbose: show build progress
            if (len_trim(flag_string) > 0) then
                command = 'cd "'//trim(project_dir)//'" && '// &
                          'fpm build --flag "'//trim(flag_string)//'"'
            else
                command = 'cd "'//trim(project_dir)//'" && '// &
                          'fpm build'
            end if
        end if

        ! TODO: Add jobs_flag when FPM supports it
        ! Future: 'fpm build' // trim(jobs_flag) // '--flag ...

     call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat, wait=.true.)

        if (cmdstat /= 0 .or. exitstat /= 0) then
            if (verbose_level == 0) then
                ! Parse FPM errors and provide helpful messages
                block
                    character(len=256) :: output_file
                    output_file = get_temp_file_path(create_temp_dir('fortran_build'), 'fpm_build_output.txt')
                    call provide_helpful_error_message(output_file)
                end block
            end if
            call release_lock(cache_dir, basename)
            exit_code = 1
            return
        end if

        ! Cache newly compiled dependency modules after successful build
        call cache_build_artifacts(project_dir, verbose_level)

        ! Run the executable directly
        command = trim(project_dir)//'/build/gfortran_*/app/'//trim(basename)
     call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat, wait=.true.)

        if (cmdstat /= 0) then
            print '(a)', 'Error: Failed to execute fpm'
            exit_code = 1
        else if (exitstat /= 0) then
            ! FPM returned non-zero, likely compilation error
            exit_code = exitstat
        end if

        ! Release the lock
        call release_lock(cache_dir, basename)

        ! Clean up (optional for now - we might want to keep for caching)
        ! command = 'rm -rf "' // trim(project_dir) // '"'
        ! call execute_command_line(command)

    end subroutine run_fortran_file

    subroutine get_absolute_path(filename, absolute_path)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: absolute_path
        character(len=512) :: command
        integer :: unit, iostat

        ! Use realpath command to get absolute path
        block
            character(len=256) :: temp_file
 temp_file = get_temp_file_path(create_temp_dir('fortran_realpath'), 'fortran_path.tmp')
            command = 'realpath "'//trim(filename)//'" > "'//trim(temp_file)//'"'
            call execute_command_line(command)

            open (newunit=unit, file=temp_file, status='old', iostat=iostat)
        end block
        if (iostat == 0) then
            read (unit, '(a)') absolute_path
            close (unit)
            ! Cleanup handled by temp_utils
        else
            ! Fallback to original filename
            absolute_path = filename
        end if

    end subroutine get_absolute_path

    subroutine get_basename(filename, basename)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: basename
        integer :: i, last_slash, last_dot

        ! Find last slash
        last_slash = 0
        do i = len_trim(filename), 1, -1
            if (filename(i:i) == '/') then
                last_slash = i
                exit
            end if
        end do

        ! Find last dot after the slash
        last_dot = 0
        do i = len_trim(filename), last_slash + 1, -1
            if (filename(i:i) == '.') then
                last_dot = i
                exit
            end if
        end do

        if (last_dot > last_slash) then
            basename = filename(last_slash + 1:last_dot - 1)
        else
            basename = filename(last_slash + 1:)
        end if

    end subroutine get_basename

    function get_timestamp() result(timestamp)
        character(len=16) :: timestamp
        integer :: values(8)

        call date_and_time(values=values)
        write (timestamp, '(i0,5(i2.2))') values(1), values(2), values(3), &
            values(5), values(6), values(7)

    end function get_timestamp

  subroutine generate_fpm_with_dependencies(source_file, project_dir, name, verbose_level, &
                                  custom_config_dir, is_preprocessed_file, custom_flags)
        character(len=*), intent(in) :: source_file, project_dir, name
        integer, intent(in) :: verbose_level
        character(len=*), intent(in) :: custom_config_dir
        logical, intent(in) :: is_preprocessed_file
        character(len=*), intent(in), optional :: custom_flags

        type(module_info), dimension(:), allocatable :: modules
        integer :: n_modules
        type(module_cache_t) :: mod_cache
        type(compiler_t) :: compiler

        ! Ensure registry exists in config directory
        if (len_trim(custom_config_dir) > 0) then
            call ensure_registry_exists_in_dir(custom_config_dir)
        else
            call ensure_registry_exists()
        end if

        ! Scan the source file for module dependencies
        call scan_modules(source_file, modules, n_modules)

        if (verbose_level >= 1 .and. n_modules > 0) then
            print '(a,i0,a)', 'Found ', n_modules, ' external module dependencies'
        end if

        ! Initialize module cache for dependency caching
        call new_compiler(compiler, 'gfortran', 'gcc', 'g++', .true., .false.)
        compiler%id = id_gcc
        mod_cache = new_module_cache(compiler, '13.0.0')

        if (verbose_level >= 1 .and. mod_cache%enabled) then
            print '(a)', 'Module cache enabled for dependency optimization'
        end if

        ! Generate fpm.toml with dependencies
        if (len_trim(custom_config_dir) > 0) then
            if (present(custom_flags)) then
        call generate_fpm_with_deps_from_config(project_dir, name, modules, n_modules, custom_config_dir, &
                                                     is_preprocessed_file, custom_flags)
            else
        call generate_fpm_with_deps_from_config(project_dir, name, modules, n_modules, custom_config_dir, &
                                                        is_preprocessed_file, '')
            end if
        else
            if (present(custom_flags)) then
        call generate_fpm_with_deps(project_dir, name, modules, n_modules, is_preprocessed_file, custom_flags)
            else
        call generate_fpm_with_deps(project_dir, name, modules, n_modules, is_preprocessed_file, '')
            end if
        end if

        ! Implement module cache integration
        if (mod_cache%enabled .and. n_modules > 0) then
      call setup_module_cache_for_build(mod_cache, project_dir, modules, n_modules, verbose_level)
        end if

    end subroutine generate_fpm_with_dependencies

    !> Setup module cache for FPM build - check for cached modules before building
  subroutine setup_module_cache_for_build(mod_cache, project_dir, modules, n_modules, verbose_level)
        type(module_cache_t), intent(in) :: mod_cache
        character(*), intent(in) :: project_dir
        type(module_info), intent(in) :: modules(:)
        integer, intent(in) :: n_modules, verbose_level

        integer :: i
        character(len=64) :: cache_key
        logical :: found
        type(error_t), allocatable :: error
        type(srcfile_t) :: dummy_srcfile
        type(string_t) :: mod_names(1)
        character(len=256) :: build_dir

        if (verbose_level >= 1) then
            print '(a)', 'Checking module cache for dependencies...'
        end if

        build_dir = trim(project_dir)//'/build'
        call mkdir(trim(build_dir))

        ! Check each dependency module for cached versions
        do i = 1, n_modules
            ! Create a dummy srcfile for cache key generation
            dummy_srcfile%file_name = trim(modules(i)%name)//'_dependency.f90'
            dummy_srcfile%digest = get_dependency_hash(modules(i))
            mod_names(1)%s = modules(i)%name
            dummy_srcfile%modules_provided = mod_names

            ! Generate cache key for this dependency module
            cache_key = mod_cache%get_cache_key(dummy_srcfile)

            ! Check if this module is already cached
            if (mod_cache%is_cached(cache_key)) then
                if (verbose_level >= 1) then
                    print '(a,a)', '  ✓ Found cached module: ', trim(modules(i)%name)
                end if

                ! Retrieve cached module files
       call mod_cache%retrieve_module(cache_key, build_dir, dummy_srcfile, found, error)

                if (.not. found .or. allocated(error)) then
                    if (verbose_level >= 1) then
          print '(a,a)', '  ⚠ Failed to retrieve cached module: ', trim(modules(i)%name)
                    end if
                end if
            else
                if (verbose_level >= 1) then
                    print '(a,a)', '  ○ Module not cached: ', trim(modules(i)%name)
                end if
            end if
        end do

    end subroutine setup_module_cache_for_build

    !> Generate a simple hash for dependency modules based on name
    function get_dependency_hash(module_dep) result(hash)
        type(module_info), intent(in) :: module_dep
        integer(int64) :: hash
        integer :: i, name_len

        ! Simple hash based on module name
        hash = 0_int64
        name_len = len_trim(module_dep%name)

        do i = 1, name_len
            hash = hash*31_int64 + int(iachar(module_dep%name(i:i)), int64)
        end do

    end function get_dependency_hash

    !> Cache newly compiled dependency modules after FPM build
    subroutine cache_build_artifacts(project_dir, verbose_level)
        character(*), intent(in) :: project_dir
        integer, intent(in) :: verbose_level

        character(len=256) :: build_dir
        character(len=512) :: command
        integer :: exitstat

        if (verbose_level >= 1) then
            print '(a)', 'Caching newly compiled dependency modules...'
        end if

        build_dir = trim(project_dir)//'/build'

        ! Find and cache all .mod files from dependencies directory
   command = 'find "'//trim(build_dir)//'" -name "*.mod" -type f 2>/dev/null | head -10'
        call execute_command_line(command)

        ! Find and cache all .o files from dependencies directory
     command = 'find "'//trim(build_dir)//'" -name "*.o" -type f 2>/dev/null | head -10'
        call execute_command_line(command)

        if (verbose_level >= 1) then
            print '(a)', 'Module caching completed'
        end if

        ! TODO: Implement actual caching logic here
        ! For now, this is a placeholder that shows what files are available

    end subroutine cache_build_artifacts

    subroutine copy_local_modules(main_file, project_dir)
        character(len=*), intent(in) :: main_file, project_dir
        character(len=256) :: source_dir
        character(len=512) :: command
        integer :: i, last_slash

        ! Extract directory from main file path
        last_slash = 0
        do i = len_trim(main_file), 1, -1
            if (main_file(i:i) == '/') then
                last_slash = i
                exit
            end if
        end do

        if (last_slash > 0) then
            source_dir = main_file(1:last_slash - 1)
        else
            source_dir = '.'
        end if

        ! Create src directory
        call mkdir(trim(project_dir)//'/src')

        ! Copy all .f90 files except the main file (only files, not directories)
    command = 'find "' // trim(source_dir) // '" -maxdepth 1 -type f \( -name "*.f90" -o -name "*.F90" \) | ' // &
                  'while read f; do '// &
                  '  if [ "$f" != "'//trim(main_file)//'" ]; then '// &
                  '    cp "$f" "'//trim(project_dir)//'/src/"; '// &
                  '  fi; '// &
                  'done'
        call execute_command_line(command)

    end subroutine copy_local_modules

    subroutine provide_helpful_error_message(error_file)
        character(len=*), intent(in) :: error_file
        integer :: unit, iostat
        character(len=512) :: line

        ! Read the error file and show the FPM error message
        open (newunit=unit, file=error_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            ! If we can't read the error file, fall back to generic message
            print '(a)', 'Error: Build failed. Run with -v to see details.'
            return
        end if

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit

            ! Show relevant error lines from FPM
            if (index(line, '<ERROR>') > 0 .or. index(line, 'Error:') > 0) then
                print '(a)', trim(line)
            end if
        end do

        close (unit)

        ! Clean up error file
        call execute_command_line('rm -f '//trim(error_file))

    end subroutine provide_helpful_error_message

  subroutine get_project_hash_and_directory(source_file, basename, cache_dir, project_dir, verbose_level)
        character(len=*), intent(in) :: source_file, basename, cache_dir
        character(len=*), intent(out) :: project_dir
        integer, intent(in) :: verbose_level
        character(len=32) :: structure_hash
        character(len=256) :: source_dir
        integer :: last_slash

        ! Get directory containing the source file
        last_slash = index(source_file, '/', back=.true.)
        if (last_slash > 0) then
            source_dir = source_file(1:last_slash - 1)
        else
            source_dir = '.'
        end if

        ! Generate hash based on project structure (dependencies and local modules), not main file content
        structure_hash = get_project_structure_hash(source_dir, source_file)

        if (verbose_level >= 2) then
            print '(a,a)', 'Project structure hash: ', trim(structure_hash)
        end if

        ! Create project directory based on structure hash
        project_dir = trim(cache_dir)//'/'//trim(basename)//'_'//trim(structure_hash)

    end subroutine get_project_hash_and_directory

    function directory_exists(dir_path) result(exists)
        character(len=*), intent(in) :: dir_path
        logical :: exists

        ! Check if directory exists by checking for current directory marker
        inquire (file=trim(dir_path)//'/.', exist=exists)

    end function directory_exists

   subroutine setup_project_files(absolute_path, project_dir, basename, verbose_level, &
                                  custom_config_dir, is_preprocessed_file, custom_flags)
 character(len=*), intent(in) :: absolute_path, project_dir, basename, custom_config_dir
        integer, intent(in) :: verbose_level
        logical, intent(in) :: is_preprocessed_file
        character(len=*), intent(in), optional :: custom_flags
        character(len=512) :: command
        integer :: exitstat, cmdstat

        ! Copy the source file to app directory
       command = 'cp "'//trim(absolute_path)//'" "'//trim(project_dir)//'/app/main.f90"'
        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)

        if (cmdstat /= 0 .or. exitstat /= 0) then
            print '(a)', 'Error: Failed to copy source file'
            return
        end if

        ! Copy all other .f90 files from the same directory to src/
        call copy_local_modules(absolute_path, project_dir)

        ! Scan for module dependencies and generate fpm.toml
        ! Sanitize basename for use as package name (limit length and remove spaces)
        block
            character(len=256) :: sanitized_name
            integer :: i

            sanitized_name = basename

            ! Replace spaces with underscores
            do i = 1, len_trim(sanitized_name)
                if (sanitized_name(i:i) == ' ') then
                    sanitized_name(i:i) = '_'
                end if
            end do

            ! Truncate if too long (FPM limit is around 63 chars)
            if (len_trim(sanitized_name) > 60) then
                sanitized_name = sanitized_name(1:60)
            end if

            if (present(custom_flags)) then
        call generate_fpm_with_dependencies(absolute_path, project_dir, trim(sanitized_name), verbose_level, &
                                  custom_config_dir, is_preprocessed_file, custom_flags)
            else
        call generate_fpm_with_dependencies(absolute_path, project_dir, trim(sanitized_name), verbose_level, &
                                            custom_config_dir, is_preprocessed_file, '')
            end if
        end block

    end subroutine setup_project_files

    subroutine update_source_files(absolute_path, project_dir)
        character(len=*), intent(in) :: absolute_path, project_dir
        character(len=512) :: command
        integer :: exitstat, cmdstat

        ! Update the main source file in the cached project
       command = 'cp "'//trim(absolute_path)//'" "'//trim(project_dir)//'/app/main.f90"'
        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)

        if (cmdstat /= 0 .or. exitstat /= 0) then
            print '(a)', 'Warning: Failed to update main source file in cache'
        end if

        ! Update any local module files (copy all .f90 files from source directory)
        call copy_local_modules(absolute_path, project_dir)

    end subroutine update_source_files

    function get_project_structure_hash(source_dir, main_file) result(structure_hash)
        character(len=*), intent(in) :: source_dir, main_file
        character(len=32) :: structure_hash
        character(len=512) :: all_files(100)
        integer :: num_files, i
        character(len=512) :: combined_dependencies

        ! Get all .f90 files in the source directory except the main file
        call get_f90_files_except_main(source_dir, main_file, all_files, num_files)

        ! Combine local module files to create structure hash
        combined_dependencies = ''
        do i = 1, num_files
            if (len_trim(all_files(i)) > 0) then
            combined_dependencies = trim(combined_dependencies)//trim(all_files(i))//';'
            end if
        end do

        ! If no local modules, use a simple hash based on the main file path
        if (len_trim(combined_dependencies) == 0) then
            structure_hash = 'simple_'//extract_basename(main_file)
        else
            ! Generate hash based on local module files (not their content, just structure)
            structure_hash = get_content_hash(all_files(1:num_files))
            if (structure_hash == 'fallback_unknown') then
                structure_hash = 'struct_'//extract_basename(main_file)
            end if
        end if

    end function get_project_structure_hash

    subroutine get_f90_files_except_main(source_dir, main_file, files, num_files)
        character(len=*), intent(in) :: source_dir, main_file
        character(len=*), intent(out) :: files(:)
        integer, intent(out) :: num_files
        character(len=512) :: command, temp_file
        integer :: unit, iostat, i
        character(len=512) :: line, main_basename

        ! Get basename of main file for comparison
        main_basename = extract_basename(main_file)

        ! Create temporary file to list .f90 files
    temp_file = get_temp_file_path(create_temp_dir('fortran_f90_list'), 'fortran_f90_list.tmp')
    command = 'find "' // trim(source_dir) // '" -maxdepth 1 -name "*.f90" -o -name "*.F90" > ' // trim(temp_file)
        call execute_command_line(command)

        ! Read file list and exclude main file
        files = ''
        num_files = 0
        open (newunit=unit, file=temp_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do i = 1, size(files)
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit

                ! Skip if this is the main file
                if (index(line, trim(main_basename)) > 0) cycle

                num_files = num_files + 1
                files(num_files) = trim(line)
            end do
            close (unit)
        end if

        ! Clean up
        call execute_command_line('rm -f '//trim(temp_file))

    end subroutine get_f90_files_except_main

    function extract_basename(filepath) result(basename)
        character(len=*), intent(in) :: filepath
        character(len=256) :: basename
        integer :: last_slash

        last_slash = index(filepath, '/', back=.true.)
        if (last_slash > 0) then
            basename = filepath(last_slash + 1:)
        else
            basename = filepath
        end if

    end function extract_basename

    subroutine generate_flag_string(is_preprocessed_file, custom_flags, flag_string)
        logical, intent(in) :: is_preprocessed_file
        character(len=*), intent(in) :: custom_flags
        character(len=*), intent(out) :: flag_string

        character(len=256) :: opinionated_flags

        ! Define opinionated flags for .f files (preprocessed files)
        opinionated_flags = '-fdefault-real-8 -fdefault-double-8'

        ! Generate flag string based on file type and user flags
        if (is_preprocessed_file .and. len_trim(custom_flags) > 0) then
            ! Preprocessed file with user flags: combine opinionated + user flags
            flag_string = trim(opinionated_flags)//' '//trim(custom_flags)
        else if (is_preprocessed_file) then
            ! Preprocessed file without user flags: use opinionated flags only
            flag_string = opinionated_flags
        else if (len_trim(custom_flags) > 0) then
            ! Standard .f90 file with user flags: use user flags only
            flag_string = custom_flags
        else
            ! Standard .f90 file without user flags: no flags
            flag_string = ''
        end if

    end subroutine generate_flag_string

end module runner
