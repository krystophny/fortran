module notebook_executor
    use notebook_parser
    use notebook_output
    use figure_capture
    use cache, only: get_cache_dir, get_content_hash, cache_exists
    use cache_lock, only: acquire_lock, release_lock
    use frontend_integration, only: compile_with_frontend, is_simple_fortran_file
    use, intrinsic :: iso_c_binding
    use temp_utils, only: create_temp_dir, cleanup_temp_dir, get_temp_file_path
    implicit none
    private

    interface
        function c_getpid() bind(C, name="getpid")
            import :: c_int
            integer(c_int) :: c_getpid
        end function c_getpid
    end interface

    ! Cell execution result
    type, public :: cell_result_t
        logical :: success = .true.
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error
        character(len=:), allocatable :: figure_data  ! Base64 encoded PNG
    end type cell_result_t

    ! Execution results for entire notebook
    type, public :: execution_result_t
        type(cell_result_t), allocatable :: cells(:)
        logical :: success = .true.
        character(len=:), allocatable :: error_message
    end type execution_result_t

    ! Public procedures
    public :: execute_notebook
    public :: free_execution_results

contains

    subroutine execute_notebook(notebook, results, custom_cache_dir, verbose_level)
        type(notebook_t), intent(inout) :: notebook
        type(execution_result_t), intent(out) :: results
        character(len=*), intent(in), optional :: custom_cache_dir
        integer, intent(in), optional :: verbose_level

        character(len=:), allocatable :: temp_dir, fpm_project_dir, cache_dir
        character(len=:), allocatable :: cache_key, notebook_content
        logical :: cache_hit, lock_acquired
        integer :: i, verb_level

        ! Set default verbose level
        verb_level = 0
        if (present(verbose_level)) verb_level = verbose_level

        ! Allocate results for all cells
        allocate (results%cells(notebook%num_cells))
        results%success = .true.

        ! Get cache directory
        if (present(custom_cache_dir) .and. len_trim(custom_cache_dir) > 0) then
            cache_dir = trim(custom_cache_dir)
        else
            cache_dir = get_cache_dir()
        end if

        ! Ensure cache directory exists
        call execute_command_line('mkdir -p "'//trim(cache_dir)//'"')

        ! Generate cache key from notebook content
        call generate_notebook_cache_key(notebook, cache_key)

        ! Check cache
        call check_notebook_cache(cache_dir, cache_key, cache_hit, fpm_project_dir)

        if (cache_hit) then
            if (verb_level > 0) then
                print *, "Cache hit: Using existing notebook build"
            end if
        else
            if (verb_level > 0) then
                print *, "Cache miss: Building notebook"
            end if

            ! Acquire cache lock
           lock_acquired = acquire_lock(cache_dir, 'notebook_'//trim(cache_key), .true.)

            if (.not. lock_acquired) then
                results%success = .false.
                results%error_message = "Could not acquire cache lock"
                return
            end if

            ! Create temporary directory for notebook project
            call create_temp_notebook_dir(temp_dir)
            fpm_project_dir = trim(temp_dir)//'/notebook_project'

            ! Handle .f preprocessing if needed
            call preprocess_notebook_if_needed(notebook)

            ! Generate single module FPM project
    call generate_single_module_project(notebook, fpm_project_dir, cache_dir, cache_key)

            ! Build the notebook project with FPM
    call build_notebook_project(fpm_project_dir, results%success, results%error_message)

            if (.not. results%success) then
                call release_lock(cache_dir, 'notebook_'//trim(cache_key))
                call cleanup_temp_dir(temp_dir)
                return
            end if

            ! Cache the built project
            call cache_notebook_build(cache_dir, cache_key, fpm_project_dir)

            call release_lock(cache_dir, 'notebook_'//trim(cache_key))
            call cleanup_temp_dir(temp_dir)

            ! Update project dir to point to cached version
            fpm_project_dir = trim(cache_dir)//'/notebook_'//trim(cache_key)
        end if

        ! Execute the notebook and capture outputs
 call execute_notebook_project(fpm_project_dir, cache_dir, cache_key, notebook, results)

    end subroutine execute_notebook

  subroutine generate_single_module_project(notebook, project_dir, cache_dir, cache_key)
        type(notebook_t), intent(in) :: notebook
        character(len=*), intent(in) :: project_dir, cache_dir, cache_key

        character(len=512) :: command
        character(len=:), allocatable :: module_content, main_content, fpm_content
        integer :: unit, i, code_cell_count

        ! Create project directory structure
        command = 'mkdir -p '//trim(project_dir)//'/src '//trim(project_dir)//'/app'
        call execute_command_line(command)

        ! Generate fpm.toml
        fpm_content = generate_notebook_fpm_toml()
        open (newunit=unit, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (unit, '(a)') fpm_content
        close (unit)

        ! Generate the notebook execution module
        call generate_notebook_module(notebook, module_content)
        open(newunit=unit, file=trim(project_dir) // '/src/notebook_execution.f90', status='replace')
        write (unit, '(a)') module_content
        close (unit)

        ! Copy notebook_output module
        call copy_notebook_output_module(project_dir)

        ! Copy figure_capture module
        call copy_figure_capture_module(project_dir)

        ! Generate main program
        call generate_main_program(notebook, trim(cache_dir) // '/notebook_' // trim(cache_key) // '_outputs.txt', main_content)
        open (newunit=unit, file=trim(project_dir)//'/app/main.f90', status='replace')
        write (unit, '(a)') main_content
        close (unit)

    end subroutine generate_single_module_project

    subroutine generate_notebook_cache_key(notebook, cache_key)
        type(notebook_t), intent(in) :: notebook
        character(len=:), allocatable, intent(out) :: cache_key

        character(len=:), allocatable :: combined_content
        character(len=32) :: hash_str
        integer :: i

        ! Combine all cell content for hashing
        combined_content = ""
        do i = 1, notebook%num_cells
    combined_content = trim(combined_content)//trim(notebook%cells(i)%content)//char(10)
        end do

        ! Generate simple hash from content length and first/last chars
        call generate_simple_hash(combined_content, cache_key)

    end subroutine generate_notebook_cache_key

    subroutine generate_simple_hash(content, hash)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: hash

        integer :: i, hash_val, content_len
        character(len=16) :: hash_str

        content_len = len(content)
        hash_val = content_len

        ! Simple hash based on content length and character sum
        do i = 1, min(content_len, 1000)  ! Sample first 1000 chars
            hash_val = hash_val + ichar(content(i:i))*i
        end do

        ! Convert to hex string
        write (hash_str, '(z0)') hash_val
        hash = trim(hash_str)

    end subroutine generate_simple_hash

    subroutine check_notebook_cache(cache_dir, cache_key, cache_hit, project_dir)
        character(len=*), intent(in) :: cache_dir, cache_key
        logical, intent(out) :: cache_hit
        character(len=:), allocatable, intent(out) :: project_dir

        project_dir = trim(cache_dir)//'/notebook_'//trim(cache_key)

        ! Check if cached project exists by checking for fpm.toml
        inquire (file=trim(project_dir)//'/fpm.toml', exist=cache_hit)

    end subroutine check_notebook_cache

    subroutine cache_notebook_build(cache_dir, cache_key, project_dir)
        character(len=*), intent(in) :: cache_dir, cache_key, project_dir

        character(len=:), allocatable :: cached_project_dir
        character(len=512) :: command

        cached_project_dir = trim(cache_dir)//'/notebook_'//trim(cache_key)

        ! Create cache directory
        command = 'mkdir -p "'//trim(cache_dir)//'"'
        call execute_command_line(command)

        ! Copy project to cache
        command = 'cp -r "'//trim(project_dir)//'" "'//trim(cached_project_dir)//'"'
        call execute_command_line(command)

    end subroutine cache_notebook_build

    subroutine preprocess_notebook_if_needed(notebook)
        type(notebook_t), intent(inout) :: notebook

        integer :: i
        character(len=:), allocatable :: preprocessed_content

        ! Check each code cell for .f file preprocessing needs
        do i = 1, notebook%num_cells
            if (notebook%cells(i)%cell_type == CELL_CODE) then
                ! For now, just check if we need implicit none insertion
                if (index(notebook%cells(i)%content, 'implicit') == 0) then
                    ! Add implicit none at the beginning if not present
                    preprocessed_content = 'implicit none'//new_line('a')// &
                                           trim(notebook%cells(i)%content)
                    notebook%cells(i)%content = preprocessed_content
                end if
            end if
        end do

    end subroutine preprocess_notebook_if_needed

    subroutine generate_notebook_module(notebook, module_content)
        type(notebook_t), intent(in) :: notebook
        character(len=:), allocatable, intent(out) :: module_content

        character(len=:), allocatable :: variables_section, procedures_section
        integer :: i, code_cell_count

        ! Analyze variables across all cells
        call analyze_notebook_variables(notebook, variables_section)

        ! Generate procedures for each code cell
        procedures_section = ""
        code_cell_count = 0

        do i = 1, notebook%num_cells
            if (notebook%cells(i)%cell_type == CELL_CODE) then
                code_cell_count = code_cell_count + 1
                procedures_section = trim(procedures_section)//new_line('a')// &
                             generate_cell_procedure(notebook%cells(i), code_cell_count)
            end if
        end do

        ! Combine into full module
        module_content = 'module notebook_execution'//new_line('a')// &
                         '    use notebook_output'//new_line('a')// &
                         '    use figure_capture'//new_line('a')// &
                         '    implicit none'//new_line('a')// &
                         '    '//new_line('a')// &
                         '    ! Module variables for persistence'//new_line('a')// &
                         variables_section//new_line('a')// &
                         '    '//new_line('a')// &
                         'contains'//new_line('a')// &
                         procedures_section//new_line('a')// &
                         'end module notebook_execution'

    end subroutine generate_notebook_module

    subroutine analyze_notebook_variables(notebook, variables_section)
        type(notebook_t), intent(in) :: notebook
        character(len=:), allocatable, intent(out) :: variables_section

        ! For now, use common variable types
        ! TODO: Implement proper variable analysis
        variables_section = '    real(8) :: x, y, z, sum, diff, product, quotient' // new_line('a') // &
                            '    integer :: i, j, k, n, count'//new_line('a')// &
                            '    logical :: flag, ready'//new_line('a')// &
                            '    character(len=256) :: text, label'//new_line('a')// &
                      '    character(len=1024) :: temp_str  ! For print transformations'

    end subroutine analyze_notebook_variables

    function generate_cell_procedure(cell, cell_number) result(procedure_code)
        type(cell_t), intent(in) :: cell
        integer, intent(in) :: cell_number
        character(len=:), allocatable :: procedure_code
        character(len=:), allocatable :: transformed_content

        ! Transform print statements to notebook_print calls
        call transform_cell_content(cell%content, transformed_content)

        procedure_code = '    subroutine cell_' // trim(int_to_str(cell_number)) // '()' // new_line('a') // &
                     '        ! Cell '//trim(int_to_str(cell_number))//new_line('a')// &
                         '        '//new_line('a')// &
                     add_indentation(transformed_content, '        ')//new_line('a')// &
                         '        '//new_line('a')// &
                         '    end subroutine cell_'//trim(int_to_str(cell_number))

    end function generate_cell_procedure

    subroutine transform_cell_content(content, transformed)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: transformed

        character(len=:), allocatable :: lines(:)
        integer :: num_lines, i
        logical :: first_line

        ! Split content into lines
        call split_content_lines(content, lines, num_lines)

        transformed = ""
        first_line = .true.

        do i = 1, num_lines
            if (.not. first_line) then
                transformed = trim(transformed)//new_line('a')
            else
                first_line = .false.
            end if

            ! Transform print statements and show() calls
            if (index(lines(i), 'print *,') > 0) then
                transformed = trim(transformed)//transform_print_statement(lines(i))
    else if (index(lines(i), 'show()') > 0 .or. index(lines(i), 'call show()') > 0) then
                transformed = trim(transformed)//transform_show_statement(lines(i))
            else
                transformed = trim(transformed)//trim(lines(i))
            end if
        end do

    end subroutine transform_cell_content

    function transform_print_statement(line) result(transformed_line)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: transformed_line
        integer :: print_pos
        character(len=:), allocatable :: args_part

        print_pos = index(line, 'print *,')
        if (print_pos > 0) then
            args_part = trim(adjustl(line(print_pos + 8:)))

            ! Check for common patterns and transform appropriately
            if (index(args_part, ',') > 0) then
                ! Multiple arguments - need to create a format string
                ! Use a simpler, more compatible format
                transformed_line = line(1:print_pos - 1)// &
                               'write(temp_str, *) '//trim(args_part)//new_line('a')// &
                       repeat(' ', print_pos - 1)//'call notebook_print(trim(temp_str))'
            else
                ! Single argument - direct call
                transformed_line = line(1:print_pos - 1)//'call notebook_print('// &
                                   trim(args_part)//')'
            end if
        else
            transformed_line = line
        end if

    end function transform_print_statement

    function transform_show_statement(line) result(transformed_line)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: transformed_line
        integer :: show_pos

        ! Handle both "show()" and "call show()" patterns
        show_pos = index(line, 'call show()')
        if (show_pos > 0) then
            ! Replace "call show()" with "call fortplot_show_interceptor()"
         transformed_line = line(1:show_pos - 1)//'call fortplot_show_interceptor()'// &
                               line(show_pos + 11:)
        else
            show_pos = index(line, 'show()')
            if (show_pos > 0) then
                ! Replace "show()" with "call fortplot_show_interceptor()"
         transformed_line = line(1:show_pos - 1)//'call fortplot_show_interceptor()'// &
                                   line(show_pos + 6:)
            else
                transformed_line = line
            end if
        end if

    end function transform_show_statement

    subroutine generate_main_program(notebook, output_file, main_content)
        type(notebook_t), intent(in) :: notebook
        character(len=*), intent(in) :: output_file
        character(len=:), allocatable, intent(out) :: main_content

        character(len=:), allocatable :: execution_calls
        integer :: i, code_cell_count

        ! Generate calls to each cell procedure
        execution_calls = ""
        code_cell_count = 0

        do i = 1, notebook%num_cells
            if (notebook%cells(i)%cell_type == CELL_CODE) then
                code_cell_count = code_cell_count + 1
                execution_calls = trim(execution_calls)// &
                                '    call start_cell_capture(' // trim(int_to_str(code_cell_count)) // ')' // new_line('a') // &
                '    call cell_'//trim(int_to_str(code_cell_count))//'()'//new_line('a')
            end if
        end do

        main_content = 'program notebook_runner'//new_line('a')// &
                       '    use notebook_execution'//new_line('a')// &
                       '    use notebook_output'//new_line('a')// &
                       '    implicit none'//new_line('a')// &
                       '    '//new_line('a')// &
                      '    call init_output_capture(' // trim(int_to_str(code_cell_count)) // ')' // new_line('a') // &
                       '    '//new_line('a')// &
                       execution_calls// &
                       '    '//new_line('a')// &
          '    call write_outputs_to_file("'//trim(output_file)//'")'//new_line('a')// &
                       '    call finalize_output_capture()'//new_line('a')// &
                       '    '//new_line('a')// &
                       'end program notebook_runner'

    end subroutine generate_main_program

    function generate_notebook_fpm_toml() result(content)
        character(len=:), allocatable :: content

        content = 'name = "notebook_exec"'//new_line('a')// &
                  'version = "0.1.0"'//new_line('a')// &
                  ''//new_line('a')// &
                  '[build]'//new_line('a')// &
                  'auto-executables = true'//new_line('a')// &
                  ''//new_line('a')// &
                  '[fortran]'//new_line('a')// &
                  'implicit-typing = false'//new_line('a')// &
                  'implicit-external = false'//new_line('a')// &
                  'source-form = "free"'//new_line('a')

    end function generate_notebook_fpm_toml

    subroutine copy_notebook_output_module(project_dir)
        character(len=*), intent(in) :: project_dir
        character(len=512) :: command
        character(len=256) :: current_dir

        ! Get current working directory
        call getcwd(current_dir)

        ! Copy the notebook_output module to the project
        command = 'cp "'//trim(current_dir)//'/src/notebook/notebook_output.f90" "'// &
                  trim(project_dir)//'/src/"'
        call execute_command_line(command)

    end subroutine copy_notebook_output_module

    subroutine copy_figure_capture_module(project_dir)
        character(len=*), intent(in) :: project_dir
        character(len=512) :: command
        character(len=256) :: current_dir

        ! Get current working directory
        call getcwd(current_dir)

        ! Copy the figure_capture module to the project
        command = 'cp "'//trim(current_dir)//'/src/figure/figure_capture.f90" "'// &
                  trim(project_dir)//'/src/"'
        call execute_command_line(command)

    end subroutine copy_figure_capture_module

    subroutine build_notebook_project(project_dir, success, error_msg)
        character(len=*), intent(in) :: project_dir
        logical, intent(out) :: success
        character(len=:), allocatable, intent(out) :: error_msg

        character(len=512) :: command
        integer :: exit_code

        ! Build with FPM
        command = 'cd '//trim(project_dir)//' && fpm build 2>&1'
        call execute_and_capture(command, error_msg, exit_code)

        success = (exit_code == 0)

    end subroutine build_notebook_project

    subroutine execute_notebook_project(project_dir, cache_dir, cache_key, notebook, results)
        character(len=*), intent(in) :: project_dir, cache_dir, cache_key
        type(notebook_t), intent(in) :: notebook
        type(execution_result_t), intent(inout) :: results

        character(len=512) :: command
        character(len=:), allocatable :: output
        integer :: exit_code, i

        ! Initialize figure capture
        call init_figure_capture()

        ! Execute the notebook
        command = 'cd '//trim(project_dir)//' && fpm run 2>&1'
        call execute_and_capture(command, output, exit_code)

        ! Read actual output from notebook_output module
        if (exit_code == 0) then
            call read_notebook_outputs(cache_dir, cache_key, notebook, results)
            ! Collect figure data for cells that generated plots
            call collect_figure_data(notebook, results)
        else
            ! Set simple failure for all cells
            do i = 1, notebook%num_cells
                results%cells(i)%success = .false.
                if (notebook%cells(i)%cell_type == CELL_CODE) then
                    results%cells(i)%output = ""
                    results%cells(i)%error = "Execution failed"
                else
                    results%cells(i)%output = ""
                end if
            end do
        end if

        results%success = (exit_code == 0)
        if (.not. results%success) then
            results%error_message = output
        end if

        ! Cleanup figure capture
        call finalize_figure_capture()

    end subroutine execute_notebook_project

    subroutine read_notebook_outputs(cache_dir, cache_key, notebook, results)
        character(len=*), intent(in) :: cache_dir, cache_key
        type(notebook_t), intent(in) :: notebook
        type(execution_result_t), intent(inout) :: results

        character(len=:), allocatable :: output_file
        character(len=:), allocatable :: cell_outputs(:)
        integer :: i, code_cell_index
        logical :: file_exists

        output_file = trim(cache_dir)//'/notebook_'//trim(cache_key)//'_outputs.txt'

        ! Check if output file exists
        inquire (file=output_file, exist=file_exists)

        if (file_exists) then
            call read_outputs_from_file(output_file, cell_outputs)

            ! Map outputs to cell results (only code cells have outputs)
            code_cell_index = 0
            do i = 1, notebook%num_cells
                results%cells(i)%success = .true.
                if (notebook%cells(i)%cell_type == CELL_CODE) then
                    code_cell_index = code_cell_index + 1
                    if (code_cell_index <= size(cell_outputs)) then
                        results%cells(i)%output = trim(cell_outputs(code_cell_index))
                    else
                        results%cells(i)%output = ""
                    end if
                else
                    results%cells(i)%output = ""
                end if
            end do
        else
            ! Fallback if no output file
            do i = 1, notebook%num_cells
                results%cells(i)%success = .true.
                if (notebook%cells(i)%cell_type == CELL_CODE) then
                    results%cells(i)%output = "No output captured"
                else
                    results%cells(i)%output = ""
                end if
            end do
        end if

    end subroutine read_notebook_outputs

    subroutine collect_figure_data(notebook, results)
        type(notebook_t), intent(in) :: notebook
        type(execution_result_t), intent(inout) :: results

        integer :: i, code_cell_index, figure_count
        character(len=:), allocatable :: base64_data

        ! Count figures generated and assign to cells that contain show() calls
        figure_count = 0
        code_cell_index = 0

        do i = 1, notebook%num_cells
            if (notebook%cells(i)%cell_type == CELL_CODE) then
                code_cell_index = code_cell_index + 1

                ! Check if this cell contains show() calls
                if (index(notebook%cells(i)%content, 'show()') > 0 .or. &
                    index(notebook%cells(i)%content, 'call show()') > 0) then

                    figure_count = figure_count + 1

                    ! Get base64 data for this figure
                    base64_data = get_figure_data(figure_count)

                    if (len(base64_data) > 0) then
                        results%cells(i)%figure_data = base64_data
                    end if
                end if
            end if
        end do

    end subroutine collect_figure_data

    ! Helper functions (reusing from old implementation)
    function add_indentation(text, indent) result(indented_text)
        character(len=*), intent(in) :: text, indent
        character(len=:), allocatable :: indented_text
        character(len=:), allocatable :: lines(:)
        integer :: num_lines, i
        logical :: first_line

        call split_content_lines(text, lines, num_lines)

        indented_text = ""
        first_line = .true.

        do i = 1, num_lines
            if (.not. first_line) then
                indented_text = trim(indented_text)//new_line('a')
            else
                first_line = .false.
            end if
            indented_text = trim(indented_text)//trim(indent)//trim(lines(i))
        end do

    end function add_indentation

    subroutine split_content_lines(content, lines, num_lines)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: num_lines

        integer :: i, line_start, line_count, max_line_length

        ! Count lines and find max length
        line_count = 1
        max_line_length = 0
        line_start = 1

        do i = 1, len(content)
            if (content(i:i) == new_line('a')) then
                max_line_length = max(max_line_length, i - line_start)
                line_count = line_count + 1
                line_start = i + 1
            end if
        end do
        max_line_length = max(max_line_length, len(content) - line_start + 1)

        ! Allocate and fill lines array
        allocate (character(len=max_line_length) :: lines(line_count))

        line_count = 1
        line_start = 1
        do i = 1, len(content)
            if (content(i:i) == new_line('a')) then
                lines(line_count) = content(line_start:i - 1)
                line_count = line_count + 1
                line_start = i + 1
            end if
        end do
        if (line_start <= len(content)) then
            lines(line_count) = content(line_start:)
        else
            lines(line_count) = ""
        end if

        num_lines = line_count

    end subroutine split_content_lines

    subroutine free_execution_results(results)
        type(execution_result_t), intent(inout) :: results
        integer :: i

        if (allocated(results%cells)) then
            do i = 1, size(results%cells)
                if (allocated(results%cells(i)%output)) then
                    deallocate (results%cells(i)%output)
                end if
                if (allocated(results%cells(i)%error)) then
                    deallocate (results%cells(i)%error)
                end if
                if (allocated(results%cells(i)%figure_data)) then
                    deallocate (results%cells(i)%figure_data)
                end if
            end do
            deallocate (results%cells)
        end if

        if (allocated(results%error_message)) then
            deallocate (results%error_message)
        end if

    end subroutine free_execution_results

    ! Reuse helper functions from old implementation
    subroutine create_temp_notebook_dir(temp_dir)
        character(len=:), allocatable, intent(out) :: temp_dir
        character(len=:), allocatable :: output
        integer :: exit_code

        call execute_and_capture('mktemp -d', output, exit_code)

        if (exit_code == 0) then
            temp_dir = trim(adjustl(output))
     if (len(temp_dir) > 0 .and. temp_dir(len(temp_dir):len(temp_dir)) == char(10)) then
                temp_dir = temp_dir(1:len(temp_dir) - 1)
            end if
        else
            temp_dir = create_temp_dir('fortran_notebook')
            ! Directory already created by create_temp_dir
        end if

    end subroutine create_temp_notebook_dir

    subroutine execute_and_capture(command, output, exit_code)
        character(len=*), intent(in) :: command
        character(len=:), allocatable, intent(out) :: output
        integer, intent(out) :: exit_code

        character(len=256) :: temp_file
        character(len=512) :: full_command
        integer :: unit, iostat, file_size

     temp_file = get_temp_file_path(create_temp_dir('fortran_exec'), 'fortran_exec.out')

        full_command = trim(command)//' > '//trim(temp_file)//' 2>&1'
        call execute_command_line(full_command, exitstat=exit_code)

        inquire (file=temp_file, size=file_size)

        if (file_size > 0) then
            open (newunit=unit, file=temp_file, status='old', &
                  access='stream', form='unformatted', iostat=iostat)

            if (iostat == 0) then
                allocate (character(len=file_size) :: output)
                read (unit, iostat=iostat) output
                close (unit)
            else
                output = ""
            end if
        else
            output = ""
        end if

        call execute_command_line('rm -f '//trim(temp_file))

    end subroutine execute_and_capture

    function int_to_str(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str

        write (str, '(I0)') i
        str = trim(adjustl(str))

    end function int_to_str

    function get_process_id() result(pid)
        integer :: pid

        pid = int(c_getpid())

    end function get_process_id

end module notebook_executor
