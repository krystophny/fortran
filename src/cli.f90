module cli
  implicit none
  private
  public :: parse_arguments
  
contains

  subroutine parse_arguments(filename, show_help, verbose_level, custom_cache_dir, &
                             custom_config_dir, parallel_jobs, no_wait, notebook_mode, &
                             notebook_output, custom_flags)
    character(len=*), intent(out) :: filename
    logical, intent(out) :: show_help
    integer, intent(out) :: verbose_level
    character(len=*), intent(out) :: custom_cache_dir
    character(len=*), intent(out) :: custom_config_dir
    integer, intent(out) :: parallel_jobs
    logical, intent(out) :: no_wait
    logical, intent(out) :: notebook_mode
    character(len=*), intent(out) :: notebook_output
    character(len=*), intent(out), optional :: custom_flags
    
    integer :: nargs, i, iostat
    character(len=256) :: arg
    logical :: filename_found, expecting_cache_dir, expecting_config_dir, expecting_jobs, expecting_output, expecting_flags
    
    filename = ''
    show_help = .false.
    verbose_level = 0
    custom_cache_dir = ''
    custom_config_dir = ''
    parallel_jobs = 0  ! 0 means use FPM default
    no_wait = .false.
    notebook_mode = .false.
    notebook_output = ''
    if (present(custom_flags)) then
      custom_flags = ''
    end if
    filename_found = .false.
    expecting_cache_dir = .false.
    expecting_config_dir = .false.
    expecting_jobs = .false.
    expecting_output = .false.
    expecting_flags = .false.
    
    nargs = command_argument_count()
    
    if (nargs == 0) then
      show_help = .true.
      return
    end if
    
    i = 1
    do while (i <= nargs)
      call get_command_argument(i, arg)
      
      if (expecting_cache_dir) then
        custom_cache_dir = trim(arg)
        expecting_cache_dir = .false.
      else if (expecting_config_dir) then
        custom_config_dir = trim(arg)
        expecting_config_dir = .false.
      else if (expecting_output) then
        notebook_output = trim(arg)
        expecting_output = .false.
      else if (expecting_flags) then
        if (present(custom_flags)) then
          custom_flags = trim(arg)
        end if
        expecting_flags = .false.
      else if (expecting_jobs) then
        read(arg, *, iostat=iostat) parallel_jobs
        if (iostat /= 0 .or. parallel_jobs < 1) then
          print '(a)', 'Error: Invalid number of jobs. Must be a positive integer.'
          stop 1
        end if
        expecting_jobs = .false.
      else if (arg == '--help' .or. arg == '-h') then
        show_help = .true.
        return
      else if (arg == '-v') then
        verbose_level = 1
      else if (arg == '-vv') then
        verbose_level = 2
      else if (arg == '--verbose') then
        ! Check if next argument is a number (1 or 2)
        if (i < nargs) then
          call get_command_argument(i+1, arg)
          if (arg == '1') then
            verbose_level = 1
            i = i + 1  ! Skip the next argument since we consumed it
          else if (arg == '2') then
            verbose_level = 2
            i = i + 1  ! Skip the next argument since we consumed it
          else
            ! Next arg is not a number, default to level 1
            verbose_level = 1
          end if
        else
          ! No more arguments, default to level 1
          verbose_level = 1
        end if
      else if (arg == '--cache-dir') then
        expecting_cache_dir = .true.
      else if (arg == '--config-dir') then
        expecting_config_dir = .true.
      else if (arg == '-j' .or. arg == '--jobs') then
        expecting_jobs = .true.
      else if (arg == '--no-wait') then
        no_wait = .true.
      else if (arg == '--notebook') then
        notebook_mode = .true.
      else if (arg == '-o' .or. arg == '--output') then
        expecting_output = .true.
      else if (arg == '--flag') then
        if (present(custom_flags)) then
          expecting_flags = .true.
        else
          ! Flag option used but custom_flags not provided - skip silently
          if (i < nargs) i = i + 1  ! Skip the next argument
        end if
      else if (arg(1:1) /= '-') then
        ! Not a flag, must be filename
        if (.not. filename_found) then
          filename = trim(arg)
          filename_found = .true.
        end if
      end if
      
      i = i + 1
    end do
    
    if (expecting_cache_dir) then
      print '(a)', 'Error: --cache-dir requires an argument'
      show_help = .true.
    end if
    
    if (expecting_config_dir) then
      print '(a)', 'Error: --config-dir requires an argument'
      show_help = .true.
    end if
    
    if (expecting_jobs) then
      print '(a)', 'Error: -j/--jobs requires a number'
      show_help = .true.
    end if
    
    if (expecting_output) then
      print '(a)', 'Error: -o/--output requires an argument'
      show_help = .true.
    end if
    
    if (expecting_flags) then
      print '(a)', 'Error: --flag requires an argument'
      show_help = .true.
    end if
    
    if (.not. filename_found) then
      show_help = .true.
    end if
    
    ! If notebook mode and no output specified, generate default
    if (notebook_mode .and. notebook_output == '') then
      call generate_default_output_name(filename, notebook_output)
    end if
    
  end subroutine parse_arguments
  
  subroutine generate_default_output_name(input_file, output_file)
    character(len=*), intent(in) :: input_file
    character(len=*), intent(out) :: output_file
    
    integer :: dot_pos, slash_pos
    character(len=256) :: base_name
    
    ! Find last slash to get basename
    slash_pos = index(input_file, '/', back=.true.)
    if (slash_pos > 0) then
      base_name = input_file(slash_pos+1:)
    else
      base_name = input_file
    end if
    
    ! Find last dot to remove extension
    dot_pos = index(base_name, '.', back=.true.)
    if (dot_pos > 0) then
      output_file = trim(base_name(1:dot_pos-1)) // '.md'
    else
      output_file = trim(base_name) // '.md'
    end if
    
  end subroutine generate_default_output_name
  
end module cli