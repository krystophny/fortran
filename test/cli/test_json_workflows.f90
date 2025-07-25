program test_json_workflows
    use temp_utils, only: get_system_temp_dir, create_temp_dir, get_project_root, create_test_cache_dir, path_join
    use fpm_environment, only: get_os_type, OS_WINDOWS, get_env
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Skip this test on Windows CI - it runs fortran CLI 16 times
    if (get_os_type() == OS_WINDOWS .and. len_trim(get_env('CI', '')) > 0) then
        print *, 'SKIP: test_json_workflows on Windows CI (runs fortran CLI 16 times)'
        stop 0
    end if

    print *, '=== JSON Workflow Tests ==='
    print *

    ! Test complete JSON pipeline workflows
    if (.not. test_simple_assignment_workflow()) all_passed = .false.
    if (.not. test_function_workflow()) all_passed = .false.
    if (.not. test_control_flow_workflow()) all_passed = .false.
    if (.not. test_round_trip_workflow()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All JSON workflow tests passed!'
        stop 0
    else
        print *, 'Some JSON workflow tests failed!'
        stop 1
    end if

contains

    logical function test_simple_assignment_workflow()
        character(len=:), allocatable :: temp_dir, cache_dir
        integer :: iostat, unit
        logical :: success

        test_simple_assignment_workflow = .true.
        temp_dir = get_system_temp_dir()
        cache_dir = create_test_cache_dir('json_basic')

        print *, 'Testing simple assignment workflow...'

        ! Step 1: Create source file
        open (newunit=unit, file=path_join(temp_dir, 'simple.f'), status='replace')
        write (unit, '(a)') 'x = 42'
        close (unit)

        ! Step 2: Generate tokens
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                                     '"'//path_join(temp_dir, 'simple.f')//'" --debug-tokens'// &
                                     merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: Token generation failed'
            test_simple_assignment_workflow = .false.
            return
        end if

        ! Step 3: Parse to AST from tokens
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                '"'//path_join(temp_dir, 'simple_tokens.json')//'" --from-tokens --debug-ast'// &
                merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: AST generation from tokens failed'
            test_simple_assignment_workflow = .false.
            return
        end if

        ! Step 4: Semantic analysis from AST (AST file is named simple_tokens_ast.json)
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
          '"'//path_join(temp_dir, 'simple_tokens_ast.json')//'" --from-ast --debug-semantic'// &
          merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: Semantic analysis from AST failed'
            test_simple_assignment_workflow = .false.
            return
        end if

        ! Step 5: Code generation from semantic AST (semantic file is named simple_tokens_ast_semantic.json)
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                          '"'//path_join(temp_dir, 'simple_tokens_ast_semantic.json')//'" --from-ast > "'// &
                   path_join(temp_dir, 'generated.f90')//'"'//merge(' 2>nul      ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                   wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: Code generation from semantic AST failed'
            test_simple_assignment_workflow = .false.
            return
        end if

        ! For now, just verify JSON files were created
        ! (Full round-trip not implemented yet)
        if (verify_file_exists(path_join(temp_dir, 'simple_tokens.json')) .and. &
            verify_file_exists(path_join(temp_dir, 'simple_tokens_ast.json'))) then
            print *, '  PASS: Simple assignment workflow (JSON files created)'
        else
            print *, '  FAIL: JSON files not created'
            test_simple_assignment_workflow = .false.
        end if

    end function test_simple_assignment_workflow

    logical function test_function_workflow()
        character(len=:), allocatable :: temp_dir, cache_dir
        integer :: iostat, unit

        test_function_workflow = .true.
        temp_dir = get_system_temp_dir()
        cache_dir = create_test_cache_dir('json_functions')

        print *, 'Testing function workflow...'

        ! Clear cache first
        !call execute_command_line('fpm run fortran -- --clear-cache -- Removed cache clearing > /dev/null 2>&1', wait=.true.)

        ! Create source with function
        open (newunit=unit, file=path_join(temp_dir, 'func.f'), status='replace')
        write (unit, '(a)') 'real function square(x)'
        write (unit, '(a)') '  real :: x'
        write (unit, '(a)') '  square = x * x'
        write (unit, '(a)') 'end function'
        close (unit)

        ! Run full pipeline
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                                      '"'//path_join(temp_dir, 'func.f')//'" --debug-tokens'// &
                                      merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: Token generation for function failed'
            test_function_workflow = .false.
            return
        end if

        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                  '"'//path_join(temp_dir, 'func_tokens.json')//'" --from-tokens --debug-ast'// &
                  merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: AST generation for function failed'
            test_function_workflow = .false.
            return
        end if

        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
            '"'//path_join(temp_dir, 'func_tokens_ast.json')//'" --from-ast --debug-semantic'// &
            merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: Semantic analysis for function failed'
            test_function_workflow = .false.
            return
        end if

        ! Verify function in AST output
   if (verify_file_contains(path_join(temp_dir, 'func_tokens_ast.json'), '"name": "square"')) then
            print *, '  PASS: Function workflow'
        else
            print *, '  FAIL: Function not found in AST output'
            test_function_workflow = .false.
        end if

    end function test_function_workflow

    logical function test_control_flow_workflow()
        character(len=:), allocatable :: temp_dir, cache_dir
        integer :: iostat, unit

        test_control_flow_workflow = .true.
        temp_dir = get_system_temp_dir()
        cache_dir = create_test_cache_dir('json_control_flow')

        print *, 'Testing control flow workflow...'

        ! Clear cache first
        !call execute_command_line('fpm run fortran -- --clear-cache -- Removed cache clearing > /dev/null 2>&1', wait=.true.)

        ! Create source with if statement in a program
        open (newunit=unit, file=path_join(temp_dir, 'if.f'), status='replace')
        write (unit, '(a)') 'x = 5'
        write (unit, '(a)') 'if (x > 0) then'
        write (unit, '(a)') '  y = 1'
        write (unit, '(a)') 'else'
        write (unit, '(a)') '  y = -1'
        write (unit, '(a)') 'end if'
        close (unit)

        ! Generate tokens
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                                      '"'//path_join(temp_dir, 'if.f')//'" --debug-tokens'// &
                                      merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: Token generation for control flow failed'
            test_control_flow_workflow = .false.
            return
        end if

        ! Parse to AST
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                    '"'//path_join(temp_dir, 'if_tokens.json')//'" --from-tokens --debug-ast'// &
                    merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: AST generation for control flow failed'
            test_control_flow_workflow = .false.
            return
        end if

        ! Verify if node in AST (AST file is named if_tokens_ast.json)
        if (verify_file_contains(path_join(temp_dir, 'if_tokens_ast.json'), '"type": "if_statement"')) then
            print *, '  PASS: Control flow workflow'
        else
            print *, '  FAIL: If statement not found in AST'
            test_control_flow_workflow = .false.
        end if

    end function test_control_flow_workflow

    logical function test_round_trip_workflow()
        character(len=:), allocatable :: temp_dir, cache_dir
        integer :: iostat, unit
        character(len=1024) :: line
        logical :: found_assignment

        test_round_trip_workflow = .true.
        temp_dir = get_system_temp_dir()
        cache_dir = create_test_cache_dir('json_roundtrip')

        print *, 'Testing round-trip workflow...'

        ! Clear cache first
        !call execute_command_line('fpm run fortran -- --clear-cache -- Removed cache clearing > /dev/null 2>&1', wait=.true.)

        ! Create original source
        open (newunit=unit, file=path_join(temp_dir, 'original.f'), status='replace')
        write (unit, '(a)') 'x = 10'
        write (unit, '(a)') 'y = x + 5'
        write (unit, '(a)') 'print *, y'
        close (unit)

        ! Full pipeline: source -> tokens -> AST -> semantic -> code
        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                                   '"'//path_join(temp_dir, 'original.f')//'" --debug-tokens'// &
                                   merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: Initial tokenization failed'
            test_round_trip_workflow = .false.
            return
        end if

        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
              '"'//path_join(temp_dir, 'original_tokens.json')//'" --from-tokens --debug-ast'// &
              merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: AST generation in round-trip failed'
            test_round_trip_workflow = .false.
            return
        end if

        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
        '"'//path_join(temp_dir, 'original_tokens_ast.json')//'" --from-ast --debug-semantic'// &
        merge(' > nul 2>&1 ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                                      wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: Semantic analysis in round-trip failed'
            test_round_trip_workflow = .false.
            return
        end if

        block
            character(len=:), allocatable :: project_root
            project_root = get_project_root()
            call execute_command_line('cd "'//project_root//'" && '// &
                           'fpm run fortran -- --cache-dir "'//trim(cache_dir)//'" '// &
                        '"'//path_join(temp_dir, 'original_tokens_ast_semantic.json')//'" --from-ast > "'// &
                   path_join(temp_dir, 'roundtrip.f90')//'"'//merge(' 2>nul      ', ' 2>/dev/null', get_os_type() == OS_WINDOWS), &
                   wait=.true., exitstat=iostat)
        end block
        if (iostat /= 0) then
            print *, '  FAIL: Code generation in round-trip failed'
            test_round_trip_workflow = .false.
            return
        end if

        ! Just verify that all JSON files were created in the pipeline
        if (verify_file_exists(path_join(temp_dir, 'original_tokens.json')) .and. &
            verify_file_exists(path_join(temp_dir, 'original_tokens_ast.json'))) then
            print *, '  PASS: Round-trip workflow (JSON pipeline working)'
        else
            print *, '  FAIL: JSON pipeline incomplete'
            test_round_trip_workflow = .false.
        end if

    end function test_round_trip_workflow

    ! Helper function to verify file contains string
    logical function verify_file_contains(filename, search_string)
        character(len=*), intent(in) :: filename, search_string
        integer :: unit, iostat
        character(len=1024) :: line

        verify_file_contains = .false.

        open (newunit=unit, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(line, search_string) > 0) then
                verify_file_contains = .true.
                exit
            end if
        end do

        close (unit)
    end function verify_file_contains

    ! Helper function to check if file exists
    logical function verify_file_exists(filename)
        character(len=*), intent(in) :: filename
        logical :: exists

        inquire (file=filename, exist=exists)
        verify_file_exists = exists
    end function verify_file_exists

end program test_json_workflows
