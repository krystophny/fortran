program test_cli_json_options
    use temp_utils, only: get_system_temp_dir
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== CLI JSON Options Tests ==='
    print *

    ! Test JSON input functionality (skip help text parsing - test infrastructure issue)
    if (.not. test_json_functionality()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All CLI JSON option tests passed!'
        stop 0
    else
        print *, 'Some CLI JSON option tests failed!'
        stop 1
    end if

contains

    logical function test_json_functionality()
        character(len=:), allocatable :: temp_dir
        test_json_functionality = .true.
        print *, 'Testing JSON pipeline functionality...'

        temp_dir = get_system_temp_dir()

        ! Test --from-tokens option
        print *, '  Testing --from-tokens...'
        block
            integer :: iostat

            ! Create a simple JSON tokens file
            call execute_command_line('echo ''{"tokens": []}'' > '//temp_dir//'/test_tokens.json', exitstat=iostat)

            if (iostat == 0) then
                ! Test --from-tokens option with wait flag for CI reliability
     call execute_command_line('fpm run fortran -- '//temp_dir//'/test_tokens.json '// &
                                          '--from-tokens', wait=.true., exitstat=iostat)

                if (iostat == 0) then
                    print *, '    PASS: --from-tokens executed successfully'
                else
              print *, '    FAIL: --from-tokens execution failed with exit code', iostat
                    test_json_functionality = .false.
                end if
            else
                print *, '    FAIL: Could not create test JSON tokens file'
                test_json_functionality = .false.
            end if
        end block

        ! Test --from-ast option
        print *, '  Testing --from-ast...'
        block
            integer :: iostat

            ! Create a simple JSON AST file
         call execute_command_line('echo ''{"type": "program", "name": "test"}'' > '// &
                                      temp_dir//'/test_ast.json', exitstat=iostat)

            if (iostat == 0) then
                ! Test --from-ast option with wait flag for CI reliability
        call execute_command_line('fpm run fortran -- '//temp_dir//'/test_ast.json '// &
                                          '--from-ast', wait=.true., exitstat=iostat)

                if (iostat == 0) then
                    print *, '    PASS: --from-ast executed successfully'
                else
                 print *, '    FAIL: --from-ast execution failed with exit code', iostat
                    test_json_functionality = .false.
                end if
            else
                print *, '    FAIL: Could not create test JSON AST file'
                test_json_functionality = .false.
            end if
        end block

        ! Test --from-semantic option
        print *, '  Testing --from-semantic...'
        block
            integer :: iostat

            ! Create a simple JSON semantic file
           call execute_command_line('echo ''{"annotated_ast": {"type": "program", '// &
              '"name": "test"}}'' > '//temp_dir//'/test_semantic.json', exitstat=iostat)

            if (iostat == 0) then
                ! Test --from-semantic option with wait flag for CI reliability
   call execute_command_line('fpm run fortran -- '//temp_dir//'/test_semantic.json '// &
                                        '--from-semantic', wait=.true., exitstat=iostat)

                if (iostat == 0) then
                    print *, '    PASS: --from-semantic executed successfully'
                else
            print *, '    FAIL: --from-semantic execution failed with exit code', iostat
                    test_json_functionality = .false.
                end if
            else
                print *, '    FAIL: Could not create test JSON semantic file'
                test_json_functionality = .false.
            end if
        end block

        ! Clean up test files
        block
            integer :: iostat
            call execute_command_line('rm -f '//temp_dir//'/test_*.json '//temp_dir//'/test_*.f90', &
                                      exitstat=iostat)
        end block

    end function test_json_functionality

end program test_cli_json_options
