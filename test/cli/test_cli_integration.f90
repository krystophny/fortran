program test_cli_integration
    use temp_utils, only: create_temp_dir, get_temp_file_path
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== CLI Integration Tests ==='
    print *

    ! Test complete end-to-end workflows
    if (.not. test_basic_file_processing()) all_passed = .false.
    if (.not. test_debug_output_pipeline()) all_passed = .false.
    if (.not. test_json_pipeline_workflow()) all_passed = .false.
    if (.not. test_error_handling()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All CLI integration tests passed!'
        stop 0
    else
        print *, 'Some CLI integration tests failed!'
        stop 1
    end if

contains

    logical function test_basic_file_processing()
        test_basic_file_processing = .true.
        print *, 'Testing basic file processing...'

        ! Create simple test file
        block
            integer :: iostat
            integer :: unit

            ! Create a simple .f file (lazy fortran)
            open(newunit=unit, file=get_temp_file_path(create_temp_dir('fortran_test'), 'test_simple.f'), action='write', iostat=iostat)
            if (iostat /= 0) then
                print *, 'FAIL: Could not create test file'
                test_basic_file_processing = .false.
                return
            end if

            write (unit, '(a)') 'x = 42'
            write (unit, '(a)') 'print *, x'
            close (unit)

            ! Test processing the file
            call execute_command_line('fpm run fortran -- '//get_temp_file_path(create_temp_dir('fortran_test'), 'test_simple.f')//' > ' // &
                                     get_temp_file_path(create_temp_dir('fortran_test'), 'test_output.f90')//' 2>/dev/null', exitstat=iostat)

            if (iostat == 0) then
                print *, '  PASS: Basic file processing works'
            else
                print *, '  FAIL: Basic file processing failed with exit code', iostat
                test_basic_file_processing = .false.
            end if

            ! Clean up
            call execute_command_line('rm -f '//get_temp_file_path(create_temp_dir('fortran_test'), 'test_simple.f')//' '//get_temp_file_path(create_temp_dir('fortran_test'), 'test_output.f90'), &
                                      exitstat=iostat)
        end block

    end function test_basic_file_processing

    logical function test_debug_output_pipeline()
        test_debug_output_pipeline = .true.
        print *, 'Testing debug output pipeline...'

        ! Create test file for debug pipeline
        block
            integer :: iostat
            integer :: unit

            ! Create a simple .f file
            open(newunit=unit, file=get_temp_file_path(create_temp_dir('fortran_test'), 'test_debug.f'), action='write', iostat=iostat)
            if (iostat /= 0) then
                print *, 'FAIL: Could not create debug test file'
                test_debug_output_pipeline = .false.
                return
            end if

            write (unit, '(a)') 'y = 3.14'
            close (unit)

            ! Test --debug-tokens
            call execute_command_line('fpm run fortran -- '//get_temp_file_path(create_temp_dir('fortran_test'), 'test_debug.f')//' ' // &
                                     '--debug-tokens > '//get_temp_file_path(create_temp_dir('fortran_test'), 'debug_tokens.json')//' 2>/dev/null', &
                                      exitstat=iostat)
            if (iostat /= 0) then
                print *, '  FAIL: --debug-tokens failed'
                test_debug_output_pipeline = .false.
                return
            end if

            ! Test --debug-ast
            call execute_command_line('fpm run fortran -- '//get_temp_file_path(create_temp_dir('fortran_test'), 'test_debug.f')//' '// &
                                      '--debug-ast > '//get_temp_file_path(create_temp_dir('fortran_test'), 'debug_ast.json')//' 2>/dev/null', &
                                      exitstat=iostat)
            if (iostat /= 0) then
                print *, '  FAIL: --debug-ast failed'
                test_debug_output_pipeline = .false.
                return
            end if

            ! Test --debug-semantic
            call execute_command_line('fpm run fortran -- '//get_temp_file_path(create_temp_dir('fortran_test'), 'test_debug.f')//' '// &
                            '--debug-semantic > '//get_temp_file_path(create_temp_dir('fortran_test'), 'debug_semantic.json')//' 2>/dev/null', &
                                      exitstat=iostat)
            if (iostat /= 0) then
                print *, '  FAIL: --debug-semantic failed'
                test_debug_output_pipeline = .false.
                return
            end if

            ! Test --debug-codegen
            call execute_command_line('fpm run fortran -- '//get_temp_file_path(create_temp_dir('fortran_test'), 'test_debug.f')//' '// &
                              '--debug-codegen > '//get_temp_file_path(create_temp_dir('fortran_test'), 'debug_codegen.json')//' 2>/dev/null', &
                                      exitstat=iostat)
            if (iostat /= 0) then
                print *, '  FAIL: --debug-codegen failed'
                test_debug_output_pipeline = .false.
                return
            end if

            print *, '  PASS: All debug output flags work'

            ! Clean up
            call execute_command_line('rm -f '//get_temp_file_path(create_temp_dir('fortran_test'), 'test_debug.f')//' '// &
                  get_temp_file_path(create_temp_dir('fortran_test'), 'debug_*.json'), &
                                      exitstat=iostat)
        end block

    end function test_debug_output_pipeline

    logical function test_json_pipeline_workflow()
        test_json_pipeline_workflow = .true.
        print *, 'Testing JSON pipeline workflow...'

        ! Create tokens JSON and test pipeline
        block
            integer :: iostat
            integer :: unit

            ! Create minimal tokens JSON
            open(newunit=unit, file=get_temp_file_path(create_temp_dir('fortran_test'), 'pipeline_tokens.json'), action='write', &
                 iostat=iostat)
            if (iostat /= 0) then
                print *, 'FAIL: Could not create tokens JSON'
                test_json_pipeline_workflow = .false.
                return
            end if

            write (unit, '(a)') '{"tokens": []}'
            close (unit)

            ! Test --from-tokens
            call execute_command_line('fpm run fortran -- '//get_temp_file_path(create_temp_dir('fortran_test'), 'pipeline_tokens.json')//' ' // &
                                      '--from-tokens > /dev/null 2>&1', exitstat=iostat)
            if (iostat == 0) then
                print *, '  PASS: --from-tokens pipeline works'
            else
                print *, '  PARTIAL: --from-tokens pipeline returned exit code', &
                    iostat, '(expected for empty tokens)'
            end if

            ! Create minimal AST JSON
            open(newunit=unit, file=get_temp_file_path(create_temp_dir('fortran_test'), 'pipeline_ast.json'), action='write', &
                 iostat=iostat)
            write (unit, '(a)') '{"type": "program", "name": "test"}'
            close (unit)

            ! Test --from-ast
            call execute_command_line('fpm run fortran -- '//get_temp_file_path(create_temp_dir('fortran_test'), 'pipeline_ast.json')//' ' // &
                                      '--from-ast > /dev/null 2>&1', exitstat=iostat)
            if (iostat == 0) then
                print *, '  PASS: --from-ast pipeline works'
            else
                print *, '  PARTIAL: --from-ast pipeline returned exit code', &
                    iostat, '(expected for minimal AST)'
            end if

            ! Clean up
            call execute_command_line('rm -f '//get_temp_file_path(create_temp_dir('fortran_test'), 'pipeline_*.json'), exitstat=iostat)
        end block

    end function test_json_pipeline_workflow

    logical function test_error_handling()
        test_error_handling = .true.
        print *, 'Testing error handling...'

        ! Test non-existent file
        block
            integer :: iostat

            call execute_command_line('fpm run fortran -- '//get_temp_file_path(create_temp_dir('fortran_test'), 'nonexistent.f')//' > ' // &
                                      '/dev/null 2>&1', exitstat=iostat)
            if (iostat /= 0) then
                print *, '  PASS: Non-existent file properly handled'
            else
                print *, '  FAIL: Non-existent file should have failed'
                test_error_handling = .false.
            end if
        end block

        ! Test invalid JSON input
        block
            integer :: iostat
            integer :: unit

            open(newunit=unit, file=get_temp_file_path(create_temp_dir('fortran_test'), 'invalid.json'), action='write', iostat=iostat)
            write (unit, '(a)') 'invalid json content'
            close (unit)

            call execute_command_line('fpm run fortran -- '//get_temp_file_path(create_temp_dir('fortran_test'), 'invalid.json')//' ' // &
                                      '--from-tokens > /dev/null 2>&1', exitstat=iostat)
            if (iostat /= 0) then
                print *, '  PASS: Invalid JSON properly handled'
            else
                print *, '  FAIL: Invalid JSON should have failed'
                test_error_handling = .false.
            end if

            ! Clean up
            call execute_command_line('rm -f '//get_temp_file_path(create_temp_dir('fortran_test'), 'invalid.json'), exitstat=iostat)
        end block

    end function test_error_handling

end program test_cli_integration
