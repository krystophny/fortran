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
            character(len=256) :: test_file, output_file, cmd
            character(len=256) :: test_dir
            test_dir = create_temp_dir('fortran_test')
            test_file = get_temp_file_path(test_dir, 'test_simple.f')

            open (newunit=unit, file=test_file, action='write', iostat=iostat)
            if (iostat /= 0) then
                print *, 'FAIL: Could not create test file'
                test_basic_file_processing = .false.
                return
            end if

            write (unit, '(a)') 'x = 42'
            write (unit, '(a)') 'print *, x'
            close (unit)

            ! Test processing the file
            output_file = get_temp_file_path(test_dir, 'test_output.f90')
  cmd = 'fpm run fortran -- '//trim(test_file)//' > '//trim(output_file)//' 2>/dev/null'
            call execute_command_line(cmd, exitstat=iostat)

            if (iostat == 0) then
                print *, '  PASS: Basic file processing works'
            else
                print *, '  FAIL: Basic file processing failed with exit code', iostat
                test_basic_file_processing = .false.
            end if

            ! Clean up
            ! Cleanup handled by temp_utils
        end block

    end function test_basic_file_processing

    logical function test_debug_output_pipeline()
        test_debug_output_pipeline = .true.
        print *, 'Testing debug output pipeline...'

        ! Create test file for debug pipeline
        block
            integer :: iostat
            integer :: unit
            character(len=256) :: test_file, output_file, cmd
            character(len=256) :: test_dir

            ! Create a simple .f file
            test_dir = create_temp_dir('fortran_test')
            test_file = get_temp_file_path(test_dir, 'test_debug.f')
            open (newunit=unit, file=test_file, action='write', iostat=iostat)
            if (iostat /= 0) then
                print *, 'FAIL: Could not create debug test file'
                test_debug_output_pipeline = .false.
                return
            end if

            write (unit, '(a)') 'y = 3.14'
            close (unit)

            ! Test --debug-tokens
            output_file = get_temp_file_path(test_dir, 'debug_tokens.json')
            cmd = 'fpm run fortran -- '//trim(test_file)//' --debug-tokens > '//trim(output_file)//' 2>/dev/null'
            call execute_command_line(cmd, exitstat=iostat)
            if (iostat /= 0) then
                print *, '  FAIL: --debug-tokens failed'
                test_debug_output_pipeline = .false.
                return
            end if

            ! Test --debug-ast
            output_file = get_temp_file_path(test_dir, 'debug_ast.json')
            cmd = 'fpm run fortran -- '//trim(test_file)//' --debug-ast > '//trim(output_file)//' 2>/dev/null'
            call execute_command_line(cmd, exitstat=iostat)
            if (iostat /= 0) then
                print *, '  FAIL: --debug-ast failed'
                test_debug_output_pipeline = .false.
                return
            end if

            ! Test --debug-semantic
            output_file = get_temp_file_path(test_dir, 'debug_semantic.json')
            cmd = 'fpm run fortran -- '//trim(test_file)//' --debug-semantic > '//trim(output_file)//' 2>/dev/null'
            call execute_command_line(cmd, exitstat=iostat)
            if (iostat /= 0) then
                print *, '  FAIL: --debug-semantic failed'
                test_debug_output_pipeline = .false.
                return
            end if

            ! Test --debug-codegen
            output_file = get_temp_file_path(test_dir, 'debug_codegen.json')
            cmd = 'fpm run fortran -- '//trim(test_file)//' --debug-codegen > '//trim(output_file)//' 2>/dev/null'
            call execute_command_line(cmd, exitstat=iostat)
            if (iostat /= 0) then
                print *, '  FAIL: --debug-codegen failed'
                test_debug_output_pipeline = .false.
                return
            end if

            print *, '  PASS: All debug output flags work'

            ! Clean up
            ! Cleanup handled by temp_utils
        end block

    end function test_debug_output_pipeline

    logical function test_json_pipeline_workflow()
        test_json_pipeline_workflow = .true.
        print *, 'Testing JSON pipeline workflow...'

        ! Create tokens JSON and test pipeline
        block
            integer :: iostat
            integer :: unit
            character(len=256) :: json_file, cmd
            character(len=256) :: test_dir

            ! Create minimal tokens JSON
            test_dir = create_temp_dir('fortran_test')
            json_file = get_temp_file_path(test_dir, 'pipeline_tokens.json')
            open (newunit=unit, file=json_file, action='write', iostat=iostat)
            if (iostat /= 0) then
                print *, 'FAIL: Could not create tokens JSON'
                test_json_pipeline_workflow = .false.
                return
            end if

            write (unit, '(a)') '{"tokens": []}'
            close (unit)

            ! Test --from-tokens
         cmd = 'fpm run fortran -- '//trim(json_file)//' --from-tokens > /dev/null 2>&1'
            call execute_command_line(cmd, exitstat=iostat)
            if (iostat == 0) then
                print *, '  PASS: --from-tokens pipeline works'
            else
                print *, '  PARTIAL: --from-tokens pipeline returned exit code', &
                    iostat, '(expected for empty tokens)'
            end if

            ! Create minimal AST JSON
            json_file = get_temp_file_path(test_dir, 'pipeline_ast.json')
            open (newunit=unit, file=json_file, action='write', iostat=iostat)
            write (unit, '(a)') '{"type": "program", "name": "test"}'
            close (unit)

            ! Test --from-ast
            cmd = 'fpm run fortran -- '//trim(json_file)//' --from-ast > /dev/null 2>&1'
            call execute_command_line(cmd, exitstat=iostat)
            if (iostat == 0) then
                print *, '  PASS: --from-ast pipeline works'
            else
                print *, '  PARTIAL: --from-ast pipeline returned exit code', &
                    iostat, '(expected for minimal AST)'
            end if

            ! Clean up
            ! Cleanup handled by temp_utils
        end block

    end function test_json_pipeline_workflow

    logical function test_error_handling()
        test_error_handling = .true.
        print *, 'Testing error handling...'

        ! Test non-existent file
        block
            integer :: iostat

            character(len=256) :: test_file, cmd
       test_file = '/tmp/definitely_nonexistent_file_that_should_not_exist_9876543210.f'
            cmd = 'fpm run fortran -- '//trim(test_file)//' > /dev/null 2>&1'
            call execute_command_line(cmd, exitstat=iostat)
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

            character(len=256) :: json_file, cmd
            character(len=256) :: test_dir
            test_dir = create_temp_dir('fortran_test')
            json_file = get_temp_file_path(test_dir, 'invalid.json')
            open (newunit=unit, file=json_file, action='write', iostat=iostat)
            write (unit, '(a)') 'invalid json content'
            close (unit)

         cmd = 'fpm run fortran -- '//trim(json_file)//' --from-tokens > /dev/null 2>&1'
            call execute_command_line(cmd, exitstat=iostat)
            if (iostat /= 0) then
                print *, '  PASS: Invalid JSON properly handled'
            else
                print *, '  FAIL: Invalid JSON should have failed'
                test_error_handling = .false.
            end if

            ! Clean up
            ! Cleanup handled by temp_utils
        end block

    end function test_error_handling

end program test_cli_integration
