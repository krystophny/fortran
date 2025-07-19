program test_type_inference_integration
    use iso_fortran_env, only: error_unit
    use temp_utils, only: temp_dir_manager
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    write (*, '(a)') '=== Type Inference Integration Tests ==='
    write (*, '(a)') ''

    call test_basic_type_inference_integration(test_count, pass_count)
    call test_arithmetic_expressions_integration(test_count, pass_count)
    call test_mixed_types_integration(test_count, pass_count)
    call test_intrinsic_functions_integration(test_count, pass_count)
    call test_print_statement_filtering(test_count, pass_count)

    write (*, '(a)') ''
  write(*, '(a,i0,a,i0,a)') 'Type inference integration tests: ', pass_count, '/', test_count, ' passed'

    if (pass_count /= test_count) then
        error stop 'Some integration tests failed!'
    end if

contains

    subroutine test_basic_type_inference_integration(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        character(len=:), allocatable :: temp_input, temp_output
        character(len=256) :: error_msg
        character(len=1024) :: line, expected_lines(20)
        integer :: unit, ios, line_count, i
        logical :: found_declarations
        type(temp_dir_manager) :: temp_mgr

        write (*, '(a)') 'Test 1: Basic type inference integration'

        ! Create temporary directory and .f file
        call temp_mgr%create('test_basic_types')
        temp_input = temp_mgr%get_file_path('test_basic_types.f')
        temp_output = temp_mgr%get_file_path('test_basic_types.f90')

        open (newunit=unit, file=temp_input, status='replace', action='write')
        write (unit, '(a)') '! Test basic type inference'
        write (unit, '(a)') 'count = 42'
        write (unit, '(a)') 'pi = 3.14159'
        write (unit, '(a)') 'name = "Fortran"'
        write (unit, '(a)') 'ready = .true.'
        write (unit, '(a)') 'print *, count, pi, name, ready'
        close (unit)

        ! Expected declarations in order
        expected_lines(1) = 'integer :: count'
        expected_lines(2) = 'real(8) :: pi'
        expected_lines(3) = 'character(len=256) :: name'
        expected_lines(4) = 'logical :: ready'

        ! Test preprocessing
        test_count = test_count + 1
    call test_standardize_file(temp_input, temp_output, error_msg, expected_lines, 4, found_declarations)

        if (len_trim(error_msg) == 0 .and. found_declarations) then
            pass_count = pass_count + 1
        write (*, '(a)') '  ✓ PASS: Basic type inference generates correct declarations'
        else
            write (*, '(a)') '  ✗ FAIL: Basic type inference failed'
            if (len_trim(error_msg) > 0) then
                write (*, '(a)') '    Error: '//trim(error_msg)
            end if
        end if

        ! Cleanup is automatic via temp_mgr finalizer

    end subroutine test_basic_type_inference_integration

    subroutine test_arithmetic_expressions_integration(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        character(len=:), allocatable :: temp_input, temp_output
        character(len=256) :: error_msg
        character(len=1024) :: expected_lines(20)
        integer :: unit
        logical :: found_declarations
        type(temp_dir_manager) :: temp_mgr

        write (*, '(a)') 'Test 2: Arithmetic expressions integration'

        ! Create temporary directory and .f file
        call temp_mgr%create('test_arithmetic')
        temp_input = temp_mgr%get_file_path('test_arithmetic.f')
        temp_output = temp_mgr%get_file_path('test_arithmetic.f90')

        open (newunit=unit, file=temp_input, status='replace', action='write')
        write (unit, '(a)') '! Test arithmetic expression inference'
        write (unit, '(a)') 'a = 10'
        write (unit, '(a)') 'b = 20'
        write (unit, '(a)') 'sum = a + b'
        write (unit, '(a)') 'diff = a - b'
        write (unit, '(a)') 'prod = a * b'
        write (unit, '(a)') 'quot = 5.0 / 2.0'
        write (unit, '(a)') 'print *, sum, diff, prod, quot'
        close (unit)

        ! Expected declarations
        expected_lines(1) = 'integer :: a'
        expected_lines(2) = 'integer :: b'
        expected_lines(3) = 'real(8) :: sum'
        expected_lines(4) = 'real(8) :: diff'
        expected_lines(5) = 'real(8) :: prod'
        expected_lines(6) = 'real(8) :: quot'

        ! Test preprocessing
        test_count = test_count + 1
    call test_standardize_file(temp_input, temp_output, error_msg, expected_lines, 6, found_declarations)

        if (len_trim(error_msg) == 0 .and. found_declarations) then
            pass_count = pass_count + 1
            write (*, '(a)') '  ✓ PASS: Arithmetic expressions correctly inferred'
        else
            write (*, '(a)') '  ✗ FAIL: Arithmetic expression inference failed'
            if (len_trim(error_msg) > 0) then
                write (*, '(a)') '    Error: '//trim(error_msg)
            end if
        end if

        ! Cleanup is automatic via temp_mgr finalizer

    end subroutine test_arithmetic_expressions_integration

    subroutine test_mixed_types_integration(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        character(len=:), allocatable :: temp_input, temp_output
        character(len=256) :: error_msg
        character(len=1024) :: expected_lines(20)
        integer :: unit
        logical :: found_declarations
        type(temp_dir_manager) :: temp_mgr

        write (*, '(a)') 'Test 3: Mixed type expressions integration'

        ! Create a temporary .f file with mixed types
        call temp_mgr%create('test_mixed')
        temp_input = temp_mgr%get_file_path('test_mixed.f')
        temp_output = temp_mgr%get_file_path('test_mixed.f90')

        open (newunit=unit, file=temp_input, status='replace', action='write')
        write (unit, '(a)') '! Test mixed type inference and promotion'
        write (unit, '(a)') 'x = 2'
        write (unit, '(a)') 'y = 3.5'
        write (unit, '(a)') 'result = x + y  ! Should promote to real'
        write (unit, '(a)') 'print *, result'
        close (unit)

        ! Expected declarations
        expected_lines(1) = 'integer :: x'
        expected_lines(2) = 'real(8) :: y'
        expected_lines(3) = 'real(8) :: result'

        ! Test preprocessing
        test_count = test_count + 1
    call test_standardize_file(temp_input, temp_output, error_msg, expected_lines, 3, found_declarations)

        if (len_trim(error_msg) == 0 .and. found_declarations) then
            pass_count = pass_count + 1
            write (*, '(a)') '  ✓ PASS: Mixed type promotion works correctly'
        else
            write (*, '(a)') '  ✗ FAIL: Mixed type promotion failed'
            if (len_trim(error_msg) > 0) then
                write (*, '(a)') '    Error: '//trim(error_msg)
            end if
        end if

        ! Cleanup is automatic via temp_mgr finalizer

    end subroutine test_mixed_types_integration

    subroutine test_intrinsic_functions_integration(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        character(len=:), allocatable :: temp_input, temp_output
        character(len=256) :: error_msg
        character(len=1024) :: expected_lines(20)
        integer :: unit
        logical :: found_declarations
        type(temp_dir_manager) :: temp_mgr

        write (*, '(a)') 'Test 4: Intrinsic functions integration'

        ! Create a temporary .f file with intrinsic functions
        call temp_mgr%create('test_intrinsics')
        temp_input = temp_mgr%get_file_path('test_intrinsics.f')
        temp_output = temp_mgr%get_file_path('test_intrinsics.f90')

        open (newunit=unit, file=temp_input, status='replace', action='write')
        write (unit, '(a)') '! Test intrinsic function inference'
        write (unit, '(a)') 'angle = 1.0'
        write (unit, '(a)') 'sine_val = sin(angle)'
        write (unit, '(a)') 'cosine_val = cos(angle)'
        write (unit, '(a)') 'square_root = sqrt(4.0)'
        write (unit, '(a)') 'print *, sine_val, cosine_val, square_root'
        close (unit)

        ! Expected declarations
        expected_lines(1) = 'real(8) :: angle'
        expected_lines(2) = 'real(8) :: sine_val'
        expected_lines(3) = 'real(8) :: cosine_val'
        expected_lines(4) = 'real(8) :: square_root'

        ! Test preprocessing
        test_count = test_count + 1
    call test_standardize_file(temp_input, temp_output, error_msg, expected_lines, 4, found_declarations)

        if (len_trim(error_msg) == 0 .and. found_declarations) then
            pass_count = pass_count + 1
         write (*, '(a)') '  ✓ PASS: Intrinsic function return types correctly inferred'
        else
            write (*, '(a)') '  ✗ FAIL: Intrinsic function inference failed'
            if (len_trim(error_msg) > 0) then
                write (*, '(a)') '    Error: '//trim(error_msg)
            end if
        end if

        ! Cleanup is automatic via temp_mgr finalizer

    end subroutine test_intrinsic_functions_integration

    subroutine test_print_statement_filtering(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count

        character(len=:), allocatable :: temp_input, temp_output
        character(len=256) :: error_msg
        character(len=1024) :: line
        integer :: unit, ios
        logical :: found_invalid_decl
        type(temp_dir_manager) :: temp_mgr

        write (*, '(a)') 'Test 5: Print statement filtering'

        ! Create a temporary .f file with print statements containing =
        call temp_mgr%create('test_print_filter')
        temp_input = temp_mgr%get_file_path('test_print_filter.f')
        temp_output = temp_mgr%get_file_path('test_print_filter.f90')

        open (newunit=unit, file=temp_input, status='replace', action='write')
       write (unit, '(a)') '! Test that print statements are not treated as assignments'
        write (unit, '(a)') 'value = 42'
        write (unit, '(a)') 'print *, "value =", value'
        write (unit, '(a)') 'print *, "result = success"'
        write (unit, '(a)') 'write(*,*) "output =", value'
        close (unit)

        ! Test preprocessing
        test_count = test_count + 1
        call standardize_file_wrapper(temp_input, temp_output, error_msg)

        if (len_trim(error_msg) == 0) then
            ! Check that no invalid declarations were created
            found_invalid_decl = .false.

          open (newunit=unit, file=temp_output, status='old', action='read', iostat=ios)
            if (ios == 0) then
                do
                    read (unit, '(A)', iostat=ios) line
                    if (ios /= 0) exit

                    ! Look for invalid declarations that would come from parsing print statements
                    if (index(line, ':: "value') > 0 .or. &
                        index(line, ':: "result') > 0 .or. &
                        index(line, ':: "output') > 0) then
                        found_invalid_decl = .true.
                        exit
                    end if
                end do
                close (unit)
            end if

            if (.not. found_invalid_decl) then
                pass_count = pass_count + 1
                write (*, '(a)') '  ✓ PASS: Print statements correctly filtered'
            else
         write (*, '(a)') '  ✗ FAIL: Print statements incorrectly parsed as assignments'
            end if
        else
            write (*, '(a)') '  ✗ FAIL: Print filtering test failed'
            write (*, '(a)') '    Error: '//trim(error_msg)
        end if

        ! Cleanup is automatic via temp_mgr finalizer

    end subroutine test_print_statement_filtering

  subroutine test_standardize_file(input_file, output_file, error_msg, expected_lines, num_expected, found_all)
        character(len=*), intent(in) :: input_file
        character(len=*), intent(inout) :: output_file
        character(len=*), intent(out) :: error_msg
        character(len=*), intent(in) :: expected_lines(:)
        integer, intent(in) :: num_expected
        logical, intent(out) :: found_all

        character(len=1024) :: line
        integer :: unit, ios, i
        logical, dimension(num_expected) :: found_lines

        found_all = .false.
        found_lines = .false.

        ! Preprocess the file
        call standardize_file_wrapper(input_file, output_file, error_msg)

        if (len_trim(error_msg) > 0) return

        ! Check the output file for expected declarations
        open (newunit=unit, file=output_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            error_msg = 'Failed to open output file'
            return
        end if

        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            ! Check if this line matches any expected declaration
            do i = 1, num_expected
                if (index(adjustl(line), trim(expected_lines(i))) > 0) then
                    found_lines(i) = .true.
                end if
            end do
        end do

        close (unit)

        ! Check if all expected lines were found
        found_all = all(found_lines(1:num_expected))

        if (.not. found_all) then
            error_msg = 'Missing expected declarations'
            do i = 1, num_expected
                if (.not. found_lines(i)) then
                    error_msg = trim(error_msg)//', missing: '//trim(expected_lines(i))
                end if
            end do

            ! Debug: show what was actually generated
            close (unit)
          open (newunit=unit, file=output_file, status='old', action='read', iostat=ios)
            if (ios == 0) then
                write (*, '(a)') '    DEBUG: Generated file contents:'
                do
                    read (unit, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    write (*, '(a)') '      '//trim(line)
                end do
            end if
        end if

    end subroutine test_standardize_file

    subroutine standardize_file_wrapper(input_file, output_file, error_msg)
        use frontend_integration, only: compile_with_frontend
        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg

        call compile_with_frontend(input_file, output_file, error_msg)

    end subroutine standardize_file_wrapper

end program test_type_inference_integration
