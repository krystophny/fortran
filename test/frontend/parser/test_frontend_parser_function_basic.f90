! Test basic function definition parsing
program test_frontend_parser_function_basic
    use frontend_integration, only: compile_with_frontend
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    call test_simplest_function()
    call print_results()
    
contains

    subroutine test_simplest_function()
        character(len=*), parameter :: test_name = "Simplest function definition"
        character(len=256) :: input_file, output_file, error_msg
        logical :: success
        
        input_file = 'test_simple_func.f'
        output_file = 'test_simple_func.f90'
        
        ! Create the simplest possible function definition
        call create_test_file(input_file, &
            'real function add_one(x)' // new_line('a') // &
            '  real :: x' // new_line('a') // &
            '  add_one = x + 1.0' // new_line('a') // &
            'end function')
        
        ! Try to compile it
        call compile_with_frontend(input_file, output_file, error_msg)
        
        ! Test 1: No compilation errors
        success = (len_trim(error_msg) == 0)
        call assert_true(success, test_name // " - no compilation errors")
        
        if (success) then
            ! Test 2: Generated file exists and is valid Fortran
            success = check_generated_fortran_valid(output_file)
            call assert_true(success, test_name // " - generates valid Fortran")
            
            ! Debug: Show generated content if test fails
            if (.not. success) then
                call show_generated_content(output_file)
            end if
        end if
        
        ! Don't cleanup files yet - keep them for debugging
        ! call cleanup_files(input_file, output_file)
    end subroutine test_simplest_function

    subroutine create_test_file(filename, content)
        character(len=*), intent(in) :: filename, content
        integer :: unit, ios
        
        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write(unit, '(A)') content
            close(unit)
        end if
    end subroutine
    
    function check_generated_fortran_valid(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=1024) :: line
        integer :: unit, ios
        logical :: has_program, has_function, has_end_program
        
        is_valid = .false.
        has_program = .false.
        has_function = .false.
        has_end_program = .false.
        
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                
                ! Check for required Fortran structure
                if (index(line, 'program') > 0) has_program = .true.
                if (index(line, 'function') > 0) has_function = .true.
                if (index(line, 'end program') > 0) has_end_program = .true.
                
                ! Check for invalid content (like statement labels)
                if (index(adjustl(line), '0') == 1 .and. len_trim(adjustl(line)) == 1) then
                    ! Found a bare "0" - this is invalid
                    close(unit)
                    return
                end if
            end do
            close(unit)
            
            ! Valid if we have basic program structure and no invalid content
            is_valid = has_program .and. has_end_program
        end if
    end function
    
    subroutine show_generated_content(filename)
        character(len=*), intent(in) :: filename
        character(len=1024) :: line
        integer :: unit, ios
        
        write(*, '(A)') "=== Generated content ==="
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios == 0) then
            do
                read(unit, '(A)', iostat=ios) line
                if (ios /= 0) exit
                write(*, '(A)') trim(line)
            end do
            close(unit)
        end if
        write(*, '(A)') "=== End generated content ==="
    end subroutine
    
    subroutine cleanup_files(file1, file2)
        character(len=*), intent(in) :: file1, file2
        integer :: unit, ios
        
        open(newunit=unit, file=file1, status='old', iostat=ios)
        if (ios == 0) then
            close(unit, status='delete')
        end if
        
        open(newunit=unit, file=file2, status='old', iostat=ios) 
        if (ios == 0) then
            close(unit, status='delete')
        end if
    end subroutine

    subroutine assert_true(condition, test_name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        if (condition) then
            pass_count = pass_count + 1
            write(*, '(A, A)') 'PASS: ', test_name
        else
            write(*, '(A, A)') 'FAIL: ', test_name
        end if
    end subroutine

    subroutine print_results()
        write(*, '(A, I0, A, I0, A)') 'Tests: ', pass_count, '/', test_count, ' passed'
        if (pass_count /= test_count) then
            write(*, '(A, I0, A)') 'FAILED: ', test_count - pass_count, ' tests failed'
            stop 1
        end if
    end subroutine

end program