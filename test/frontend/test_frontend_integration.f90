program test_frontend_basic
    ! Basic tests for the compiler frontend
    use frontend_integration
    implicit none
    
    integer :: failures = 0
    
    ! Run tests
    call test_simple_fortran_detection()
    call test_basic_compilation()
    
    if (failures == 0) then
        print *, "All frontend basic tests passed!"
    else
        print *, "Frontend tests failed:", failures
        stop 1
    end if
    
contains

    subroutine test_simple_fortran_detection()
        logical :: result
        
        print *, "Testing Simple Fortran file detection..."
        
        ! Test .f files
        result = is_simple_fortran_file("test.f")
        if (.not. result) call fail(".f file should be Simple Fortran")
        
        result = is_simple_fortran_file("TEST.F")
        if (.not. result) call fail(".F file should be Simple Fortran")
        
        ! Test .f90 files
        result = is_simple_fortran_file("test.f90")
        if (result) call fail(".f90 file should not be Simple Fortran")
        
        result = is_simple_fortran_file("test.F90")
        if (result) call fail(".F90 file should not be Simple Fortran")
        
        ! Test no extension
        result = is_simple_fortran_file("test")
        if (result) call fail("No extension should not be Simple Fortran")
        
        print *, "  ✓ File detection works correctly"
    end subroutine test_simple_fortran_detection
    
    subroutine test_basic_compilation()
        character(len=256) :: temp_in, temp_out, error_msg
        integer :: unit, ios
        logical :: file_exists
        
        print *, "Testing basic compilation..."
        
        ! Create a simple test file
        temp_in = "test_frontend_input.f"
        temp_out = "test_frontend_output.f90"
        
        open(newunit=unit, file=temp_in, status='replace', action='write')
        write(unit, '(A)') 'x = 42'
        write(unit, '(A)') 'print *, x'
        close(unit)
        
        ! Compile with frontend
        call compile_with_frontend(temp_in, temp_out, error_msg)
        
        if (len_trim(error_msg) > 0) then
            call fail("Frontend compilation failed: " // trim(error_msg))
        else
            ! Check output exists
            inquire(file=temp_out, exist=file_exists)
            if (.not. file_exists) then
                call fail("Output file not created")
            else
                print *, "  ✓ Basic compilation works"
            end if
        end if
        
        ! Clean up
        open(newunit=unit, file=temp_in, status='old', iostat=ios)
        if (ios == 0) close(unit, status='delete')
        
        open(newunit=unit, file=temp_out, status='old', iostat=ios)
        if (ios == 0) close(unit, status='delete')
        
    end subroutine test_basic_compilation
    
    subroutine fail(msg)
        character(len=*), intent(in) :: msg
        print *, "  ✗ FAIL:", msg
        failures = failures + 1
    end subroutine fail
    
end program test_frontend_basic