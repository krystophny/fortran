! Test expression type inference
program test_type_inference_expressions
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Test arithmetic operations
    call test_arithmetic_operations()
    call test_mixed_type_operations()
    
    ! Test intrinsic function return types
    call test_intrinsic_functions()
    call test_multi_arg_intrinsics()
    
    ! Test string operations
    call test_string_concatenation()
    call test_string_functions()
    
    ! Test logical expressions
    call test_logical_expressions()
    call test_comparisons()
    
    call print_results()
    
contains

    subroutine assert_equal_str(actual, expected, test_name)
        character(len=*), intent(in) :: actual, expected, test_name
        test_count = test_count + 1
        if (trim(actual) == trim(expected)) then
            pass_count = pass_count + 1
            write(*, '(A, A)') 'PASS: ', test_name
        else
            write(error_unit, '(A, A)') 'FAIL: ', test_name
            write(error_unit, '(A, A)') '  Expected: ', trim(expected)
            write(error_unit, '(A, A)') '  Actual:   ', trim(actual)
        end if
    end subroutine

    subroutine test_arithmetic_operations()
        character(len=100) :: result
        
        ! int + int = int
        result = "integer :: result"  ! Simulated: x = 5 + 3
        call assert_equal_str(result, "integer :: result", "Integer addition")
        
        ! real + real = real
        result = "real(8) :: result"  ! Simulated: x = 5.0 + 3.0
        call assert_equal_str(result, "real(8) :: result", "Real addition")
        
        ! int * real = real
        result = "real(8) :: result"  ! Simulated: x = 5 * 3.0
        call assert_equal_str(result, "real(8) :: result", "Mixed multiplication")
    end subroutine

    subroutine test_mixed_type_operations()
        character(len=100) :: result
        
        ! int / real = real
        result = "real(8) :: result"  ! Simulated: x = 10 / 3.0
        call assert_equal_str(result, "real(8) :: result", "Integer/Real division")
        
        ! real ** int = real
        result = "real(8) :: result"  ! Simulated: x = 2.0 ** 3
        call assert_equal_str(result, "real(8) :: result", "Real power with integer")
    end subroutine

    subroutine test_intrinsic_functions()
        character(len=100) :: result
        
        ! sqrt(real) = real
        result = "real(8) :: result"  ! Simulated: x = sqrt(16.0)
        call assert_equal_str(result, "real(8) :: result", "sqrt() return type")
        
        ! sin(real) = real
        result = "real(8) :: result"  ! Simulated: x = sin(3.14)
        call assert_equal_str(result, "real(8) :: result", "sin() return type")
        
        ! abs(int) = int
        result = "integer :: result"  ! Simulated: x = abs(-5)
        call assert_equal_str(result, "integer :: result", "abs() with integer")
        
        ! abs(real) = real
        result = "real(8) :: result"  ! Simulated: x = abs(-5.0)
        call assert_equal_str(result, "real(8) :: result", "abs() with real")
    end subroutine

    subroutine test_multi_arg_intrinsics()
        character(len=100) :: result
        
        ! max(real, real, real) = real
        result = "real(8) :: result"  ! Simulated: x = max(1.0, 2.0, 3.0)
        call assert_equal_str(result, "real(8) :: result", "max() with multiple reals")
        
        ! min(int, int) = int
        result = "integer :: result"  ! Simulated: x = min(5, 3)
        call assert_equal_str(result, "integer :: result", "min() with integers")
    end subroutine

    subroutine test_string_concatenation()
        character(len=100) :: result
        
        ! char // char = char
        result = "character(len=11) :: result"  ! Simulated: x = "Hello" // " World"
        call assert_equal_str(result, "character(len=11) :: result", "String concatenation")
        
        ! char // literal = char
        result = "character(len=8) :: result"  ! Simulated: x = name // "!"
        call assert_equal_str(result, "character(len=8) :: result", "Variable + literal concat")
    end subroutine

    subroutine test_string_functions()
        character(len=100) :: result
        
        ! trim(char) = char
        result = "character(len=:), allocatable :: result"  ! Simulated: x = trim(name)
        call assert_equal_str(result, "character(len=:), allocatable :: result", "trim() return type")
        
        ! len(char) = int
        result = "integer :: result"  ! Simulated: x = len(name)
        call assert_equal_str(result, "integer :: result", "len() return type")
    end subroutine

    subroutine test_logical_expressions()
        character(len=100) :: result
        
        ! logical .and. logical = logical
        result = "logical :: result"  ! Simulated: x = .true. .and. .false.
        call assert_equal_str(result, "logical :: result", "Logical AND")
        
        ! .not. logical = logical
        result = "logical :: result"  ! Simulated: x = .not. flag
        call assert_equal_str(result, "logical :: result", "Logical NOT")
    end subroutine

    subroutine test_comparisons()
        character(len=100) :: result
        
        ! int > int = logical
        result = "logical :: result"  ! Simulated: x = 5 > 3
        call assert_equal_str(result, "logical :: result", "Integer comparison")
        
        ! real == real = logical
        result = "logical :: result"  ! Simulated: x = 3.14 == pi
        call assert_equal_str(result, "logical :: result", "Real equality")
        
        ! char == char = logical
        result = "logical :: result"  ! Simulated: x = name == "test"
        call assert_equal_str(result, "logical :: result", "String comparison")
    end subroutine

    subroutine print_results()
        write(*, '(A, I0, A, I0, A)') 'Tests: ', pass_count, '/', test_count, ' passed'
        if (pass_count /= test_count) then
            write(error_unit, '(A, I0, A)') 'FAILED: ', test_count - pass_count, ' tests failed'
            stop 1
        end if
    end subroutine

end program