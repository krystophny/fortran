! intrinsic_functions.f90 - Standard Fortran intrinsic function examples
program intrinsic_functions
    implicit none
    real(8) :: x, number, angle, value, cosine, absolute
    character(len=20) :: text
    integer :: length

    ! Initialize input variables with different types
    x = 1.5_8
    text = "Hello World"
    number = 16.0_8

    ! Intrinsic function calls
    angle = sin(x)           ! real(8) - from intrinsic sin
    length = len_trim(text)  ! integer - from intrinsic len_trim
    value = sqrt(number)     ! real(8) - from intrinsic sqrt
    cosine = cos(angle)      ! real(8) - from intrinsic cos
    absolute = abs(-5.5_8)   ! real(8) - from intrinsic abs

    print *, 'X:', x
    print *, 'Text:', text
    print *, 'Number:', number
    print *, 'Angle (sin):', angle
    print *, 'Length:', length
    print *, 'Value (sqrt):', value
    print *, 'Cosine:', cosine
    print *, 'Absolute:', absolute
end program intrinsic_functions
