! calc.f90 - Standard Fortran equivalent of calc.f
program calc
    implicit none
    integer :: a, b, sum, diff, prod
    real(8) :: quot, angle, sine_val, cosine_val

    ! Variables get their types from declarations
    a = 10
    b = 20
    sum = a + b
    diff = a - b
    prod = a*b
    quot = real(a, 8)/real(b, 8)  ! Force real division

    print *, "a =", a
    print *, "b =", b
    print *, "sum =", sum
    print *, "difference =", diff
    print *, "product =", prod
    print *, "quotient =", quot

    ! Using functions
    angle = 0.5_8
    sine_val = sin(angle)
    cosine_val = cos(angle)

    print *, "sin(", angle, ") =", sine_val
    print *, "cos(", angle, ") =", cosine_val
end program calc
