! simple_math.f90 - Standard Fortran version of notebook example
program simple_math
    implicit none
    real :: x, y, sum, diff, product, quotient, square, cube, sqrt_x

    ! Define some variables
    x = 5.0
    y = 3.0

    print *, "x =", x
    print *, "y =", y

    ! Basic Arithmetic Operations
    ! Addition and subtraction
    sum = x + y
    diff = x - y

    print *, "x + y =", sum
    print *, "x - y =", diff

    ! Multiplication and division
    product = x*y
    quotient = x/y

    print *, "x * y =", product
    print *, "x / y =", quotient

    ! Power Operations
    ! Powers
    square = x**2
    cube = y**3
    sqrt_x = sqrt(x)

    print *, "x^2 =", square
    print *, "y^3 =", cube
    print *, "sqrt(x) =", sqrt_x

end program simple_math
