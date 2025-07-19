! math.f90 - Standard Fortran equivalent of math.f
program math
    implicit none
    real(8) :: x, y, result

    x = 5.0_8
    y = 3.0_8

    result = add(x, y)
    print *, 'Sum:', result

    result = multiply(x, y)
    print *, 'Product:', result

contains

    function add(a, b) result(sum)
        real(8), intent(in) :: a, b
        real(8) :: sum
        sum = a + b
    end function add

    function multiply(a, b) result(product)
        real(8), intent(in) :: a, b
        real(8) :: product
        product = a*b
    end function multiply

end program math
