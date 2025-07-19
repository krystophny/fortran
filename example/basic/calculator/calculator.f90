program calculator
    use math_utils, only: add, multiply
    implicit none

    real :: x, y, sum, product

    x = 5.0
    y = 3.0

    sum = add(x, y)
    product = multiply(x, y)

    print '(a,f5.1,a,f5.1,a,f5.1)', 'Sum of ', x, ' and ', y, ' is ', sum
    print '(a,f5.1,a,f5.1,a,f5.1)', 'Product of ', x, ' and ', y, ' is ', product

end program calculator
