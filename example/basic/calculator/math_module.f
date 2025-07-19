module math_utils
    private
    public :: add, multiply

contains

    real function add(a, b)
        real :: a, b
        add = a + b
    end function add

    real function multiply(a, b)
        real :: a, b
        multiply = a*b
    end function multiply

end module math_utils
