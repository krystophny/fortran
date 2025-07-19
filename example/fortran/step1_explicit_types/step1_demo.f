! Step 1: Explicit function types with opinionated defaults
! This demonstrates how the preprocessor applies modern defaults to explicit types

result = square(5.0)
print *, "Square of 5.0 is:", result

real function square(x)
    real :: x
    square = x*x
end function
