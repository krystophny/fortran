! Comprehensive Step 1 demonstration
! Shows all working Step 1 features: forward type propagation,
! function signature enhancement, and parameter type enhancement

! Forward type propagation examples
value = compute_area(5.0, 3.0)
diameter = calculate_diameter(7.0)
volume = get_volume(diameter)
print *, "Area:", value, "Diameter:", diameter, "Volume:", volume

! Multiple variable assignments from same function
result1 = square(4.0)
result2 = square(6.0)
print *, "Squares:", result1, result2

! Nested function call example
final_result = double_value(square(3.0))
print *, "Final result:", final_result

! Function definitions with explicit types (Step 1 enhanced)
real function compute_area(length, width)
    real :: length, width
    compute_area = length*width
end function

real function calculate_diameter(radius)
    real :: radius
    calculate_diameter = 2.0*radius
end function

real function get_volume(diameter)
    real :: diameter
    get_volume = 3.14159*(diameter/2.0)**3*4.0/3.0
end function

real function square(x)
    real :: x
    square = x*x
end function

real function double_value(input)
    real :: input
    double_value = input*2.0
end function
