! Example with subroutines
! This demonstrates that the preprocessor adds intent(in) by default
! For output parameters, you need to explicitly declare them
real :: radius, area, circumference

radius = 5.0
call calculate_circle(radius, area, circumference)

print *, 'Radius:', radius
print *, 'Area:', area
print *, 'Circumference:', circumference

subroutine calculate_circle(r, a, c)
    real :: r
    real, intent(out) :: a, c  ! Explicitly declare output parameters
    real :: pi = 3.14159265359

    a = pi*r*r
    c = 2.0*pi*r
end subroutine calculate_circle
