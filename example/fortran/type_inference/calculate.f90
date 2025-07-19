! calculate.f90
program calculate
    implicit none
    real :: radius, area, circumference
    real, parameter :: pi = 3.14159
    integer :: n_circles
    character(len=20) :: label
    logical :: is_large

    radius = 5.0
    n_circles = 3
    label = "Test Circle"

    area = pi*radius*radius
    circumference = 2.0*pi*radius
    is_large = area > 50.0

    print *, 'Label:', trim(label)
    print *, 'Number of circles:', n_circles
    print *, 'Radius:', radius
    print *, 'Area:', area
    print *, 'Circumference:', circumference
    print *, 'Is large?', is_large
end program calculate
