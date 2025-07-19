! subroutines.f90 - Standard Fortran equivalent of subroutines.f
program subroutines
    implicit none
    real(8) :: radius, area, circumference

    radius = 5.0_8
    call calculate_circle(radius, area, circumference)

    print *, 'Radius:', radius
    print *, 'Area:', area
    print *, 'Circumference:', circumference

contains

    subroutine calculate_circle(r, a, c)
        real(8), intent(in) :: r
        real(8), intent(out) :: a, c
        real(8), parameter :: pi = 3.14159265359_8

        a = pi*r*r
        c = 2.0_8*pi*r
    end subroutine calculate_circle

end program subroutines
