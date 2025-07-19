module geometry
    use constants, only: pi
    implicit none

    public :: calculate_area, calculate_volume

contains

    function calculate_area(radius) result(area)
        real(8), intent(in) :: radius
        real(8) :: area

        area = pi*radius**2
    end function calculate_area

    function calculate_volume(radius, height) result(volume)
        real(8), intent(in) :: radius, height
        real(8) :: volume

        ! Volume of a cylinder = π * r² * h
        volume = calculate_area(radius)*height
    end function calculate_volume

end module geometry
