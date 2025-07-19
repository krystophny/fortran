module geometry
    use constants, only: pi

    public :: calculate_area, calculate_volume

contains

    function calculate_area(radius) result(area)
        area = pi*radius**2
    end function calculate_area

    function calculate_volume(radius, height) result(volume)
        ! Volume of a cylinder = π * r² * h
        volume = calculate_area(radius)*height
    end function calculate_volume

end module geometry
