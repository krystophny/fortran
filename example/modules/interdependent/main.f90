program main
    use geometry, only: calculate_area, calculate_volume
    use input_output, only: print_results
    implicit none

    real(8) :: radius, height, area, volume

    ! Input data
    radius = 5.0_8
    height = 10.0_8

    ! Calculate area and volume
    area = calculate_area(radius)
    volume = calculate_volume(radius, height)

    ! Print results
    call print_results(radius, height, area, volume)

end program main
