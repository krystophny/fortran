use geometry, only: calculate_area, calculate_volume
use input_output, only: print_results

! Input data
radius = 5.0
height = 10.0

! Calculate area and volume
area = calculate_area(radius)
volume = calculate_volume(radius, height)

! Print results
call print_results(radius, height, area, volume)
