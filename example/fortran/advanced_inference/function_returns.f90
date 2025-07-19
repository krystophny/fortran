! function_returns.f90 - Standard Fortran function examples
program function_returns
    implicit none
    real(8) :: radius, x, y, number, area, result, angle, value
    character(len=20) :: text
    integer :: input_data, length
    logical :: flag

    ! Initialize some input variables
    radius = 5.0_8
    x = 1.5_8
    y = 2.5_8
    text = "Hello World"
    number = 16.0_8
    input_data = 42

    ! For demonstration, we'll use intrinsic functions that actually exist
    area = 3.14159_8*radius**2  ! Simulated compute_area
    result = x + y                ! Simulated calculate_sum
    flag = input_data > 0         ! Simulated is_valid

    ! Intrinsic function calls
    angle = sin(x)           ! real(8) - from intrinsic sin
    length = len_trim(text)  ! integer - from intrinsic len_trim
    value = sqrt(number)     ! real(8) - from intrinsic sqrt

    print *, 'Area:', area
    print *, 'Result:', result
    print *, 'Flag:', flag
    print *, 'Angle:', angle
    print *, 'Length:', length
    print *, 'Value:', value
end program function_returns
