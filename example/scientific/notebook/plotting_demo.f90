! plotting_demo.f90 - Standard Fortran version of notebook example
program plotting_demo
    implicit none
    integer, parameter :: n_points = 100
    real(8), parameter :: x_min = 0.0_8, x_max = 6.28318530718_8
    real(8) :: x_data(n_points), y_sin(n_points), y_cos(n_points), y_combined(n_points)
    real(8) :: mean_sin, mean_cos, mean_combined, variance_sin, std_sin
    integer :: i, plot_pos, max_sin_idx, min_sin_idx
    character(len=52) :: line

    print *, "Setting up analysis with", n_points, "data points"
    print *, "Range: [", x_min, ",", x_max, "]"

    ! Generate data arrays
    do i = 1, n_points
        x_data(i) = x_min + (i - 1)*(x_max - x_min)/(n_points - 1)
        y_sin(i) = sin(x_data(i))
        y_cos(i) = cos(x_data(i))
        y_combined(i) = sin(x_data(i))*cos(x_data(i))
    end do

    print *, "Generated", n_points, "data points"
    print *, "X range: [", x_data(1), ",", x_data(n_points), "]"
    print *, "Sin range: [", minval(y_sin), ",", maxval(y_sin), "]"
    print *, "Cos range: [", minval(y_cos), ",", maxval(y_cos), "]"

    ! Calculate statistics
    mean_sin = sum(y_sin)/n_points
    mean_cos = sum(y_cos)/n_points
    mean_combined = sum(y_combined)/n_points

    ! Calculate variance (simplified)
    variance_sin = sum((y_sin - mean_sin)**2)/n_points
    std_sin = sqrt(variance_sin)

    print *, "=== Statistical Analysis ==="
    print *, "Mean sin(x):", mean_sin
    print *, "Mean cos(x):", mean_cos
    print *, "Mean sin*cos:", mean_combined
    print *, "Std dev sin(x):", std_sin

    ! Simple ASCII plot of sine function
    print *, ""
    print *, "ASCII Plot of sin(x):"
    print *, "+"//repeat("-", 50)//"+"

    do i = 1, n_points, 5  ! Sample every 5th point
        plot_pos = int((y_sin(i) + 1.0_8)*25) + 1  ! Scale to 1-50
        if (plot_pos < 1) plot_pos = 1
        if (plot_pos > 50) plot_pos = 50

        line = "|"//repeat(" ", plot_pos - 1)//"*"//repeat(" ", 50 - plot_pos)//"|"
        print *, line
    end do

    print *, "+"//repeat("-", 50)//"+"
    print *, "Scale: -1.0 to +1.0 (left to right)"

    ! Note: Fortplotlib graphics would be included here in a real implementation
    ! For this example, we'll just print a message indicating where plots would appear
    print *, ""
    print *, "=== Graphics Output ==="
    print *, "Figure 1: Trigonometric functions plot (sin and cos)"
    print *, "Figure 2: Product function plot (sin*cos)"
    print *, "(In notebook mode, these would be auto-converted to base64 PNG images)"

    ! Export summary data
    print *, ""
    print *, "=== Data Summary ==="
    print *, "Sample points (every 10th):"
    print *, "    X        Sin(X)     Cos(X)    Sin*Cos"
    print *, repeat("-", 45)

    do i = 1, n_points, 10
        print '(4f11.6)', x_data(i), y_sin(i), y_cos(i), y_combined(i)
    end do

    ! Calculate peak locations
    max_sin_idx = maxloc(y_sin, 1)
    min_sin_idx = minloc(y_sin, 1)

    print *, ""
    print *, "Peak Analysis:"
    print *, "Max sin(x) =", y_sin(max_sin_idx), "at x =", x_data(max_sin_idx)
    print *, "Min sin(x) =", y_sin(min_sin_idx), "at x =", x_data(min_sin_idx)

    print *, ""
    print *, "This program demonstrates scientific computing capabilities"
    print *, "including data analysis, statistics, and visualization."

end program plotting_demo
