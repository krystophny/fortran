! %% [markdown]
! # Scientific Computing with Fortran
!
! This notebook demonstrates scientific computing capabilities including
! data analysis, mathematical functions, and ASCII visualization.

! %%
! Initialize parameters for our analysis
n_points = 100
x_min = 0.0
x_max = 6.28318530718  ! 2*pi

print *, "Setting up analysis with", n_points, "data points"
print *, "Range: [", x_min, ",", x_max, "]"

! %% [markdown]
! ## Data Generation
!
! Let's create arrays for our analysis and fill them with computed values:

! %%
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

! %% [markdown]
! ## Statistical Analysis
!
! Compute basic statistics for our generated data:

! %%
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

! %% [markdown]
! ## ASCII Visualization
!
! Create ASCII plots of our data for visualization:

! %%
! Simple ASCII plot of sine function
print *, ""
print *, "ASCII Plot of sin(x):"
print *, "+"//repeat("-", 50)//"+"

do i = 1, n_points, 5  ! Sample every 5th point
    plot_pos = int((y_sin(i) + 1.0)*25) + 1  ! Scale to 1-50
    if (plot_pos < 1) plot_pos = 1
    if (plot_pos > 50) plot_pos = 50

    line = "|"//repeat(" ", plot_pos - 1)//"*"//repeat(" ", 50 - plot_pos)//"|"
    print *, line
end do

print *, "+"//repeat("-", 50)//"+"
print *, "Scale: -1.0 to +1.0 (left to right)"

! %% [markdown]
! ## Graphics Generation with Fortplotlib
!
! Generate plots using fortplotlib - the .show() calls will be automatically
! converted to base64-encoded PNG images in notebook mode:

! %%
use fortplotlib

! Create a figure and plot the sine data
call figure()
call plot(x_data, y_sin, 'b-', label='sin(x)')
call plot(x_data, y_cos, 'r--', label='cos(x)')
call xlabel('x')
call ylabel('y')
call title('Trigonometric Functions')
call legend()
call grid(.true.)
call show()  ! This will be auto-converted to base64 PNG in notebook mode

! Create a second figure for combined function
call figure()
call plot(x_data, y_combined, 'g-', linewidth=2.0)
call xlabel('x')
call ylabel('sin(x) * cos(x)')
call title('Product of Sine and Cosine')
call grid(.true.)
call show()  ! Another auto-converted PNG

! %% [markdown]
! ## Data Summary
!
! Export summary of our computed data:

! %%
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

! %% [markdown]
! ## Summary
!
! This notebook demonstrated:
! - Data generation with mathematical functions
! - Statistical analysis (mean, variance, standard deviation)
! - ASCII plotting for terminal visualization
! - PNG generation with base64 encoding for notebook display
! - Data export and peak analysis
!
! All computations were performed with double precision arithmetic
! automatically provided by the .f file preprocessing.
!
! The PNG plots are embedded as base64-encoded images in the markdown
! output, creating a complete self-contained document.
