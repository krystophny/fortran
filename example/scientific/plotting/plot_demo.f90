program plot_demo
    use pyplot_module
    implicit none

    real(8), dimension(100) :: x, y
    integer :: i

    ! Generate some data
    do i = 1, 100
        x(i) = real(i - 1, 8)*0.1
        y(i) = sin(x(i))
    end do

    ! This would plot if pyplot-fortran was installed
    print *, 'Plot demo - would plot sin(x) with pyplot-fortran'
    print *, 'x range: ', x(1), ' to ', x(100)
    print *, 'y range: ', minval(y), ' to ', maxval(y)

end program plot_demo
