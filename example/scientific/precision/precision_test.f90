program main
    implicit none

    ! Auto-generated variable declarations:
    real(8) :: x
    real(8) :: y

    x = 1.0/3.0
    y = x*3.0

    print '(a,f20.15)', 'x = 1/3 = ', x
    print '(a,f20.15)', 'y = x*3 = ', y
    print '(a,i0)', 'Precision digits: ', precision(x)
end program main
