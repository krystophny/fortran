program precision_compare
    implicit none
    real :: x4
    real(8) :: x8

    x4 = 1.0/3.0
    x8 = 1.0d0/3.0d0

    print '(a)', 'Default real (should be real*8 with flags):'
    print '(a,f20.15)', '  x4 = 1/3 = ', x4
    print '(a,i0)', '  precision = ', precision(x4)
    print '(a)', ''
    print '(a)', 'Explicit real(8):'
    print '(a,f20.15)', '  x8 = 1/3 = ', x8
    print '(a,i0)', '  precision = ', precision(x8)

end program precision_compare
