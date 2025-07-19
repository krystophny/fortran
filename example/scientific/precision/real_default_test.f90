program real_default_test
    implicit none
    real :: x
    real*4 :: x4
    real*8 :: x8

    print *, 'Size comparison:'
    print *, 'sizeof(real) = ', sizeof(x), ' bytes'
    print *, 'sizeof(real*4) = ', sizeof(x4), ' bytes'
    print *, 'sizeof(real*8) = ', sizeof(x8), ' bytes'
    print *, ''
    print *, 'If -fdefault-real-8 is working, real should be 8 bytes'

end program real_default_test
