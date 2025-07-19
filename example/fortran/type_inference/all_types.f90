program main
    implicit none

    ! Auto-generated variable declarations:
    integer :: count
    real(8) :: pi
    character(len=7) :: name
    logical :: ready
    integer :: sum_int
    real(8) :: product_real
    real(8) :: mixed
    logical :: is_positive

    ! all_types.f - Demonstrates type inference for all basic types
    count = 42
    pi = 3.14159
    name = "Fortran"
    ready = .true.

    ! Arithmetic operations preserve types
    sum_int = count + 10
    product_real = pi*2.0

    ! Mixed expressions promote to real
    mixed = count + pi

    ! Logical from comparison
    is_positive = count > 0

    ! Print all values
    print *, 'Integer count:', count
    print *, 'Real pi:', pi
    print *, 'String name:', name
    print *, 'Logical ready:', ready
    print *, 'Integer sum:', sum_int
    print *, 'Real product:', product_real
    print *, 'Mixed result:', mixed
    print *, 'Is positive?', is_positive
end program main
