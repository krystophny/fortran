! arrays.f90 - Standard Fortran array examples
program arrays
    implicit none
    integer, parameter :: arr1(3) = [1, 2, 3]
    real(8), parameter :: arr2(3) = [1.0_8, 2.0_8, 3.0_8]
    real(8), parameter :: mixed_arr(3) = [1.0_8, 2.0_8, 3.0_8]
    integer, parameter :: matrix(2, 2) = reshape([1, 2, 3, 4], [2, 2])

    print *, 'Array 1:', arr1
    print *, 'Array 2:', arr2
    print *, 'Mixed array:', mixed_arr
    print *, 'Matrix:', matrix
end program arrays
