! arrays_loops.f90 - Standard Fortran version
program arrays_loops
    implicit none
    real(8) :: numbers(5), doubled(5), shifted(5), filtered(5)
    real(8) :: total, prod, avg, max_val, min_val
    integer :: i, count_large

    ! Initialize array with squares
    do i = 1, 5
        numbers(i) = real(i, 8)**2
    end do

    print *, "Array of squares:", numbers

    ! Array arithmetic
    doubled = 2.0_8*numbers
    shifted = numbers + 10.0_8

    print *, "Original:", numbers
    print *, "Doubled:", doubled
    print *, "Shifted:", shifted

    ! Sum, product, and statistics
    total = sum(numbers)
    prod = product(numbers)
    avg = sum(numbers)/size(numbers)
    max_val = maxval(numbers)
    min_val = minval(numbers)

    print *, "Sum:", total
    print *, "Product:", prod
    print *, "Average:", avg
    print *, "Maximum:", max_val
    print *, "Minimum:", min_val

    ! Filter values
    where (numbers > 5.0_8)
        filtered = numbers
    elsewhere
        filtered = 0.0_8
    end where

    print *, "Values > 5:", filtered
    count_large = count(numbers > 5.0_8)
    print *, "Number of elements > 5:", count_large
end program arrays_loops
