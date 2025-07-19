! %% [markdown]
! # Arrays and Loops in Fortran
!
! This notebook demonstrates array operations and loop constructs.

! %%
! Create an array
real(8) :: numbers(5)
integer :: i

! Initialize array with squares
do i = 1, 5
    numbers(i) = real(i)**2
end do

print *, "Array of squares:", numbers

! %% [markdown]
! ## Array Operations
!
! Fortran supports vectorized operations on arrays:

! %%
! Array arithmetic
real(8) :: doubled(5), shifted(5)

doubled = 2.0*numbers
shifted = numbers + 10.0

print *, "Original:", numbers
print *, "Doubled:", doubled
print *, "Shifted:", shifted

! %% [markdown]
! ## Array Reductions
!
! Common reduction operations:

! %%
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

! %% [markdown]
! ## Conditional Operations
!
! Using WHERE construct for conditional array operations:

! %%
! Filter values
real(8) :: filtered(5)

where (numbers > 5.0)
    filtered = numbers
elsewhere
    filtered = 0.0
end where

print *, "Values > 5:", filtered

! Count elements meeting condition
count_large = count(numbers > 5.0)
print *, "Number of elements > 5:", count_large
