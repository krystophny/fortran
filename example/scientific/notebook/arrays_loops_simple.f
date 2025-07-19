! arrays_loops_simple.f - Simplified array operations
! Create and initialize array with squares
do i = 1, 5
    numbers(i) = i**2
end do

print *, "Array of squares:", numbers

! Array arithmetic
doubled = 2.0*numbers
shifted = numbers + 10.0

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
where (numbers > 5.0)
    filtered = numbers
elsewhere
    filtered = 0.0
end where

print *, "Values > 5:", filtered
count_large = count(numbers > 5.0)
print *, "Number of elements > 5:", count_large
