! %% [markdown]
! # Control Flow in Fortran
!
! This notebook demonstrates various control flow constructs including
! conditionals, loops, and subroutines.

! %%
! Initialize some test variables
real(8) :: temperature = 25.5
integer :: grade = 85
logical :: is_passed = .false.

print *, "Temperature:", temperature, "°C"
print *, "Grade:", grade

! %% [markdown]
! ## Conditional Statements
!
! Fortran supports if-then-else constructs for decision making:

! %%
! Temperature classification
if (temperature < 0.0) then
    print *, "It's freezing!"
else if (temperature < 20.0) then
    print *, "It's cold"
else if (temperature < 30.0) then
    print *, "It's mild"
else
    print *, "It's hot!"
end if

! %% [markdown]
! ## Logical Operations
!
! Working with boolean values and logical operators:

! %%
! Grade evaluation
if (grade >= 60) then
    is_passed = .true.
    print *, "Student passed with grade:", grade
else
    is_passed = .false.
    print *, "Student failed with grade:", grade
end if

! Complex logical condition
if (grade >= 90 .and. is_passed) then
    print *, "Excellent work!"
else if (grade >= 80 .and. is_passed) then
    print *, "Good job!"
else if (is_passed) then
    print *, "Keep trying!"
else
    print *, "Need more study"
end if

! %% [markdown]
! ## Loop Constructs
!
! Different types of loops for iteration:

! %%
! Simple do loop
print *, "Counting from 1 to 5:"
do i = 1, 5
    print *, "Count:", i
end do

! %% [markdown]
! ## Do While Loops
!
! Loops with conditions:

! %%
! Fibonacci sequence (first 10 terms)
integer :: fib1 = 0, fib2 = 1, fib_next, n = 0

print *, "Fibonacci sequence:"
print *, "F(0) =", fib1
print *, "F(1) =", fib2

do while (n < 8)
    fib_next = fib1 + fib2
    n = n + 1
    print *, "F(", n + 1, ") =", fib_next
    fib1 = fib2
    fib2 = fib_next
end do

! %% [markdown]
! ## Select Case Constructs
!
! Multi-way branching based on values:

! %%
! Day of week classification
integer :: day_num = 3

select case (day_num)
case (1)
    print *, "Monday - Start of work week"
case (2:5)
    print *, "Weekday - Work day"
case (6)
    print *, "Saturday - Weekend starts"
case (7)
    print *, "Sunday - Day of rest"
case default
    print *, "Invalid day number"
end select

! %% [markdown]
! ## Nested Loops and Exit
!
! Using nested loops with exit and cycle statements:

! %%
! Find prime numbers up to 20
integer :: num, divisor
logical :: is_prime

print *, "Prime numbers up to 20:"

outer: do num = 2, 20
    is_prime = .true.

    ! Check for divisors
    inner: do divisor = 2, int(sqrt(real(num)))
        if (mod(num, divisor) == 0) then
            is_prime = .false.
            exit inner
        end if
    end do inner

    if (is_prime) then
        print *, num, "is prime"
    end if
end do outer

! %% [markdown]
! ## Summary
!
! This notebook demonstrated:
! - If-then-else statements
! - Logical operations and conditions
! - Do loops and do-while loops
! - Select case constructs
! - Nested loops with exit and cycle
! - String formatting with adjustl and trim
!
! These control flow constructs form the foundation of structured
! programming in Fortran.
