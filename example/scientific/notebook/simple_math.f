! %% [markdown]
! # Simple Mathematical Computations
!
! This notebook demonstrates basic mathematical operations in Fortran
! with automatic output capture.

! %%
! Define some variables
x = 5.0
y = 3.0

print *, "x =", x
print *, "y =", y

! %% [markdown]
! ## Basic Arithmetic Operations
!
! Let's perform some basic calculations:

! %%
! Addition and subtraction
sum = x + y
diff = x - y

print *, "x + y =", sum
print *, "x - y =", diff

! %%
! Multiplication and division
product = x*y
quotient = x/y

print *, "x * y =", product
print *, "x / y =", quotient

! %% [markdown]
! ## Power Operations
!
! Fortran supports exponentiation using the ** operator:

! %%
! Powers
square = x**2
cube = y**3
sqrt_x = sqrt(x)

print *, "x^2 =", square
print *, "y^3 =", cube
print *, "sqrt(x) =", sqrt_x

! %% [markdown]
! ## Summary
!
! We've demonstrated basic arithmetic operations in Fortran.
! All print statements are automatically captured and displayed
! below each code cell in the rendered output.
