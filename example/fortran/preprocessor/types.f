! Example showing automatic type inference
! No variable declarations needed!

count = 0
pi = 3.14159265359
name = "Fortran"
is_ready = .true.

! Type inference from expressions
total = count + 10
radius = 5.0
area = pi*radius*radius

! Mixed expressions
x = 2
y = 3.5
result = x + y  ! Should infer real

print *, "Count:", count
print *, "Pi:", pi
print *, "Name:", name
print *, "Ready:", is_ready
print *, "Total:", total
print *, "Area:", area
print *, "Result:", result
