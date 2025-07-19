! Function return type inference examples

! Initialize some input variables
radius = 5.0
x = 1.5
y = 2.5
text = "Hello World"
number = 16.0
input_data = 42

! Function calls - types inferred from returns
area = compute_area(radius)    ! Would be real if compute_area existed
result = calculate_sum(x, y)   ! Would be real if calculate_sum existed
flag = is_valid(input_data)    ! Would be logical if is_valid existed

! Intrinsic function calls - types correctly inferred
angle = sin(x)           ! real(8) - from intrinsic sin
length = len_trim(text)  ! integer - from intrinsic len_trim
value = sqrt(number)     ! real(8) - from intrinsic sqrt

print *, 'Area:', area
print *, 'Result:', result
print *, 'Flag:', flag
print *, 'Angle:', angle
print *, 'Length:', length
print *, 'Value:', value

! Note: Function definitions would be needed for user-defined functions
! The tool can infer types from intrinsic functions automatically
