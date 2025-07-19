! Intrinsic function return type inference examples

! Initialize input variables with different types
x = 1.5
text = "Hello World"
number = 16.0

! Intrinsic function calls - types correctly inferred
angle = sin(x)           ! real(8) - from intrinsic sin
length = len_trim(text)  ! integer - from intrinsic len_trim
value = sqrt(number)     ! real(8) - from intrinsic sqrt
cosine = cos(angle)      ! real(8) - from intrinsic cos
! WORKAROUND: Basic codegen can't handle negative literals in function calls
neg_val = -5.5
absolute = abs(neg_val)  ! real(8) - from intrinsic abs

print *, 'X:', x
print *, 'Text:', text
print *, 'Number:', number
print *, 'Angle (sin):', angle
print *, 'Length:', length
print *, 'Value (sqrt):', value
print *, 'Cosine:', cosine
print *, 'Absolute:', absolute
