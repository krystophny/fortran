! Calculator with type inference
! Variables get their types from first use

a = 10
b = 20
sum = a + b
diff = a - b
prod = a*b
quot = real(a)/real(b)  ! Force real division

print *, "a =", a
print *, "b =", b
print *, "sum =", sum
print *, "difference =", diff
print *, "product =", prod
print *, "quotient =", quot

! Using functions - type inferred from return
angle = 0.5
sine_val = sin(angle)
cosine_val = cos(angle)

print *, "sin(", angle, ") =", sine_val
print *, "cos(", angle, ") =", cosine_val
