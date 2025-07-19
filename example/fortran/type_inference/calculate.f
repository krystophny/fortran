! calculate.f
pi = 3.14159          ! real constant
radius = 5.0          ! real
n_circles = 3         ! integer
label = "Test Circle" ! character string

area = pi*radius*radius        ! real (inferred)
circumference = 2.0*pi*radius  ! real (inferred)
is_large = area > 50.0             ! logical (inferred)

print *, 'Label:', trim(label)
print *, 'Number of circles:', n_circles
print *, 'Radius:', radius
print *, 'Area:', area
print *, 'Circumference:', circumference
print *, 'Is large?', is_large
