use pyplot_module

! Generate some data
do i = 1, 100
    x(i) = (i - 1)*0.1
    y(i) = sin(x(i))
end do

! This would plot if pyplot-fortran was installed
print *, 'Plot demo - would plot sin(x) with pyplot-fortran'
print *, 'x range: ', x(1), ' to ', x(100)
print *, 'y range: ', minval(y), ' to ', maxval(y)
