! Math example with functions but no contains statement
real :: x, y, result

x = 5.0
y = 3.0

result = add(x, y)
print *, 'Sum:', result

result = multiply(x, y)
print *, 'Product:', result

real function add(a, b)
    real :: a, b
    add = a + b
end function add

real function multiply(a, b)
    real :: a, b
    multiply = a*b
end function multiply
