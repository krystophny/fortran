! Test default real precision
x = 1.0
x4 = 1.0
x8 = 1.0d0

print *, 'Size comparison:'
print *, 'sizeof(real) = ', sizeof(x), ' bytes'
print *, 'sizeof(real*4) = ', sizeof(x4), ' bytes'
print *, 'sizeof(real*8) = ', sizeof(x8), ' bytes'
print *, ''
print *, 'With .f files and -fdefault-real-8, real should be 8 bytes'
