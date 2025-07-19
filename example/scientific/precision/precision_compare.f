x4 = 1.0/3.0
x8 = 1.0d0/3.0d0

print '(a)', 'Default real (will be real*8 with .f files):'
print '(a,f20.15)', '  x4 = 1/3 = ', x4
print '(a,i0)', '  precision = ', precision(x4)
print '(a)', ''
print '(a)', 'Explicit double precision:'
print '(a,f20.15)', '  x8 = 1/3 = ', x8
print '(a,i0)', '  precision = ', precision(x8)
