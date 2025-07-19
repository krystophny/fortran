# Calculator Example

Demonstrates local module usage with a simple calculator program in both syntax styles.

## Files

**Simplified syntax (.f files):**
- **`calculator.f`**: Main program with simplified syntax
- **`math_module.f`**: Module with type inference (no explicit declarations)

**Standard Fortran (.f90 files):**
- **`calculator.f90`**: Standard program (shows expected preprocessor output)
- **`math_module.f90`**: Standard module with explicit types

## Module Dependencies

```
calculator.f / calculator.f90
└── math_utils (from math_module.f / math_module.f90)
```

## Running

**Simplified syntax:**
```bash
fortran calculator.f
```

**Standard Fortran:**
```bash
fortran calculator.f90
```

## Preprocessor Example

**Simplified `.f` version:**
```fortran
! calculator.f
use math_utils, only: add, multiply

x = 5.0  ! Type inference: real(8)
y = 3.0  ! Type inference: real(8)
sum = add(x, y)
product = multiply(x, y)
print '(a,f5.1,a,f5.1,a,f5.1)', 'Sum of ', x, ' and ', y, ' is ', sum
```

**Generated `.f90` equivalent:**
```fortran
! calculator.f90
program calculator
  use math_utils, only: add, multiply
  implicit none

  real :: x, y, sum, product  ! Explicit declarations

  x = 5.0
  y = 3.0
  sum = add(x, y)
  ! ... rest of code
end program calculator
```

This demonstrates the Fortran CLI tool's ability to:
- Detect local module dependencies
- Handle both simplified and standard syntax  
- Automatically generate type declarations and program structure

## Current Status
- ✅ **calculator.f90**: Fully working with standard modules
- ⚠️ **calculator.f**: Known limitation - USE statements after variable declarations not yet supported
- ✅ **math_module.f90**: Standard module format working
- 🔄 **math_module.f**: Module preprocessing not yet implemented

**Workaround**: Use .f90 modules with .f main programs until module preprocessing is implemented.
