# Step 1: Explicit Function Types with Opinionated Defaults

This example demonstrates **Step 1** of the advanced type inference system, which applies opinionated modern defaults to explicitly typed functions.

## What Step 1 Does

When you provide explicit type information, the preprocessor enhances it with modern Fortran best practices:

1. **Function signatures**: `real function` → `real(8) function`
2. **Parameter declarations**: `real :: x` → `real(8), intent(in) :: x`  
3. **Forward type propagation**: Variables get types from function return types
4. **Automatic `intent(in)`**: Applied as opinionated default for parameters

## Example

### Input (.f file)
```fortran
result = square(5.0)

real function square(x)
  real :: x
  square = x * x
end function
```

### Generated output (.f90 file)
```fortran
program main
  implicit none

  real(8) :: result  ! ← Forward type propagation

  result = square(5.0)

contains
real(8) function square(x)  ! ← Enhanced signature
  implicit none

  real(8), intent(in) :: x  ! ← Enhanced parameter

  square = x * x
end function
end program
```

## Run the Example

```bash
# Run the .f file (gets preprocessed automatically)
fpm run fortran -- example/step1_explicit_types/step1_demo.f

# Compare with the .f90 equivalent
gfortran -fdefault-real-8 -fdefault-double-8 example/step1_explicit_types/step1_demo.f90 -o step1_demo
./step1_demo
```

## Key Benefits

1. **Explicitness**: Types are made explicit even when compiler defaults apply
2. **Intent clarity**: Parameters get `intent(in)` by default for better interfaces
3. **Modern precision**: `real` becomes `real(8)` for consistent double precision
4. **Forward compatibility**: Generated code is self-documenting and portable

## Next Steps

- **Step 2**: Function return type inference from function body analysis
- **Step 3**: Parameter type inference from call site analysis

This forms the foundation for more advanced type inference capabilities.
