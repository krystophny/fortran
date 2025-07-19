# Preprocessor Examples

This directory contains examples of simplified Fortran files (`.f`) that demonstrate the preprocessor functionality.

## What is the Preprocessor?

The preprocessor allows you to write Fortran code without the boilerplate:
- No need for `program`/`end program` statements
- No need for `contains` statements before functions/subroutines
- Automatic `implicit none` insertion
- The tool wraps your code in proper Fortran structure

## Examples

### hello.f
The simplest possible Fortran program - just print statements:
```fortran
print *, 'Hello from preprocessed Fortran!'
```

### math.f
Functions without the `contains` boilerplate:
```fortran
x = 5.0
result = add(x, 3.0)
print *, 'Sum:', result

real function add(a, b)
  real :: a, b
  add = a + b
end function
```

### subroutines.f
Subroutines work the same way:
```fortran
radius = 5.0
call calculate_circle(radius, area, circumference)

subroutine calculate_circle(r, a, c)
  real :: r, a, c
  a = 3.14159 * r * r
  c = 2.0 * 3.14159 * r
end subroutine
```

## Running the Examples

```bash
# Run any .f file directly
fortran hello.f
fortran math.f
fortran subroutines.f

# Use verbose mode to see preprocessing
fortran -v hello.f
```

## How it Works

When you run a `.f` file, the tool:
1. Detects it's a preprocessor file (by the `.f` extension)
2. Analyzes the code structure
3. If no `program` statement exists, wraps everything in `program main`
4. Adds `implicit none` automatically
5. Inserts `contains` before any functions/subroutines
6. Generates a proper `.f90` file in the cache directory
7. Compiles and runs the generated file

This allows for much cleaner, more concise Fortran code!
