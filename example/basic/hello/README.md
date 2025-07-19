# Hello World Example

Simple "Hello, World!" program demonstrating basic Fortran CLI usage with both file types.

## Files

- **`hello.f`**: Simplified syntax (no program boilerplate needed)
- **`hello.f90`**: Standard Fortran (shows expected preprocessor output)

## Running

**Simplified syntax (.f file):**
```bash
fortran hello.f
```

**Standard Fortran (.f90 file):**
```bash
fortran hello.f90
```

Both produce identical output but demonstrate different input styles.

## Preprocessor Example

The `.f` file shows simplified syntax:
```fortran
print *, 'Hello from fortran CLI!'
```

The `.f90` file shows what the preprocessor generates:
```fortran
program hello
  implicit none
  print *, 'Hello from fortran CLI!'
end program hello
```

This demonstrates the Fortran CLI tool's ability to:
- Execute simple Fortran programs
- Handle both simplified (.f) and standard (.f90) syntax
- Automatically wrap code in program structure
