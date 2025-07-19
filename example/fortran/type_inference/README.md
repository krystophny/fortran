# Type Inference Example

This example demonstrates the difference between standard Fortran (.f90) and simplified Fortran (.f) formats, showcasing automatic type inference.

## Files

- `calculate.f90` - Standard Fortran with explicit declarations
- `calculate.f` - Simplified format with automatic type inference
- `all_types.f` - Comprehensive example showing inference for all basic types

## Running

```bash
# Run the standard version
fortran calculate.f90

# Run the simplified version
fortran calculate.f

# Run the comprehensive type example
fortran all_types.f
```

Both `calculate` examples produce identical output, but the `.f` version eliminates all type declaration boilerplate through automatic type inference.

## Type Inference

The `.f` format automatically infers types from literals and expressions:
- Integer literals (e.g., `42`) → `integer`
- Real literals (e.g., `3.14159`) → `real(8)`
- Character strings (e.g., `"Fortran"`) → `character(len=n)`
- Logical literals (e.g., `.true.`) → `logical`
- Arithmetic expressions inherit types from operands with promotion rules
- Comparison expressions (e.g., `x > 0`) → `logical`
