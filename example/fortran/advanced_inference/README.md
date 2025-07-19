# Advanced Type Inference Examples

This directory contains examples demonstrating the advanced type inference capabilities of the Fortran CLI tool.

## Array Type Inference

The `arrays.f` example shows:
- Simple array literals: `[1, 2, 3]` → `integer, dimension(3)`
- Real arrays: `[1.0, 2.0, 3.0]` → `real(8), dimension(3)`  
- Mixed type arrays: `[1, 2.0, 3]` → `real(8), dimension(3)` (promoted)
- Multi-dimensional arrays via `reshape`

## Derived Type Inference

The `derived_types.f` example shows:
- Field access patterns: `person.name = "Alice"` → creates `person_type`
- Multiple field assignments automatically infer the derived type structure
- Different derived types in the same program

## Function Return Type Inference

The `function_returns.f` example shows:
- Intrinsic function return types: `sin(x)` → `real(8)`
- String functions: `len_trim(text)` → `integer`
- Math functions: `sqrt(number)` → `real(8)`
- Future: User-defined function inference from definitions

## Running the Examples

**Standard Fortran (.f90 files) - Working:**
```bash
fortran arrays.f90           # ✅ Working
fortran derived_types.f90    # ✅ Working  
fortran function_returns.f90 # ✅ Working
fortran intrinsic_functions.f90 # ✅ Working
```

**Simplified syntax (.f files) - Current Status:**
```bash
fortran arrays.f           # ❌ Array inference not implemented yet
fortran derived_types.f    # ❌ Derived type inference not implemented yet
fortran function_returns.f # ❌ Complex inference patterns not ready
fortran intrinsic_functions.f # ❌ Function return inference not ready
```

## Current Implementation Status
- ✅ **Basic type inference**: integer, real(8), character(len=N), logical
- 🔄 **Array inference**: Array literals and multi-dimensional arrays (planned)
- 🔄 **Derived type inference**: Field access pattern recognition (planned)
- 🔄 **Function return inference**: Return type detection from calls (planned)
- 🔄 **Advanced patterns**: Complex expressions and mixed types (planned)

These examples demonstrate the **target capabilities** for future development phases.
The .f90 versions show the expected output that the preprocessor should eventually generate.
