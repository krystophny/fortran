# Plotting Example

Demonstrates external dependency resolution (currently disabled in build).

## Files

- **`plot_demo.f90`**: Example program that would use pyplot-fortran for plotting

## External Dependencies

This example depends on `pyplot_module` which would be resolved from the registry to `pyplot-fortran`.

## Status

Currently disabled in the build (`auto-examples = false` in fpm.toml) because the external dependency is not available in the test environment.

## Running

```bash
fortran plot_demo.f90
```

This would test the Fortran CLI tool's ability to:
- Detect external module dependencies
- Resolve modules to packages using the registry
- Generate fpm.toml with external dependencies
- Build with external packages from git repositories
