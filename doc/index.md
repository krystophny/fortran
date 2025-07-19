title: Manual
---

# Command Line Interface

## Basic Usage

```bash
fortran [OPTIONS] SOURCE_FILE
```

**Examples:**
```bash
fortran hello.f90                    # Run program
fortran -v calculator.f90            # Verbose output  
fortran --cache-dir /tmp calc.f90    # Custom cache
```

## Options

### Verbosity
```bash
-v, --verbose       Show build commands and progress
-vv                 Very verbose (debug output)
```

### Build Options
```bash
-j, --jobs N        Number of parallel build jobs (default: auto)
--flag FLAGS        Additional compiler flags
```

### Notebook Mode
```bash
--notebook          Enable Jupyter notebook mode
--notebook-output F Save notebook execution to file
```

### Cache Control
```bash
--cache-dir DIR     Use custom cache directory
                    Default: ~/.cache/fortran (Linux/macOS)
                             %LOCALAPPDATA%/fortran/cache (Windows)
```

### Configuration  
```bash
--config-dir DIR    Use custom config directory
                    Default: ~/.config/fortran (Linux/macOS)
                             %LOCALAPPDATA%/fortran/config (Windows)
```

### Debug Options
```bash
--debug-tokens      Output token JSON to SOURCE_tokens.json
--debug-ast         Output AST JSON to SOURCE_ast.json
--debug-codegen     Output codegen JSON to SOURCE_codegen.json
```

### Other Options
```bash
-h, --help          Show help message
--version           Show version information
--clear-cache       Clear the cache directory
--cache-info        Display cache directory information
--preprocess-only   Only compile .f files to Fortran IR, don't run
--no-wait          Don't wait for parallel job completion
```

# Caching System

The tool caches compiled modules for fast reuse across runs.

## Cache Location

**Default locations:**
- Linux/macOS: `~/.cache/fortran/`
- Windows: `%LOCALAPPDATA%/fortran/cache/`

**Custom location:**
```bash
fortran --cache-dir /tmp/my-cache program.f90
```

## Cache Structure
```
cache/
├── modules/           # Compiled .mod files
├── objects/           # Compiled .o files  
├── executables/       # Built executables
└── timestamps/        # Build metadata
```

## Cache Behavior

- **Automatic invalidation** - Rebuilds when source files change
- **Dependency tracking** - Updates dependent modules automatically
- **Parallel safe** - Uses file locking for concurrent builds
- **Content-based keys** - Uses FPM's digest system for cache keys

**Manual cache management:**
```bash
# Clear cache (manual - no CLI option yet)
rm -rf ~/.cache/fortran/
```

# Module Registry

External dependencies are resolved through a package registry.

## Registry File

**Location:** `~/.config/fortran/registry.toml`

**Example:**
```toml
[packages]

[packages.pyplot-fortran]
git = "https://github.com/jacobwilliams/pyplot-fortran"

[packages.fortplotlib]  
git = "https://github.com/krystophny/fortplotlib"
prefix = "fortplot"  # Any module starting with "fortplot"
```

## Resolution Strategy

1. **Explicit mapping** - Direct module-to-package mapping
2. **Prefix matching** - Module starts with registered prefix  
3. **Underscore inference** - `module_name` → `package-name`
4. **Direct inference** - `module_name` → `package-name`

**Examples:**
```fortran
use pyplot_module        ! → pyplot-fortran package
use fortplot_figure     ! → fortplotlib package (prefix: "fortplot")
use json_module         ! → json-fortran package (inferred)
```

# Architecture

## Execution Flow

```
Source File → Module Scanner → Registry Resolver → FPM Generator → Cache → Execute
```

## Core Components

### Module Scanner (`module_scanner.f90`)
- Parses Fortran source for `use` statements
- Builds dependency graph
- Handles interdependent modules

### Registry Resolver (`registry_resolver.f90`)  
- Maps module names to packages
- Resolves version constraints
- Downloads external dependencies

### FPM Generator (`fpm_generator.f90`)
- Creates dynamic `fpm.toml` files
- Applies modern defaults for .f files only
- Configures compiler flags based on file type

### Cache Manager (`cache.f90`)
- Content-based cache keys using FPM's digest
- Timestamp-based invalidation
- Cross-platform cache directories

### Runner (`runner.f90`)
- Orchestrates the entire build process
- Handles error reporting
- Manages temporary files

## Modern Defaults

The tool applies different defaults based on file type:

**For .f90 files (Standard Fortran):**
```toml
# Generated fpm.toml
[fortran]
implicit-typing = false      # Enforces 'implicit none'
implicit-external = false    # No implicit externals
source-form = "free"         # Modern free-form source
# No special compiler flags
```

**For .f files (Lowercase Fortran with Frontend):**
```toml
# Generated fpm.toml (same base + additional flags)
[build]
flag = "-fdefault-real-8 -fdefault-double-8"  # Double precision for .f files
```

# Advanced Usage

## Local Module Development

```bash
# Project structure
my-project/
├── main.f90
├── utils.f90
└── math_helpers.f90
```

```bash
# Automatically finds and builds all modules
fortran main.f90
```

## External Dependencies

**Install packages to registry:**
```toml
# ~/.config/fortran/registry.toml
[packages.my-package]
git = "https://github.com/user/my-package"
tag = "v1.0.0"  # Optional version constraint
```

**Use in code:**
```fortran
program example
    use my_module  ! Automatically resolved and built
    ! ...
end program
```

## Integration with Existing Projects

**Option 1: Direct usage**
```bash
cd existing-fpm-project/
fortran app/main.f90  # Uses existing src/ modules
```

**Option 2: Registry registration**
```bash
# Add local project to registry
echo '[packages.my-lib]' >> ~/.config/fortran/registry.toml
echo 'path = "/path/to/my-lib"' >> ~/.config/fortran/registry.toml
```

## Performance Considerations

- **First run**: Downloads and builds dependencies (slower)
- **Subsequent runs**: Uses cached modules (fast)
- **Source changes**: Rebuilds only affected modules
- **Cache size**: Grows over time, manual cleanup needed

**Benchmark results:**
```bash
# First run (with dependencies)
time fortran plotting_example.f90  # ~30s

# Cached run  
time fortran plotting_example.f90  # ~2s

# Modified source (partial rebuild)
time fortran plotting_example.f90  # ~5s
```

# Troubleshooting

## Common Issues

**Module not found:**
```
Error: Module 'my_module' not found in registry
```
**Solution:** Add package to `~/.config/fortran/registry.toml`

**Build failures:**
```bash
# Use verbose mode to see compiler errors
fortran -vv problematic.f90
```

**Cache corruption:**
```bash
# Clear cache and retry
rm -rf ~/.cache/fortran/
fortran myprogram.f90
```

**Permission errors:**
```bash
# Check cache directory permissions
ls -la ~/.cache/fortran/
```

## Debug Output

**Verbose mode (`-v`):**
```bash
$ fortran -v hello.f90
[INFO] Scanning for modules in hello.f90
[INFO] No external dependencies found
[INFO] Generating fpm.toml
[INFO] Building with FPM...
[INFO] Execution completed
```

**Very verbose mode (`-vv`):**
```bash
$ fortran -vv hello.f90
[DEBUG] Cache directory: /home/user/.cache/fortran
[DEBUG] Config directory: /home/user/.config/fortran  
[DEBUG] Source file: hello.f90
[DEBUG] Module dependencies: []
[DEBUG] Cache key: abc123def456
[DEBUG] FPM command: fpm run --flag "-fdefault-real-8 -fdefault-double-8"
[DEBUG] Build output: [compiler messages]
```

**JSON Debug Output:**

The tool can output JSON representations of compilation stages for debugging:

```bash
# Output all stages
$ fortran example.f --debug-tokens --debug-ast --debug-codegen

# Creates:
# - example_tokens.json   (lexical tokens)
# - example_ast.json      (abstract syntax tree)
# - example_codegen.json  (generated code)
```

Example token JSON:
```json
{
  "tokens": [
    {"type": "identifier", "text": "x", "line": 1, "column": 1},
    {"type": "operator", "text": "=", "line": 1, "column": 3},
    {"type": "number", "text": "42", "line": 1, "column": 5}
  ]
}
```

Example AST JSON:
```json
{
  "type": "assignment",
  "target": {"type": "identifier", "name": "x"},
  "value": {"type": "literal", "value": "42", "kind": "integer"}
}
```

## Error Messages Guide

| Error | Meaning | Solution |
|-------|---------|----------|
| `Module 'X' not found` | Missing package in registry | Add to registry.toml |
| `FPM build failed` | Compiler error | Check source code, use `-vv` |
| `Cache lock timeout` | Parallel build conflict | Wait or clear locks |
| `Permission denied` | Cache directory access | Check permissions |

## Getting Help

- Use `fortran --help` for CLI reference
- Check [[Examples]] for working code samples  
- Report issues at https://github.com/krystophny/fortran/issues
- Enable verbose mode (`-vv`) when reporting bugs
