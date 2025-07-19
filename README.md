![fortran](media/logo.png)

[![codecov](https://codecov.io/gh/krystophny/fortran/branch/main/graph/badge.svg)](https://codecov.io/gh/krystophny/fortran)
[![Documentation](https://img.shields.io/badge/docs-FORD-blue.svg)](https://krystophny.github.io/fortran/)

**Make Python Fortran again.** - A command-line tool that enables running Fortran programs without manual compilation, automatically resolving dependencies and applying modern defaults.

Our experimental *lowercase fortran* dialect pushes beyond all alternative scientific computing languages to combine Fortran's performance with modern expressiveness!

## Quick Start

```bash
# Run any Fortran program instantly
fortran hello.f90

# *lowercase fortran* .f files (no boilerplate needed!)
fortran script.f

# Notebook mode with figure capture
fortran --notebook analysis.f

# Verbose mode for debugging
fortran -v myprogram.f90

# Cache management
fortran --clear-cache               # Clear all cached files
fortran --clear-cache example.f90   # Clear cache and run
fortran --cache-info                # Show cache statistics
```

## *lowercase fortran* Showcase

Write fortran code with **zero boilerplate** - just the logic you need:

```fortran
x = 5.0
y = 3.0
z = sqrt(x**2 + y**2)
print *, "Distance:", z

result = distance(3.0, 4.0)
print *, "Function result:", result

real function distance(a, b)
  real :: a, b
  distance = sqrt(a**2 + b**2)
end function distance
```

**Runs as:** `fortran calculate.f`

**Automatically transforms to:**
- ✅ Wrapped in `program` statement
- ✅ Modern implicit typing via automatic type inference
- ✅ Double precision defaults (`real(8)`)
- ✅ Type declarations automatically generated
- ✅ `contains` section for functions
- ✅ Function parameters default to `intent(in)` for safety

## Features

🚀 **Zero Configuration**
- No Makefiles, build scripts, or project setup
- Automatic dependency detection and resolution
- Smart caching with 2-4x performance improvements

🎯 **Two Execution Modes**  
- **Standard Fortran** (`.f90` files): Modern Fortran with no opinionated changes
- ***lowercase fortran*** (`.f` files): Compiler frontend with automatic type inference, no boilerplate, `real(8)` defaults
- **Notebook Mode**: Interactive analysis with figure capture (`.f` files only)

📦 **Smart Dependencies**
- Local modules: Auto-includes Fortran files from same directory
- Package registry: Resolves external modules to git repositories  
- FPM integration: Leverages existing Fortran ecosystem

🚀 **Advanced Features**
- **Type Inference**: Automatic variable declarations in `.f` files
- **Notebook Mode**: Jupytext-style notebooks with figure capture
- **Incremental Compilation**: Only rebuilds changed files
- **Cache Management**: CLI commands to clear cache and view statistics

## Examples

| Feature | Example | Link |
|---------|---------|------|
| **Hello World** | Simple program | [hello.f90](example/hello/) |
| **Local Modules** | Calculator with math module | [calculator.f90](example/calculator/) |
| ***lowercase fortran* Syntax** | Type inference showcase | [all_types.f](example/type_inference/) |
| **Interdependent Modules** | Complex dependency chain | [main.f90](example/interdependent/) |
| **Notebook Mode** | Interactive analysis | [simple_math.f](example/notebook/) |
| **Plotting** | Figure generation | [plotting_demo.f](example/notebook/) |

## Installation

```bash
git clone https://github.com/krystophny/fortran
cd fortran
./install.sh
```

## Documentation

- 📋 **[ROADMAP.md](ROADMAP.md)** - Development phases and future plans
- 📝 **[TODO.md](TODO.md)** - Current development tasks and progress
- 🏗️ **[CLAUDE.md](CLAUDE.md)** - Technical implementation details

## Usage

```bash
# Two execution modes
fortran program.f90           # Standard Fortran mode
fortran script.f              # lowercase fortran mode (compiler frontend)

# Notebook mode (lowercase fortran .f files only)
fortran --notebook analysis.f # Interactive analysis with figure capture

# Options
fortran -v file.f90           # Verbose output
fortran --cache-dir DIR file  # Custom cache directory
fortran --flag "-O3" file.f90 # Pass custom flags to FPM compiler

# Help
fortran --help                # Show all options
```

## Cache Management

The `fortran` tool uses an intelligent caching system to speed up repeated builds. When developing or testing, you may need to manage the cache:

```bash
# View cache information
fortran --cache-info
# Output:
# Fortran Cache Information:
#   Cache directory: /home/user/.cache/fortran
#   Number of files: 487 files
#   Total size: 3.9M

# Clear entire cache
fortran --clear-cache

# Clear cache and run a file (useful for testing)
fortran --clear-cache example.f90

# Use custom cache directory
fortran --cache-dir /tmp/fortran-cache example.f90
```

**Important for Development**: Always clear the cache when testing new frontend features or compiler flags to ensure you're not using outdated cached files.

## Project Status

**Current**: Production AST Frontend Complete ✅
- ✅ Complete 4-phase compiler architecture (lexer → parser → semantic → codegen)
- ✅ Hindley-Milner type inference system with Algorithm W
- ✅ Smart caching system (2-4x speedup)
- ✅ *lowercase fortran* .f syntax with automatic type inference
- ✅ Notebook mode with figure capture
- ✅ Comprehensive test suite (30+ frontend tests)

**Next**: FPM Registry integration and advanced language features

---

*"Fortran is the Python of scientific computing - it just doesn't know it yet."*
