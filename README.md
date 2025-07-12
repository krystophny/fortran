![fortran](media/logo.png)

[![codecov](https://codecov.io/gh/krystophny/fortran/branch/main/graph/badge.svg)](https://codecov.io/gh/krystophny/fortran)
[![Documentation](https://img.shields.io/badge/docs-FORD-blue.svg)](https://krystophny.github.io/fortran/)

**Make Python Fortran again.** - A command-line tool that enables running Fortran programs without manual compilation, automatically resolving dependencies and applying modern defaults.

## Quick Start

```bash
# Run any Fortran program instantly
fortran hello.f90

# Simplified .f files (no boilerplate needed!)
fortran script.f

# Notebook mode with figure capture
fortran --notebook analysis.f

# Verbose mode for debugging
fortran -v myprogram.f90
```

## Simplified .f Syntax Showcase

Write Fortran code with **zero boilerplate** - just the logic you need:

```fortran
! calculate.f - No program/end program needed!
x = 5.0        ! Automatic type inference: real(8) :: x
y = 3.0        ! Automatic type inference: real(8) :: y  
z = sqrt(x**2 + y**2)
print *, "Distance:", z

! Functions work too - automatic contains insertion
distance(a, b) = sqrt(a**2 + b**2)
print *, "Function result:", distance(3.0, 4.0)
```

**Runs as:** `fortran calculate.f`

**Automatically transforms to:**
- ✅ Wrapped in `program` statement
- ✅ `implicit none` enforced  
- ✅ Double precision defaults (`real(8)`)
- ✅ Type declarations automatically generated
- ✅ `contains` section for functions

## Features

🚀 **Zero Configuration**
- No Makefiles, build scripts, or project setup
- Automatic dependency detection and resolution
- Smart caching with 2-4x performance improvements

🎯 **Opinionated Modern Defaults**  
- `implicit none` enforced automatically
- Double precision (`real(8)`) as default
- Modern compiler flags applied

📦 **Smart Dependencies**
- Local modules: Auto-includes `.f90` files from same directory
- Package registry: Resolves external modules to git repositories  
- FPM integration: Leverages existing Fortran ecosystem

🚀 **Advanced Features**
- **Type Inference**: Automatic variable declarations in `.f` files
- **Notebook Mode**: Jupytext-style notebooks with figure capture
- **Incremental Compilation**: Only rebuilds changed files

## Examples

| Feature | Example | Link |
|---------|---------|------|
| **Hello World** | Simple program | [hello.f90](example/hello/) |
| **Local Modules** | Calculator with math module | [calculator.f90](example/calculator/) |
| **Simplified Syntax** | Type inference showcase | [all_types.f](example/type_inference/) |
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
# Basic usage
fortran file.f90              # Run Fortran program
fortran file.f                # Run simplified .f file  

# Options
fortran -v file.f90           # Verbose output
fortran --cache-dir DIR file  # Custom cache directory
fortran --notebook file.f     # Notebook mode with markdown output

# Help
fortran --help                # Show all options
```

## Project Status

**Current**: Phase 8 Complete ✅
- ✅ Basic CLI and dependency resolution
- ✅ Smart caching system (2-4x speedup)
- ✅ Simplified .f syntax with type inference
- ✅ Notebook mode with figure capture
- ✅ Advanced type inference (arrays, functions, derived types)

**Next**: Enhanced syntax features and ecosystem integration

---

*"Fortran is the Python of scientific computing - it just doesn't know it yet."*