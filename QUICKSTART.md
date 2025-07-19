# Getting Started

**Make Python Fortran again** - Run Fortran programs directly without compilation hassles.

## Installation

**Prerequisites**: FPM and a Fortran compiler (gfortran, ifort, etc.)

```bash
# Clone and build
git clone https://github.com/krystophny/fortran
cd fortran
fpm build --profile release

# Add to PATH or copy binary
cp build/gfortran_*/app/fortran /usr/local/bin/
```

## First Run

**Standard Fortran (.f90):**
```bash
# Create hello.f90
cat > hello.f90 << 'EOF'
program hello
    print *, "Hello, World!"
end program hello
EOF

# Run it
fortran hello.f90
```

Output:
```
Hello, World!
```

**Lowercase Fortran (.f with type inference):**
```bash
# Create hello.f
echo 'print *, "Hello, World!"' > hello.f

# Run it - automatically adds program structure
fortran hello.f
```

Output:
```
Hello, World!
```

That's it. No makefiles, no manual compilation. Both .f90 and .f files work seamlessly.

## Basic Examples

**Simple calculation (.f90 - standard):**
```fortran
! calc.f90
program calc
    implicit none
    real :: x = 3.14159
    real :: area = x * x  
    print *, 'Area:', area
end program
```

```bash
fortran calc.f90
# Area: 9.869604
```

**Simple calculation (.f - lowercase fortran):**
```fortran
! calc.f
x = 3.14159
area = x * x  
print *, 'Area:', area
```

```bash
fortran calc.f
# Area: 9.869604
```

The .f version automatically gets program structure and type inference.

**With modules:**
```fortran
! math_utils.f90  
module math_utils
    implicit none
contains
    function square(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x * x
    end function
end module

! main.f90
program main
    use math_utils
    print *, square(5.0)
end program
```

```bash
fortran main.f90
# 25.0
```

The tool automatically finds `math_utils.f90`, compiles it, and links everything.

## Key Features

- **Zero configuration** - No build files needed
- **Smart dependency detection** - Finds local modules automatically  
- **Package registry** - Resolves external dependencies from FPM registry
- **Fast caching** - Recompiles only when source changes
- **Modern defaults** - `implicit none` via fpm.toml, double precision for .f files

## What Makes This Different?

**Existing tools and their complexity:**

*Direct compiler usage (for single files):*
```bash
gfortran myprogram.f90 -o myprogram
./myprogram
```

*Multiple files with dependencies:*
```bash
gfortran -c module1.f90
gfortran -c module2.f90  
gfortran -c main.f90
gfortran -o main main.o module1.o module2.o
./main
```

*Using build systems (CMake):*
```cmake
# CMakeLists.txt
cmake_minimum_required(VERSION 3.10)
project(MyProject Fortran)
add_executable(main main.f90 module1.f90 module2.f90)
```
```bash
mkdir build && cd build
cmake ..
make
./main
```

*Using FPM:*
```toml
# fpm.toml
name = "myproject"
[dependencies]
stdlib = "*"
```
```bash
fpm build
fpm run
```

**With fortran tool - all complexity hidden:**
```bash
fortran main.f90
```

The fortran tool:
- ✅ No build directories cluttering your workspace
- ✅ No manual dependency tracking
- ✅ No build configuration files needed
- ✅ Automatic caching in system directories
- ✅ Python-like simplicity: just run your code

## Common Usage

```bash
# Basic execution
fortran myprogram.f90

# Verbose output (see what's happening)
fortran -v myprogram.f90

# Very verbose (debug mode)  
fortran -vv myprogram.f90

# Custom cache location
fortran --cache-dir /tmp/cache myprogram.f90

# Help
fortran --help
```

## Next Steps

- See [[Manual|Manual]] for complete CLI reference
- Browse [[Examples|Examples]] for more usage patterns
- Check out the [example/](example/) directory for working code

The goal: Make Fortran development as frictionless as Python.
