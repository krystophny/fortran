title: Examples
---

# Examples

Practical examples demonstrating `fortran` tool features. All examples are organized in the [example/](https://github.com/krystophny/fortran/tree/main/example) directory by category:

- **basic/** - Simple getting started examples
- **scientific/** - Scientific computing and visualization
- **modules/** - Module usage and dependencies
- **fortran/** - Lazy fortran dialect features
- **frontend_test_cases/** - Frontend compiler test cases (for development)

## Basic Usage

### Hello World
**Location:** [example/basic/hello/](https://github.com/krystophny/fortran/tree/main/example/basic/hello)

```bash
# Create and run
echo 'print *, "Hello, World!"' > hello.f90
fortran hello.f90
```

Simple program execution - the most basic use case.

### Calculator with Local Modules
**Location:** [example/basic/calculator/](https://github.com/krystophny/fortran/tree/main/example/basic/calculator)

```fortran
! math_module.f90
module math_module
    implicit none
contains
    function add(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function
end module

! calculator.f90  
program calculator
    use math_module
    print *, add(5.0, 3.0)
end program
```

```bash
fortran calculator.f90  # Automatically finds and builds math_module.f90
```

✅ **Status**: Fully working with .f90 files  
⚠️ **Limitation**: .f file preprocessing with modules not yet supported

Demonstrates automatic local module detection and compilation.

## Advanced Features

### Interdependent Modules
**Location:** [example/modules/interdependent/](https://github.com/krystophny/fortran/tree/main/example/modules/interdependent)

Complex module relationships where modules depend on each other:

```fortran
! constants.f90
module constants
    real, parameter :: pi = 3.14159
end module

! geometry.f90
module geometry
    use constants
    ! ... uses pi from constants
end module

! main.f90
program main
    use geometry
    use input_output
    ! ...
end program
```

```bash
fortran main.f90  # Resolves all interdependencies automatically
```

Shows how the tool handles complex dependency graphs.

### Type Inference (.f files)
**Location:** [example/fortran/type_inference/](https://github.com/krystophny/fortran/tree/main/example/fortran/type_inference)

```fortran
! calculate.f (note: .f extension)
x = 5
y = 3.14
result = x * y
print *, result
```

```bash
fortran calculate.f  # Automatically infers types and wraps in program
```

✅ **Status**: Basic type inference working (integer, real(8), character(len=N), logical)  
🔄 **Planned**: Array inference, derived types, function returns

Demonstrates the preprocessor that adds type declarations and program structure.

### Preprocessor Features  
**Location:** [example/fortran/preprocessor/](https://github.com/krystophny/fortran/tree/main/example/fortran/preprocessor)

```fortran
! math.f
function square(x)
    square = x * x
end

! calc.f
y = square(5.0)
print *, y
```

```bash
fortran calc.f  # Automatically wraps functions and adds program structure
```

Shows automatic program wrapping and function handling.

## Specialized Use Cases

### Modern Precision Defaults
**Location:** [example/scientific/precision/](https://github.com/krystophny/fortran/tree/main/example/scientific/precision)

```fortran
! precision_test.f90
program precision_test
    real :: x = 1.0/3.0
    print *, 'Default real precision:', precision(x)
    print *, 'Value:', x
end program
```

```bash
fortran precision_test.f90
# Output shows standard single precision for .f90 files
```

Demonstrates standard Fortran precision behavior for .f90 files.

### Notebook-Style Execution
**Location:** [example/scientific/notebook/](https://github.com/krystophny/fortran/tree/main/example/scientific/notebook)

**Simple mathematical computations:**
```fortran
! simple_math.f (simplified syntax)
x = 10
y = 20  
sum = x + y
print *, 'Sum:', sum
```

**Array operations and loops:**
```fortran
! arrays_loops_simple.f (simplified syntax)
do i = 1, 5
    numbers(i) = i**2
end do
print *, "Array of squares:", numbers
```

**Control flow examples:**
```fortran
! control_flow_simple.f (simplified syntax)
temperature = 25.5
if (temperature < 30.0) then
    print *, "It's mild"
end if
```

**Scientific computing with visualization:**
```fortran
! plotting_demo.f (notebook with multiple cells)
! %% [markdown]
! # Scientific Computing with Fortran

! %%
n_points = 100
do i = 1, n_points
    x_data(i) = (i-1) * 6.28318 / (n_points - 1)
    y_sin(i) = sin(x_data(i))
end do

! Create plots with fortplotlib - auto-converted to base64 PNG in notebook mode
use fortplotlib
call figure()
call plot(x_data, y_sin, 'b-', label='sin(x)')
call xlabel('x')
call ylabel('y')
call title('Sine Wave')
call show()  ! Auto-converted to base64 PNG image
```

```bash
fortran simple_math.f
fortran arrays_loops_simple.f  
fortran control_flow_simple.f
fortran plotting_demo.f
```

⚠️ **Status**: Notebook parsing works, but type inference issues in notebook context  
❌ **Limitations**: Array syntax, function return inference not ready  
🔄 **Planned**: Enhanced inference for notebook use cases

Demonstrates script-like execution for exploratory programming and notebook-style analysis.

### External Dependencies
**Location:** [example/scientific/plotting/](https://github.com/krystophny/fortran/tree/main/example/scientific/plotting)

```fortran
! plot_demo.f90
program plot_demo
    use pyplot_module  ! External dependency
    ! ... plotting code
end program
```

```bash
# First ensure registry is configured
echo '[packages.pyplot-fortran]' >> ~/.config/fortran/registry.toml
echo 'git = "https://github.com/jacobwilliams/pyplot-fortran"' >> ~/.config/fortran/registry.toml

fortran plot_demo.f90  # Downloads and builds pyplot-fortran automatically
```

✅ **Status**: External package integration working  
⚠️ **Limitation**: Phase 8 will add FPM package sharing across programs  
🔄 **Planned**: Enhanced caching for external dependencies

Shows integration with external FPM packages.

### Advanced Type Inference
**Location:** [example/fortran/advanced_inference/](https://github.com/krystophny/fortran/tree/main/example/fortran/advanced_inference)

```fortran
! arrays.f
data = [1, 2, 3, 4, 5]
do i = 1, size(data)
    print *, data(i)**2
end do

! derived_types.f  
type :: point
    real :: x, y
end type
p = point(1.0, 2.0)
print *, p%x, p%y
```

```bash
fortran arrays.f       # Infers array types
fortran derived_types.f # Handles derived types
```

❌ **Status**: Array and derived type inference not yet implemented  
✅ **Working**: .f90 versions demonstrate target capabilities  
🔄 **Planned**: Phase 6 development

Advanced preprocessing with complex type inference.

### Step 1 Type Inference (Explicit Types)
**Location:** [example/fortran/step1_explicit_types/](https://github.com/krystophny/fortran/tree/main/example/fortran/step1_explicit_types)

```fortran
! step1_demo.f - Input with explicit types
result = square(5.0)
print *, "Square of 5.0 is:", result

real function square(x)
  real :: x
  square = x * x
end function
```

```fortran
! step1_demo.f90 - Generated output with opinionated defaults
program main
  implicit none

  real(8) :: result  ! ← Forward type propagation

  result = square(5.0_8)
  print *, "Square of 5.0 is:", result

contains
real(8) function square(x)  ! ← Enhanced signature
  implicit none

  real(8), intent(in) :: x  ! ← Enhanced parameter

  square = x * x
end function
end program main
```

```bash
fortran step1_demo.f   # Applies Step 1 enhancements
fortran step1_demo.f90 # Direct compilation for comparison
```

**Step 1 enhancements:**
- **Function signatures**: `real function` → `real(8) function`
- **Parameter declarations**: `real :: x` → `real(8), intent(in) :: x`  
- **Forward type propagation**: Variables get types from function return types
- **Automatic `intent(in)`**: Applied as opinionated default for parameters

## Regression Testing

**Example File Convention**: Each example directory contains both `.f` and `.f90` file pairs:

- **`.f` files**: Input files that demonstrate preprocessing features
- **`.f90` files**: Expected output files (often copied from generated versions)
- **Purpose**: Regression testing to ensure preprocessing produces consistent results

**Examples:**
```
example/hello/
├── hello.f           # Input: simplified syntax
├── hello.f90         # Expected: full Fortran with program wrapper
└── README.md

example/calculator/
├── calculator.f      # Input: with type inference
├── calculator.f90    # Expected: with explicit declarations
├── math_module.f     # Input: module with simplified syntax
├── math_module.f90   # Expected: full module syntax
└── README.md
```

**Test Coverage**: The `test_examples.f90` test suite runs both versions of each example and validates:
- Both `.f` and `.f90` versions compile and run successfully
- Output consistency between preprocessed and manual versions
- Expected program output matches known good values

This ensures the preprocessor maintains backward compatibility and produces correct transformations.

## Running Examples

**Run all examples:**
```bash
cd example/
for dir in */; do
    echo "Running $dir..."
    cd "$dir"
    fortran *.f90 2>/dev/null || fortran *.f 2>/dev/null || echo "Skipped"
    cd ..
done
```

**Run specific example:**
```bash
cd example/calculator/
fortran calculator.f90
```

**With verbose output:**
```bash
fortran -v example/hello/hello.f90
```

## Example Output

**Hello World:**
```
$ fortran example/hello/hello.f90
Hello, World!
```

**Calculator:**
```
$ fortran example/calculator/calculator.f90  
8.0
```

**Type Inference:**
```
$ fortran example/type_inference/calculate.f
15.7079999
```

**Precision Test:**
```
$ fortran example/precision/precision_test.f90
Default real precision: 15
Value: 0.33333333333333331
```

## Implementation Status Summary

### ✅ **Working Examples**
- **hello/**: Basic program execution
- **precision/**: Modern defaults demonstration  
- **interdependent/**: Complex module dependencies (.f90 files)
- **type_inference/**: Basic type inference (integer, real, character, logical)
- **plotting/**: External FPM package integration

### ⚠️ **Partially Working Examples**  
- **calculator/**: Working with .f90, module preprocessing limitations with .f
- **notebook/**: Parsing works, inference issues in notebook context

### ❌ **Planned Examples**
- **advanced_inference/**: Array and derived type inference (Phase 6)
- **preprocessor/**: Advanced preprocessing features

### 🔄 **Future Enhancements**
- **Phase 8**: FPM package sharing across programs for faster builds
- **Phase 6**: Advanced type inference (arrays, derived types, functions)

## Testing Examples

All examples are automatically tested:
```bash
fpm test test_examples  # Runs all example validations
```

Each example includes:
- Source code
- README.md with explanation  
- Expected output validation
- Error handling tests

## Next Steps

- Explore the [example/](https://github.com/krystophny/fortran/tree/main/example) directory
- Try modifying examples to see how caching works
- Add your own examples following the same structure
- Check the [Manual](index.html) for detailed CLI options
