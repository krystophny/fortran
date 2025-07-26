# Test-Driven Development Backlog: MLIR Backend Implementation

## Overview

This backlog outlines the test-driven development approach for implementing an MLIR backend that abstracts the current code generation architecture. The goal is to create a modular backend system where the current Fortran codegen becomes one backend implementation, and the new MLIR backend becomes another.

## Test-Driven Development Methodology

### TDD Cycle
1. **Red**: Write a failing test that defines the desired functionality
2. **Green**: Write the minimal code to make the test pass
3. **Refactor**: Improve the code while keeping tests passing

### Testing Philosophy
- Write tests before implementation
- Each feature should have comprehensive test coverage
- Tests should validate both syntax and semantics
- Use real MLIR/LLVM tools for validation where possible

## Prerequisites

### Required Tools
Ensure these command-line tools are installed and available:

```bash
# MLIR/LLVM Tools
mlir-opt          # MLIR optimization and transformation tool
mlir-translate    # MLIR format conversion tool
llc               # LLVM static compiler
clang             # C/C++ compiler with LLVM backend
opt               # LLVM optimizer

# Verification Tools
mlir-opt --verify-each    # MLIR verification
spirv-opt                 # SPIR-V optimizer (for GPU targets)
spirv-val                 # SPIR-V validator
```

### Documentation Resources
- **MLIR Documentation**: https://mlir.llvm.org/docs/
- **FIR Dialect**: https://mlir.llvm.org/docs/Dialects/FIR/
- **Flang MLIR Tutorial**: https://mlir.llvm.org/docs/FlangTutorial/
- **LLVM Documentation**: https://llvm.org/docs/
- **Enzyme AD**: https://enzyme.mit.edu/

## Backend Abstraction Design

### Current Architecture Analysis
- **Location**: `src/frontend/frontend.f90`
- **Backend Constants**: `BACKEND_FORTRAN`, `BACKEND_LLVM`, `BACKEND_C`
- **Current Implementation**: Only `BACKEND_FORTRAN` is implemented
- **Integration Point**: `generate_fortran_code()` function

### Target Architecture
```
AST Arena → Backend Interface → [ Fortran Backend ]
                              → [ MLIR Backend   ]
                              → [ C Backend      ]
```

## Epic 1: Backend Abstraction Infrastructure

### Story 1.1: Define Backend Interface
**Priority**: Critical
**Effort**: 3 days

#### Acceptance Criteria
- [ ] Abstract base type `backend_t` defined
- [ ] Polymorphic procedure interface for code generation
- [ ] Error handling strategy defined
- [ ] Documentation for backend implementers

#### Test Cases
```fortran
! Test: Backend interface compilation
! File: test/unit/test_backend_interface.f90
test_backend_interface_compiles()
test_backend_polymorphic_dispatch()
test_backend_error_handling()
```

#### Implementation Tasks
1. Create `src/backend/backend_interface.f90`
2. Define abstract `backend_t` type
3. Specify `generate_code()` abstract procedure
4. Add compilation options parameter passing

### Story 1.2: Fortran Backend Refactoring
**Priority**: Critical
**Effort**: 5 days

#### Acceptance Criteria
- [ ] Current Fortran codegen moved to new backend structure
- [ ] No behavioral changes to existing functionality
- [ ] All existing tests pass
- [ ] Clean separation from frontend

#### Test Cases
```fortran
! Test: Fortran backend regression
! File: test/integration/test_fortran_backend.f90
test_fortran_backend_equivalence()
test_all_ast_nodes_supported()
test_indentation_preserved()
test_error_messages_preserved()
```

#### Implementation Tasks
1. Create `src/backend/fortran/fortran_backend.f90`
2. Move `codegen_core.f90` logic to Fortran backend
3. Implement `backend_t` interface
4. Update frontend to use new interface

### Story 1.3: Backend Factory and Registration
**Priority**: High
**Effort**: 2 days

#### Acceptance Criteria
- [ ] Backend factory for creating backend instances
- [ ] Registration mechanism for new backends
- [ ] Runtime backend selection based on options
- [ ] Clear error messages for unsupported backends

#### Test Cases
```fortran
! Test: Backend factory
! File: test/unit/test_backend_factory.f90
test_create_fortran_backend()
test_create_mlir_backend()
test_unsupported_backend_error()
test_backend_selection_logic()
```

## Epic 2: MLIR Backend Foundation

### Story 2.1: Basic FIR Module Generation
**Priority**: Critical
**Effort**: 5 days

#### Acceptance Criteria
- [ ] Generate valid MLIR module structure
- [ ] Basic FIR function emission
- [ ] MLIR syntax validation with `mlir-opt --verify-each`
- [ ] Round-trip testing (text → bytecode → text)

#### Test Cases
```bash
# Test: Basic MLIR generation
# File: test/mlir/test_basic_generation.sh
test_empty_module_generation() {
    # Input: empty Fortran program
    # Expected: valid empty MLIR module
    fortran_compiler --backend=mlir empty.f90 > empty.mlir
    mlir-opt --verify-each empty.mlir
}

test_simple_function_generation() {
    # Input: simple Fortran function
    # Expected: valid FIR function
    fortran_compiler --backend=mlir simple.f90 > simple.mlir
    mlir-opt --verify-each simple.mlir
    grep "fir.func @simple" simple.mlir
}
```

#### Implementation Tasks
1. Create `src/backend/mlir/mlir_backend.f90`
2. Implement basic module and function emission
3. Add MLIR syntax utilities
4. Create test infrastructure for MLIR validation

### Story 2.2: AST Node Mapping to FIR
**Priority**: Critical
**Effort**: 8 days

#### Acceptance Criteria
- [ ] All AST node types mapped to FIR constructs
- [ ] Variable declarations (`fir.alloca`, `fir.global`)
- [ ] Arithmetic operations (`arith.*` dialect)
- [ ] Control flow (`scf.for`, `scf.if`)
- [ ] Function calls (`fir.call`)

#### Test Cases
```bash
# Test: AST to FIR mapping
# File: test/mlir/test_ast_mapping.sh
test_variable_declaration() {
    echo "program test; integer :: x; end program" | \
    fortran_compiler --backend=mlir > var.mlir
    mlir-opt --verify-each var.mlir
    grep "fir.alloca.*i32" var.mlir
}

test_arithmetic_operations() {
    echo "program test; integer :: x, y, z; z = x + y; end program" | \
    fortran_compiler --backend=mlir > arith.mlir
    grep "arith.addi" arith.mlir
}

test_loop_generation() {
    echo "program test; do i=1,10; end do; end program" | \
    fortran_compiler --backend=mlir > loop.mlir
    grep "scf.for" loop.mlir
}
```

#### Implementation Tasks
1. Implement AST visitor for MLIR emission
2. Create FIR emission utilities for each node type
3. Handle Fortran-specific semantics in FIR
4. Add comprehensive test cases for all constructs

### Story 2.3: Type System Integration
**Priority**: High
**Effort**: 4 days

#### Acceptance Criteria
- [ ] Fortran types mapped to FIR types
- [ ] Array types and memory references
- [ ] Character string handling
- [ ] Type conversion operations

#### Test Cases
```bash
# Test: Type system
# File: test/mlir/test_types.sh
test_integer_types() {
    echo "integer(kind=4) :: x" | \
    fortran_compiler --backend=mlir > types.mlir
    grep "i32" types.mlir
}

test_array_types() {
    echo "real, dimension(10) :: arr" | \
    fortran_compiler --backend=mlir > arrays.mlir
    grep "memref<.*xf32>" arrays.mlir
}
```

## Epic 3: MLIR Pipeline Integration

### Story 3.1: MLIR Optimization Pipeline
**Priority**: High
**Effort**: 3 days

#### Acceptance Criteria
- [ ] Integration with `mlir-opt` optimization passes
- [ ] Configurable optimization levels
- [ ] Pass pipeline validation
- [ ] Performance benchmarking

#### Test Cases
```bash
# Test: Optimization pipeline
# File: test/mlir/test_optimization.sh
test_basic_optimization() {
    fortran_compiler --backend=mlir -O1 input.f90 > input.mlir
    mlir-opt --canonicalize input.mlir > input.opt.mlir
    # Verify optimizations applied
    diff input.mlir input.opt.mlir
}
```

### Story 3.2: LLVM IR Lowering
**Priority**: Critical
**Effort**: 6 days

#### Acceptance Criteria
- [ ] FIR to LLVM IR conversion
- [ ] Executable binary generation
- [ ] Runtime library linking
- [ ] Debugging information preservation

#### Test Cases
```bash
# Test: LLVM IR generation
# File: test/mlir/test_llvm_lowering.sh
test_fir_to_llvm() {
    fortran_compiler --backend=mlir simple.f90 > simple.mlir
    mlir-opt --convert-fir-to-llvm simple.mlir > simple.ll
    llc simple.ll -o simple.o
    clang simple.o -o simple_exec
    ./simple_exec
}
```

### Story 3.3: Enzyme AD Integration
**Priority**: Medium
**Effort**: 8 days

#### Acceptance Criteria
- [ ] Enzyme AD pass integration
- [ ] Gradient function generation
- [ ] AD annotation support
- [ ] Validation against analytical gradients

#### Test Cases
```bash
# Test: Automatic differentiation
# File: test/mlir/test_enzyme_ad.sh
test_simple_gradient() {
    fortran_compiler --backend=mlir --enable-ad gradient.f90 > gradient.mlir
    mlir-opt --load-plugin=libEnzyme.so --enzyme gradient.mlir > gradient.ad.mlir
    grep "__enzyme_autodiff" gradient.ad.mlir
}
```

## Epic 4: Testing Infrastructure

### Story 4.1: MLIR Test Harness
**Priority**: High
**Effort**: 4 days

#### Acceptance Criteria
- [ ] Automated test execution framework
- [ ] MLIR validation integration
- [ ] Test result reporting
- [ ] Continuous integration support

#### Implementation Tasks
1. Create `test/mlir/` directory structure
2. Implement test runner script
3. Add MLIR tool integration
4. Create test case templates

### Story 4.2: Backend Comparison Tests
**Priority**: Medium
**Effort**: 5 days

#### Acceptance Criteria
- [ ] Semantic equivalence validation
- [ ] Performance comparison framework
- [ ] Regression test suite
- [ ] Error message consistency tests

#### Test Cases
```bash
# Test: Backend equivalence
# File: test/integration/test_backend_equivalence.sh
test_semantic_equivalence() {
    # Generate code with both backends
    fortran_compiler --backend=fortran input.f90 > output_fortran.f90
    fortran_compiler --backend=mlir input.f90 > output.mlir
    
    # Compile MLIR to executable
    mlir-opt --convert-fir-to-llvm output.mlir > output.ll
    clang output.ll -o output_mlir
    
    # Compare runtime behavior
    gfortran output_fortran.f90 -o output_fortran
    ./output_fortran > result_fortran.txt
    ./output_mlir > result_mlir.txt
    diff result_fortran.txt result_mlir.txt
}
```

### Story 4.3: Performance Benchmarking
**Priority**: Low
**Effort**: 3 days

#### Acceptance Criteria
- [ ] Compilation time benchmarks
- [ ] Runtime performance comparison
- [ ] Memory usage analysis
- [ ] Optimization effectiveness metrics

## Epic 5: Documentation and Examples

### Story 5.1: Developer Documentation
**Priority**: Medium
**Effort**: 2 days

#### Deliverables
- [ ] Backend implementation guide
- [ ] MLIR debugging techniques
- [ ] Troubleshooting common issues
- [ ] API reference documentation

### Story 5.2: Example Programs
**Priority**: Low
**Effort**: 2 days

#### Deliverables
- [ ] Basic Fortran to FIR examples
- [ ] AD-enabled example programs
- [ ] GPU offload examples
- [ ] Optimization case studies

## Implementation Order

### Phase 1: Foundation (Weeks 1-3)
1. Backend abstraction infrastructure (Epic 1)
2. Basic MLIR generation (Story 2.1)
3. Test harness setup (Story 4.1)

### Phase 2: Core Functionality (Weeks 4-7)
1. Complete AST to FIR mapping (Story 2.2)
2. Type system integration (Story 2.3)
3. LLVM IR lowering (Story 3.2)

### Phase 3: Advanced Features (Weeks 8-10)
1. Optimization pipeline (Story 3.1)
2. Backend comparison tests (Story 4.2)
3. Enzyme AD integration (Story 3.3)

### Phase 4: Polish (Weeks 11-12)
1. Performance benchmarking (Story 4.3)
2. Documentation (Epic 5)
3. Final integration testing

## Success Criteria

### Functional Requirements
- [ ] MLIR backend generates valid FIR from all supported Fortran constructs
- [ ] Generated code compiles to working executables
- [ ] Semantic equivalence with Fortran backend
- [ ] All tests pass with both backends

### Quality Requirements
- [ ] >95% test coverage for new code
- [ ] No performance regression vs current implementation
- [ ] Clear error messages and debugging support
- [ ] Maintainable and extensible architecture

### Integration Requirements
- [ ] Seamless integration with existing build system
- [ ] Compatible with current CLI interface
- [ ] Preserves all existing functionality
- [ ] Easy to add new backends in the future

## Risk Mitigation

### Technical Risks
- **MLIR complexity**: Start with simple examples, build incrementally
- **Tool dependencies**: Verify tool availability in CI environment
- **Performance concerns**: Establish benchmarks early

### Process Risks
- **Scope creep**: Strict adherence to TDD cycle
- **Integration issues**: Frequent integration testing
- **Documentation debt**: Write docs alongside implementation

---

*This backlog should be regularly updated as implementation progresses and new requirements are discovered.*