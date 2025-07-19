# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Project Purpose

This project develops a command-line tool called `fortran` that **Makes Python Fortran again**. The tool enables running Fortran programs directly without manual compilation, automatically resolving and building module dependencies using FPM, with opinionated modern defaults and zero configuration. Our *lowercase fortran* dialect pushes beyond all alternative scientific computing languages to combine Fortran's performance with modern expressiveness.

## Test-Driven Development (CRITICAL)

**ALWAYS follow TDD: Write tests FIRST, then implement!**

1. **RED**: Write a failing test first
2. **GREEN**: Write minimal code to pass the test  
3. **REFACTOR**: Clean up code while keeping tests green

## ⚠️ CRITICAL: OBSOLETE CODE REMOVAL POLICY ⚠️

**ALWAYS REMOVE OBSOLETE AND DEPRECATED CODE - NO EXCEPTIONS!**

- **NEVER** leave deprecated functions, subroutines, or modules in the codebase
- **NEVER** leave commented out sections of code in source files
- **IMMEDIATELY DELETE** any code marked as "DEPRECATED", "OBSOLETE", or "REMOVED"
- **AGGRESSIVELY PURGE** dead code, unused imports, and redundant implementations
- You are safe to delete things that are obsolete if they are under version control
- Don't hamster old messy stuff - clean as you go!

This keeps the codebase clean, reduces maintenance burden, and prevents confusion.

## Development Principles

- We follow TDD, SOLID, KISS, SRP and DRY as strict requirements in the development process.
- Always keep work units small and manageable.
- Always work sequentially. Never do multiple things in parallel.
- Never be lazy. Don't take shortcuts. Fulfill every task fully to 100%

## Build System

This project uses FPM (Fortran Package Manager).

### Essential Commands

```bash
# Build and Run
fpm build                         # Build project
fpm run fortran -- example.f90    # Run main app (IMPORTANT: Use -- separator)
fpm run fortran -- --clear-cache  # Clear cache (CRITICAL before testing frontend!)
fpm clean --skip                  # Clean build directory without prompting

# Testing
fpm test                          # Run all tests
fpm test test_name                # Run specific test
fpm test > /dev/null              # Run tests quietly (recommended during development)

# Cache Management
fpm run fortran -- --clear-cache  # Clear all cached files
fpm run fortran -- --cache-info   # Show cache statistics

# Debug 4-Phase Compilation Pipeline
fpm run fortran -- example.f90 --debug-tokens    # Phase 1: Tokenization (outputs tokens.json)
fpm run fortran -- example.f90 --debug-ast       # Phase 2: AST parsing (outputs ast.json)
fpm run fortran -- example.f90 --debug-semantic  # Phase 3: Type inference (outputs annotated ast.json)
fpm run fortran -- example.f90 --debug-codegen   # Phase 4: Code generation (debug info + Fortran code)

# Start from intermediate JSON representations
fpm run fortran -- --from-tokens tokens.json     # Start from Phase 2 (Parser)
fpm run fortran -- --from-ast ast.json           # Start from Phase 3 (Semantic) or Phase 4 (Codegen)
```

## Project Architecture

Clean organized directory structure:
- `src/` - Library modules
  - `frontend/` - Complete compilation pipeline (lexer → parser → semantic → codegen)
  - `[utilities]/` - cli/, cache/, config/, runner/, notebook/, etc.
- `app/` - Executable programs
- `test/` - Test programs organized to match src/ structure
  - `frontend/` - Frontend tests with wildcard naming (test_frontend_lexer_*.f90, etc.)
- `example/` - Example programs
  - `basic/` - Simple getting started examples
  - `scientific/` - Scientific computing examples
  - `modules/` - Module usage examples
  - `fortran/` - Lowercase fortran dialect features
  - `frontend_test_cases/` - Frontend test cases with JSON input/output pairs

## CRITICAL: No Shortcuts in Frontend ⚠️

**All code MUST go through the AST pipeline**:
- **Lowercase Fortran**: Lexer → Parser → AST → Semantic Analysis → Code Generation
- **Standard Fortran**: Lexer → Parser → AST → Code Generation

- ❌ NO direct token-to-code shortcuts
- ✅ ALL processing through proper AST pipeline
- ✅ Semantic analysis only for type inference (lowercase fortran)

## Current Status

✅ **Production AST Frontend Complete**
- Complete 4-phase compiler architecture with JSON intermediate representations
- Hindley-Milner type inference with Algorithm W
- Comprehensive test suite (30+ frontend tests)

## *Lowercase Fortran* Dialect

Experimental dialect features:
1. **Implicit program wrapping** - No need for `program`/`end program`
2. **Automatic type inference** - Variables declared through assignment
3. **Modern defaults** - `implicit none`, `real(8)`, `intent(in)`
4. **Automatic contains insertion** - For functions/subroutines

## Module Registry System

Maps modules to FPM packages via `~/.config/fortran/registry.toml`:
```toml
[packages.fortplotlib]
git = "https://github.com/krystophny/fortplotlib"
prefix = "fortplot"
```

## Testing Strategy

1. **Write tests FIRST** (TDD: red-green-refactor)
2. **Test components individually** - Direct API tests, not full compiler runs
3. **Use JSON for test data** - Each stage accepts/produces JSON
4. **Start with ONE LINE tests** - Minimal test cases first

## ⚠️ CRITICAL SAFE FORTRAN REQUIREMENTS ⚠️

### Memory Management Rules (MANDATORY)
1. **NO manual `deallocate`** - Let Fortran scope handle deallocation automatically
2. **Use `allocatable`, NEVER `pointer`** - Allocatable provides automatic memory management
3. **NO shared memory ownership** - Each data structure owns its memory exclusively
4. **Implement proper deep copy operations** - Avoid shallow copies that cause double-free
5. **`move-alloc` is forbidden** - Find other safe ways to handle memory allocation

### Container Patterns (REQUIRED)

#### Polymorphic Arrays - Use Wrapper Pattern
```fortran
type :: ast_node_wrapper
    class(ast_node), allocatable :: node
end type ast_node_wrapper
```

#### Array Extension - Use Temporary Variables
```fortran
! CORRECT: Use temporary array for safe extension
type(ast_node_wrapper), allocatable :: temp_array(:)
if (allocated(array)) then
    allocate(temp_array(size(array) + 1))
    temp_array(1:size(array)) = array
    temp_array(size(array) + 1) = new_element
    array = temp_array
else
    array = [new_element]
end if

! WRONG: Direct extension with function calls
! array = [array, function_call()]
```

#### Deep Copy Operations (MANDATORY)
```fortran
! Each type MUST implement deep_copy method
function deep_copy(this) result(copy)
    class(my_type), intent(in) :: this
    type(my_type) :: copy
    ! Copy all fields, deep copy allocatable components
    if (allocated(this%some_field)) then
        copy%some_field = this%some_field%deep_copy()
    end if
end function deep_copy
```

### Forbidden Practices (NEVER ALLOW)
- ❌ Manual `deallocate` calls
- ❌ `pointer` attributes
- ❌ Direct array extension: `array = [array, new_element]` with function calls
- ❌ Shared memory references between data structures
- ❌ Shallow copies of complex types
- ❌ Move semantics with `move_alloc` for complex types

## Test Categories

```bash
# Frontend components
fpm test test_frontend_lexer_*
fpm test test_frontend_parser_*
fpm test test_frontend_semantic_*
fpm test test_frontend_codegen_*

# Other systems
fpm test test_cli_comprehensive
fpm test test_runner_comprehensive
fpm test test_cache
```

## JSON Test Structure

```
example/frontend_test_cases/single_assignment/
├── input.txt                    # Just "x = 1"
├── expected_tokens.json         # Expected tokenizer output
├── expected_ast.json           # Expected parser output
├── expected_ast_typed.json     # Expected AST with type annotations (from semantic analysis)
└── expected_code.f90           # Expected generated Fortran code
```

## ⚠️ CRITICAL FILE CREATION DISCIPLINE ⚠️

**NEVER create files randomly in the working directory!**

### File Creation Rules:
1. **Debug programs** → `app/` directory, run with `fpm run <name>`
2. **Test programs** → `test/` directory, discoverable by `fpm test`
3. **Frontend test data** → `example/frontend_test_cases/` (with input/output pairs)
4. **User examples** → `example/` (organized by category)
5. **Temporary/draft work** → `draft/` directory (gitignored)

### Draft Directory Usage:
- Use `draft/` for messy, experimental work
- Convert to proper categories once established:
  - `draft/` → `test/` (for test cases)
  - `draft/` → `example/` (for examples)
  - `draft/` → `app/` (for debug programs)
- Never commit anything in `draft/` - it's for local work only

### Forbidden Practices:
- ❌ Creating test files in working directory
- ❌ Leaving temporary files after testing
- ❌ Creating random debug files outside proper structure
- ❌ Committing anything from `draft/`

## Critical Notes

- **Clear cache before testing frontend**: `fpm run fortran -- --clear-cache`
- **Run tests quietly**: `fpm test > /dev/null`
- **Fortran 95 standard**: https://wg5-fortran.org/N1151-N1200/N1191.pdf
- **CLI options documented in**: doc/index.md


## Reference Documentation

- Fortran standards: doc/standard/
- Current tasks: TODO.md
- Architecture: doc/design/
- Roadmap: ROADMAP.md
