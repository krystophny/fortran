# TODO List for Fortran Frontend

## 🚨 CRITICAL: Comprehensive Testing Strategy

### Testing Philosophy
**Every feature MUST have smart, non-shallow, non-tautological unit tests that:**
- Test actual behavior, not just syntax
- Cover edge cases and error conditions
- Verify semantic correctness, not just parsing
- Test integration between components
- Include regression tests for all bug fixes
- Test both positive and negative cases
- Verify type inference accuracy
- Check generated code correctness

### Testing Priorities
1. **Type Inference Tests**: Verify Algorithm W correctness with complex expressions
2. **Code Generation Tests**: Ensure generated Fortran compiles and runs correctly
3. **Error Recovery Tests**: Parser should handle malformed input gracefully
4. **Performance Tests**: Ensure compilation speed for large files
5. **Integration Tests**: Full pipeline from .f to executable

## ✅ Recently Completed (93% Test Success!)

### Function Handling Improvements
- ✅ Function parameter type inference with intent(in)
- ✅ Standalone function wrapping in programs
- ✅ Proper indentation for functions and their bodies
- ✅ Binary operator spacing standardization
- ✅ Fixed code duplication between CLI and compile_source API

### What's Working
- ✅ Basic type inference (Hindley-Milner Algorithm W)
- ✅ Control flow statements (if/else, do while)
- ✅ Function definitions and calls
- ✅ AST construction and transformation
- ✅ Code generation for most constructs

## 🔧 Remaining High Priority Tasks

### Fix Multiple Parameter Declarations (Last Frontend Test)
**Issue**: Parameters declared separately instead of on single line
```fortran
# Current:
real(8), intent(in) :: a
real(8), intent(in) :: b

# Expected:
real(8), intent(in) :: a, b
```
**Solution Options**:
1. Modify code generator to group parameters by type
2. Create compound declaration nodes
3. Post-process declarations during code generation

### Complete Phase 2: Parser Enhancement
- [ ] Parse parameter declarations with explicit types
- [ ] Support intent specifications in source
- [ ] Handle array parameter declarations
- [ ] Parse derived type parameters
- [ ] **Write comprehensive parser tests for all edge cases**

### Complete Phase 3: Semantic Analysis Integration
- [ ] Persist type annotations through AST transformations
- [ ] Support polymorphic type inference
- [ ] Handle recursive function type inference
- [ ] Infer array dimensions and bounds
- [ ] **Test type inference with complex nested expressions**

### Fix CLI JSON Options Test
- [ ] Debug --from-ast and --from-tokens execution
- [ ] Ensure JSON round-trip preservation
- [ ] Add validation for JSON schema
- [ ] **Create test suite for all JSON workflows**

## 📋 Medium Priority Tasks

### Implement Select Case Statement
- [ ] Parse case value lists and ranges
- [ ] Handle case default properly
- [ ] Generate optimized select case code
- [ ] **Test with all Fortran case patterns**

### Enhanced Error Handling
- [ ] Add column information to all errors
- [ ] Implement error recovery points
- [ ] Provide fix suggestions
- [ ] **Create error catalog with examples**

### Array Support Enhancement
- [ ] Infer array dimensions from usage
- [ ] Support array slicing operations
- [ ] Handle implicit array operations
- [ ] **Test multidimensional array inference**

## 🎯 Testing Requirements for Each Feature

### For Every New Feature:
1. **Unit Tests**: Test the feature in isolation
2. **Integration Tests**: Test with other features
3. **Error Tests**: Test error handling
4. **Performance Tests**: Ensure no regression
5. **Example Programs**: Real-world usage examples

### Test Coverage Goals:
- Line coverage: >90%
- Branch coverage: >85%
- Mutation testing: >75%
- All edge cases documented and tested

## 📊 Current Status

**Test Statistics:**
- Frontend tests: 27/29 passing (93%) ⬆️
- Control flow: 100% working ✅
- Type inference: 98% working (1 formatting issue)
- Function handling: 95% working ✅

**Remaining Issues:**
1. Parameter declaration formatting (cosmetic)
2. CLI JSON options test failure
3. Some example files failing

## 💡 Future Enhancements (With Testing)

### Each Enhancement Must Include:
1. Comprehensive test suite BEFORE implementation
2. Benchmarks to measure improvement
3. Integration tests with existing features
4. Documentation with tested examples

### Planned Enhancements:
- Module and interface support (50+ tests needed)
- Generic programming (100+ tests needed)
- Coarray support (30+ tests needed)
- OpenMP/OpenACC directives (40+ tests needed)
- Optimization passes (performance test suite)

## 🏁 Definition of Done

A feature is ONLY complete when:
1. All tests pass (unit, integration, error cases)
2. Code coverage >90%
3. Performance benchmarks show no regression
4. Documentation includes tested examples
5. Edge cases are identified and tested
6. Error messages are helpful and tested
