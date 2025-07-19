# ROADMAP.md

## Project Vision

**Make Python Fortran again.** The `fortran` CLI tool aims to make Fortran development as seamless as Python, with automatic dependency resolution, transparent compilation, and eventually a simplified syntax that compiles to standard Fortran.

## Completed Features ✅

- **Basic CLI, OS Cache Management, Local Dependencies, FPM Integration**
- **Module Registry with Smart Resolution**
- **Smart Caching System with 2-4x Performance Improvements**
- **Modern Defaults (implicit none, real*8)**
- **Production-Ready AST Frontend with 4-Phase Architecture**
- **Hindley-Milner Type Inference System**
- **Comprehensive Test Suite (30+ frontend tests)**
- **Notebook Support with Figure/Plot Integration**

## Current Phase: Full Type Inference Implementation 🚧

### **Phase 8 Complete: Production AST System** ✅

The AST-based frontend architecture is **complete and production-ready**:

### **Phase 9 In Progress: Full Fortran 95 Type Inference**

Currently implementing complete type inference for all Fortran 95 features:

#### **Existing Infrastructure** ✅
- ✅ **4-Phase Architecture**: Lexer → Parser → Semantic → Codegen
- ✅ **JSON Pipeline**: All phases support JSON input/output
- ✅ **Basic Type Inference**: Simple assignments and expressions
- ✅ **Test Framework**: 30+ tests with wildcard discovery
- ✅ **Debug Flags**: --debug-tokens, --debug-ast, --debug-semantic, --debug-codegen
- ✅ **Pipeline Flags**: --from-tokens, --from-ast, --from-semantic

#### **Key Architectural Decisions** 📐

1. **Unified AST Structure**: The semantic analyzer augments the existing AST with type information rather than creating a new structure. This allows the code generator to work with both:
   - **Typed AST**: From semantic analysis (lowercase fortran with inferred types)
   - **Untyped AST**: From parser (standard Fortran with explicit types)

2. **Unified JSON Serialization**: Both parser and semantic analyzer use the same json_writer/json_reader modules:
   - All AST nodes implement `to_json` method
   - json_reader can deserialize any AST structure
   - Single `--from-ast` flag works for both semantic analyzer and code generator inputs

This architecture eliminates duplication and ensures consistency across the pipeline.

#### **Phase 9 Goals** 🚧
- [ ] **Complete Fortran 95 Coverage**: All language features
- [ ] **Full Type Inference**: Arrays, derived types, procedures
- [ ] **Double Standardization**: Output is idempotent
- [ ] **Comprehensive Tests**: Every Fortran 95 construct
- [ ] **Remove Legacy Code**: Clean up temporary fallbacks

## Modern Fortran Ecosystem Integration 🌐

### FPM Registry Integration
- **Official Registry Sync**: Direct integration with fortran-lang/fpm-registry
- **Module Database**: Extract and index all modules from registered packages
- **Fast Local Resolution**: Build efficient module→package lookup database
- **Automatic Updates**: Periodic sync with official registry

### Module Resolution Architecture
1. **Registry Crawler**: Fetch package metadata from FPM registry
2. **Module Extractor**: Parse packages to build module inventory
3. **Local Database**: SQLite/binary format for sub-millisecond lookups
4. **Smart Caching**: Incremental updates, not full re-scans
5. **Fallback Strategy**: Current TOML registry for custom/private packages

### Performance Targets
- **Module Resolution**: < 1ms for module→package lookup
- **Database Size**: < 10MB for entire FPM ecosystem
- **Update Time**: < 30s for incremental registry sync
- **Zero Network Calls**: During normal compilation

### Implementation Plan
- [ ] Design efficient module database schema
- [ ] Build FPM registry crawler and parser
- [ ] Create module extraction tool
- [ ] Implement fast local lookup engine
- [ ] Add registry sync command (`fortran --sync-registry`)
- [ ] Integrate with existing resolution pipeline

## Long-term Vision

### Multi-Standard Support
- **Phase 1**: Fortran 95 target (current)
- **Phase 2**: Fortran 2003 features (OOP, parameterized types)
- **Phase 3**: Fortran 2008+ (coarrays, submodules)
- **Flexibility**: Pluggable code generation backends

### Compiler Integration
- **Direct IR Generation**: Skip source-to-source transformation
- **LLVM Backend**: Generate LLVM IR directly from AST
- **GFortran Integration**: Interface with GCC's intermediate representation
- **Benefits**: Faster compilation, better optimization opportunities

### Advanced Language Features
- **Multiple Dispatch**: Julia-like generic programming
- **Type Classes**: Haskell-inspired abstractions
- **Metaprogramming**: Compile-time code generation
- **Interop**: Seamless C/C++/Python integration

## Implementation Strategy

### Next Priority: Registry Integration & Language Evolution
1. **Official FPM Registry Integration**: Complete module database system
2. **Enhanced Language Features**: Advanced lowercase fortran syntax extensions
3. **Performance Optimization**: Faster compilation and execution
4. **Extended Type System**: More sophisticated inference patterns

### Architecture Principles
- **Modular Design**: Separate lexer, parser, AST, codegen phases
- **Clean Interfaces**: Well-defined boundaries between components
- **Extensibility**: Easy to add new language features
- **Performance**: Fast parsing and code generation
- **Debugging**: Preserve source locations, generate source maps

### Quality Metrics
- All existing tests pass
- All examples work without modification
- Performance equal or better than current preprocessor
- Clean, maintainable codebase
- Foundation ready for IR generation

## Success Criteria

### Short-term (AST Implementation) - **COMPLETE** ✅
- ✅ Working lexer with comprehensive tokenization
- ✅ Complete AST node definitions with visitor pattern
- ✅ Full parser for Lowercase Fortran (expressions, statements, functions)
- ✅ Code generator producing valid Fortran 90 with modern defaults
- ✅ All regression tests passing (30+ test files)
- ✅ Advanced type inference with Hindley-Milner system

### Medium-term (Compiler Integration)
- ⬜ Direct LLVM IR generation prototype
- ⬜ GFortran IR interface exploration
- ⬜ Performance benchmarks vs traditional compilation
- ⬜ Source map generation for debugging

### Long-term (Language Evolution)
- ⬜ Multiple dispatch implementation
- ⬜ Advanced type system features
- ⬜ Metaprogramming capabilities
- ⬜ Full Python/Julia-like developer experience

## Development Sequence

- **✅ Complete**: Production AST frontend with Hindley-Milner type inference
- **🎯 Current**: FPM Registry integration & Module database system
- **Following**: Advanced lowercase fortran syntax & Fortran 2003 support
- **Later**: LLVM IR generation & Compiler integration
- **Future**: Multiple dispatch & Advanced language features

The goal remains: **Make Python Fortran again** - combining Python's ease of use with Fortran's performance and numerical computing strengths, while being a first-class citizen in the Modern Fortran ecosystem.
