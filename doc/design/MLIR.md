**Design Document: FIR Text‑Based Workflow with Enzyme AD (Linked Program Execution)**

**1. Overview**

- Frontend emits FIR MLIR text files (.mlir) from the annotated Fortran AST.
- `mlir-opt` pipeline applies optimizations and optional Enzyme AD, producing optimized FIR.
- Lower FIR to CPU (LLVM IR → object) and GPU (SPIR‑V → binary) artifacts.
- Host driver links CPU object code and loads GPU kernels at runtime for dispatch and data exchange.

**2. Goals**

- Maintain text‑first workflow for debugging.
- Support reverse‑mode AD via Enzyme on FIR.
- Produce a single host executable plus GPU shader assets.
- Define clear linking and execution steps for primal and adjoint runs.
- Abstract backend architecture to support multiple code generation targets.
- Enable test-driven development with comprehensive test infrastructure.

**3. Backend Architecture Integration**

The MLIR backend will be integrated through a backend abstraction layer:

```text
AST Arena → Backend Interface → [ Fortran Backend ]
                              → [ MLIR Backend   ]
                              → [ C Backend      ]
```

**Backend Interface Requirements:**
- Polymorphic dispatch based on compilation options
- Shared AST traversal utilities
- Backend-specific emission logic
- Test harness support for validation

**4. High‑Level Pipeline**

```text
1. Frontend:             gen_fir    → module.mlir
2. Optimization & AD:    mlir-opt   → module.opt.mlir    # fusion, enzyme, canonicalize
3. CPU Lowering:         fir-to-llvm → module.ll       # FIR→LLVM IR
                         llc         → module.o        # LLVM IR→object
4. GPU Lowering:         mlir-opt    → module.spv       # FIR→SPIR‑V dialect→.spv
                         spirv-opt   → module.opt.spv   # optimize & validate
5. Linking:              c++ linker  → host_exec       # link module.o + RTL + driver
6. Deployment:           host_exec + module.opt.spv files
```

**5. Emit FIR Text**  

Emit your FIR MLIR in plain text for maximum debug visibility.  

- **FIR Dialect Reference**: See the FIR dialect spec → https://mlir.llvm.org/docs/Dialects/FIR/  
- **Flang Tutorial**: `-emit-mlir` example → https://mlir.llvm.org/docs/FlangTutorial/#emit-mlir  

**Workflow**:  
1. **AST Mapping**: For each procedure in your semantically annotated AST, print a `module { ... }` with FIR ops.  
   - `fir.func` for functions (see `fir.func` docs)  
   - `fir.alloca` and `fir.global` for variables  
   - `scf.for` for loops  
   - `fir.call` for calls  
   - `func.return` for returns  
2. **Autodiff Annotation**: If `-fad` is enabled, emit a separate `grad_` function with `call @__enzyme_autodiff`.  
3. **Example Snippet**:  
```mlir
// module.mlir
module {
  // Main entry
  fir.func @main() attributes {llvm.linkage = #llvm.linkage<"external">} {
    %0 = fir.alloca() : memref<1xi32>
    ...
    func.return
  }

  // Kernel with autodiff
  fir.func @compute(%arg: memref<128xf32>) attributes {llvm.linkage = #llvm.linkage<"external">} {
    scf.for %i = %c0 to %cN step %c1 {
      %v = fir.load %arg[%i] : memref<128xf32>
      %r = arith.mulf %v, %v : f32
      fir.store %r, %arg[%i] : memref<128xf32>
    }
    func.return
  }

  // Gradient entry (Enzyme annotation)
  fir.func @grad_compute(%arg: memref<128xf32>) {
    %g = call @__enzyme_autodiff(%arg) : (memref<128xf32>) -> memref<128xf32>
    func.return %g : memref<128xf32>
  }
}
```

**Debug Tips**:  
- Validate text with `mlir-opt --verify-each`.  
- Round‑trip with `mlir-translate` to catch syntax issues.  

**Proceed to** **6. IR Optimization & AD**

- Run passes:
  - `--passes="canonicalize,fir-fuse,inline,canonicalize"`
  - Optional Enzyme: `--load-dialect-plugin=libEnzymeFIR.so --pass-pipeline="enzyme,canonicalize"`
- Output: `module.opt.mlir` containing both primal and adjoint FIR definitions.

**7. CPU Artifact Generation**

1. **FIR→LLVM IR**
   ```bash
   mlir-opt module.opt.mlir --convert-fir-to-llvm > module.ll
   ```
2. **LLVM IR→Object**
   ```bash
   clang -c module.ll -o module.o \        # or llc+gcc
         `llvm-config --cflags --ldflags --libs` 
   ```
3. **Link**
   ```bash
   clang module.o fortran_rtl.o driver.o -o fortran_app \
         -lLLVM -lmlir_fir -lEnzymeFIR
   ```

**8. GPU Artifact Generation**

1. **FIR→SPIR‑V dialect**
   ```bash
   mlir-opt module.opt.mlir --convert-fir-to-gpu \
           --convert-gpu-to-spv > module.spv
   ```
2. **SPIR‑V Optimize & Validate**
   ```bash
   spirv-opt -O2 module.spv -o module.opt.spv
   spirv-val module.opt.spv
   ```
3. **Package**
   - Ship `module.opt.spv` alongside `fortran_app`, or embed as byte array.

**9. Host Driver Design**

- **Initialization**
  - Parse command‑line flags (`-fad`, `-acc`, GPU device, workgroup sizes).
  - Load Fortran runtime (I/O, coarrays, OpenMP host stubs).
- **Module Registration**
  - **CPU functions**: linked directly into `fortran_app` text segment.
  - **GPU kernels**: at startup, read `module.opt.spv` from disk or memory, call:
    ```c++
    VkShaderModule createShaderModule(VkDevice d, const void* data, size_t size);
    cl_program   createCLProgram(cl_context, size, data);
    ```
- **Data Movement**
  - Host allocates buffers (CPU or GPU) via runtime API: `allocate()`, `omp_target_alloc()`.
  - Copy in data for GPU kernels: `omp_target_memcpy()` or Vulkan buffer uploads.
- **Dispatch**
  - For each kernel (primal or adjoint), set up arguments.
  - Submit dispatch (`vkCmdDispatch` or `clEnqueueNDRangeKernel`).
  - Wait for completion and copy results back if needed.

**10. Adjoint Execution**

- If `-fad` used: application entry point calls `grad_main` instead of `main`.
- PRIMAL run followed by AD run can be sequenced automatically.
- Host driver provides accumulation buffers and hooks for gradient retrieval.

**11. Linked Program Workflow**

```text
# Build and link
./build.sh  # executes steps 1–7 above

# Run primal-only
./fortran_app input.dat

# Run with gradient
./fortran_app -fad input.dat
  → driver executes main() → grad_main()
  → outputs primal results + gradients

# GPU offload
./fortran_app -acc -device 0 input.dat
  → loads module.opt.spv, dispatches kernels
  → optionally calls grad_compute_kernel()
```

**12. Debugging & Validation**

- Inspect each `.mlir` with `mlir-opt --print-ir-after-all`.
- Round‑trip text→bytecode→text to catch serialization bugs:
  ```bash
  mlir-translate --mlir-to-bytecode module.opt.mlir -o m.bc
  mlir-translate --bytecode-to-mlir m.bc      > m2.mlir
  diff module.opt.mlir m2.mlir
  ```
- Use Enzyme's test suite to validate gradients at MLIR level.

**13. Test-Driven Development Strategy**

The MLIR backend implementation will follow a comprehensive test-driven development approach:

**Test Infrastructure Requirements:**
- Unit tests for AST-to-FIR conversion
- Integration tests using `mlir-opt` and `mlir-translate`
- Round-trip validation tests (AST → FIR → LLVM IR → execution)
- Regression tests comparing outputs between backends
- Performance benchmarks for code generation

**Test Tools Integration:**
- `mlir-opt --verify-each` for MLIR validation
- `mlir-translate` for format conversion testing
- `llc` and `clang` for compilation pipeline testing
- Custom test harness for backend comparison

**Test Categories:**
1. **Syntax Tests**: Verify correct FIR dialect generation
2. **Semantic Tests**: Ensure behavior preservation across backends
3. **Optimization Tests**: Validate AD and optimization passes
4. **Integration Tests**: End-to-end compilation and execution
5. **Error Handling Tests**: Proper error reporting and recovery

**14. Future Extensions**

- Automatic pipeline script (`build.sh`) to orchestrate IR transforms.
- Custom `fir-opt` plugins for domain‑specific tiling/fusion.
- Embedding SPIR‑V blobs into the host binary for a single artifact.

---

*End of Updated Design Document*

