# Fortran Notebook Examples

This directory contains example Fortran notebook files (`.f`) that demonstrate the jupytext-like features of the `fortran` command.

## Running Notebooks

To execute a notebook and generate markdown output:

```bash
fortran --notebook simple_math.f -o simple_math_output.md
```

## Examples

### simple_math.f
Basic mathematical operations with output capture.
- ⚠️ **Current Status**: Variable 'cube' and 'square' not properly inferred from function calls
- **Issue**: Function return type inference not yet implemented

### arrays_loops.f  
Array operations, loops, and reductions.
- ❌ **Current Status**: Array syntax `[1, 2, 3, 4, 5]` not supported
- **Issue**: Array literal inference not yet implemented

### control_flow.f
Control flow constructs including conditionals, loops, and case statements.
- ❌ **Current Status**: Complex control flow patterns cause inference failures
- **Issue**: Advanced pattern recognition not ready

### plotting_demo.f
Integration with fortplotlib to embed plots as inline images.
- 🔄 **Current Status**: Requires fortplotlib integration testing
- **Issue**: External dependency and base64 PNG generation needs verification

## Current Implementation Status

- ✅ **Basic notebook parsing**: Markdown and code cell separation working
- ✅ **Output capture**: Print statements captured correctly
- ❌ **Type inference**: Variable inference failures in notebook context
- 🔄 **Plot embedding**: fortplotlib integration needs testing
- 🔄 **Array operations**: Array syntax not supported yet
- 🔄 **Function calls**: Return type inference not implemented

## Notebook Syntax

- `! %% [markdown]` - Start a markdown cell
- `! %%` - Start a new code cell
- All code without markers is treated as part of the current cell
- Print statements are captured and displayed below each code cell
- `show()` calls from fortplotlib are converted to inline PNG images

## Output Format

The generated markdown includes:
- Formatted markdown cells
- Code cells with syntax highlighting
- Captured output from print statements
- Inline base64-encoded PNG images from plots
