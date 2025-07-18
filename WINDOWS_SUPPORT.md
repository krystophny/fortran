# Windows Support for Fortran Tool

This document describes the Windows compatibility improvements made to fix issue #8.

## Problem Summary

The original issue was that the `fortran` tool failed to run on Windows with the error:
```
The system cannot find the path specified.
A subdirectory or file C:\Users\Vivek\AppData\Local/fortran/cache already exists.
Error occurred while processing: C:\Users\Vivek\AppData\Local/fortran/cache.
Error: Failed to create cache directory
```

## Root Causes

1. **Mixed path separators**: The code was mixing forward slashes (`/`) and backslashes (`\`) in Windows paths
2. **POSIX-only shell commands**: Commands like `mkdir -p`, `cp`, `rm -rf`, `chmod +x` don't work on Windows
3. **Hard-coded Unix path construction**: String concatenation instead of proper cross-platform path handling

## Solutions Implemented

### 1. Cross-Platform Path Handling

**Before:**
```fortran
cache_dir = trim(cache_dir) // '/fortran'
modules_dir = trim(modules_dir) // '/' // trim(cache_key)
```

**After:**
```fortran
cache_dir = join_path(trim(cache_dir), 'fortran')
modules_dir = join_path(modules_dir, cache_key)
```

### 2. Cross-Platform Shell Commands

**Before:**
```fortran
command = 'mkdir -p "' // trim(cache_dir) // '"'
call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
```

**After:**
```fortran
call mkdir(cache_dir)  ! Uses FPM's cross-platform mkdir
success = exists(cache_dir)
```

### 3. Platform-Specific File Operations

**Before:**
```fortran
command = 'cp "' // trim(src) // '" "' // trim(dst) // '"'
call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat)
```

**After:**
```fortran
if (get_os_type() == OS_WINDOWS) then
  command = 'copy "' // trim(src) // '" "' // trim(dst) // '" >nul 2>&1'
else
  command = 'cp "' // trim(src) // '" "' // trim(dst) // '"'
end if
call run(command, exitstat=exitstat)
```

### 4. Cross-Platform Directory Operations

| Operation | Unix Command | Windows Command |
|-----------|-------------|-----------------|
| Create directory | `mkdir -p "dir"` | `cmd /C if not exist "dir" mkdir "dir"` |
| Copy file | `cp "src" "dst"` | `copy "src" "dst" >nul 2>&1` |
| Copy directory | `cp -r "src/*" "dst/"` | `xcopy /E /I /Y "src\*" "dst" >nul 2>&1` |
| Remove directory | `rm -rf "dir"` | `rmdir /S /Q "dir" >nul 2>&1` |
| Make executable | `chmod +x "file"` | *(not needed on Windows)* |

### 5. Improved Filename Extraction

**Before:**
```fortran
last_slash = index(filepath, '/', back=.true.)
if (last_slash > 0) then
  filename = filepath(last_slash+1:)
```

**After:**
```fortran
last_slash = index(filepath, '/', back=.true.)
last_backslash = index(filepath, '\', back=.true.)
last_separator = max(last_slash, last_backslash)
if (last_separator > 0) then
  filename = filepath(last_separator+1:)
```

## Key Libraries Used

- **`fpm_filesystem`**: Cross-platform file operations (`mkdir`, `join_path`, `exists`, `run`)
- **`fpm_environment`**: OS detection (`get_os_type`, `OS_WINDOWS`)

## Files Modified

1. **`src/cache/cache.f90`** - Main cache system with Windows compatibility
2. **`.github/workflows/windows.yml`** - Windows CI/CD pipeline
3. **`test/cache/test_windows_cache.f90`** - Windows-specific tests

## Testing

The Windows CI/CD pipeline tests:
- ✅ Cache directory creation with Windows paths
- ✅ Cross-platform file operations
- ✅ Module resolution and caching
- ✅ Example program execution
- ✅ Performance testing

## Build Instructions

### Windows with MinGW
```bash
# Install MinGW
choco install mingw

# Build
fpm build --compiler gfortran

# Run
.\build\gfortran_*\app\fortran.exe example.f
```

### Windows with Intel Fortran
```bash
# Setup Intel oneAPI (requires separate installation)
# Then build
fpm build --compiler ifort
```

## Cache Directory Locations

| Platform | Default Cache Location |
|----------|------------------------|
| Linux | `~/.cache/fortran/` |
| macOS | `~/.cache/fortran/` |
| Windows | `%LOCALAPPDATA%\fortran\cache\` |

## Backwards Compatibility

All changes maintain full backwards compatibility with Unix-like systems while adding Windows support. The code automatically detects the platform and uses appropriate commands.

## Future Improvements

1. Consider using pure Fortran file I/O instead of shell commands for better portability
2. Add support for Windows-specific compiler flags
3. Implement Windows-specific error handling and logging
4. Add support for Windows package managers (vcpkg, Conan)
