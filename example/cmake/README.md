# CMake Integration for fortplotlib

This example demonstrates how to use fortplotlib in a CMake project by automatically fetching and building it via FPM.

## Files

- **`cmake/FindFPMPackage.cmake`**: Generic CMake module for integrating any FPM package
- **`CMakeLists.txt`**: Example CMake configuration using fortplotlib
- **`fortplotlib_test.f90`**: Simple test program using fortplotlib
- **`README.md`**: This documentation

## Usage

### Building

```bash
mkdir build
cd build
cmake ..
make
```

### Running

```bash
./fortplotlib_test
```

This will create a `cmake_test_plot.png` file with a simple sine wave plot.

## How it Works

The `FindFPMPackage.cmake` module:

1. **Downloads** the FPM project from a git repository
2. **Builds** it using FPM with the specified flags and dependencies
3. **Discovers** the build output automatically (handles FPM's hash-based directory names)
4. **Creates** symlinks to make libraries and modules accessible to CMake
5. **Provides** a standard CMake target `fortplotlib::fortplotlib`

## Generic Usage

To use this with any FPM package:

```cmake
# Add the module to your CMake module path
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
include(FindFPMPackage)

# Fetch and build any FPM package
find_fpm_package(
    NAME my_package
    GIT_REPOSITORY https://github.com/user/my_package.git
    GIT_TAG main
    PKG_CONFIG_PACKAGES freetype2 zlib  # Optional system dependencies
    C_FLAGS "-DSOME_FLAG"               # Optional C flags  
    LINK_FLAGS "-lsomelib"              # Optional link flags
)

# Use in your target
target_link_libraries(my_program my_package::my_package)
```

## Features

- **Automatic dependency detection** via pkg-config
- **Flexible path discovery** - automatically finds FPM's hash-based build directories
- **Separate library and module detection** - handles cases where they're in different directories
- **Standard CMake interface** - creates proper imported targets
- **System library integration** - passes through pkg-config flags

## Requirements

- CMake 3.20+
- FPM (Fortran Package Manager)
- pkg-config (for system dependencies)
- Git (for fetching repositories)