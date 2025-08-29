# CMake Project Template for Fortplot Users

This template demonstrates how to use fortplot in a CMake-based Fortran project. It provides a complete working example with modern CMake practices and comprehensive plotting demonstrations.

## Quick Start

1. **Copy this template** to your project directory:
   ```bash
   cp -r cmake_project_template my_cmake_project
   cd my_cmake_project
   ```

2. **Build the project**:
   ```bash
   cmake -B build
   cmake --build build
   ```

3. **Run the demo**:
   ```bash
   ./build/bin/fortplot_demo
   ```

## What This Template Includes

### Modern CMake Configuration
- **Automatic dependency fetching** using `FetchContent`
- **Proper target linking** with `fortplot::fortplot`
- **Compiler flag management** for debug/release builds
- **Fortran standard compliance** (Fortran 2008+)

### Comprehensive Plotting Demonstration
The template program shows:
1. **Basic plots** - Mathematical functions with legends and styling
2. **Scientific visualization** - Error bars, subplots, and data comparison
3. **Multiple output formats** - PNG, PDF, and ASCII output

### Project Structure
```
cmake_project_template/
├── CMakeLists.txt        # CMake configuration with fortplot integration
├── src/
│   └── main.f90         # Demonstration program with multiple examples
├── build/               # Build directory (created by CMake)
└── README.md            # This file
```

## Customizing for Your Project

### Basic Project Setup

1. **Edit project name** in `CMakeLists.txt`:
   ```cmake
   project(YourProjectName VERSION 1.0.0 LANGUAGES Fortran)
   ```

2. **Replace the demo program** in `src/main.f90`:
   ```fortran
   program your_analysis
       use fortplot
       implicit none
       
       ! Your plotting code here
       call figure()
       ! ... create your plots ...
       call savefig("your_results.png")
   end program your_analysis
   ```

3. **Update executable name**:
   ```cmake
   add_executable(your_program
       src/main.f90
   )
   ```

### Advanced Project Structure

For larger projects with multiple source files:

```cmake
# Multiple source files
add_executable(your_program
    src/main.f90
    src/data_analysis.f90
    src/plotting_utils.f90
)

# Add include directories if needed
target_include_directories(your_program PRIVATE src)
```

### Adding Additional Dependencies

```cmake
# Add other libraries
find_package(BLAS REQUIRED)
target_link_libraries(your_program 
    PRIVATE 
    fortplot::fortplot
    ${BLAS_LIBRARIES}
)
```

## Build Configuration Options

### Debug Build
```bash
cmake -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build
```
- Enables debugging symbols
- Adds runtime checks
- Includes compiler warnings

### Release Build
```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
```
- Optimizes for performance
- Removes debugging overhead
- Production-ready code

### Custom Compiler Flags
```bash
cmake -B build -DCMAKE_Fortran_FLAGS="-march=native -O3"
cmake --build build
```

## Testing Your Setup

The template includes comprehensive tests that verify:
- ✅ **Compilation** - CMake finds and links fortplot correctly
- ✅ **Basic plotting** - Simple line plots with styling
- ✅ **Advanced features** - Error bars, subplots, multiple formats
- ✅ **Output generation** - PNG, PDF, and ASCII file creation

If the template builds and runs without errors, your CMake + fortplot setup is working perfectly.

## Troubleshooting

### CMake Configuration Issues

**Problem**: CMake can't find fortplot
```
Could not resolve fortplot dependency
```
**Solution**: Check internet connection and git access:
```bash
git ls-remote https://github.com/lazy-fortran/fortplot
```

**Problem**: Compiler not found
```
No CMAKE_Fortran_COMPILER could be found
```
**Solution**: Install gfortran:
```bash
# Ubuntu/Debian
sudo apt install gfortran

# macOS
brew install gcc

# Windows (MSYS2)
pacman -S mingw-w64-x86_64-gcc-fortran
```

### Build Issues

**Problem**: Fortran standard errors
**Solution**: Ensure gfortran version 8+ or adjust standard:
```cmake
set(CMAKE_Fortran_STANDARD 2003)  # For older compilers
```

**Problem**: Linking errors
**Solution**: Check that fortplot target is properly linked:
```cmake
target_link_libraries(your_program PRIVATE fortplot::fortplot)
```

### Runtime Issues

**Problem**: Output files not created
**Solution**: Check write permissions and disk space:
```bash
ls -la *.png *.pdf *.txt
```

**Problem**: Segmentation fault
**Solution**: Build with debug flags:
```bash
cmake -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build
gdb ./build/bin/fortplot_demo
```

## Performance Tips

1. **Use Release builds** for production code
2. **Enable native optimization** for your CPU
3. **Profile your code** with gprof if needed
4. **Consider OpenMP** for parallel computations

## Next Steps

1. **Explore the documentation**: `/doc/user_compilation_guide.md`
2. **Study more examples**: `/example/fortran/*/`
3. **Read the API documentation**: `make doc` in the main project
4. **Join the community**: Contribute to the fortplot project

Happy plotting with CMake!