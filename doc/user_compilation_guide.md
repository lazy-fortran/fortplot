title: User Compilation Guide
---

# User Compilation Guide

This guide helps you compile your own Fortran programs that use the fortplot library. Whether you're writing scientific applications, data visualization tools, or learning projects, this guide covers all compilation scenarios.

## Quick Start for Users

### Option 1: FPM Project (Recommended)

**Step 1**: Create your project structure:
```bash
mkdir my_plotting_app
cd my_plotting_app
```

**Step 2**: Create `fpm.toml`:
```toml
name = "my_plotting_app"
version = "0.1.0"

[[dependencies]]
fortplot = { git = "https://github.com/lazy-fortran/fortplot" }
```

**Step 3**: Create your program in `app/main.f90`:
```fortran
program main
    use fortplot
    implicit none
    
    real, dimension(100) :: x, y
    integer :: i
    
    ! Generate sample data
    do i = 1, 100
        x(i) = real(i-1) * 0.1
        y(i) = sin(x(i))
    end do
    
    ! Create plot
    call figure()
    call plot(x, y, label="sin(x)")
    call title("My First Plot")
    call xlabel("x")
    call ylabel("y")
    call legend()
    call savefig("my_plot.png")
    
    print *, "Plot saved as my_plot.png"
end program main
```

**Step 4**: Build and run:
```bash
fpm build
fmp run
```

### Option 2: CMake Project

**Step 1**: Create your project structure:
```bash
mkdir my_plotting_app
cd my_plotting_app
```

**Step 2**: Create `CMakeLists.txt`:
```cmake
cmake_minimum_required(VERSION 3.20)
project(MyPlottingApp Fortran)

include(FetchContent)

# Fetch fortplot library
FetchContent_Declare(
    fortplot
    GIT_REPOSITORY https://github.com/lazy-fortran/fortplot
    GIT_TAG        main
)
FetchContent_MakeAvailable(fortplot)

# Create your executable
add_executable(my_app main.f90)
target_link_libraries(my_app fortplot::fortplot)
```

**Step 3**: Create your program in `main.f90` (same as above)

**Step 4**: Build and run:
```bash
cmake -B build
cmake --build build
./build/my_app
```

### Option 3: Manual Compilation (Advanced Users)

**Step 1**: Clone and build fortplot library:
```bash
git clone https://github.com/lazy-fortran/fortplot
cd fortplot
make build
```

**Step 2**: Find library location:
```bash
# Find module files
find build -name "fortplot.mod" -type f
# Find library file
find build -name "libfortplot.a" -type f
```

**Step 3**: Compile your program:
```bash
gfortran -I /path/to/fortplot/build/gfortran_*/fortplot/*/src \
         -o my_app my_program.f90 \
         /path/to/fortplot/build/gfortran_*/fortplot/*/src/libfortplot.a \
         -lm
```

## Common User Scenarios

### Scientific Data Visualization

```fortran
program scientific_plot
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    integer, parameter :: n = 1000
    real(wp), dimension(n) :: time, voltage, current
    real(wp), dimension(n) :: voltage_err, current_err
    integer :: i
    
    ! Generate experimental data with errors
    do i = 1, n
        time(i) = (i-1) * 0.01_wp  ! 10 ms time steps
        voltage(i) = 5.0_wp * sin(2.0_wp * 3.14159_wp * time(i)) + &
                     0.2_wp * (rand() - 0.5_wp)  ! Noisy sine wave
        current(i) = voltage(i) / 1000.0_wp  ! Simple Ohm's law
        voltage_err(i) = 0.1_wp
        current_err(i) = 0.0001_wp
    end do
    
    ! Create publication-quality plot
    call figure(800, 600)
    call subplot(2, 1, 1)
    call errorbar(time, voltage, yerr=voltage_err, &
                  label="Measured Voltage", capsize=2.0_wp)
    call xlabel("Time (s)")
    call ylabel("Voltage (V)")
    call title("Experimental Results")
    call legend()
    call grid(.true.)
    
    call subplot(2, 1, 2)
    call errorbar(time, current, yerr=current_err, &
                  label="Calculated Current", capsize=2.0_wp, &
                  color="red")
    call xlabel("Time (s)")
    call ylabel("Current (A)")
    call legend()
    call grid(.true.)
    
    call savefig("scientific_results.pdf")  ! Vector graphics for publication
    call savefig("scientific_results.png")  ! Raster for presentations
    
    print *, "Scientific plots generated successfully!"
end program scientific_plot
```

### Engineering Analysis

```fortran
program engineering_analysis
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    integer, parameter :: nx = 50, ny = 50
    real(wp), dimension(nx) :: x
    real(wp), dimension(ny) :: y
    real(wp), dimension(nx, ny) :: temperature, pressure
    integer :: i, j
    
    ! Create coordinate grids
    do i = 1, nx
        x(i) = (i-1) * 0.1_wp  ! 0 to 4.9
    end do
    
    do j = 1, ny
        y(j) = (j-1) * 0.1_wp  ! 0 to 4.9
    end do
    
    ! Generate field data (e.g., heat distribution)
    do i = 1, nx
        do j = 1, ny
            ! Temperature field with hot spot in center
            temperature(i,j) = 100.0_wp * exp(-((x(i)-2.5_wp)**2 + (y(j)-2.5_wp)**2))
            ! Pressure field
            pressure(i,j) = 50.0_wp + 10.0_wp * sin(x(i)) * cos(y(j))
        end do
    end do
    
    ! Create field visualization
    call figure(1200, 500)
    
    call subplot(1, 2, 1)
    call contour_filled(x, y, temperature, colormap="plasma", show_colorbar=.true.)
    call title("Temperature Distribution (°C)")
    call xlabel("X Position (m)")
    call ylabel("Y Position (m)")
    
    call subplot(1, 2, 2)
    call pcolormesh(x, y, pressure, colormap="coolwarm", show_colorbar=.true.)
    call title("Pressure Field (Pa)")
    call xlabel("X Position (m)")
    call ylabel("Y Position (m)")
    
    call savefig("engineering_analysis.png")
    
    print *, "Engineering analysis plots complete!"
end program engineering_analysis
```

### Educational Examples

```fortran
program math_functions
    use fortplot
    implicit none
    
    integer, parameter :: n = 200
    real, dimension(n) :: x, sin_x, cos_x, tan_x, exp_x
    integer :: i
    
    ! Generate mathematical functions
    do i = 1, n
        x(i) = (i-1) * 0.05 - 5.0  ! Range: -5 to +5
        sin_x(i) = sin(x(i))
        cos_x(i) = cos(x(i))
        if (abs(x(i)) < 1.5) then  ! Avoid tan() singularities
            tan_x(i) = tan(x(i))
        else
            tan_x(i) = 0.0  ! Set to zero outside valid range
        end if
        exp_x(i) = exp(x(i))
    end do
    
    ! Create educational plot
    call figure(800, 600)
    call plot(x, sin_x, label="sin(x)", linestyle="b-", linewidth=2.0)
    call plot(x, cos_x, label="cos(x)", linestyle="r--", linewidth=2.0)
    call plot(x, tan_x, label="tan(x)", linestyle="g:", linewidth=2.0)
    
    call title("Mathematical Functions")
    call xlabel("x")
    call ylabel("f(x)")
    call xlim(-5.0, 5.0)
    call ylim(-2.0, 2.0)
    call legend()
    call grid(.true.)
    
    ! Save in multiple formats for different uses
    call savefig("math_functions.png")     ! For web/presentations
    call savefig("math_functions.pdf")     ! For printing
    call savefig("math_functions.txt")     ! For terminal display
    
    print *, "Educational plots created in PNG, PDF, and ASCII formats!"
end program math_functions
```

## Troubleshooting Common Issues

### Problem: Module not found
```
Error: Can't open module file 'fortplot.mod' for reading
```

**Solution**: Ensure the module path is correctly specified:
- **FPM**: Check your `fpm.toml` dependencies
- **CMake**: Verify `target_link_libraries` is correct
- **Manual**: Use `-I` flag pointing to the correct build directory

### Problem: Library not found at linking
```
Error: undefined reference to 'figure_'
```

**Solution**: Ensure the library is linked:
- **FPM**: Dependencies should link automatically
- **CMake**: Check `fortplot::fortplot` target is linked
- **Manual**: Include `-L/path/to/lib -lfortplot` and `-lm`

### Problem: Runtime errors
```
Segmentation fault (core dumped)
```

**Common causes and solutions**:
1. **Array bounds**: Ensure your data arrays are properly sized
2. **Memory allocation**: Use allocatable arrays for large datasets
3. **File permissions**: Ensure write permissions for output files

### Problem: Build fails with FPM
```
Error: Could not resolve dependency: fortplot
```

**Solution**: Check your internet connection and git access:
```bash
# Test git access
git ls-remote https://github.com/lazy-fortran/fortplot

# Alternative: Use specific commit hash
[[dependencies]]
fortplot = { git = "https://github.com/lazy-fortran/fortplot", tag = "main" }
```

## Advanced Usage Tips

### Optimizing Compilation Speed

**For development** (faster compilation):
```bash
fpm build --profile debug
```

**For production** (optimized code):
```bash
fpm build --profile release
```

### Memory Management for Large Datasets

```fortran
program large_dataset
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    integer, parameter :: n = 1000000  ! 1 million points
    real(wp), allocatable :: x(:), y(:)
    integer :: i
    
    ! Allocate arrays
    allocate(x(n), y(n))
    
    ! Generate data
    do i = 1, n
        x(i) = real(i-1, wp) / 1000.0_wp
        y(i) = sin(x(i)) + 0.1_wp * sin(10.0_wp * x(i))
    end do
    
    ! Create plot (automatically handles large datasets)
    call figure(1200, 800)
    call plot(x, y, alpha=0.7)  ! Use transparency for dense data
    call title("Large Dataset Visualization")
    call savefig("large_data.png")
    
    ! Arrays are automatically deallocated
    print *, "Large dataset plot completed!"
end program large_dataset
```

### Cross-Platform Considerations

**Linux/macOS**:
```bash
# Standard compilation
make build
```

**Windows with MinGW**:
```bash
# Ensure proper compiler flags
fpm build --flag "-static-libgcc -static-libgfortran"
```

**Windows with MSYS2**:
```bash
pacman -S mingw-w64-x86_64-gcc-fortran
fpm build
```

## Integration with Other Tools

### Using with Make-based Projects

Create a `Makefile`:
```makefile
FC = gfortran
FCFLAGS = -O2
FORTPLOT_DIR = /path/to/fortplot
FORTPLOT_MOD = $(FORTPLOT_DIR)/build/gfortran_*/fortplot/*/src
FORTPLOT_LIB = $(FORTPLOT_MOD)/libfortplot.a

my_app: main.f90
	$(FC) $(FCFLAGS) -I $(FORTPLOT_MOD) -o $@ $< $(FORTPLOT_LIB) -lm

clean:
	rm -f my_app *.mod *.o
```

### Using with Meson Build System

Create `meson.build`:
```meson
project('my_plotting_app', 'fortran')

fortplot = dependency('fortplot', fallback : ['fortplot', 'fortplot_dep'])

executable('my_app', 'main.f90', dependencies : fortplot)
```

## Performance Tips

1. **Batch operations**: Create multiple plots in one program run
2. **Appropriate formats**: Use PDF for vector graphics, PNG for raster
3. **Memory management**: Use allocatable arrays for large datasets
4. **Compilation optimization**: Use release builds for production

## Getting Help

1. **Check examples**: Look in `example/fortran/` directory for working code
2. **Read documentation**: Use `make doc` to generate API documentation
3. **Test compilation**: Use the provided test programs to verify setup
4. **Community support**: Submit issues to the GitHub repository

## Validation Test Program

To verify your compilation setup works correctly, create and run this test:

```fortran
program compilation_test
    use fortplot
    implicit none
    
    real, dimension(10) :: x, y
    integer :: i
    
    print *, "Testing fortplot compilation..."
    
    ! Simple data
    do i = 1, 10
        x(i) = real(i)
        y(i) = real(i)**2
    end do
    
    ! Test basic functionality
    call figure()
    call plot(x, y)
    call title("Compilation Test")
    call savefig("compilation_test.png")
    
    print *, "SUCCESS: fortplot compilation and basic functionality working!"
    print *, "Check compilation_test.png to verify plot generation."
end program compilation_test
```

If this program compiles and runs successfully, your fortplot setup is working correctly!