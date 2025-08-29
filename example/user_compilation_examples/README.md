title: User Compilation Examples
---

# User Compilation Examples

This directory contains complete working examples and templates to help users compile their own Fortran programs with the fortplot library. Whether you're new to Fortran or experienced with other plotting libraries, these examples will get you started quickly.

## Quick Start Guide

### 1. Test Your Setup
```bash
# Test basic compilation and functionality
./scripts/user_compilation_helper.sh --test-compilation

# Check system dependencies
./scripts/user_compilation_helper.sh --check-deps
```

### 2. Try the Basic Example
```bash
cd example/user_compilation_examples
./scripts/user_compilation_helper.sh basic_user_program.f90
./basic_user_program
```

### 3. Choose Your Project Template
- **FPM users**: Copy `fpm_project_template/` to start your project
- **CMake users**: Copy `cmake_project_template/` to start your project
- **Manual compilation**: Use the helper script with your `.f90` files

## Available Examples and Templates

### 📁 `basic_user_program.f90`
**Purpose**: Simple standalone program demonstrating basic fortplot usage
- ✅ Basic line plots with multiple series
- ✅ Legend, grid, axis labels
- ✅ Multiple output formats (PNG, PDF, ASCII)
- ✅ Error checking and user feedback

**How to use**:
```bash
./scripts/user_compilation_helper.sh basic_user_program.f90
./basic_user_program
```

### 📁 `fpm_project_template/`
**Purpose**: Complete FPM project template for fortplot users
- ✅ Modern `fpm.toml` configuration with fortplot dependency
- ✅ Professional project structure (`app/`, `src/`, etc.)
- ✅ Comprehensive plotting demonstrations
- ✅ Detailed documentation and customization guide

**How to use**:
```bash
cp -r fmp_project_template my_project
cd my_project
fpm build && fpm run
```

### 📁 `cmake_project_template/`  
**Purpose**: Complete CMake project template with modern practices
- ✅ `FetchContent` integration for automatic dependency management
- ✅ Cross-platform build configuration
- ✅ Debug/Release build modes
- ✅ Advanced plotting examples with scientific visualizations

**How to use**:
```bash
cp -r cmake_project_template my_cmake_project
cd my_cmake_project
cmake -B build && cmake --build build
./build/bin/fortplot_demo
```

### 🛠️ `scripts/user_compilation_helper.sh`
**Purpose**: Enhanced compilation script with detailed error messages
- ✅ Automatic fortplot detection and path resolution
- ✅ Comprehensive dependency checking
- ✅ Debug mode compilation with extra checking
- ✅ Verbose output for troubleshooting
- ✅ Built-in compilation testing

**How to use**:
```bash
# Basic compilation
./scripts/user_compilation_helper.sh my_program.f90

# Debug compilation with verbose output
./scripts/user_compilation_helper.sh -d -v -o my_debug my_program.f90

# Check dependencies only
./scripts/user_compilation_helper.sh --check-deps
```

## User Workflow Examples

### Scientific Data Analysis
```fortran
program scientific_analysis
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    ! Your data analysis code
    real(wp), dimension(1000) :: time, signal, processed
    
    ! Load or generate your data
    call load_experimental_data(time, signal)
    call process_data(signal, processed)
    
    ! Create publication-quality plots
    call figure(1200, 800)
    call subplot(2, 1, 1)
    call plot(time, signal, label="Raw Data", alpha=0.7_wp)
    call plot(time, processed, label="Processed", linewidth=2.0_wp)
    call legend()
    call title("Scientific Data Analysis")
    
    call subplot(2, 1, 2)
    call plot(time, processed - signal, label="Difference")
    call xlabel("Time (s)")
    call ylabel("Residual")
    call legend()
    
    call savefig("analysis_results.pdf")  ! Vector graphics for publication
end program scientific_analysis
```

### Engineering Simulation Results
```fortran
program engineering_results
    use fortplot
    implicit none
    
    ! Load simulation results
    real, dimension(100,100) :: temperature, pressure
    real, dimension(100) :: x_grid, y_grid
    
    call load_simulation_results(x_grid, y_grid, temperature, pressure)
    
    ! Create field visualizations
    call figure(1600, 600)
    call subplot(1, 2, 1)
    call contour_filled(x_grid, y_grid, temperature, &
                       colormap="plasma", show_colorbar=.true.)
    call title("Temperature Distribution")
    
    call subplot(1, 2, 2)
    call pcolormesh(x_grid, y_grid, pressure, &
                   colormap="coolwarm", show_colorbar=.true.)
    call title("Pressure Field")
    
    call savefig("simulation_results.png")
end program engineering_results
```

### Educational Mathematics
```fortran
program math_education
    use fortplot
    implicit none
    
    integer, parameter :: n = 200
    real, dimension(n) :: x, y_sin, y_cos, y_tan
    integer :: i
    
    ! Generate mathematical functions
    do i = 1, n
        x(i) = (i-1) * 0.1 - 10.0  ! -10 to +10
        y_sin(i) = sin(x(i))
        y_cos(i) = cos(x(i))
        if (abs(cos(x(i))) > 0.1) then  ! Avoid tan singularities
            y_tan(i) = tan(x(i))
        else
            y_tan(i) = 0.0
        end if
    end do
    
    ! Create educational plot
    call figure(1000, 700)
    call plot(x, y_sin, label="sin(x)", linestyle="b-", linewidth=2.0)
    call plot(x, y_cos, label="cos(x)", linestyle="r--", linewidth=2.0)
    call plot(x, y_tan, label="tan(x)", linestyle="g:", linewidth=2.0)
    
    call title("Trigonometric Functions")
    call xlabel("x (radians)")
    call ylabel("f(x)")
    call xlim(-10.0, 10.0)
    call ylim(-2.0, 2.0)
    call legend()
    call grid(.true.)
    
    ! Save for different uses
    call savefig("trig_functions.png")  ! For presentations
    call savefig("trig_functions.pdf")  # For printing
    call savefig("trig_functions.txt")  # For terminal display
    
    print *, "Educational plots created!"
end program math_education
```

## Troubleshooting Guide

### Common Compilation Issues

**❌ Error**: `Can't open module file 'fortplot.mod'`
**✅ Solution**: Use the compilation helper script or ensure `-I` flag points to correct module directory

**❌ Error**: `undefined reference to 'figure_'`
**✅ Solution**: Ensure library is linked with `-lfortplot -lm` or use helper script

**❌ Error**: `Segmentation fault`
**✅ Solution**: Check array bounds, use debug compilation: `./scripts/user_compilation_helper.sh -d yourfile.f90`

### System Setup Issues

**❌ Issue**: No Fortran compiler
**✅ Solution**: Install gfortran:
- Ubuntu/Debian: `sudo apt install gfortran`
- macOS: `brew install gcc` 
- Windows: Install MinGW-w64 or MSYS2

**❌ Issue**: FPM not found
**✅ Solution**: Install FPM: `pip install fpm` or download from GitHub

**❌ Issue**: Git access problems
**✅ Solution**: Check internet connection and git configuration

### Runtime Issues

**❌ Issue**: No output files generated
**✅ Solution**: Check write permissions and disk space

**❌ Issue**: Poor plot quality
**✅ Solution**: Use appropriate output format (PDF for vector graphics, PNG for raster)

**❌ Issue**: Large dataset performance
**✅ Solution**: Use transparency (`alpha=0.5`) and appropriate data sampling

## Integration Examples

### Makefile Integration
```makefile
FC = gfortran
FCFLAGS = -O2
FORTPLOT_SCRIPT = ./scripts/user_compilation_helper.sh

my_program: my_program.f90
	$(FORTPLOT_SCRIPT) $(FCFLAGS) -o $@ $<

debug: my_program.f90
	$(FORTPLOT_SCRIPT) -d -v -o my_program_debug $<

clean:
	rm -f my_program my_program_debug *.png *.pdf *.txt
```

### Batch Processing
```bash
#!/bin/bash
# Process multiple data files

for data_file in data/*.dat; do
    basename=$(basename "$data_file" .dat)
    echo "Processing $basename..."
    
    # Create analysis program for this dataset
    cat > "analyze_${basename}.f90" << EOF
program analyze
    use fortplot
    call load_and_plot("$data_file", "${basename}_plot.png")
end program analyze
EOF
    
    # Compile and run
    ./scripts/user_compilation_helper.sh "analyze_${basename}.f90"
    ./analyze_${basename}
    
    # Clean up
    rm "analyze_${basename}.f90" "analyze_${basename}"
done
```

## Performance Tips

1. **Choose the right output format**:
   - PNG: Raster graphics, good for photos/complex images
   - PDF: Vector graphics, perfect for publications/printing
   - ASCII: Terminal display, debugging, text-only environments

2. **Optimize for large datasets**:
   - Use data sampling for >10,000 points
   - Consider transparency (`alpha`) for dense scatter plots
   - Use appropriate figure sizes

3. **Compilation optimization**:
   - Use release flags (`-O2`, `-O3`) for production
   - Use debug flags (`-g`, `-fcheck=all`) during development
   - Consider parallel compilation for large projects

## Getting Help

1. **Start with the compilation helper**: `./scripts/user_compilation_helper.sh --help`
2. **Check the comprehensive guide**: `/doc/user_compilation_guide.md`
3. **Study working examples**: All templates include detailed comments
4. **Test your setup**: Use `--test-compilation` flag to verify everything works
5. **Join the community**: Submit issues or questions to the GitHub repository

## Contributing

Found a common use case missing? Please contribute:
1. Create your example program
2. Test it thoroughly 
3. Add documentation
4. Submit a pull request

We especially welcome examples from:
- Specific scientific domains
- Industrial applications  
- Educational use cases
- Integration with other tools

Happy plotting! 📈✨