# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**fortplotlib** is a modern Fortran plotting library providing scientific visualization with PNG, PDF, and ASCII backends. The architecture is heavily modularized following SOLID principles, with specialized modules handling specific concerns (text rendering, backends, scaling, etc.).

## API Usage

```fortran
use fortplot
type(figure_t) :: fig

call fig%initialize(width, height)
call fig%add_plot(x, y, label="data")
call fig%add_contour(x_grid, y_grid, z_grid)
call fig%savefig('output.png')  ! Auto-detects backend from extension
```

## Development Commands

All development work must use the Makefile. Never run `fpm` commands directly.

### Primary Development Commands

- `make test` - Run all unit tests (most important for TDD workflow)
- `make example` - Build and run all examples
- `make debug` - Build and run apps in app/ directory for debugging  
- `make coverage` - Generate coverage report (coverage.txt)
- `make build` - Compile the project
- `make clean` - Clean build artifacts and generated plots

### Command Line Arguments Support

All make targets support passing additional fpm arguments. Use `ARGS` to pass extra parameters:

```bash
# Run specific example
make example ARGS="basic_plots"

# Run specific test
make test ARGS="--target test_specific_feature"

# Run specific app for debugging
make debug ARGS="--target debug_feature"

# Build with verbose output
make build ARGS="--verbose"
```

## Architecture Overview

### Core Architecture 
The library follows a layered architecture with clear separation of concerns:

- **`fortplot.f90`** - Top-level public API module that re-exports functionality
- **`fortplot_figure_core.f90`** - Main figure management and plot data structures
- **`fortplot_figure.f90`** - Compatibility wrapper that delegates to specialized modules

### Backend System
Three output backends with polymorphic interfaces:
- **PNG**: Raster graphics via STB libraries (`fortplot_png.f90`, `fortplot_raster.f90`) 
- **PDF**: Vector graphics (`fortplot_pdf.f90`)
- **ASCII**: Terminal display (`fortplot_ascii.f90`)

### Specialized Modules
Following Single Responsibility Principle:
- **Text Rendering**: `fortplot_text.f90`, `fortplot_unicode.f90`, `fortplot_latex_parser.f90`
- **Layout**: `fortplot_layout.f90`, `fortplot_margins.f90`, `fortplot_label_positioning.f90`
- **Scaling**: `fortplot_scales.f90`, `fortplot_ticks.f90`, `fortplot_axes.f90`
- **Visualization**: `fortplot_colormap.f90`, `fortplot_markers.f90`, `fortplot_legend.f90`
- **Advanced**: `fortplot_streamline*.f90` (streamplot functionality), `fortplot_animation.f90`

### File Organization

**Library Sources**: Place library sources in `src/` directory
**Debugging**: Place debugging sources in `app/` directory and execute with `make debug`
**Unit Tests**: Place unit tests in `test/` directory
**Examples**: Place examples in `example/` directory under `example/<language>/<case>` subdirectories

## Third party References and Inspiration

**Matplotlib Source Code**: Located in `thirdparty/matplotlib/` for layout, styling, and API reference
- **Core Reference**: `thirdparty/matplotlib/lib/matplotlib/` contains the main matplotlib implementation
- Use for understanding margin calculations, text positioning, plot layout algorithms
- Reference for color schemes, tick generation, and professional plot styling
- Study layout managers and backend implementations for consistency

**Pyplot-Fortran Wrapper**: Located in `thirdparty/pyplot-fortran/src/pyplot_module.F90`
- **API Inspiration**: Study the Fortran wrapper patterns and interface design
- Reference for clean Fortran API design and Python integration patterns
- Learn from error handling and parameter passing approaches
- Use as inspiration for functional API design (not implementation)

### FPM Automatic Discovery

FPM automatically discovers all sources and resolves module dependencies when run via make:

- **Sources**: All `.f90` files in `src/`, `example/`, `test/`, and `app/` are automatically found
- **Dependencies**: Module dependencies are automatically resolved - no manual specification needed
- **Executables**: Each program in `example/`, `test/`, or `app/` becomes a buildable target

### Finding Available Targets

To discover available executable targets:

```bash
# List example programs
find example/ -name "*.f90" -not -name "CMakeLists.txt"

# List test programs
ls test/

# List debug/app programs
ls app/

# Find available modules in src/
grep -r "^module " src/
```

Example usage with discovered targets:
```bash
# Run specific example
make example ARGS="basic_plots"

# Run specific test
make test ARGS="--target test_figure_basics"

# Run specific debug app
make debug ARGS="--target debug_symlog_data"
```

```fortran
! app/debug_feature.f90 - For debugging/development
program debug_feature
    use fortplot
    ! Your debugging code here
end program
```

## Coding Standards

### SOLID Principles (Adapted for Fortran)
- **Single Responsibility**: Each routine has one purpose, max 30 lines
- **Open/Closed**: Extend through inheritance/composition, not modification  
- **Dependency Inversion**: Depend on abstractions (abstract types), not concrete implementations

### DRY and KISS Principles
- **DRY**: Extract common functionality into shared modules (e.g., `fortplot_margins` for margin calculations)
- **KISS**: Write clear, readable code over "clever" optimizations

### Test-Driven Development
All changes must follow strict TDD:
1. Write failing test first in `test/test_*.f90`
2. Run `make test` to confirm failure (RED)
3. Write minimal code to pass (GREEN)  
4. Refactor while keeping tests green (REFACTOR)

```fortran
! test/test_new_feature.f90
program test_new_feature
    call test_should_calculate_bounds()
contains
    subroutine test_should_calculate_bounds()
        ! Arrange, Act, Assert pattern
    end subroutine
end program
```

**⚠️ CRITICAL: ALWAYS REFERENCE THIRD PARTY SOURCES WHEN IMPLEMENTING FEATURES ⚠️**

When implementing new features or improving existing ones:
1. **Check matplotlib implementation** in `thirdparty/matplotlib/src/lib/matplotlib/` first
2. **Study layout and styling patterns** to ensure consistency with professional plotting standards
3. **Reference pyplot-fortran** in `thirdparty/pyplot-fortran/src/pyplot_module.F90` for clean API design patterns
4. **Test against matplotlib output** using `make ref` to generate reference plots for comparison

### Fortran-Specific Rules
- Always explicitly import with `use only` (no wildcard imports)
- Use `implicit none` in all modules and programs
- Variable declarations MUST come before executable code in routines
- No magic numbers - use named constants with descriptive names
- Max 30 lines per routine, single responsibility
- Never use `cd` in bash commands - use absolute paths instead

### State Management
- No global mutable state - pass all state as parameters
- Prefer immutable data structures and pure functions
- Each piece of state must have clear ownership and scope

### Backend Architecture
- No scattered dispatch logic - backend-specific behavior in backend implementations
- Use polymorphic interfaces to hide backend differences
- Configure backend-specific behavior at initialization

### Boy Scout Rule
**ALWAYS leave the code cleaner than you found it.** Everyone is encouraged to:
- Fix any minor issues, cleanup, or improvements that fit the scope of current task
- Make general fixes that are obviously beneficial (typos, formatting, dead code removal)
- Commit and push these improvements immediately

**Timing Restrictions:**
- **Before/During Development**: Full boy scout rule applies - clean freely
- **During/After Review Phase**: Be very restrictive to avoid breaking things

