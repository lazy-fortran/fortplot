# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**fortplotlib** is a modern Fortran plotting library that provides high-quality scientific visualization with multiple output formats. It offers a clean, pyplot-style API for creating line plots and contour plots across PNG, PDF, and ASCII backends.

## Quick Start

```fortran
use fortplot_figure
type(figure_t) :: fig

call fig%initialize(640, 480)
call fig%add_plot(x, y, label="sin(x)")
call fig%add_contour(x_grid, y_grid, z_grid, label="2D function")
call fig%savefig('output.png')  ! Auto-detects backend from extension
call fig%show()                 ! ASCII terminal display
```

## Development Commands

- `make run` - Build and run examples (recommended)
- `make build` - Compile the project
- `make clean` - Clean build artifacts

## Architecture

### Unified Multi-Backend System
- **fortplot_figure.f90**: Main plotting interface with unified architecture
- **fortplot_png.f90**: PNG backend (raster graphics)  
- **fortplot_pdf.f90**: PDF backend (vector graphics)
- **fortplot_ascii.f90**: ASCII backend (terminal display)
- **fortplot_context.f90**: Abstract backend interface

### Core Features
- **Line Plots**: `add_plot(x, y)` for 1D data visualization
- **Contour Plots**: `add_contour(x, y, z)` using marching squares algorithm
- **Mixed Plotting**: Combine different plot types in same figure
- **Auto-Detection**: Backend chosen automatically from file extension (.png, .pdf, .txt)
- **Deferred Rendering**: Same data renders to multiple formats efficiently

### Text Rendering
- **FreeType Integration**: Professional text rendering via C wrapper
- **System Fonts**: Auto-detects fonts on all platforms
- **Proper Spacing**: Uses FreeType advance metrics and kerning

### Dependencies
- **zlib**: PNG compression
- **FreeType**: Text rendering  
- **pkg-config**: Library detection
- **Fortran 2008**: Modern language features

## Usage Patterns

### Basic Plotting
```fortran
call fig%initialize(640, 480)
call fig%set_title("My Plot")
call fig%set_xlabel("x")
call fig%set_ylabel("y")
call fig%add_plot(x, y)
call fig%savefig('plot.png')
```

### Contour Plotting
```fortran
! With default levels
call fig%add_contour(x_grid, y_grid, z_grid)

! With custom levels  
call fig%add_contour(x_grid, y_grid, z_grid, levels=[1.0, 2.0, 3.0])
```

### Multi-Backend Output
```fortran
call fig%savefig('plot.png')  ! PNG backend
call fig%savefig('plot.pdf')  ! PDF backend
call fig%savefig('plot.txt')  ! ASCII backend
call fig%show()               ! Terminal display
```

## Implementation Details

### Contour Algorithm
- **Marching Squares**: Industry-standard algorithm with all 16 cases
- **Edge Interpolation**: Linear interpolation for smooth contour lines
- **Saddle Points**: Proper handling of ambiguous configurations
- **Level Filtering**: Automatic filtering of levels outside data range

### Backend Architecture
- **Polymorphic Design**: Abstract `plot_context` base class
- **Unified Rendering**: Same algorithm works across all backends
- **Coordinate Mapping**: Automatic coordinate system management
- **Memory Safety**: Proper allocation/deallocation patterns

### File Organization
- **app/examples.f90**: Comprehensive examples and demonstrations
- **src/fortplot_*.f90**: Core plotting modules
- **src/freetype_wrapper.c**: C interface for text rendering
- **Makefile**: Build system with automatic dependency detection

## Coding Standards

### Routine Organization and Naming

**Core Principle**: Use descriptive routine names to eliminate the need for explanatory comments. Extract commented code blocks into well-named functions.

#### Routine Size Guidelines
- **Maximum routine length**: 30 lines (excluding declarations)
- **Extract logic blocks**: Any section with explanatory comments should become a separate routine
- **One responsibility**: Each routine should have a single, clear purpose

#### Extraction Strategy
```fortran
! BAD: Long routine with comments
subroutine process_data(data)
    ! Validate input data
    if (size(data) == 0) return
    
    ! Transform coordinates to screen space
    do i = 1, size(data)
        screen_x(i) = (data(i)%x - min_x) / (max_x - min_x) * width
        screen_y(i) = (data(i)%y - min_y) / (max_y - min_y) * height
    end do
    
    ! Apply antialiasing filter
    ! ... complex filtering logic ...
end subroutine

! GOOD: Extracted into focused routines
subroutine process_data(data)
    if (.not. validate_input_data(data)) return
    call transform_coordinates_to_screen(data, screen_coords)
    call apply_antialiasing_filter(screen_coords)
end subroutine
```

#### Routine Placement Rules

1. **Local helper routines**: Place immediately after the calling routine
   ```fortran
   subroutine render_all_plots(self)
       call setup_coordinate_system(self)
       call render_individual_plots(self)
   end subroutine render_all_plots
   
   ! Used only by render_all_plots - place here
   subroutine render_individual_plots(self)
       ! Implementation
   end subroutine render_individual_plots
   ```

2. **Shared utility routines**: Place at end of module before `end module`
   ```fortran
   ! Shared routines used by multiple procedures
   subroutine interpolate_edge_crossings(...)
       ! Implementation 
   end subroutine interpolate_edge_crossings
   
   subroutine apply_marching_squares_lookup(...)
       ! Implementation
   end subroutine apply_marching_squares_lookup
   
   end module fortplot_figure
   ```

#### Naming Conventions

**Function/Subroutine Names**: Use action verbs with descriptive objects
- `calculate_bounding_box()` not `calc_bbox()`
- `interpolate_edge_crossings()` not `interpolate()`
- `validate_input_data()` not `check_data()`
- `transform_coordinates_to_screen()` not `transform()`

**Variable Names**: Use complete words, avoid abbreviations
- `marching_squares_config` not `ms_config`
- `pixel_coordinates` not `px_coords`
- `contour_level` not `level`

#### Comment Policy

**Essential Comments Only**: Keep comments for complex algorithms and interfaces
```fortran
!! Generate default contour levels with boundary margins
!! This routine implements the marching squares algorithm for contour generation
subroutine complex_algorithm()
```

**Remove Obvious Comments**: Let code speak for itself
```fortran
! BAD
x = 5  ! Set x to 5

! GOOD  
initial_grid_size = 5
```

#### Refactoring Examples

**Before**: Long routine with internal comments
```fortran
subroutine trace_contour_level(self, plot_index, level)
    ! Get cell corners and values
    x1 = self%plots(plot_index)%x_grid(i)
    ! ... 10 lines of coordinate extraction
    
    ! Calculate marching squares configuration  
    config = 0
    if (z1 >= level) config = config + 1
    ! ... configuration logic
    
    ! Apply marching squares lookup table
    select case (config)
    ! ... 50 lines of case statements
end subroutine
```

**After**: Clean, focused routines
```fortran
subroutine trace_contour_level(self, plot_index, level)
    do i = 1, nx-1
        do j = 1, ny-1
            call process_contour_cell(self, plot_index, i, j, level)
        end do
    end do
end subroutine trace_contour_level

subroutine process_contour_cell(self, plot_index, i, j, level)
    call get_cell_coordinates(self, plot_index, i, j, x1, y1, x2, y2, x3, y3, x4, y4)
    call get_cell_values(self, plot_index, i, j, z1, z2, z3, z4)
    call calculate_marching_squares_config(z1, z2, z3, z4, level, config)
    call apply_marching_squares_lookup(config, xa, ya, xb, yb, xc, yc, xd, yd, line_points, num_lines)
end subroutine process_contour_cell
```

This approach results in:
- **Self-documenting code**: Function names explain what happens
- **Easier testing**: Small routines can be tested independently  
- **Better maintainability**: Changes isolated to specific functions
- **Cleaner interfaces**: Clear input/output contracts