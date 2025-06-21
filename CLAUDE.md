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