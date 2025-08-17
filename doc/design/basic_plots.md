# Basic Plots Implementation Design

## Overview

This document describes the matplotlib-compatible basic plotting implementation in fortplot, covering line plots, scatter plots, and bar charts. The implementation follows matplotlib's API and behavior exactly to ensure drop-in compatibility and familiar user experience.

## Problem Statement

The current basic plotting implementation has several limitations:
1. **Limited format string support** - Only basic line styles and markers
2. **No scatter plot functionality** - Missing dedicated scatter implementation
3. **No bar plot support** - No bar chart capabilities
4. **Incomplete color handling** - No color parsing from format strings
5. **Missing line style variants** - No dotted (:) or dash-dot (-.) styles
6. **Single dataset limitation** - Cannot handle `plot(x1, y1, x2, y2)` syntax

## Solution: Matplotlib-Compatible Implementation

### Architecture

The solution consists of four main components:

1. **Enhanced format string parser** - Complete matplotlib-compatible parsing
2. **Scatter plot implementation** - Dedicated scatter functionality
3. **Bar plot implementation** - Bar chart support with alignment options
4. **Multi-dataset plotting** - Support for multiple data series in one call

### Key Components

#### 1. Format String Processing

Following matplotlib's `_process_plot_format()` in `axes/_base.py` (lines 3134-3200):

```fortran
type :: plot_format_t
    character(len=20) :: linestyle = '-'     ! '-', '--', '-.', ':', 'None'
    character(len=20) :: marker = 'None'     ! 'o', 's', '^', etc.
    character(len=20) :: color = 'auto'      ! 'r', 'g', 'b', 'C0'-'C9', etc.
end type

subroutine parse_format_string(fmt_str, format_spec)
    ! Implements matplotlib's exact parsing logic:
    ! 1. Check if entire string is color specification
    ! 2. Parse two-character line styles first ('--', '-.')
    ! 3. Parse single-character elements (markers, colors, '-', ':')
    ! 4. Apply matplotlib defaults (linestyle='-' if no marker specified)
end subroutine
```

**Matplotlib Format String Examples:**
- `'ko'` → black circles (color + marker)
- `'r--'` → red dashed line (color + linestyle)
- `'C2-.'` → third color cycle with dash-dot line
- `'o'` → circles only (marker, no line)

#### 2. Line Style Implementation

Following matplotlib's `lineStyles` dictionary in `lines.py`:

```fortran
! Supported line styles matching matplotlib exactly
character(len=*), parameter :: LINESTYLE_SOLID = '-'
character(len=*), parameter :: LINESTYLE_DASHED = '--'
character(len=*), parameter :: LINESTYLE_DOTTED = ':'
character(len=*), parameter :: LINESTYLE_DASHDOT = '-.'
character(len=*), parameter :: LINESTYLE_NONE = 'None'

subroutine draw_line_with_style(backend, x, y, linestyle)
    select case (trim(linestyle))
    case ('-')
        call backend%draw_solid_line(x, y)
    case ('--')
        call backend%draw_dashed_line(x, y, dash_pattern=[5.0, 3.0])
    case (':')
        call backend%draw_dotted_line(x, y, dot_pattern=[1.5, 2.0])
    case ('-.')
        call backend%draw_dash_dot_line(x, y, pattern=[5.0, 2.0, 1.5, 2.0])
    end select
end subroutine
```

#### 3. Marker Support

Following matplotlib's `MarkerStyle.markers` in `markers.py`:

```fortran
type :: marker_spec_t
    character(len=10) :: symbol
    real(wp) :: size = 6.0
    character(len=20) :: color = 'auto'
    character(len=20) :: edgecolor = 'auto'
    real(wp) :: linewidth = 1.0
end type

! Matplotlib-compatible marker symbols
character(len=*), parameter :: SUPPORTED_MARKERS(*) = [ &
    'o  ', 's  ', '^  ', 'v  ', '<  ', '>  ', &  ! basic shapes
    '+  ', 'x  ', '*  ', 'D  ', 'd  ', 'p  ', &  ! symbols
    'h  ', 'H  ', '1  ', '2  ', '3  ', '4  ', &  ! specialized
    '.  ', ',  ', '|  ', '_  '                  ] ! points and lines
```

#### 4. Scatter Plot Implementation

Following matplotlib's `scatter()` in `axes/_axes.py` (lines 4500+):

```fortran
subroutine scatter(fig, x, y, s, c, marker, alpha, linewidths, edgecolors)
    real(wp), intent(in) :: x(:), y(:)
    real(wp), intent(in), optional :: s(:)        ! sizes in points²
    character(*), intent(in), optional :: c(:)     ! colors
    character(*), intent(in), optional :: marker   ! marker symbol
    real(wp), intent(in), optional :: alpha        ! transparency
    real(wp), intent(in), optional :: linewidths(:) ! edge widths
    character(*), intent(in), optional :: edgecolors(:) ! edge colors
    
    ! Size handling (matplotlib default: rcParams['lines.markersize'] ** 2)
    if (present(s)) then
        sizes = s
    else
        sizes = [(36.0, i=1, size(x))]  ! 6.0² default marker size
    end if
    
    ! Render each point with individual size/color
    do i = 1, size(x)
        call draw_marker(fig%backend, x(i), y(i), marker, &
                        sqrt(sizes(i)), colors(i), edges(i))
    end do
end subroutine
```

#### 5. Bar Plot Implementation

Following matplotlib's `bar()` in `axes/_axes.py` (lines 2300+):

```fortran
subroutine bar(fig, x, height, width, bottom, align, color, edgecolor)
    real(wp), intent(in) :: x(:), height(:)
    real(wp), intent(in), optional :: width(:)     ! bar widths (default 0.8)
    real(wp), intent(in), optional :: bottom(:)    ! baseline (default 0)
    character(*), intent(in), optional :: align    ! 'center', 'left', 'right'
    
    ! Alignment calculation (matplotlib default: 'center')
    select case (trim(align_type))
    case ('center')
        x_left = x - width * 0.5
    case ('left') 
        x_left = x
    case ('right')
        x_left = x - width
    end select
    
    ! Draw rectangular bars
    do i = 1, size(x)
        call draw_rectangle(fig%backend, x_left(i), bottom_val(i), &
                           width_val(i), height(i), color_val(i))
    end do
end subroutine
```

### Implementation Details

#### Enhanced Plot Function

Following matplotlib's `plot()` signature and multi-dataset support:

```fortran
subroutine plot(fig, varargin)
    ! Support matplotlib calling patterns:
    ! plot(y)                    - y vs index
    ! plot(x, y)                 - basic plot
    ! plot(x, y, fmt)            - with format string
    ! plot(x1, y1, x2, y2)       - multiple datasets
    ! plot(x1, y1, fmt1, x2, y2, fmt2) - multiple with formats
    
    call parse_plot_arguments(varargin, datasets, formats)
    
    do i = 1, n_datasets
        format_spec = parse_format_string(formats(i))
        
        if (format_spec%marker /= 'None' .and. format_spec%linestyle == 'None') then
            ! Marker-only plot
            call scatter(fig, datasets(i)%x, datasets(i)%y, &
                        marker=format_spec%marker, c=format_spec%color)
        else
            ! Line plot (with optional markers)
            call draw_line_with_style(fig%backend, datasets(i)%x, datasets(i)%y, &
                                    format_spec%linestyle)
            if (format_spec%marker /= 'None') then
                call scatter(fig, datasets(i)%x, datasets(i)%y, &
                            marker=format_spec%marker, c=format_spec%color)
            end if
        end if
    end do
end subroutine
```

#### Color Cycle Implementation

Following matplotlib's color cycle system:

```fortran
! Matplotlib default color cycle (C0-C9)
character(len=*), parameter :: DEFAULT_COLORS(*) = [ &
    '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', &
    '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf'  ]

integer :: current_color_index = 1

function get_next_color() result(color)
    color = DEFAULT_COLORS(current_color_index)
    current_color_index = modulo(current_color_index, size(DEFAULT_COLORS)) + 1
end function
```

## Verification

### Test Cases

1. **Format string parsing** - Verify exact matplotlib behavior for all combinations
2. **Line style rendering** - Compare visual output with matplotlib references
3. **Marker rendering** - Test all supported markers and sizes
4. **Scatter plots** - Verify size and color handling
5. **Bar plots** - Test alignment and positioning
6. **Multi-dataset plots** - Confirm proper color cycling and legend support

### Expected Results

**Before (current implementation):**
- Limited format strings ('o', 'x', '-', '--')
- No scatter or bar plot support
- Basic color handling
- Single dataset per plot call

**After (matplotlib-compatible):**
- Complete format string support matching matplotlib
- Dedicated scatter() and bar() functions
- Full color cycle and named color support
- Multi-dataset plotting with automatic color cycling
- Professional-quality rendering matching matplotlib output

## Performance Considerations

### Computational Complexity

- **Format parsing**: O(format_string_length) - constant for typical usage
- **Line rendering**: O(n_points) per dataset
- **Marker rendering**: O(n_points) with backend-optimized drawing
- **Multi-dataset**: O(total_points) across all datasets

### Memory Usage

- **Plot data**: n_datasets × n_points × 2 floats (x, y coordinates)
- **Format specs**: n_datasets × format_spec_t structures
- **Color cycle**: Constant 10-color array
- **Marker cache**: Reusable marker rendering buffers

### Optimization Opportunities

1. **Format caching** - Cache parsed format strings for repeated use
2. **Batch rendering** - Group similar markers/lines for efficient drawing
3. **LOD rendering** - Level-of-detail for large datasets
4. **SIMD operations** - Vectorize coordinate transformations

## Future Enhancements

### Matplotlib Feature Parity

1. **Error bars** - Add errorbar() function with caps and styles
2. **Fill plots** - Implement fill_between() and fill()
3. **Step plots** - Add step() function with different step styles
4. **Log plots** - Enhance with loglog(), semilogx(), semilogy()

### Advanced Features

1. **3D plotting** - Extend to plot3() for 3D line plots
2. **Animation support** - Frame-based plotting for animations
3. **Interactive features** - Hover information and selection
4. **Statistical plots** - Histograms, box plots, violin plots

## References

1. **Matplotlib plot()**: `thirdparty/matplotlib/lib/matplotlib/axes/_axes.py` (lines 1591-1600)
2. **Format parsing**: `thirdparty/matplotlib/lib/matplotlib/axes/_base.py` (lines 3134-3200)
3. **Line2D class**: `thirdparty/matplotlib/lib/matplotlib/lines.py`
4. **Scatter function**: `thirdparty/matplotlib/lib/matplotlib/axes/_axes.py` (lines 4500+)
5. **Bar function**: `thirdparty/matplotlib/lib/matplotlib/axes/_axes.py` (lines 2300+)
6. **pyplot-fortran**: `thirdparty/pyplot-fortran/src/pyplot_module.F90` (plotting patterns)

## Implementation Files

- **`src/fortplot_basic_plots.f90`** - Enhanced plot(), scatter(), bar() functions
- **`src/fortplot_format_parser.f90`** - Complete matplotlib format string parser
- **`src/fortplot_markers.f90`** - Extended marker support and rendering
- **`src/fortplot_linestyles.f90`** - Complete line style implementations
- **`src/fortplot_colors.f90`** - Color cycle and named color support
- **`python/fortplot/fortplot.py`** - Updated Python interface
- **`test/test_basic_plots.f90`** - Comprehensive plotting tests
- **`test/test_format_parser.f90`** - Format string parsing verification

This implementation ensures fortplot provides matplotlib-compatible basic plotting functionality with professional rendering quality and complete API compatibility for seamless migration from matplotlib-based workflows.