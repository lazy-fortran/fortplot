# Enhanced Scatter Plot Guide

## Overview

Fortplot's enhanced scatter plot functionality provides powerful visualization for multi-dimensional data through size and color mapping. This guide covers everything from basic scatter plots to advanced bubble charts and scientific data visualization.

## Quick Start

### Basic Scatter Plot
```fortran
use fortplot
real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
real(wp) :: y(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]

call figure()
call scatter(x, y, label='Basic Scatter')
call title('My First Scatter Plot')
call savefig('basic_scatter.png')
```

### Bubble Chart with Size Mapping
```fortran
real(wp) :: sizes(5) = [10.0_wp, 50.0_wp, 100.0_wp, 25.0_wp, 75.0_wp]

type(figure_t) :: fig
call fig%initialize(600, 400)
call fig%scatter(x, y, s=sizes, marker='circle', label='Bubble Chart')
call fig%set_title('Population by City')
call fig%savefig('bubble_chart.pdf')
```

### Color-Mapped Scatter
```fortran
real(wp) :: colors(5) = [0.1_wp, 0.3_wp, 0.7_wp, 0.9_wp, 0.5_wp]

call figure(800, 600)
call scatter(x, y, c=colors, colormap='viridis', label='Color Mapped')
call title('Temperature vs Pressure')
call xlabel('Temperature (K)')
call ylabel('Pressure (Pa)')
call savefig('color_scatter.png')
```

## API Reference

### scatter() Method

#### Stateful API
```fortran
call scatter(x, y [, optional parameters])
```

#### Object-Oriented API
```fortran
call fig%scatter(x, y [, optional parameters])
```

### Parameters

| Parameter | Type | Description | Example |
|-----------|------|-------------|---------|
| `x, y` | `real(wp)(:)` | Position data arrays (required) | `[1.0, 2.0, 3.0]` |
| `s` | `real(wp)(:)` | Size mapping array | `[10.0, 50.0, 100.0]` |
| `c` | `real(wp)(:)` | Color mapping array | `[0.1, 0.5, 0.9]` |
| `marker` | `character(*)` | Marker shape | `'circle'`, `'square'`, `'star'` |
| `colormap` | `character(*)` | Colormap name | `'viridis'`, `'plasma'`, `'coolwarm'` |
| `alpha` | `real(wp)` | Transparency (0-1) | `0.7_wp` |
| `facecolor` | `real(wp)(3)` | RGB face color | `[1.0, 0.0, 0.0]` |
| `edgecolor` | `real(wp)(3)` | RGB edge color | `[0.0, 0.0, 0.0]` |
| `linewidth` | `real(wp)` | Edge line width | `2.0_wp` |
| `label` | `character(*)` | Legend label | `'Data Points'` |

### Marker Shapes

| Shape | Description | Best For |
|-------|-------------|----------|
| `'circle'` | Circular markers | General purpose, bubble charts |
| `'square'` | Square markers | Categorical data, grids |
| `'triangle'` | Triangular markers | Directional data |
| `'diamond'` | Diamond markers | Special values, outliers |
| `'star'` | Star markers | Important points, ratings |
| `'pentagon'` | Pentagon markers | Complex categorical data |
| `'hexagon'` | Hexagon markers | Packed data representations |

### Colormaps

| Colormap | Type | Best For |
|----------|------|----------|
| `'viridis'` | Perceptually uniform | Scientific data |
| `'plasma'` | Perceptually uniform | High contrast data |
| `'coolwarm'` | Diverging | Temperature, correlation |
| `'jet'` | Rainbow | Legacy compatibility |
| `'grayscale'` | Monochrome | Print publication |

## Examples

### Scientific Data Visualization
```fortran
program scientific_scatter
    use fortplot
    implicit none
    
    integer, parameter :: n = 100
    real(wp) :: temperature(n), pressure(n), humidity(n), wind_speed(n)
    integer :: i
    
    ! Generate scientific dataset
    do i = 1, n
        temperature(i) = 273.0_wp + 30.0_wp * real(i-1, wp) / real(n-1, wp)
        pressure(i) = 101325.0_wp * (1.0_wp + 0.1_wp * sin(real(i, wp) * 0.1_wp))
        humidity(i) = 0.5_wp + 0.3_wp * cos(real(i, wp) * 0.2_wp)
        wind_speed(i) = 5.0_wp + 10.0_wp * abs(sin(real(i, wp) * 0.15_wp))
    end do
    
    ! Create multi-dimensional visualization
    type(figure_t) :: fig
    call fig%initialize(800, 600)
    call fig%scatter(temperature, pressure, s=wind_speed*5, c=humidity, &
                     colormap='coolwarm', alpha=0.7_wp, &
                     marker='circle', label='Weather Data')
    
    call fig%set_title('Multi-dimensional Weather Data')
    call fig%set_xlabel('Temperature (K)')
    call fig%set_ylabel('Pressure (Pa)')
    call fig%legend()
    call fig%savefig('weather_analysis.png')
end program
```

### Performance Optimization for Large Datasets
```fortran
program large_dataset_example
    use fortplot
    implicit none
    
    integer, parameter :: n_points = 10000
    real(wp) :: x(n_points), y(n_points), sizes(n_points), colors(n_points)
    integer :: i
    
    ! Generate large realistic dataset
    do i = 1, n_points
        x(i) = real(i, wp) / real(n_points, wp) * 10.0_wp
        y(i) = sin(x(i)) + 0.1_wp * real(i, wp) / real(n_points, wp)
        sizes(i) = 5.0_wp + 20.0_wp * abs(sin(x(i) * 2.0_wp))
        colors(i) = (y(i) + 1.1_wp) / 2.2_wp
    end do
    
    ! Optimized rendering for 10K+ points
    type(figure_t) :: fig
    call fig%initialize(800, 600)
    call fig%scatter(x, y, s=sizes, c=colors, &
                     colormap='viridis', alpha=0.6_wp, &
                     marker='circle', label='10K Points')
    
    call fig%set_title('Large Dataset Performance Test')
    call fig%savefig('large_dataset.png')  ! < 100ms target
end program
```

### Multiple Marker Types Comparison
```fortran
program marker_comparison
    use fortplot
    implicit none
    
    real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp) :: y_base(3) = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp) :: sizes(3) = [30.0_wp, 40.0_wp, 35.0_wp]
    
    type(figure_t) :: fig
    call fig%initialize(800, 600)
    
    ! Compare different marker shapes
    call fig%scatter(x, y_base, s=sizes, marker='circle', &
                     facecolor=[1.0_wp, 0.0_wp, 0.0_wp], label='Circles')
    
    call fig%scatter(x, y_base + 0.5_wp, s=sizes, marker='square', &
                     facecolor=[0.0_wp, 1.0_wp, 0.0_wp], label='Squares')
    
    call fig%scatter(x, y_base + 1.0_wp, s=sizes, marker='triangle', &
                     facecolor=[0.0_wp, 0.0_wp, 1.0_wp], label='Triangles')
    
    call fig%scatter(x, y_base + 1.5_wp, s=sizes, marker='diamond', &
                     facecolor=[1.0_wp, 0.5_wp, 0.0_wp], label='Diamonds')
    
    call fig%scatter(x, y_base + 2.0_wp, s=sizes, marker='star', &
                     facecolor=[0.5_wp, 0.0_wp, 1.0_wp], label='Stars')
    
    call fig%set_title('Marker Shape Comparison')
    call fig%legend()
    call fig%savefig('marker_shapes.pdf')
end program
```

## Backend-Specific Notes

### PNG/PDF Backends
- High-quality vector graphics with antialiasing
- Perfect scaling at any zoom level
- Supports transparency and complex shapes
- Optimal for publication and presentation

### ASCII Backend
- Character-based marker representation
- Size mapping through character choice: `.`, `o`, `O`, `@`
- Color mapping through ASCII brightness
- Perfect for terminal display and debugging

## Performance Guidelines

### Large Dataset Optimization
- **Target**: <100ms for 10,000 points
- Use moderate alpha values (0.6-0.8) for overlapping points
- Consider data decimation for extremely large datasets
- PNG backend most efficient for dense scatter plots

### Memory Efficiency
- Automatic memory management prevents leaks
- Size and color arrays automatically validated
- Figure cleanup releases all allocated memory

## Error Handling

### Data Validation
```fortran
! NaN and infinite values automatically filtered
real(wp) :: x_with_nan(4) = [1.0_wp, NaN, 3.0_wp, 4.0_wp]
real(wp) :: y_clean(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]

! Only valid points plotted, invalid points skipped gracefully
call scatter(x_with_nan, y_clean, label='Filtered Data')
```

### Array Size Mismatch
```fortran
! Size and color arrays validated against position arrays
! Automatically handles mismatched array sizes with clear error messages
```

## Integration with Other Plot Types

### Combining with Line Plots
```fortran
call figure()
call plot(x_theory, y_theory, label='Theory', linestyle='-')
call scatter(x_data, y_data, s=error_sizes, c=confidence, &
             colormap='plasma', alpha=0.7_wp, label='Experimental')
call legend()
call savefig('theory_vs_experiment.png')
```

### Adding Colorbars
```fortran
! Colorbar automatically generated for color-mapped scatter
call scatter(x, y, c=values, colormap='viridis')
! Colorbar appears with appropriate scale and labels
```