# Enhanced Scatter Plot Guide

Fortplot's enhanced scatter plots provide size and color mapping for multi-dimensional data visualization.

## Quick Examples

### Basic Scatter
```fortran
use fortplot
real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
real(wp) :: y(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]

type(figure_t) :: fig
call fig%initialize()
call fig%add_scatter_2d(x, y, label='Data Points')
call fig%savefig('scatter.png')
```

### Bubble Chart - Size Mapping
```fortran
real(wp) :: sizes(5) = [10.0_wp, 50.0_wp, 100.0_wp, 25.0_wp, 75.0_wp]

call fig%add_scatter_2d(x, y, s=sizes, label='Bubble Chart')
call fig%savefig('bubble.pdf')
```

### Color-Mapped with Colorbar
```fortran
real(wp) :: colors(5) = [0.1_wp, 0.3_wp, 0.7_wp, 0.9_wp, 0.5_wp]

call fig%add_scatter_2d(x, y, c=colors, colormap='viridis', &
                       show_colorbar=.true., label='Color Mapped')
call fig%savefig('color_scatter.png')
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

| Parameter | Type | Description |
|-----------|------|-------------|
| `x, y` | `real(wp)(:)` | Position coordinates (required) |
| `s` | `real(wp)(:)` | Marker sizes (optional) |
| `c` | `real(wp)(:)` | Color values (optional) |
| `colormap` | `character(*)` | Color mapping: 'viridis', 'plasma', 'coolwarm' |
| `show_colorbar` | `logical` | Display colorbar for color mapping |
| `vmin, vmax` | `real(wp)` | Color scale range (auto if not specified) |
| `label` | `character(*)` | Legend label |

### Supported Colormaps

| Colormap | Best For |
|----------|----------|
| `'viridis'` | Scientific data (default) |
| `'plasma'` | High contrast visualization |
| `'coolwarm'` | Temperature, correlation data |

## Scientific Example

```fortran
program scientific_scatter
    use fortplot
    implicit none
    
    integer, parameter :: n = 50
    real(wp) :: temp(n), pressure(n), humidity(n), wind(n)
    type(figure_t) :: fig
    integer :: i
    
    ! Generate weather data
    do i = 1, n
        temp(i) = 273.0_wp + 30.0_wp * real(i-1, wp) / real(n-1, wp)
        pressure(i) = 101325.0_wp * (1.0_wp + 0.1_wp * sin(real(i, wp) * 0.1_wp))
        humidity(i) = 0.5_wp + 0.3_wp * cos(real(i, wp) * 0.2_wp)
        wind(i) = 5.0_wp + 10.0_wp * abs(sin(real(i, wp) * 0.15_wp))
    end do
    
    call fig%initialize(800, 600)
    call fig%add_scatter_2d(temp, pressure, s=wind*3, c=humidity, &
                           colormap='coolwarm', show_colorbar=.true.)
    call fig%set_xlabel('Temperature (K)')
    call fig%set_ylabel('Pressure (Pa)')
    call fig%savefig('weather.png')
end program
```

## Performance Notes

- Automatically filters NaN and infinite values
- Optimized for 10,000+ points with efficient rendering
- Size values clamped to 1-200 pixels for optimal display


## Error Handling

Automatic validation and filtering:
- Array size consistency checking
- NaN/infinite value removal
- Size values clamped to valid ranges

