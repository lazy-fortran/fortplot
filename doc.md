---
project: fortplot
summary: Modern Fortran plotting library with multiple backends (PNG, PDF, ASCII) 
author: fortplot developers
src_dir: ./src
output_dir: ./build/doc
page_dir: ./doc
example_dir: ./example
include: ./src
exclude: thirdparty
exclude_dir: thirdparty
         test
         app
graph: true
search: true
source: true
display: public
         protected
sort: alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
project_website: https://github.com/lazy-fortran/fortplot
project_download: https://github.com/lazy-fortran/fortplot/releases
license: MIT
---

@warning This documentation is for developers. For usage examples, see the [Examples Gallery](./page/example/index.html).

# fortplot

Welcome to the fortplot documentation. This library provides a modern, matplotlib-inspired plotting interface for Fortran with multiple backends including PNG, PDF, and ASCII.

## Features

- Multiple backends: PNG (via STB), PDF (native), ASCII (terminal)
- Matplotlib-compatible API design
- No external dependencies
- Pure Fortran implementation with C interop for image writing
- Support for various plot types: line plots, scatter plots, contours, heatmaps
- Unicode support for mathematical symbols
- Animation support

## Quick Start

```fortran
program example
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real :: x(100), y(100)
    integer :: i
    
    ! Generate data
    do i = 1, 100
        x(i) = real(i-1) * 0.1
        y(i) = sin(x(i))
    end do
    
    ! Create plot
    call fig%initialize(800, 600)
    call fig%add_plot(x, y, label='sin(x)')
    call fig%xlabel('x')
    call fig%ylabel('y')
    call fig%title('Simple Plot')
    call fig%legend()
    call fig%savefig('output.png')
    call fig%destroy()
end program
```

## Installation

### Using FPM (Fortran Package Manager)

Add fortplot to your `fpm.toml`:

```toml
[dependencies]
fortplot = { git = "https://github.com/lazy-fortran/fortplot" }
```

### Manual Installation

```bash
git clone https://github.com/lazy-fortran/fortplot
cd fortplot
make build
```

## Navigation

- [API Documentation](./lists/modules.html) - Detailed module and procedure documentation
- [Examples Gallery](./page/example/index.html) - Visual examples with code
- [Source Files](./lists/files.html) - Browse source code

## Examples Gallery

Browse our collection of examples demonstrating various features:

- [Basic Plots](./page/example/basic_plots.html) - Simple line plots
- [Line Styles](./page/example/line_styles.html) - Different line styles and colors
- [Markers](./page/example/marker_demo.html) - Scatter plots with markers
- [Contours](./page/example/contour_demo.html) - Contour plots
- [Colormaps](./page/example/colored_contours.html) - Different colormaps
- [Heatmaps](./page/example/pcolormesh_demo.html) - 2D heatmaps
- [Stream Plots](./page/example/streamplot_demo.html) - Vector field visualization
- [Legends](./page/example/legend_demo.html) - Legend positioning
- [Scales](./page/example/scale_examples.html) - Log and symlog scales
- [Unicode](./page/example/unicode_demo.html) - Mathematical symbols
- [Animation](./page/example/animation.html) - Creating animations
- [ASCII Output](./page/example/ascii_heatmap.html) - Terminal-based plots

## Contributing

We welcome contributions! Please see our [GitHub repository](https://github.com/lazy-fortran/fortplot) for more information.

## License

This project is licensed under the MIT License.