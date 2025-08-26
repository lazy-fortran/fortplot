title: Fortran Examples
---

# fortplot Examples

This directory contains all fortplot examples written in Fortran. Each subdirectory contains a specific example demonstrating different features of the library.

## Quick Start

Run any example using:

```bash
make example ARGS="example_name"
```

## Examples Overview

### Basic Plotting
- [basic_plots](./basic_plots/) - Fundamental plotting with functional and OO APIs
- [line_styles](./line_styles/) - Different line styles and customization
- [marker_demo](./marker_demo/) - Marker types and scatter plots
- [format_string_demo](./format_string_demo/) - Matplotlib-style format strings
- [scatter_demo](./scatter_demo/) - Enhanced scatter plots with color mapping

### Statistical and Categorical
- [errorbar_demo](./errorbar_demo/) - Error bars for scientific data
- [bar_chart_demo](./bar_chart_demo/) - Bar charts for categorical data
- [boxplot_demo](./boxplot_demo/) - Box-and-whisker plots
- [histogram_demo](./histogram_demo/) - Data distribution histograms

### Advanced Plotting
- [3d_plotting](./3d_plotting/) - 3D surface and line plots
- [contour_demo](./contour_demo/) - Contour plots with levels
- [colored_contours](./colored_contours/) - Filled contours with colormaps
- [pcolormesh_demo](./pcolormesh_demo/) - Pseudocolor mesh plots
- [streamplot_demo](./streamplot_demo/) - Vector field visualization

### Scaling and Styling
- [scale_examples](./scale_examples/) - Log and symlog scales
- [legend_demo](./legend_demo/) - Legend placement options
- [unicode_demo](./unicode_demo/) - Mathematical symbols
- [grid_demo](./grid_demo/) - Grid lines and formatting

### Annotations and Layout
- [annotation_demo](./annotation_demo/) - Text and arrow annotations
- [subplot_demo](./subplot_demo/) - Multiple plot grids
- [disconnected_lines](./disconnected_lines/) - Multi-segment plots

### Special Features
- [animation](./animation/) - Animated plots with FFmpeg
- [ascii_heatmap](./ascii_heatmap/) - Terminal-based visualization
- [show_viewer_demo](./show_viewer_demo/) - Interactive display
- [smart_show_demo](./smart_show_demo/) - Intelligent display mode

## Output Formats

Each example can generate output in multiple formats:
- **PNG** - High-quality raster images
- **PDF** - Vector graphics for publications
- **TXT** - ASCII art for terminal display

## Building Individual Examples

Examples are automatically discovered by fpm. To build a specific example:

```bash
make build ARGS="--target example_name"
```

## Adding New Examples

1. Create a new directory under `example/fortran/`
2. Add your `.f90` source file
3. Create a `README.md` describing the example
4. Run the example to generate outputs
5. The example will be automatically discovered by fpm