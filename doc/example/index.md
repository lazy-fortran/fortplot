title: Examples Gallery
---

# fortplot Examples Gallery

This gallery showcases various features of the fortplot library with working examples. Each example includes source code, generated plots, and explanations.

## Basic Features

### [Basic Plots](basic_plots.html)
Simple line plots demonstrating the fundamental plotting capabilities of fortplot.

### [Line Styles](line_styles.html)
Demonstration of different line styles, widths, and colors available in fortplot.

### [Marker Demo](marker_demo.html)
Scatter plots with various marker types, sizes, and colors.

### [Format String Demo](format_string_demo.html)
Using matplotlib-style format strings for quick plot styling.

## Advanced Plots

### [Contour Demo](contour_demo.html)
Creating contour plots from 2D data with customizable levels.

### [Colored Contours](colored_contours.html)
Contour plots with different colormaps including viridis, jet, coolwarm, inferno, and plasma.

### [Pcolormesh Demo](pcolormesh_demo.html)
2D heatmaps using the pcolormesh function with various colormaps.

### [Streamplot Demo](streamplot_demo.html)
Vector field visualization using streamlines.

### [Stateful Streamplot](stateful_streamplot.html)
Advanced streamplot with state management.

## Scales and Transformations

### [Scale Examples](scale_examples.html)
Demonstration of logarithmic and symmetric logarithmic scales.

## Styling and Annotations

### [Legend Demo](legend_demo.html)
Legend positioning and styling options.

### [Legend Box Demo](legend_box_demo.html)
Advanced legend box customization.

### [Unicode Demo](unicode_demo.html)
Mathematical symbols and Unicode support in labels and titles.

## Special Features

### [ASCII Heatmap](ascii_heatmap.html)
Terminal-based heatmap visualization using ASCII characters.

### [Animation](animation.html)
Creating animated plots and saving them as video files.

### [Show Viewer Demo](show_viewer_demo.html)
Interactive plot viewing using external image viewers.

### [Smart Show Demo](smart_show_demo.html)
Intelligent display mode selection based on environment.

## Running the Examples

All examples can be run using:

```bash
make example ARGS="example_name"
```

For example:
```bash
make example ARGS="basic_plots"
```

## Source Code

The complete source code for all examples is available in the [GitHub repository](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran).