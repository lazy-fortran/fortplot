title: Marker Demo Example
---

# Marker Demo

This example showcases various marker types and scatter plot capabilities in fortplotlib.

## Files

- `marker_demo.f90` - Source code
- `scatter_plot.png/pdf/txt` - Basic scatter plot
- `all_marker_types.png/pdf/txt` - All available marker types
- `marker_colors.png/pdf/txt` - Colored markers example

## Running

```bash
make example ARGS="marker_demo"
```

## Features Demonstrated

- **Marker types**: Circle, square, triangle, diamond, plus, cross, star
- **Scatter plots**: Data points without connecting lines
- **Color customization**: Different colors for markers
- **Size control**: Adjustable marker sizes

## Available Markers

- `o` - Circle
- `s` - Square
- `^` - Triangle up
- `v` - Triangle down
- `<` - Triangle left
- `>` - Triangle right
- `d` - Diamond
- `+` - Plus
- `x` - Cross
- `*` - Star

## Output Examples

The example generates the following output files:
- `scatter_plot.png` - Basic scatter plot demonstrating data points without connecting lines
- `scatter_plot.pdf` - Vector format of the scatter plot
- `scatter_plot.txt` - ASCII art representation of scatter points
- `all_marker_types.png` - Showcase of all available marker types (circle, square, triangle variants, diamond, plus, cross, star)
- `all_marker_types.pdf` - Vector format showing all marker types
- `all_marker_types.txt` - ASCII representation of different markers
- `marker_colors.png` - Demonstrates colored markers with size variations
- `marker_colors.pdf` - Vector format of colored markers
- `marker_colors.txt` - ASCII art with different marker representations

See the [documentation gallery](https://lazy-fortran.github.io/fortplot/) for visual examples.