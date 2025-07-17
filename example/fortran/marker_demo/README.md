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

### Scatter Plot
![Scatter Plot](scatter_plot.png)

### All Marker Types
![All Marker Types](all_marker_types.png)