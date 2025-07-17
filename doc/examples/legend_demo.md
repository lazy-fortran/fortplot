title: Legend Demo
---

# Legend Demo

This example demonstrates legend placement and customization options.

## Files

- `legend_demo.f90` - Source code
- `basic_legend.png/pdf/txt` - Basic legend with default placement
- `legend_upper_left.png/pdf/txt` - Legend in upper left
- `legend_upper_right.png/pdf/txt` - Legend in upper right
- `legend_lower_left.png/pdf/txt` - Legend in lower left
- `legend_lower_right.png/pdf/txt` - Legend in lower right
- `multi_function_legend.png/pdf/txt` - Multiple functions with legend

## Running

```bash
make example ARGS="legend_demo"
```

## Features Demonstrated

- **Automatic placement**: Default "best" location
- **Manual placement**: Specify corner positions
- **Multi-line legends**: Handle multiple labeled plots
- **Smart positioning**: Avoids overlapping with data

## Legend Locations

- `best` - Automatic optimal placement
- `upper left` - Top left corner
- `upper right` - Top right corner (default)
- `lower left` - Bottom left corner
- `lower right` - Bottom right corner

## Output

### Basic Legend
![Basic Legend](../../media/examples/basic_legend.png)

### Multi-function Legend
![Multi-function Legend](../../media/examples/multi_function_legend.png)
