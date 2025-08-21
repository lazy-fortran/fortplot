title: Legend Demo Example
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
- `legend_box_default.png/pdf/txt` - Legend box with default styling
- `legend_box_upper_left.png/pdf/txt` - Legend box in upper left position
- `legend_box_lower_right.png/pdf/txt` - Legend box in lower right position

## Running

```bash
make example ARGS="legend_demo"
```

## Features Demonstrated

- **Automatic placement**: Default "best" location
- **Manual placement**: Specify corner positions
- **Multi-line legends**: Handle multiple labeled plots
- **Smart positioning**: Avoids overlapping with data
- **Box styling**: Legend boxes with frames and backgrounds
- **Multiple entries**: Complex legends with 4+ labeled plots
- **Multi-format output**: PNG, PDF, and ASCII text versions

## Legend Locations

- `best` - Automatic optimal placement
- `upper left` - Top left corner
- `upper right` - Top right corner (default)
- `lower left` - Bottom left corner
- `lower right` - Bottom right corner

## Output Examples

### Basic Legend
![Basic Legend](basic_legend.png)

### Multi-function Legend
![Multi-function Legend](multi_function_legend.png)

### Legend Box Styling
![Legend Box Default](legend_box_default.png)