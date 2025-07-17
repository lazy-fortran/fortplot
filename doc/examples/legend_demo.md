title: Legend Demo
---

# Legend Demo

This example demonstrates legend placement and customization options.

## Source Files

### Fortran Source

📄 [legend_demo.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/legend_demo/legend_demo.f90)

### Python Equivalent

🐍 [legend_demo.py](https://github.com/krystophny/fortplotlib/blob/main/example/python/legend_demo/legend_demo.py)

### Generated Output Files

- `basic_legend.png/pdf/txt` - Basic legend example
- `legend_*.png/pdf/txt` - Various legend positions
- `multi_function_legend.png/pdf/txt` - Multiple functions

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
