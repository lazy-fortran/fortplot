title: Pcolormesh Demo
---

# Pcolormesh Demo

This example demonstrates pseudocolor plots for efficient 2D data visualization.

## Source Files

### Fortran Source

📄 [pcolormesh_demo.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/pcolormesh_demo/pcolormesh_demo.f90)

### Python Equivalent

🐍 [pcolormesh_demo.py](https://github.com/krystophny/fortplotlib/blob/main/example/python/pcolormesh_demo/pcolormesh_demo.py)

### Generated Output Files

- Various output files in PNG, PDF, and ASCII formats

## Running

```bash
make example ARGS="pcolormesh_demo"
```

## Features Demonstrated

- **Grid-based visualization**: Efficient for large 2D datasets
- **Colormap support**: All standard colormaps available
- **Cell-centered data**: Each cell shows one data value
- **Automatic scaling**: Data range mapped to colors

## Key Differences from Contour

- **Pcolormesh**: Shows actual data values as colored cells
- **Contour**: Interpolates and shows level curves
- **Performance**: Pcolormesh faster for large grids

## Output

