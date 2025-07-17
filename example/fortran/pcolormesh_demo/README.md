title: Pcolormesh Demo Example
---

# Pcolormesh Demo

This example demonstrates pseudocolor plots for efficient 2D data visualization.

## Files

- `pcolormesh_demo.f90` - Source code
- `pcolormesh_basic.png/pdf/txt` - Basic pcolormesh plot
- `pcolormesh_plasma.png/pdf/txt` - Pcolormesh with plasma colormap
- `pcolormesh_sinusoidal.png/pdf/txt` - Sinusoidal pattern

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

## Output Examples

### Basic Pcolormesh
![Basic Pcolormesh](pcolormesh_basic.png)

### Plasma Colormap
![Plasma Colormap](pcolormesh_plasma.png)