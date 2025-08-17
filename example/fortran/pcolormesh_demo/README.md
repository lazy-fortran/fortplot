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

The example generates the following output files:
- `pcolormesh_basic.png` - Basic pcolormesh plot showing grid-based data visualization
- `pcolormesh_basic.pdf` - Vector format of the basic pcolormesh
- `pcolormesh_basic.txt` - ASCII representation of the grid data
- `pcolormesh_plasma.png` - Pcolormesh visualization using the plasma colormap
- `pcolormesh_plasma.pdf` - Vector format with plasma colormap
- `pcolormesh_plasma.txt` - ASCII art with plasma color representation
- `pcolormesh_sinusoidal.png` - Sinusoidal pattern demonstrating smooth gradients
- `pcolormesh_sinusoidal.pdf` - Vector format of the sinusoidal pattern
- `pcolormesh_sinusoidal.txt` - ASCII representation of the sine wave pattern

See the [documentation gallery](https://lazy-fortran.github.io/fortplot/) for visual examples.