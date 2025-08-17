title: Contour Demo Example
---

# Contour Demo

This example demonstrates contour plotting capabilities, including basic contours, custom levels, and mixing contour plots with line plots.

## Files

- `contour_demo.f90` - Source code
- `contour_gaussian.png/pdf/txt` - Gaussian function contour plot
- `mixed_plot.png/pdf/txt` - Combined contour and line plot

## Running

```bash
make example ARGS="contour_demo"
```

## Features Demonstrated

- **Basic contours**: Automatic level selection
- **Custom levels**: User-defined contour levels
- **Mixed plots**: Combining contours with line plots
- **Label formatting**: Contour level labels

## Output Examples

The example generates the following output files:
- `contour_gaussian.png` - Contour plot of a Gaussian function with automatic level selection
- `contour_gaussian.pdf` - Vector format of the Gaussian contour plot
- `contour_gaussian.txt` - ASCII art representation of the contours
- `mixed_plot.png` - Combined contour and line plot showing integration of different plot types
- `mixed_plot.pdf` - Vector format of the mixed plot
- `mixed_plot.txt` - ASCII art representation of the mixed plot

See the [documentation gallery](https://lazy-fortran.github.io/fortplot/) for visual examples.
