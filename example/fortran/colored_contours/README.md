title: Colored Contours Example
---

# Colored Contours

This example shows filled contour plots with customizable colormaps for visualizing 2D scalar fields.

## Files

- `colored_contours.f90` - Source code
- `gaussian_default.png/pdf/txt` - Gaussian with default colormap
- `ripple_jet.png/pdf/txt` - Ripple pattern with jet colormap
- `ripple_coolwarm.png/pdf/txt` - Ripple pattern with coolwarm colormap
- `ripple_inferno.png/pdf/txt` - Ripple pattern with inferno colormap
- `saddle_plasma.png/pdf/txt` - Saddle point with plasma colormap

## Running

```bash
make example ARGS="colored_contours"
```

## Features Demonstrated

- **Filled contours**: Continuous color gradients
- **Multiple colormaps**: viridis, jet, coolwarm, inferno, plasma
- **Custom levels**: Control contour density
- **Various functions**: Gaussian, ripple, saddle point patterns

## Available Colormaps

- `viridis` - Default, perceptually uniform
- `jet` - Classic rainbow colormap
- `coolwarm` - Blue to red diverging
- `inferno` - Black to yellow sequential
- `plasma` - Purple to yellow sequential

## Output Examples

The example generates the following output files:
- `gaussian_default.png` - Gaussian function with default viridis colormap showing perceptually uniform colors
- `gaussian_default.pdf` - Vector format of the Gaussian contour plot
- `gaussian_default.txt` - ASCII representation of filled contours
- `ripple_jet.png` - Ripple pattern visualized with classic rainbow jet colormap
- `ripple_jet.pdf` - Vector format of the ripple with jet colormap
- `ripple_jet.txt` - ASCII art version of the ripple pattern
- `ripple_coolwarm.png` - Same ripple pattern with blue-to-red diverging colormap
- `ripple_coolwarm.pdf` - Vector format with coolwarm colormap
- `ripple_coolwarm.txt` - ASCII representation with coolwarm styling
- `ripple_inferno.png` - Ripple pattern with black-to-yellow inferno colormap
- `ripple_inferno.pdf` - Vector format with inferno colormap
- `ripple_inferno.txt` - ASCII version with inferno representation
- `saddle_plasma.png` - Saddle point function with purple-to-yellow plasma colormap
- `saddle_plasma.pdf` - Vector format of the saddle point
- `saddle_plasma.txt` - ASCII art of the saddle point pattern

See the [documentation gallery](https://lazy-fortran.github.io/fortplot/) for visual examples.
