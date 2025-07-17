title: Colored Contours
---

# Colored Contours

This example shows filled contour plots with customizable colormaps for visualizing 2D scalar fields.

## Source Files

### Fortran Source

📄 [colored_contours.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/colored_contours/colored_contours.f90)

### Python Equivalent

🐍 [colored_contours.py](https://github.com/krystophny/fortplotlib/blob/main/example/python/colored_contours/colored_contours.py)

### Generated Output Files

- `gaussian_default.png/pdf/txt` - Default colormap
- `ripple_*.png/pdf/txt` - Various colormaps (jet, coolwarm, inferno)
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

## Output

