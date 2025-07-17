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

### Gaussian Default
![Gaussian Default](gaussian_default.png)

### Ripple Jet
![Ripple Jet](ripple_jet.png)