---
title: Colored Contours
---

# Colored Contours

Source: [colored_contours.f90](../../sourcefile/colored_contours.f90.html)

This example shows filled (colored) contour plots with customizable colormaps for visualizing 2D scalar fields.

Note: For line contours (contour level lines) and a mixed contour+line example, see: [Contour Demo](./contour_demo.html).

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
- **Multiple colormaps**: crest, jet, coolwarm, inferno, plasma
- **Custom levels**: Control contour density
- **Various functions**: Gaussian, ripple, saddle point patterns

## Available Colormaps

- `crest` - Default, perceptually uniform colorblind-safe
- `jet` - Classic rainbow colormap
- `coolwarm` - Blue to red diverging
- `inferno` - Black to yellow sequential
- `plasma` - Purple to yellow sequential

## Output

### 2D Gaussian with Default Colorblind-Safe Colormap

![gaussian_default.png](../../media/examples/colored_contours/gaussian_default.png)

<!-- ASCII preview removed to keep pages concise; full ASCII linked below. -->

> **Full ASCII Output**: [Download gaussian_default.txt](../../media/examples/colored_contours/gaussian_default.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/colored_contours/gaussian_default.pdf)

### Saddle Function with Plasma Colormap

![saddle_plasma.png](../../media/examples/colored_contours/saddle_plasma.png)

<!-- ASCII preview removed to keep pages concise; full ASCII linked below. -->

> **Full ASCII Output**: [Download saddle_plasma.txt](../../media/examples/colored_contours/saddle_plasma.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/colored_contours/saddle_plasma.pdf)

### Ripple Function with Jet Colormap

![ripple_jet.png](../../media/examples/colored_contours/ripple_jet.png)

<!-- ASCII preview removed to keep pages concise; full ASCII linked below. -->

> **Full ASCII Output**: [Download ripple_jet.txt](../../media/examples/colored_contours/ripple_jet.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/colored_contours/ripple_jet.pdf)

### Ripple Function with Coolwarm Colormap

![ripple_coolwarm.png](../../media/examples/colored_contours/ripple_coolwarm.png)

<!-- ASCII preview removed to keep pages concise; full ASCII linked below. -->

> **Full ASCII Output**: [Download ripple_coolwarm.txt](../../media/examples/colored_contours/ripple_coolwarm.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/colored_contours/ripple_coolwarm.pdf)

### Ripple Function with Inferno Colormap

![ripple_inferno.png](../../media/examples/colored_contours/ripple_inferno.png)

<!-- ASCII preview removed to keep pages concise; full ASCII linked below. -->

> **Full ASCII Output**: [Download ripple_inferno.txt](../../media/examples/colored_contours/ripple_inferno.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/colored_contours/ripple_inferno.pdf)
