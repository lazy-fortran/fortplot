title: Contour Demo
---

# Contour Demo

Source: [example/fortran/contour_demo/contour_demo.f90](../../example/fortran/contour_demo/contour_demo.f90)

This example demonstrates line contour plotting (level lines) with custom levels and a mixed plot combining contours with a line overlay.

Note: For filled/colored contours (continuous shaded regions) and colormap comparisons, see also: [Colored Contours](./colored_contours.html).

## Files

- `contour_demo.f90` - Source code
- `contour_gaussian.png/pdf/txt` - Gaussian function contour plot
- `mixed_plot.png/pdf/txt` - Combined contour and line plot

## Running

```bash
make example ARGS="contour_demo"
```

## Features Demonstrated

- **Line contours**: Level lines with automatic or custom levels
- **Custom levels**: User-defined contour levels
- **Mixed plots**: Combine contours with a line profile overlay

## Output

### Contour Gaussian

![contour_gaussian.png](../../media/examples/contour_demo/contour_gaussian.png)

<!-- ASCII preview removed to keep pages concise; full ASCII linked below. -->

> **Full ASCII Output**: [Download contour_gaussian.txt](../../media/examples/contour_demo/contour_gaussian.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/contour_demo/contour_gaussian.pdf)

### Mixed Plot

![mixed_plot.png](../../media/examples/contour_demo/mixed_plot.png)

<!-- ASCII preview removed to keep pages concise; full ASCII linked below. -->

> **Full ASCII Output**: [Download mixed_plot.txt](../../media/examples/contour_demo/mixed_plot.txt) | [ASCII Format Guide](../ascii_output_format.md)

[Download PDF](../../media/examples/contour_demo/mixed_plot.pdf)
