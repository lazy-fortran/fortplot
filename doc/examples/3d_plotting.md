title: 3D Plotting
---

# 3D Plotting

Source: [3d_plotting.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/3d_plotting/3d_plotting.f90)

3D plotting (lines and surfaces) with axes, ticks, and labels.

- 3D line plots: helices and parametric curves
- 3D scatter plots: multiple patterns and marker types
- 3D surface plots: mathematical functions and meshes
- Mixed plots: combining 2D and 3D in one figure
- Plot combinations: multiple 3D plot types together

- `3d_helix.png` - Basic 3D helix line plot
- `parametric_curve.png` - Parametric spiral curve
- `scatter_sphere.png` - 3D scatter in sphere pattern
- `scatter_multiple.png` - Multiple scatter patterns with legend
- `surface_paraboloid.png` - Paraboloid surface plot
- `surface_gaussian.png` - Gaussian surface plot
- `mixed_plots.png` - Combined 2D and 3D plots
- `scatter_line_combo.png` - 3D scatter + line combination

call fig%add_3d_plot(x, y, z, label="3D Line")

call fig%add_scatter_3d(x, y, z, label="Scatter", marker='o')

call fig%add_surface(x_grid, y_grid, z_grid, label="Surface")

- **Automatic 3D Detection**: fortplot automatically detects 3D data
- **No Special Initialization**: Same `fig%initialize()` for 2D and 3D
- **Mixed Dimensionality**: Can combine 2D and 3D plots in same figure
- **Surface Grid Requirements**: `size(z_grid,1) == size(x_grid)` and `size(z_grid,2) == size(y_grid)`
- **Memory Management**: Uses allocatable arrays for efficient memory usage

## Files

- `3d_plotting.f90` - Source code
- Generated media in `output/example/fortran/3d_plotting/`

## Running

```bash
make example ARGS="3d_plotting"
```

## Output

### 3D Helix

![3d_helix.png](../../media/examples/3d_plotting/3d_helix.png)

### Mixed Plots

![mixed_plots.png](../../media/examples/3d_plotting/mixed_plots.png)

### Parametric Curve

![parametric_curve.png](../../media/examples/3d_plotting/parametric_curve.png)

### Scatter Line Combo

![scatter_line_combo.png](../../media/examples/3d_plotting/scatter_line_combo.png)

### Scatter Multiple

![scatter_multiple.png](../../media/examples/3d_plotting/scatter_multiple.png)

### Scatter Sphere

![scatter_sphere.png](../../media/examples/3d_plotting/scatter_sphere.png)

### Surface Filled Jet

![surface_filled_jet.png](../../media/examples/3d_plotting/surface_filled_jet.png)

### Surface Filled Plasma

![surface_filled_plasma.png](../../media/examples/3d_plotting/surface_filled_plasma.png)

### Surface Filled Viridis

![surface_filled_viridis.png](../../media/examples/3d_plotting/surface_filled_viridis.png)

### Surface Gaussian

![surface_gaussian.png](../../media/examples/3d_plotting/surface_gaussian.png)

### Surface Paraboloid

![surface_paraboloid.png](../../media/examples/3d_plotting/surface_paraboloid.png)

