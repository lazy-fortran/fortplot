title: 3d Plotting
---

# 3d Plotting

Source: [3d_plotting.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/3d_plotting/3d_plotting.f90)

This comprehensive example demonstrates all 3D plotting capabilities in fortplot, consolidating features from multiple examples.

- **3D Line Plots**: Helices and parametric curves
- **3D Scatter Plots**: Multiple patterns and marker types
- **3D Surface Plots**: Mathematical functions and meshes
- **Mixed Plots**: Combining 2D and 3D in single figures
- **Plot Combinations**: Multiple 3D plot types together

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
- Run the example to populate `output/example/fortran/3d_plotting/`

## Running

```bash
make example ARGS="3d_plotting"
```

## Output

Run this example to generate plots and other media assets.

