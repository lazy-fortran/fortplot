# TODO: 3D Plotting Features

## Phase 1: Basic 3D Line Plots
- [ ] Implement `fig%add_3d_plot(x, y, z)` for 3D line plots
- [ ] Add 3D axes with proper transformations
- [ ] Implement perspective projection
- [ ] Add depth sorting for correct rendering order

## Phase 2: 3D Surface Plots
- [ ] Implement `fig%add_surface(x, y, z)` for 3D surface plots
- [ ] Add wireframe option
- [ ] Implement shading/coloring based on z-values
- [ ] Add colormap support for surfaces

## Phase 3: 3D Scatter Plots
- [ ] Implement `fig%add_3d_scatter(x, y, z)` for 3D scatter plots
- [ ] Support variable marker sizes
- [ ] Support variable marker colors

## Phase 4: 3D Contour Plots
- [ ] Implement `fig%add_3d_contour(x, y, z)` for 3D contour plots
- [ ] Support contour3D (lines in 3D space)
- [ ] Support contourf3D (filled contours in 3D)

## Phase 5: Advanced Features
- [ ] Implement view angle control (azimuth, elevation)
- [ ] Add axis equal/scaled functionality for 3D
- [ ] Implement 3D axis labels and ticks
- [ ] Add grid lines for 3D plots
- [ ] Support multiple 3D plots in same figure

## Technical Implementation Details
- [ ] Create `fortplot_3d_transforms` module for coordinate transformations
- [ ] Create `fortplot_3d_axes` module for 3D axis handling
- [ ] Extend backends to support 3D rendering
- [ ] Add z-buffer or painter's algorithm for depth handling

## API Design (following matplotlib)
```fortran
! Basic 3D plot
call fig%add_3d_plot(x, y, z, label="3D curve")

! Surface plot
call fig%add_surface(x_grid, y_grid, z_grid, colormap="viridis")

! Set 3D view
call fig%set_view(azimuth=45.0_wp, elevation=30.0_wp)

! Set 3D axis limits
call fig%set_xlim3d(xmin, xmax)
call fig%set_ylim3d(ymin, ymax)
call fig%set_zlim3d(zmin, zmax)
```