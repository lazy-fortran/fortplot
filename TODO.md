# TODO: 3D Plotting Support

## Overview
Add 3D plotting capabilities to fortplotlib with GLB file format output support.
API design follows pyplot-fortran conventions for compatibility.

## Phase 1: Core Infrastructure
- [ ] Add `mplot3d` flag to figure initialization (following pyplot-fortran)
- [ ] Create `fortplot_glb` module for GLB file format support
- [ ] Implement basic GLB writer with mesh and material support

## Phase 2: Basic 3D Plotting APIs
- [ ] Implement `add_3d_plot(x, y, z, label, linestyle, markersize, linewidth)` 
  - Match pyplot-fortran signature exactly
  - Store 3D line data in plot_data_t
- [ ] Implement `add_surface(x, y, z, label)` for surface plots
  - x, y: 1D arrays defining grid
  - z: 2D array of values on grid

## Phase 3: GLB Export
- [ ] Convert 3D line plots to GLB line geometry
- [ ] Convert surface plots to GLB triangle mesh
- [ ] Add basic material/color support
- [ ] Implement `savefig` for .glb extension

## Implementation Notes
- No 2D projection/rendering needed initially
- Focus on data storage and GLB export
- Reference matplotlib's mpl_toolkits.mplot3d for algorithms
- Keep minimal - just add_3d_plot and add_surface for now

## Example API Usage
```fortran
use fortplot
type(figure_t) :: fig

! Initialize with 3D support
call fig%initialize(mplot3d=.true.)

! Add 3D line plot
call fig%add_3d_plot(x, y, z, label="3D curve")

! Add surface plot
call fig%add_surface(x_grid, y_grid, z_values)

! Save as GLB file
call fig%savefig('output.glb')
```