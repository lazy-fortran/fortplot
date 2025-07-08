# TODO: 3D Plotting Support

## Overview
Add 3D plotting capabilities to fortplotlib with GLTF/GLB file format output support.
API design follows pyplot-fortran conventions for compatibility.

**⚠️ CRITICAL DEVELOPMENT PRINCIPLES ⚠️**
- **TDD MANDATORY**: Write comprehensive tests FIRST - no implementation without failing tests
- **SRP ENFORCED**: Each module/routine has ONE clear responsibility (max 30 lines)
- **DRY REQUIRED**: Extract ALL common functionality - no duplication allowed
- **KISS ESSENTIAL**: Keep solutions simple - no clever tricks, clear over clever

## Phase 1: GLTF Text Format Support (Debug-friendly) ✅
- [x] **TEST FIRST**: Write tests for GLTF JSON structure generation
- [x] Create `fortplot_gltf_base` module for shared GLTF constants/types (DRY)
- [x] Create `fortplot_gltf_writer` module for GLTF text output (SRP)
  - Separate routines for: header, scenes, nodes, meshes, accessors, bufferViews
  - Each routine ≤ 30 lines with single responsibility
- [x] Implement GLTF 2.0 minimal valid file structure
- [x] **TEST**: Validate output against GLTF validator

## Phase 2: Basic 3D Plotting APIs with TDD ✅
- [x] **TEST FIRST**: Write failing tests for 3D plot data storage
- [x] Extend `plot_data_t` to support optional z coordinates (follow existing patterns)
  - 2D plots: z not allocated
  - 3D plots: z allocated automatically when provided
- [x] **TEST FIRST**: Write tests for `add_3d_plot` API behavior
- [x] Implement `add_3d_plot(x, y, z, label, linestyle, markersize, linewidth)` 
  - Natural extension of add_plot - just add z coordinate
  - Figure automatically handles 3D when z is present
  - Delegate to specialized storage routine (SRP)
- [x] **TEST FIRST**: Write tests for `add_surface` grid validation
- [x] Implement `add_surface(x, y, z, label)` for surface plots
  - x, y: 1D arrays defining grid
  - z: 2D array of values on grid
  - Separate validation from storage (SRP)

## Phase 3: GLTF Export with Proper Testing
- [ ] **TEST FIRST**: Write tests for line geometry conversion
- [ ] Create `fortplot_gltf_geometry` module (SRP)
  - Routine for line plot → GLTF line primitive
  - Routine for surface → GLTF triangle mesh
  - Routine for color/material generation
- [ ] **TEST FIRST**: Write tests for buffer packing
- [ ] Create `fortplot_gltf_buffer` module for data packing (SRP)
- [ ] Implement `savefig` for .gltf extension
- [ ] **TEST**: Compare output with reference GLTF files

## Phase 4: GLB Binary Format (Built on GLTF)
- [ ] **TEST FIRST**: Write tests for GLTF→GLB conversion
- [ ] Create `fortplot_glb_writer` module (SRP, reuse GLTF modules via DRY)
  - Convert GLTF JSON to binary format
  - Pack buffers into GLB container
- [ ] Implement `savefig` for .glb extension
- [ ] **TEST**: Binary comparison with reference GLB files

## Phase 5: 2D Projection Output (PNG/PDF) for 3D Plots ✅
- [x] **TEST FIRST**: Write tests for 3D to 2D projection transformation
- [x] Create `fortplot_projection` module for 3D→2D transformation (SRP)
  - Default viewing angles: azimuth=-60°, elevation=30° (matplotlib defaults)
  - Perspective projection with focal_length=1, camera_distance=10
  - Transform 3D coordinates to 2D screen coordinates
- [x] **TEST FIRST**: Write tests for rendering 3D plots in PNG
- [x] Extend PNG backend to handle 3D plot types
  - Detect 3D plots via `has_3d_plots()` or `is_3d()` 
  - Apply projection before rendering
  - Draw projected lines/surfaces
- [ ] **TEST FIRST**: Write tests for rendering 3D plots in PDF
- [ ] Extend PDF backend similarly
- [ ] **TEST**: Visual comparison with matplotlib 3D plot outputs

## Phase 6: 3D Axes and Visual Improvements
- [ ] **TEST FIRST**: Write tests for 3D axis frame generation
- [ ] Create `fortplot_3d_axes` module for 3D axis rendering (SRP)
  - Generate 3D bounding box corners and project to 2D
  - Draw 3D axis lines (X, Y, Z axes visible in 3D space)
  - Add tick marks and labels on projected 3D axes
  - Optional: 3D grid lines for better depth perception
- [ ] **TEST FIRST**: Write tests for surface plot improvements
- [ ] Improve surface plot rendering to show actual 3D surface
  - Convert surface data to triangular mesh
  - Apply proper shading/depth cues
  - Handle hidden surface removal
- [ ] **TEST FIRST**: Write tests for 3D visual enhancements
- [ ] Add visual depth cues
  - Line thickness variation based on depth
  - Alpha/transparency for depth ordering
  - Optional: simple lighting model for surfaces

## Implementation Notes
- **No shallow tests**: Test behavior, edge cases, and error conditions
- **Modular design**: Each module has clear, single responsibility
- **No magic numbers**: All GLTF constants in fortplot_gltf_base
- Reference matplotlib's mpl_toolkits.mplot3d for algorithms
- GLTF before GLB enables debugging with text editors

## Example API Usage
```fortran
use fortplot
type(figure_t) :: fig

! Regular initialization - no backend needed
call fig%initialize(width, height)

! Add 3D line plot - figure automatically detects 3D
call fig%add_3d_plot(x, y, z, label="3D curve")

! Add surface plot
call fig%add_surface(x_grid, y_grid, z_values)

! Save to any format - backend created based on extension
call fig%savefig('output.png')   ! 2D projection to PNG
call fig%savefig('output.pdf')   ! 2D projection to PDF
call fig%savefig('output.gltf')  ! 3D model in text format
call fig%savefig('output.glb')   ! 3D model in binary format
```