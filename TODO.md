# TODO: 3D Plotting Support

## Overview
Add 3D plotting capabilities to fortplotlib with GLTF/GLB file format output support.
API design follows pyplot-fortran conventions for compatibility.

**⚠️ CRITICAL DEVELOPMENT PRINCIPLES ⚠️**
- **TDD MANDATORY**: Write comprehensive tests FIRST - no implementation without failing tests
- **SRP ENFORCED**: Each module/routine has ONE clear responsibility (max 30 lines)
- **DRY REQUIRED**: Extract ALL common functionality - no duplication allowed

## Phase 1: GLTF Text Format Support (Debug-friendly)
- [ ] **TEST FIRST**: Write tests for GLTF JSON structure generation
- [ ] Create `fortplot_gltf_base` module for shared GLTF constants/types (DRY)
- [ ] Create `fortplot_gltf_writer` module for GLTF text output (SRP)
  - Separate routines for: header, scenes, nodes, meshes, accessors, bufferViews
  - Each routine ≤ 30 lines with single responsibility
- [ ] Implement GLTF 2.0 minimal valid file structure
- [ ] **TEST**: Validate output against GLTF validator

## Phase 2: Basic 3D Plotting APIs with TDD
- [ ] **TEST FIRST**: Write failing tests for 3D plot data storage
- [ ] Extend `plot_data_t` to support 3D data (follow existing patterns)
- [ ] **TEST FIRST**: Write tests for `add_3d_plot` API behavior
- [ ] Implement `add_3d_plot(x, y, z, label, linestyle, markersize, linewidth)` 
  - Match pyplot-fortran signature exactly
  - Delegate to specialized storage routine (SRP)
- [ ] **TEST FIRST**: Write tests for `add_surface` grid validation
- [ ] Implement `add_surface(x, y, z, label)` for surface plots
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

! Initialize with 3D support
call fig%initialize(mplot3d=.true.)

! Add 3D line plot
call fig%add_3d_plot(x, y, z, label="3D curve")

! Add surface plot
call fig%add_surface(x_grid, y_grid, z_values)

! Save as GLB file
call fig%savefig('output.glb')
```