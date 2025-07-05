# Test Suite Documentation

This document provides an overview of all tests in the fortplotlib test suite, including what they test, whether they create output files, and what modules they cover.

## Test Organization

Tests are organized in the `test/` directory and can be run using:
```bash
make test
make test-coverage  # With coverage reporting
```

## Test Categories

### Core Functionality Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_figure_basics.f90` | `fortplot_figure_core` | None | Basic figure initialization, plot addition, and labeling |
| `test_figure_labels.f90` | `fortplot_figure_core` | None | Figure labeling functionality |
| `test_figure_set_ydata.f90` | `fortplot_figure_core` | None | Dynamic data updates using set_ydata |
| `test_format_parser.f90` | `fortplot_format_parser` | None | Format string parsing (colors, markers, line styles) |
| `test_module_separation.f90` | Multiple modules | None | Module dependency and separation verification |
| `test_public_api.f90` | `fortplot` | None | Public API interface testing |
| `test_refactored_modules.f90` | Multiple modules | None | Refactored module structure verification |
| `test_simple_validation.f90` | Core modules | None | Simple validation of basic functionality |

### Backend Tests

#### ASCII Backend
| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_ascii.f90` | `fortplot_ascii` | None | ASCII terminal plotting (uses show() only) |

#### PNG Backend
| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_bitmap_rotation.f90` | `fortplot_png` | None | Bitmap rotation functionality |
| `test_bitmap_rotation_both.f90` | `fortplot_png` | None | Combined rotation operations |
| `test_bitmap_to_png_buffer.f90` | `fortplot_png` | None | PNG buffer generation |
| `test_character_bitmap_rendering.f90` | `fortplot_text` | None | Character rendering to bitmap |

#### JPEG Backend
| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_jpeg_backend.f90` | `fortplot_jpeg` | None | JPEG backend core functionality |
| `test_jpeg_block_validation.f90` | `fortplot_jpeg` | `test_jpeg_validation.jpg` | JPEG block structure validation |
| `test_jpeg_complete_validation.f90` | `fortplot_jpeg` | Multiple JPG files | Complete JPEG encoding validation |
| `test_jpeg_dct_encoding.f90` | `fortplot_jpeg` | None | DCT encoding algorithms |
| `test_jpeg_dct_validation.f90` | `fortplot_jpeg` | None | DCT transformation validation |
| `test_jpeg_encoding_validation.f90` | `fortplot_jpeg` | None | JPEG encoding process |
| `test_jpeg_final_validation.f90` | `fortplot_jpeg` | None | Final JPEG validation |
| `test_jpeg_fix.f90` | `fortplot_jpeg` | None | JPEG bug fixes verification |
| `test_jpeg_fixed_complete.f90` | `fortplot_jpeg_fixed` | None | Fixed JPEG implementation |
| `test_jpeg_fixed_validation.f90` | `fortplot_jpeg_fixed` | None | Fixed JPEG validation |
| `test_jpeg_huffman.f90` | `fortplot_jpeg` | None | Huffman encoding |
| `test_jpeg_stb_byte_comparison.f90` | `fortplot_jpeg` | None | STB byte-level comparison |
| `test_jpeg_stb_validation.f90` | `fortplot_jpeg` | None | STB algorithm validation |
| `test_jpeg_ac_encoding.f90` | `fortplot_jpeg` | None | AC coefficient encoding |
| `test_jpeg_bit_perfect.f90` | `fortplot_jpeg` | `test_our.jpg` | Bit-perfect JPEG generation |
| `test_stb_bit_validation.f90` | `fortplot_jpeg` | None | STB bit writer validation |
| `test_stb_jpeg_validation.f90` | `fortplot_jpeg` | `test_stb_structure.jpg` | STB JPEG structure validation |

### Text and Font Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_stb_truetype.f90` | `fortplot_stb_truetype` | None | TrueType font rendering |
| `test_text_rendering.f90` | `fortplot_text` | None | Text rendering functionality |
| `test_text_centering.f90` | `fortplot_text` | None | Text centering algorithms |
| `test_text_width_measurements.f90` | `fortplot_text` | None | Text width calculations |
| `test_minimal_vertical_text.f90` | `fortplot_text` | None | Vertical text rendering |
| `test_y_label_orientation.f90` | `fortplot_text` | None | Y-axis label orientation |

### Layout and Positioning Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_axis_range_verification.f90` | `fortplot_axes` | None | Axis range calculations |
| `test_centered_label_positioning.f90` | `fortplot_label_positioning` | PNG files | Centered label positioning |
| `test_consistent_label_spacing.f90` | `fortplot_layout` | None | Label spacing consistency |
| `test_consistent_tick_digits.f90` | `fortplot_ticks` | PNG files | Tick digit formatting |
| `test_consistent_tick_formatting.f90` | `fortplot_ticks` | None | Tick format consistency |
| `test_coordinate_fix_final.f90` | `fortplot_layout` | None | Coordinate system fixes |
| `test_fine_tuned_positioning.f90` | `fortplot_layout` | PNG files | Fine-tuned element positioning |
| `test_label_positioning.f90` | `fortplot_label_positioning` | None | Label positioning logic |
| `test_matplotlib_margins.f90` | `fortplot_margins` | None | Matplotlib-compatible margins |
| `test_matplotlib_text_positioning.f90` | `fortplot_text` | None | Matplotlib text positioning |
| `test_separate_label_positioning.f90` | `fortplot_label_positioning` | None | Independent label positioning |
| `test_tick_label_positioning.f90` | `fortplot_ticks` | None | Tick label placement |
| `test_tick_labels.f90` | `fortplot_ticks` | None | Tick label generation |
| `test_tick_label_spacing.f90` | `fortplot_ticks` | None | Tick label spacing |
| `test_tick_vs_axis_labels.f90` | `fortplot_ticks` | None | Tick vs axis label interaction |
| `test_ticks_refactored.f90` | `fortplot_ticks` | None | Refactored tick system |
| `test_ylabel_positioning.f90` | `fortplot_label_positioning` | None | Y-label positioning |
| `test_visual_boundaries.f90` | `fortplot_layout` | None | Visual boundary calculations |

### Scale and Transform Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_log_scale_ticks.f90` | `fortplot_scales` | None | Logarithmic scale ticks |
| `test_log_symlog_ticks.f90` | `fortplot_scales` | PNG files | Log and symlog tick generation |
| `test_logarithmic_working.f90` | `fortplot_scales` | None | Logarithmic functionality |
| `test_scale_transforms.f90` | `fortplot_scales` | None | Scale transformations |
| `test_scaling.f90` | `fortplot_scales` | None | General scaling operations |
| `test_symlog_bounds.f90` | `fortplot_scales` | None | Symlog scale boundaries |
| `test_nice_tick_locations.f90` | `fortplot_ticks` | None | Nice tick value selection |

### Plot Type Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_scatter_behavior.f90` | `fortplot_figure_core` | PNG files | Scatter plot behavior |
| `test_scatter_marker_only.f90` | `fortplot_figure_core` | None | Marker-only scatter plots |
| `test_mixed_marker_line.f90` | `fortplot_figure_core` | None | Combined marker and line plots |
| `test_antialiased_markers.f90` | `fortplot_markers` | PNG files | Antialiased marker rendering |
| `test_edge_cases.f90` | Multiple modules | None | Edge case handling |

### Legend Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_legend.f90` | `fortplot_legend` | Multiple PNG/PDF/TXT | Basic legend functionality |
| `test_legend_box_improvements.f90` | `fortplot_legend` | PNG files | Legend box rendering |
| `test_legend_systematic_sizing.f90` | `fortplot_legend` | PNG files | Legend sizing algorithms |

### Advanced Plot Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_pcolormesh.f90` | `fortplot_pcolormesh` | None | Pseudocolor mesh plots |
| `test_streamplot.f90` | `fortplot_streamline` | None | Stream plot functionality |
| `test_streamplot_circles.f90` | `fortplot_streamline` | PNG files | Circular flow streamplots |
| `test_streamline_density.f90` | `fortplot_streamline` | None | Streamline density control |
| `test_streamline_integration.f90` | `fortplot_streamline_integrator` | None | Streamline integration |
| `test_streamline_placement.f90` | `fortplot_streamline_placement` | None | Streamline placement algorithms |
| `test_streamline_rendering.f90` | `fortplot_streamline` | None | Streamline rendering |
| `test_streamline_termination.f90` | `fortplot_streamline` | None | Streamline termination logic |
| `test_dopri5_integration.f90` | `fortplot_streamline_integrator` | None | DOPRI5 integrator |
| `test_arrow_placement.f90` | `fortplot_streamline` | None | Arrow placement on streamlines |
| `test_circle_closure.f90` | `fortplot_streamline` | PNG files | Circular streamline closure |

### Animation Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_animation_save.f90` | `fortplot_animation` | MP4 (temporary) | Animation saving to video |

### Comparison Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `test_matplotlib_comparison.f90` | Multiple modules | None | Matplotlib compatibility |
| `test_matplotlib_equivalence.f90` | `fortplot_streamline` | PNG files | Matplotlib equivalence verification |

### Debug Tests

| Test File | Module Coverage | Output Files | Description |
|-----------|----------------|--------------|-------------|
| `debug_bitmap_visual.f90` | `fortplot_text` | None | Visual debugging for bitmaps |
| `debug_stb_positioning.f90` | `fortplot_stb_truetype` | PNG files | STB positioning debug |
| `debug_symlog_data.f90` | `fortplot_scales` | None | Symlog data debugging |
| `test_debug_axis_labels.f90` | `fortplot_axes` | None | Axis label debugging |
| `test_debug_corner_cases.f90` | Multiple modules | PNG files | Corner case debugging |
| `test_debug_text_width.f90` | `fortplot_text` | None | Text width debugging |

## Running Tests

### Run all tests:
```bash
make test
```

### Run specific test:
```bash
make test ARGS="--target test_figure_basics"
```

### Run tests with coverage:
```bash
make test-coverage
make coverage  # Generate HTML report
```

## Output File Locations

Tests that create output files typically write to:
- `test/` directory for PNG, PDF, TXT files
- Temporary files (like MP4s) are usually cleaned up after testing

## Notes

- Most unit tests do not create output files - they test internal logic only
- Visual tests create PNG/PDF files for manual inspection
- Animation tests create and delete video files during testing
- JPEG tests extensively validate against STB reference implementation
- Coverage reports exclude test files themselves from metrics

## Module Coverage Summary

Based on `make coverage-report` output, here's the current test coverage for each module:

| Module | Line Coverage | Function Coverage | Primary Test Files |
|--------|--------------|-------------------|-------------------|
| `fortplot.f90` | 14.9% | 27.3% | `test_public_api.f90` |
| `fortplot_animation.f90` | 63.6% | 63.9% | `test_animation_save.f90` |
| `fortplot_ascii.f90` | 80.3% | 81.0% | `test_ascii.f90` |
| `fortplot_colormap.f90` | 0.0% | 0.0% | *(needs tests)* |
| `fortplot_context.f90` | 100% | 100% | Multiple tests |
| `fortplot_figure_core.f90` | 48.0% | 57.4% | `test_figure_basics.f90`, `test_figure_labels.f90`, `test_scatter_*.f90` |
| `fortplot_format_parser.f90` | 100% | 100% | `test_format_parser.f90` |
| `fortplot_jpeg.f90` | 76.9% | 70.7% | `test_jpeg_*.f90` (extensive test suite) |
| `fortplot_jpeg_fixed.f90` | 93.1% | 86.4% | `test_jpeg_fixed_*.f90` |
| `fortplot_label_positioning.f90` | 81.2% | 100% | `test_*label_positioning.f90` |
| `fortplot_layout.f90` | 88.9% | 33.3% | Layout-related tests |
| `fortplot_legend.f90` | 77.1% | 57.1% | `test_legend*.f90` |
| `fortplot_legend_layout.f90` | 74.2% | 66.7% | `test_legend_*.f90` |
| `fortplot_margins.f90` | 56.5% | 50.0% | `test_matplotlib_margins.f90` |
| `fortplot_markers.f90` | 62.5% | 100% | `test_antialiased_markers.f90` |
| `fortplot_pcolormesh.f90` | 34.7% | 25.0% | `test_pcolormesh.f90` |
| `fortplot_pdf.f90` | 65.4% | 67.9% | Tests using PDF output |
| `fortplot_pipe.f90` | 100% | 100% | Animation tests |
| `fortplot_png.f90` | 64.7% | 66.7% | `test_bitmap_*.f90` |
| `fortplot_raster.f90` | 64.5% | 62.7% | Backend tests |
| `fortplot_scales.f90` | 60.0% | 62.5% | `test_*scale*.f90`, `test_log*.f90` |
| `fortplot_stb_truetype.f90` | 71.4% | 81.8% | `test_stb_truetype.f90` |
| `fortplot_streamline.f90` | 67.1% | 53.8% | `test_streamline_*.f90` |
| `fortplot_streamline_integrator.f90` | 79.8% | 66.7% | `test_dopri5_integration.f90` |
| `fortplot_streamline_placement.f90` | 94.3% | 73.3% | `test_streamline_placement.f90` |
| `fortplot_streamplot_matplotlib.f90` | 78.3% | 72.7% | `test_matplotlib_*.f90` |
| `fortplot_text.f90` | 46.0% | 60.0% | `test_text_*.f90` |
| `fortplot_ticks.f90` | 93.3% | 95.5% | `test_tick*.f90` |
| `fortplot_utils.f90` | 85.2% | 100% | Various tests |
| `fortplot_vector.f90` | 96.7% | 71.4% | Streamline tests |
| `fortplot_zlib.f90` | 96.8% | 100% | PNG tests |

### Coverage Highlights
- **Well tested (>90%)**: `fortplot_context`, `fortplot_format_parser`, `fortplot_jpeg_fixed`, `fortplot_ticks`, `fortplot_vector`, `fortplot_zlib`
- **Good coverage (70-90%)**: `fortplot_ascii`, `fortplot_jpeg`, `fortplot_label_positioning`, `fortplot_streamline_placement`
- **Needs improvement (<50%)**: `fortplot` (main API), `fortplot_colormap`, `fortplot_pcolormesh`, `fortplot_text`