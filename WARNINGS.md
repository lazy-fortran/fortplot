# Compiler Warnings Documentation

This document tracks all compiler warnings in the fortplotlib codebase as of 2025-07-14.

## Summary

**Initial warnings**: ~50+  
**Fixed warnings**: 9 (as of 2025-07-14)  
**Remaining warnings**: ~40+

### Fixed
- ✅ Same actual argument warnings: 1 (fixed)
- ✅ Unused functions: 4 (fixed) 
- ✅ Unused variables: 4 (fixed)

### Remaining
- Unused dummy arguments: ~35 (mostly interface consistency)
- Unused variables: ~6
- Unused functions: 2 (large complex functions)

## Detailed Warning List

### fortplot_ascii.f90

#### Unused Dummy Arguments
- `ascii_set_marker_colors_with_alpha`: edge_alpha, edge_b, edge_g, edge_r, face_alpha, face_b, face_g, face_r
- `ascii_set_marker_colors`: edge_b, edge_g, edge_r, face_b, face_g, face_r  
- `ascii_set_line_width`: width
- `create_ascii_canvas`: width, height

#### Unused Variables
- Line 220: variables 'i', 'j' declared but not used

### fortplot_axes.f90

#### Unused Dummy Arguments
- `draw_single_tick`: is_x_axis, label, plot_area, screen_pos
- `clean_scientific_notation`: str
- `add_positive_symlog_ticks`: data_max, lower_bound, num_ticks, tick_positions
- `add_linear_symlog_ticks`: lower_bound, num_ticks, tick_positions, upper_bound
- `add_negative_symlog_ticks`: data_min, num_ticks, tick_positions, upper_bound

#### Unused Variables
- Line 54: variable 'i' declared but not used

#### Unused Functions
- `clean_scientific_notation`
- `remove_trailing_zeros`
- `draw_single_tick`

### fortplot_text.f90

#### Unused Dummy Arguments
- `render_character_bitmap`: b, g, r
- `render_simple_character_block`: b, g, r
- `calculate_text_height`: text

#### Unused Functions
- `render_simple_character_block`
- `render_character_bitmap`

### fortplot_ticks.f90

#### Unused Variables
- Line 601: variable 'exponent' declared but not used
- Line 599: variable 'temp_format' declared but not used
- Line 527: variable 'j' declared but not used
- Line 421: variable 'i' declared but not used
- Line 282: variables 'actual_num_ticks', 'i', 'max_power', 'min_power', 'power', 'tick_value' declared but not used

#### Unused Functions
- `format_tick_value_consistent`

### fortplot_pdf.f90

#### Same Actual Argument Warning
- Line 518: Same actual argument associated with INTENT(IN) argument 'tick_y' and INTENT(OUT) argument 'label_y'

#### Unused Dummy Arguments
- `pdf_set_marker_colors_with_alpha`: edge_alpha, edge_b, edge_g, edge_r, face_alpha, face_b, face_g, face_r
- `pdf_set_marker_colors`: edge_b, edge_g, edge_r, face_b, face_g, face_r
- (many more truncated in output)

## Categories of Warnings

### 1. Placeholder Functions
Many functions have unused parameters because they are placeholder implementations for features not yet implemented (e.g., color support in ASCII backend).

### 2. Interface Consistency
Some functions have unused parameters to maintain consistent interfaces across different backends.

### 3. Dead Code
Some functions and variables appear to be leftovers from refactoring or incomplete features.

### 4. Aliasing Issues
The same actual argument warning indicates a potential bug where the same variable is used for both input and output.