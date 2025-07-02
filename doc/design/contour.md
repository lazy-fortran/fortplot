# Contour Plots Implementation Design

## Overview

This document describes the matplotlib-compatible contour plotting implementation in fortplotlib, covering contour line generation, filled contours, automatic level calculation, and contour labeling. The implementation follows matplotlib's ContourSet architecture while leveraging fortplotlib's existing marching squares algorithm and colormap system.

## Problem Statement

The current contour implementation has several limitations:
1. **No contour labeling** - Missing text labels on contour lines for value identification
2. **Basic level generation** - Simple level calculation without optimal spacing algorithms
3. **No path optimization** - Contour paths lack smoothing and optimization for clean rendering
4. **Missing colorbar support** - No legend system for filled contour color scales
5. **Limited label placement** - No intelligent label positioning or collision detection
6. **No triangular grids** - Only supports regular rectangular grids

## Solution: Matplotlib-Compatible Contour System

### Architecture

The solution consists of six main components:

1. **Enhanced ContourSet class** - Complete contour storage and management system
2. **Automatic level generation** - Optimal contour level calculation algorithms
3. **Contour labeling system** - Intelligent text placement with collision detection
4. **Path optimization** - Contour line smoothing and rendering optimization
5. **Colorbar integration** - Color scale legends for filled contours
6. **Triangular grid support** - Unstructured mesh contour generation

### Key Components

#### 1. Enhanced ContourSet Architecture

Following matplotlib's ContourSet class in `contour.py` (lines 552-1456):

```fortran
type :: contour_set_t
    ! Contour identification
    character(len=50) :: label = ''             ! Dataset label
    logical :: filled = .false.                 ! Line vs filled contours
    
    ! Grid data (regular grid support)
    real(wp), allocatable :: x(:), y(:)         ! 1D coordinate arrays
    real(wp), allocatable :: z(:,:)             ! 2D data grid (z(ny, nx))
    integer :: nx, ny                           ! Grid dimensions
    
    ! Contour levels and colors
    real(wp), allocatable :: levels(:)          ! Contour level values
    type(color_t), allocatable :: colors(:)     ! Level colors
    type(colormap_t) :: cmap                    ! Colormap for filled contours
    type(normalization_t) :: norm               ! Value normalization
    
    ! Generated contour paths
    type(contour_path_t), allocatable :: paths(:)    ! All contour paths
    integer :: n_paths = 0
    
    ! Labeling system
    type(contour_label_t), allocatable :: labels(:)  ! Text labels
    logical :: label_inline = .true.            ! Inline vs separate labels
    character(len=20) :: label_format = '%g'    ! Printf-style format
    
    ! Rendering properties
    real(wp) :: linewidth = 1.0
    character(len=20) :: linestyle = '-'
    real(wp) :: alpha = 1.0
contains
    procedure :: generate_contours
    procedure :: add_labels
    procedure :: optimize_paths
    procedure :: get_level_color
end type

type :: contour_path_t
    real(wp), allocatable :: vertices(:,:)      ! Path vertices (2, n_vertices)
    integer, allocatable :: codes(:)            ! Path codes (MOVETO, LINETO, etc.)
    real(wp) :: level                           ! Contour level value
    logical :: is_closed = .false.              ! Closed vs open path
    type(bbox_t) :: bounds                      ! Path bounding box
end type

type :: contour_label_t
    character(len=32) :: text = ''              ! Label text
    real(wp) :: x, y                           ! Label position
    real(wp) :: rotation = 0.0                 ! Text rotation angle
    type(font_properties_t) :: font             ! Font properties
    logical :: inline = .true.                  ! Inline vs external label
end type
```

#### 2. Automatic Level Generation

Following matplotlib's level generation in `contour.py` (lines 964-1005):

```fortran
type :: level_generator_t
    integer :: max_levels = 10                  ! Maximum number of levels
    logical :: symmetric = .false.              ! Symmetric around zero
    real(wp) :: extend_factor = 0.05           ! Extension beyond data range
contains
    procedure :: auto_levels                    ! Automatic level calculation
    procedure :: nice_levels                    ! "Nice" level spacing
    procedure :: log_levels                     ! Logarithmic level spacing
end type

function auto_levels(generator, z_data, n_levels) result(levels)
    ! Implements matplotlib's MaxNLocator-style algorithm
    real(wp), intent(in) :: z_data(:,:)
    integer, intent(in), optional :: n_levels
    real(wp), allocatable :: levels(:)
    
    ! Calculate data range
    z_min = minval(z_data, mask=is_finite(z_data))
    z_max = maxval(z_data, mask=is_finite(z_data))
    
    if (present(n_levels)) then
        target_levels = n_levels
    else
        target_levels = generator%max_levels
    end if
    
    ! Generate "nice" level spacing
    range_val = z_max - z_min
    if (range_val == 0.0) then
        levels = [z_min]
        return
    end if
    
    ! Find optimal step size (matplotlib MaxNLocator algorithm)
    log_range = log10(range_val)
    step_size = 10.0_wp**(floor(log_range))
    
    ! Refine step size for target number of levels
    nice_steps = [1.0_wp, 2.0_wp, 2.5_wp, 5.0_wp, 10.0_wp]
    do i = 1, size(nice_steps)
        test_step = step_size * nice_steps(i)
        n_steps = ceiling(range_val / test_step)
        if (n_steps <= target_levels) then
            final_step = test_step
            exit
        end if
    end do
    
    ! Generate level array
    level_min = floor(z_min / final_step) * final_step
    level_max = ceil(z_max / final_step) * final_step
    
    n_actual = nint((level_max - level_min) / final_step) + 1
    allocate(levels(n_actual))
    
    do i = 1, n_actual
        levels(i) = level_min + (i - 1) * final_step
    end do
    
    ! Filter levels within data range
    levels = pack(levels, levels >= z_min .and. levels <= z_max)
end function
```

#### 3. Contour Labeling System

Following matplotlib's ContourLabeler in `contour.py` (lines 58-501):

```fortran
type :: contour_labeler_t
    real(wp) :: label_spacing = 10.0            ! Minimum spacing between labels (points)
    logical :: smart_placement = .true.         ! Use intelligent placement
    real(wp) :: straightness_threshold = 0.2    ! Path straightness for labeling
    integer :: max_labels_per_contour = 5       ! Limit labels per contour
contains
    procedure :: locate_label_positions
    procedure :: calculate_label_rotation
    procedure :: check_label_collision
    procedure :: format_label_text
end type

subroutine locate_label_positions(labeler, contour_path, label_positions)
    ! Implements matplotlib's locate_label algorithm
    type(contour_path_t), intent(in) :: contour_path
    type(label_position_t), allocatable :: label_positions(:)
    
    ! Find straight segments suitable for labeling
    call find_straight_segments(contour_path, segments)
    
    ! Score segments by suitability
    do i = 1, size(segments)
        segments(i)%score = calculate_segment_score(segments(i))
    end do
    
    ! Sort by score and select best positions
    call sort_segments_by_score(segments)
    
    ! Place labels with spacing constraints
    allocate(label_positions(0))
    last_position = -huge(1.0_wp)
    
    do i = 1, size(segments)
        if (segments(i)%score > labeler%straightness_threshold) then
            position = segments(i)%center_position
            if (abs(position - last_position) > labeler%label_spacing) then
                label_positions = [label_positions, create_label_position(segments(i))]
                last_position = position
                if (size(label_positions) >= labeler%max_labels_per_contour) exit
            end if
        end if
    end do
end subroutine

function calculate_segment_score(segment) result(score)
    ! Score based on straightness and length
    type(path_segment_t), intent(in) :: segment
    real(wp) :: score
    
    ! Calculate path curvature
    curvature = calculate_path_curvature(segment%vertices)
    
    ! Calculate segment length
    segment_length = calculate_path_length(segment%vertices)
    
    ! Combine factors (straight, long segments score higher)
    score = (1.0 - curvature) * min(segment_length / 20.0, 1.0)
end function

subroutine calculate_label_rotation(path_segment, rotation_angle)
    ! Calculate text rotation to follow contour direction
    real(wp), intent(in) :: path_segment(:,:)   ! Segment vertices
    real(wp), intent(out) :: rotation_angle
    
    ! Calculate tangent direction at segment center
    n_vertices = size(path_segment, 2)
    center_idx = n_vertices / 2
    
    if (center_idx > 1 .and. center_idx < n_vertices) then
        dx = path_segment(1, center_idx + 1) - path_segment(1, center_idx - 1)
        dy = path_segment(2, center_idx + 1) - path_segment(2, center_idx - 1)
        
        rotation_angle = atan2(dy, dx) * 180.0 / pi
        
        ! Keep text readable (never upside down)
        if (abs(rotation_angle) > 90.0) then
            rotation_angle = rotation_angle + sign(180.0_wp, -rotation_angle)
        end if
    else
        rotation_angle = 0.0
    end if
end subroutine
```

#### 4. Path Optimization and Smoothing

Following matplotlib's path optimization strategies:

```fortran
type :: path_optimizer_t
    real(wp) :: smoothing_factor = 0.1          ! Path smoothing strength
    real(wp) :: simplification_tolerance = 0.5  ! Douglas-Peucker tolerance
    logical :: remove_artifacts = .true.        ! Remove marching squares artifacts
contains
    procedure :: smooth_path
    procedure :: simplify_path
    procedure :: remove_marching_artifacts
end type

subroutine smooth_path(optimizer, path, smoothed_path)
    ! Apply smoothing to reduce marching squares artifacts
    type(contour_path_t), intent(in) :: path
    type(contour_path_t), intent(out) :: smoothed_path
    
    if (size(path%vertices, 2) < 3) then
        smoothed_path = path
        return
    end if
    
    ! Apply moving average smoothing
    allocate(smoothed_path%vertices(2, size(path%vertices, 2)))
    smoothed_path%codes = path%codes
    smoothed_path%level = path%level
    
    ! Smooth interior vertices (preserve endpoints)
    smoothed_path%vertices(:, 1) = path%vertices(:, 1)
    
    do i = 2, size(path%vertices, 2) - 1
        alpha = optimizer%smoothing_factor
        smoothed_path%vertices(:, i) = &
            (1.0 - 2.0*alpha) * path%vertices(:, i) + &
            alpha * (path%vertices(:, i-1) + path%vertices(:, i+1))
    end do
    
    smoothed_path%vertices(:, size(path%vertices, 2)) = &
        path%vertices(:, size(path%vertices, 2))
end subroutine

subroutine simplify_path(optimizer, path, simplified_path)
    ! Douglas-Peucker line simplification
    type(contour_path_t), intent(in) :: path
    type(contour_path_t), intent(out) :: simplified_path
    
    call douglas_peucker_simplify(path%vertices, optimizer%simplification_tolerance, &
                                 simplified_vertices, keep_indices)
    
    simplified_path%vertices = simplified_vertices
    simplified_path%codes = path%codes(keep_indices)
    simplified_path%level = path%level
    simplified_path%is_closed = path%is_closed
end subroutine
```

#### 5. Enhanced Marching Squares Integration

Building on fortplotlib's existing implementation in `fortplot_figure_core.f90`:

```fortran
subroutine generate_contour_paths(cs, grid_x, grid_y, grid_z, levels)
    ! Enhanced version of existing marching squares algorithm
    type(contour_set_t), intent(inout) :: cs
    real(wp), intent(in) :: grid_x(:), grid_y(:), grid_z(:,:)
    real(wp), intent(in) :: levels(:)
    
    allocate(cs%paths(0))
    cs%n_paths = 0
    
    ! Generate contours for each level
    do level_idx = 1, size(levels)
        current_level = levels(level_idx)
        
        ! Trace contour using existing marching squares
        call trace_contour_level(grid_x, grid_y, grid_z, current_level, level_paths)
        
        ! Add paths to contour set
        do path_idx = 1, size(level_paths)
            level_paths(path_idx)%level = current_level
            cs%paths = [cs%paths, level_paths(path_idx)]
            cs%n_paths = cs%n_paths + 1
        end do
    end do
    
    ! Apply path optimization
    if (cs%optimize_paths_flag) then
        call cs%optimize_paths()
    end if
end subroutine

subroutine trace_contour_level(x, y, z, level, paths)
    ! Wrapper around existing marching squares implementation
    ! Converts fortplotlib's current algorithm to path objects
    
    ! Use existing marching squares algorithm
    call generate_contour_lines(x, y, z, level, line_segments)
    
    ! Convert line segments to connected paths
    call connect_line_segments(line_segments, connected_paths)
    
    ! Convert to path objects
    allocate(paths(size(connected_paths)))
    do i = 1, size(connected_paths)
        call convert_to_path_object(connected_paths(i), paths(i))
    end do
end subroutine
```

#### 6. Colorbar Integration

Following matplotlib's colorbar system:

```fortran
type :: colorbar_t
    type(contour_set_t), pointer :: parent_contours => null()
    type(axes_t), pointer :: axes => null()       ! Colorbar axes
    
    ! Appearance
    character(len=20) :: orientation = 'vertical'  ! 'vertical', 'horizontal'
    real(wp) :: fraction = 0.15                   ! Fraction of parent axes
    real(wp) :: pad = 0.05                        ! Padding from parent axes
    character(len=20) :: aspect = 'auto'          ! Aspect ratio
    
    ! Ticks and labels
    type(tick_locator_t) :: tick_locator
    type(tick_formatter_t) :: tick_formatter
    character(len=100) :: label = ''              ! Colorbar label
contains
    procedure :: create_colorbar_axes
    procedure :: draw_color_gradient
    procedure :: add_tick_labels
end type

subroutine create_colorbar_for_contours(contours, colorbar)
    type(contour_set_t), intent(in) :: contours
    type(colorbar_t), intent(out) :: colorbar
    
    ! Link to parent contours
    colorbar%parent_contours => contours
    
    ! Create colorbar axes
    call colorbar%create_colorbar_axes(contours%parent_axes)
    
    ! Draw color gradient
    call colorbar%draw_color_gradient(contours%levels, contours%colors)
    
    ! Add tick labels
    call colorbar%add_tick_labels(contours%levels)
end subroutine
```

### Implementation Details

#### Integration with Existing Fortplotlib Code

Building on current strengths while adding missing functionality:

```fortran
! Enhanced API maintaining backward compatibility
subroutine add_contour_enhanced(fig, x, y, z, levels, colors, labels)
    type(figure_t), intent(inout) :: fig
    real(wp), intent(in) :: x(:), y(:), z(:,:)
    real(wp), intent(in), optional :: levels(:)
    type(color_t), intent(in), optional :: colors(:)
    logical, intent(in), optional :: labels
    
    type(contour_set_t) :: contour_set
    
    ! Generate levels if not provided
    if (present(levels)) then
        contour_set%levels = levels
    else
        contour_set%levels = auto_generate_levels(z, 10)
    end if
    
    ! Generate contour paths using existing algorithm
    call contour_set%generate_contours(x, y, z)
    
    ! Add labels if requested
    if (present(labels) .and. labels) then
        call contour_set%add_labels()
    end if
    
    ! Store in figure (maintain existing storage system)
    call fig%add_contour_set(contour_set)
end subroutine
```

#### Performance Optimizations

Following matplotlib's optimization strategies:

```fortran
! Level filtering for performance
function filter_levels_in_range(levels, z_min, z_max) result(filtered_levels)
    ! Only compute contours that intersect data range
    real(wp), intent(in) :: levels(:), z_min, z_max
    real(wp), allocatable :: filtered_levels(:)
    
    filtered_levels = pack(levels, levels >= z_min .and. levels <= z_max)
end function

! Path caching for re-rendering
type :: contour_cache_t
    real(wp), allocatable :: cached_levels(:)
    type(contour_path_t), allocatable :: cached_paths(:)
    logical :: is_valid = .false.
contains
    procedure :: invalidate, lookup_paths
end type
```

## Verification

### Test Cases

1. **Level generation** - Verify optimal level spacing matches matplotlib
2. **Path tracing** - Test contour path accuracy against reference data
3. **Label placement** - Verify intelligent positioning and collision detection
4. **Performance** - Benchmark contour generation speed and memory usage
5. **Visual comparison** - Compare output with matplotlib contour plots
6. **Edge cases** - Test degenerate grids and extreme data values

### Expected Results

**Before (current implementation):**
- Complete marching squares algorithm
- Basic contour line generation
- Colormap integration for filled contours
- Multi-backend rendering support

**After (matplotlib-compatible):**
- Automatic optimal level generation
- Professional contour labeling with intelligent placement
- Path optimization and smoothing for clean rendering
- Colorbar support for filled contour legends
- Complete matplotlib API compatibility
- Enhanced performance with level filtering and caching

## Performance Considerations

### Computational Complexity

- **Level generation**: O(1) - constant time for "nice" level calculation
- **Contour tracing**: O(nx × ny × n_levels) - linear in grid size and level count
- **Label placement**: O(n_paths × avg_path_length) - proportional to contour complexity
- **Path optimization**: O(n_vertices) - linear in path vertex count

### Memory Usage

- **Contour paths**: O(total_contour_length) - proportional to contour complexity
- **Level arrays**: O(n_levels) - minimal memory overhead
- **Label storage**: O(n_labels) - text and position information
- **Cache storage**: O(cached_paths) - optional performance enhancement

### Optimization Opportunities

1. **Level filtering** - Only compute contours within data range
2. **Spatial indexing** - Accelerate label collision detection
3. **Path simplification** - Reduce vertex count while preserving appearance
4. **Parallel processing** - Multi-threaded contour generation for large grids

## Future Enhancements

### Matplotlib Feature Parity

1. **Triangular grid support** - Unstructured mesh contour generation
2. **3D contours** - Extend to 3D surface contours and isosurfaces
3. **Animation support** - Dynamic contour plots with smooth transitions
4. **Interactive labeling** - User-controlled label placement and editing

### Advanced Features

1. **Adaptive refinement** - Automatic grid refinement for smooth contours
2. **Multi-resolution** - Level-of-detail contours for large datasets
3. **Vector field integration** - Streamlines combined with contours
4. **Scientific colormaps** - Perceptually uniform color schemes

## References

1. **Matplotlib contour**: `thirdparty/matplotlib/lib/matplotlib/contour.py` (lines 552-1456)
2. **Contour labeling**: `thirdparty/matplotlib/lib/matplotlib/contour.py` (lines 58-501)
3. **Level generation**: `thirdparty/matplotlib/lib/matplotlib/contour.py` (lines 964-1005)
4. **Triangular contours**: `thirdparty/matplotlib/lib/matplotlib/tri/_tricontour.py`
5. **Current implementation**: `src/fortplot_figure_core.f90` (lines 882-1138)
6. **pyplot-fortran**: `thirdparty/pyplot-fortran/src/pyplot_module.F90` (contour patterns)

## Implementation Files

- **`src/fortplot_contour_set.f90`** - Enhanced ContourSet class and management
- **`src/fortplot_level_generator.f90`** - Automatic level calculation algorithms
- **`src/fortplot_contour_labeler.f90`** - Intelligent label placement system
- **`src/fortplot_path_optimizer.f90`** - Contour path smoothing and optimization
- **`src/fortplot_colorbar.f90`** - Colorbar creation and management
- **`src/fortplot_contour_cache.f90`** - Performance caching system
- **`python/fortplotlib/fortplot.py`** - Enhanced Python contour interface
- **`test/test_contour_levels.f90`** - Level generation algorithm tests
- **`test/test_contour_labeling.f90`** - Label placement verification
- **`test/test_contour_performance.f90`** - Contour generation benchmarks

This implementation ensures fortplotlib provides matplotlib-compatible contour plotting with professional labeling, optimal level generation, path optimization, and colorbar support while building on the existing robust marching squares foundation for high-quality scientific visualization.