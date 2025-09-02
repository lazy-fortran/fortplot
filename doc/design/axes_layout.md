title: Axes and Layout Design
---

# Axes and Layout Implementation Design

## Overview

This document describes the matplotlib-compatible axes and layout management system in fortplot, covering coordinate systems, axis scaling, tick generation, spine rendering, and layout optimization. The implementation follows matplotlib's Axes class architecture and transform system patterns while maintaining Fortran's performance characteristics.

## Problem Statement

The current axes and layout implementation has several limitations:
1. **Simplified coordinate system** - No transform hierarchy or coordinate space separation
2. **Limited subplot support** - Single plot focus without axes management
3. **Basic layout system** - Fixed margins without tight layout or optimization
4. **No spine management** - Simple rectangular frame only
5. **Direct backend coupling** - No unified coordinate transform system
6. **Missing axis sharing** - No shared axes or synchronized limits

## Solution: Matplotlib-Compatible Axes and Layout

### Architecture

The solution consists of six main components:

1. **Transform system** - Hierarchical coordinate transformations
2. **Axes class** - Complete axes management with data and view limits
3. **Spine system** - Independent axis boundary positioning
4. **Layout engine** - Tight layout and margin optimization
5. **Tick system** - Advanced tick generation and formatting
6. **Scale integration** - Unified scaling with coordinate transforms

### Key Components

#### 1. Transform System

Following matplotlib's transform hierarchy in `transforms.py` (lines 84-100):

```fortran
type, abstract :: transform_t
    logical :: is_affine = .false.
    logical :: is_separable = .false.
    integer :: input_dims = 2, output_dims = 2
    logical :: is_dirty = .true.
contains
    procedure(transform_interface), deferred :: transform_non_affine
    procedure(transform_interface), deferred :: transform_affine
    procedure :: transform_bbox
    procedure :: invalidate
end type

type :: composite_transform_t
    class(transform_t), allocatable :: transform_a, transform_b
contains
    procedure :: transform_non_affine => composite_transform_non_affine
    procedure :: transform_affine => composite_transform_affine
end type

! Core transforms following matplotlib pattern
type :: axes_transform_t
    type(bbox_t) :: axes_bbox           ! Axes position in figure coordinates
    type(bbox_t) :: data_limits         ! Current view limits in data coordinates
    type(scale_transform_t) :: xscale_transform, yscale_transform
contains
    procedure :: data_to_display        ! Data coordinates → display pixels
    procedure :: display_to_data        ! Display pixels → data coordinates
    procedure :: data_to_axes          ! Data coordinates → axes coordinates (0-1)
end type
```

#### 2. Enhanced Axes Class

Following matplotlib's Axes class in `_base.py` (lines 928-963):

```fortran
type :: axes_t
    ! Identity and parent figure
    type(figure_t), pointer :: figure => null()
    integer :: index = 0                     ! Axes index in figure
    character(len=64) :: label = ''          ! Optional axes label
    
    ! Position and geometry
    type(bbox_t) :: position                 ! Axes position in figure coordinates
    logical :: aspect_auto = .true.          ! Automatic aspect ratio
    real(wp) :: aspect_ratio = 1.0          ! Fixed aspect ratio if not auto
    
    ! Coordinate systems (matplotlib pattern)
    type(axes_transform_t) :: transAxes     ! Axes coordinates (0-1)
    type(axes_transform_t) :: transData     ! Data coordinates
    type(scale_transform_t) :: transScale   ! Scale transformation
    type(axes_transform_t) :: transLimits   ! Limit transformation
    
    ! Data and view limits
    type(bbox_t) :: dataLim                 ! Bounding box of all data
    type(bbox_t) :: viewLim                 ! Current view limits
    logical :: autoscale_x = .true., autoscale_y = .true.
    
    ! Axis objects
    type(xaxis_t) :: xaxis
    type(yaxis_t) :: yaxis
    
    ! Spine container (matplotlib pattern)
    type(spine_t) :: spines(4)              ! left, right, top, bottom
    character(len=10) :: spine_names(4) = ['left  ', 'right ', 'top   ', 'bottom']
    
    ! Scale settings
    character(len=20) :: xscale = 'linear'   ! 'linear', 'log', 'symlog'
    character(len=20) :: yscale = 'linear'
    type(scale_t), allocatable :: xscale_obj, yscale_obj
    
    ! Grid and appearance
    logical :: grid_on = .false.
    character(len=20) :: grid_color = 'lightgray'
    character(len=20) :: grid_linestyle = '-'
    real(wp) :: grid_alpha = 0.5
    
    ! Shared axes (matplotlib pattern)
    type(axes_t), pointer :: sharex => null(), sharey => null()
    logical :: sharex_group = .false., sharey_group = .false.
contains
    procedure :: set_xlim, set_ylim
    procedure :: set_xscale, set_yscale
    procedure :: autoscale_view
    procedure :: add_line, add_collection
    procedure :: grid
    procedure :: relim                       ! Recalculate data limits
end type
```

#### 3. Spine System

Following matplotlib's spine implementation in `spines.py` (lines 14-100):

```fortran
type :: spine_t
    character(len=10) :: spine_type = 'linear'  ! 'linear', 'arc', 'circle'
    character(len=10) :: position_type = 'outward' ! 'outward', 'data', 'axes'
    real(wp) :: position_value = 0.0            ! Position parameter
    logical :: visible = .true.
    
    ! Appearance
    character(len=20) :: color = 'black'
    real(wp) :: linewidth = 1.0
    character(len=20) :: linestyle = '-'
    
    ! Bounds (matplotlib smart_bounds)
    logical :: smart_bounds = .false.
    real(wp) :: bounds(2) = [0.0_wp, 1.0_wp]   ! Start, end positions
    
    ! Parent axes
    type(axes_t), pointer :: axes => null()
contains
    procedure :: set_position                    ! Set spine position
    procedure :: set_bounds                      ! Set spine bounds
    procedure :: get_spine_transform            ! Get positioning transform
    procedure :: draw                           ! Render spine
end type

subroutine set_spine_position(spine, position_type, amount)
    ! Implements matplotlib's spine positioning
    select case (trim(position_type))
    case ('outward')
        ! Position spine outward by 'amount' points
        spine%position_type = 'outward'
        spine%position_value = amount
    case ('data')
        ! Position spine at data coordinate 'amount'
        spine%position_type = 'data'
        spine%position_value = amount
    case ('axes')
        ! Position spine at axes coordinate 'amount' (0-1)
        spine%position_type = 'axes'
        spine%position_value = amount
    case ('zero')
        ! Convenience for data position at 0
        spine%position_type = 'data'
        spine%position_value = 0.0_wp
    end select
end subroutine
```

#### 4. Layout Engine

Following matplotlib's layout engine in `layout_engine.py` (lines 32-100):

```fortran
type, abstract :: layout_engine_t
contains
    procedure(layout_interface), deferred :: execute
end type

type :: tight_layout_engine_t
    real(wp) :: pad = 1.08                   ! Padding around axes (inches)
    real(wp) :: h_pad = 0.0, w_pad = 0.0    ! Height/width padding
    logical :: rect_specified = .false.      ! Use custom rect
    real(wp) :: rect(4) = [0.0, 0.0, 1.0, 1.0] ! left, bottom, right, top
contains
    procedure :: execute => tight_layout_execute
end type

subroutine tight_layout_execute(engine, fig, renderer)
    ! Implements matplotlib's tight_layout algorithm
    
    ! 1. Calculate required margins for all text elements
    call calculate_text_margins(fig, text_margins)
    
    ! 2. Determine optimal subplot spacing
    call optimize_subplot_spacing(fig, text_margins, spacing)
    
    ! 3. Calculate axes positions to minimize overlap
    call calculate_axes_positions(fig, spacing, new_positions)
    
    ! 4. Update axes positions
    do i = 1, size(fig%axes)
        fig%axes(i)%position = new_positions(i)
        call update_axes_transform(fig%axes(i))
    end do
end subroutine
```

#### 5. Advanced Tick System

Following matplotlib's tick system in `ticker.py` and `axis.py`:

```fortran
type :: tick_t
    real(wp) :: position                     ! Tick position in data coordinates
    character(len=32) :: label = ''          ! Tick label text
    logical :: major = .true.                ! Major or minor tick
    logical :: visible = .true.
    
    ! Visual properties
    real(wp) :: size = 4.0                   ! Tick length in points
    character(len=20) :: color = 'black'
    real(wp) :: width = 1.0                  ! Tick line width
    
    ! Grid line properties
    logical :: grid_on = .false.
    character(len=20) :: grid_color = 'lightgray'
    character(len=20) :: grid_linestyle = '-'
    real(wp) :: grid_alpha = 0.5
end type

type :: axis_t
    character(len=1) :: axis_name            ! 'x' or 'y'
    type(axes_t), pointer :: axes => null()
    
    ! Tick management
    type(tick_t), allocatable :: major_ticks(:), minor_ticks(:)
    class(locator_t), allocatable :: major_locator, minor_locator
    class(formatter_t), allocatable :: major_formatter, minor_formatter
    
    ! Scale integration
    class(scale_t), pointer :: scale => null()
    type(transform_t) :: axis_transform      ! Axis-specific transform
    
    ! Labels and appearance
    character(len=128) :: label_text = ''
    real(wp) :: label_pad = 4.0              ! Distance from axis (points)
    logical :: label_visible = .true.
contains
    procedure :: set_major_locator, set_minor_locator
    procedure :: set_major_formatter, set_minor_formatter
    procedure :: set_label, set_label_position
    procedure :: tick_update                 ! Update tick positions/labels
end type
```

#### 6. Scale Integration

Following matplotlib's scale system in `scale.py`:

```fortran
type, abstract :: scale_t
    character(len=20) :: name
contains
    procedure(scale_interface), deferred :: get_transform
    procedure(scale_interface), deferred :: set_default_locators_and_formatters
    procedure(scale_interface), deferred :: limit_range_for_scale
end type

type :: linear_scale_t
contains
    procedure :: get_transform => linear_get_transform
    procedure :: set_default_locators_and_formatters => linear_set_defaults
end type

type :: log_scale_t
    real(wp) :: base = 10.0
    logical :: nonpositive = .false.         ! How to handle non-positive values
contains
    procedure :: get_transform => log_get_transform
    procedure :: set_default_locators_and_formatters => log_set_defaults
end type

subroutine register_scale_with_axis(axis, scale)
    ! Integrate scale with axis (matplotlib pattern)
    axis%scale => scale
    call scale%set_default_locators_and_formatters(axis)
    call axis%tick_update()
end subroutine
```

### Implementation Details

#### Coordinate System Integration

Following matplotlib's transform composition:

```fortran
subroutine setup_axes_transforms(axes)
    ! Create transform chain: data → display
    
    ! 1. Scale transform (data → linear space)
    call setup_scale_transform(axes%transScale, axes%xscale, axes%yscale)
    
    ! 2. Limits transform (scaled data → axes coordinates)
    call setup_limits_transform(axes%transLimits, axes%viewLim)
    
    ! 3. Axes transform (axes → figure coordinates)
    call setup_axes_transform(axes%transAxes, axes%position)
    
    ! 4. Composite transform (data → display)
    axes%transData = compose_transforms([axes%transScale, &
                                        axes%transLimits, &
                                        axes%transAxes])
end subroutine
```

#### Automatic Layout Calculation

Following matplotlib's tight_layout algorithm:

```fortran
subroutine calculate_optimal_layout(fig, axes_positions)
    ! 1. Measure text elements for all axes
    do i = 1, size(fig%axes)
        call measure_axis_text(fig%axes(i), text_extents(i))
    end do
    
    ! 2. Calculate required margins
    required_margins = calculate_margins_from_text(text_extents)
    
    ! 3. Optimize axes positioning
    call optimize_positions(fig%figsize, required_margins, axes_positions)
    
    ! 4. Validate and adjust for constraints
    call validate_positions(axes_positions, fig%constraints)
end subroutine
```

#### Shared Axes Implementation

Following matplotlib's shared axes system:

```fortran
subroutine setup_shared_axes(axes_array, sharex, sharey)
    ! Implement matplotlib's axis sharing behavior
    if (sharex == 'all' .or. sharex == 'col') then
        call create_shared_x_groups(axes_array, sharex)
    end if
    
    if (sharey == 'all' .or. sharey == 'row') then
        call create_shared_y_groups(axes_array, sharey)
    end if
end subroutine

subroutine sync_shared_limits(primary_axes, shared_axes, axis)
    ! Synchronize limits across shared axes group
    if (axis == 'x') then
        do i = 1, size(shared_axes)
            call shared_axes(i)%set_xlim(primary_axes%viewLim%x0, &
                                        primary_axes%viewLim%x1, emit=.false.)
        end do
    end if
end subroutine
```

## Verification

### Test Cases

1. **Transform system** - Verify coordinate transformations match matplotlib
2. **Spine positioning** - Test all spine position types and bounds
3. **Layout optimization** - Compare tight layout with matplotlib output
4. **Scale integration** - Test all scale types with proper tick generation
5. **Shared axes** - Verify limit synchronization and interaction
6. **Performance** - Benchmark coordinate transformations and layout

### Expected Results

**Before (current implementation):**
- Direct backend coordinate mapping
- Fixed rectangular axis frame
- Simple margin calculations
- Basic tick generation without scale integration
- Single plot focus

**After (matplotlib-compatible):**
- Hierarchical transform system with lazy evaluation
- Independent spine positioning and styling
- Automatic tight layout optimization
- Scale-integrated tick system with locators/formatters
- Complete multi-axes support with sharing

## Performance Considerations

### Computational Complexity

- **Transform updates**: O(n_points) for coordinate transformation
- **Layout calculation**: O(n_axes²) for overlap detection and optimization
- **Tick generation**: O(log(range)) for optimal tick placement
- **Spine rendering**: O(1) per spine (constant geometric operations)

### Memory Usage

- **Transform cache**: O(n_unique_transforms) - reuse common transforms
- **Tick storage**: O(n_major_ticks + n_minor_ticks) per axis
- **Layout cache**: O(n_axes) - cached position calculations
- **Coordinate arrays**: O(n_points) - data coordinate storage

### Optimization Opportunities

1. **Transform caching** - Cache composed transforms for reuse
2. **Lazy evaluation** - Update transforms only when needed
3. **Vectorized operations** - SIMD coordinate transformations
4. **Layout memoization** - Cache layout calculations for unchanged figures

## Future Enhancements

### Matplotlib Feature Parity

1. **3D axes** - Extend to Axes3D with 3D transforms and projections
2. **Polar coordinates** - Implement PolarAxes with radial/angular coordinates
3. **Inset axes** - Support for zoomed inset plots and connecting lines
4. **Twin axes** - Secondary y-axis with different scales

### Advanced Features

1. **Custom projections** - Cartographic and astronomical coordinate systems
2. **Interactive axes** - Pan, zoom, and selection with coordinate feedback
3. **Animated layouts** - Smooth transitions between layout configurations
4. **Constraint-based layout** - Advanced layout with user-defined constraints

## References

1. **Matplotlib Axes class**: `thirdparty/matplotlib/lib/matplotlib/axes/_base.py` (lines 928-963)
2. **Transform system**: `thirdparty/matplotlib/lib/matplotlib/transforms.py` (lines 84-100)
3. **Spine implementation**: `thirdparty/matplotlib/lib/matplotlib/spines.py` (lines 14-100)
4. **Layout engine**: `thirdparty/matplotlib/lib/matplotlib/layout_engine.py` (lines 32-100)
5. **Tick system**: `thirdparty/matplotlib/lib/matplotlib/ticker.py` and `axis.py`
6. **pyplot-fortran**: `thirdparty/pyplot-fortran/src/pyplot_module.F90` (coordinate patterns)

## Implementation Files

- **`src/fortplot_axes.f90`** - Enhanced axes_t type and coordinate systems
- **`src/fortplot_transforms.f90`** - Hierarchical transform system
- **`src/fortplot_spines.f90`** - Independent spine positioning and rendering
- **`src/fortplot_layout_engine.f90`** - Tight layout and margin optimization
- **`src/fortplot_axis.f90`** - Advanced axis management with scale integration
- **`src/fortplot_scales.f90`** - Enhanced scale objects with transform integration
- **`src/fortplot_shared_axes.f90`** - Shared axes implementation and synchronization
- **`test/test_axes_layout.f90`** - Comprehensive axes and layout tests
- **`test/test_transforms.f90`** - Coordinate transformation verification
- **`test/test_tight_layout.f90`** - Layout optimization algorithm tests

This implementation ensures fortplot provides matplotlib-compatible axes and layout management with professional coordinate systems, flexible spine positioning, automatic layout optimization, and complete multi-axes support for complex scientific visualization workflows.
