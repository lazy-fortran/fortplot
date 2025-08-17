# Figure Management Implementation Design

## Overview

This document describes the matplotlib-compatible figure management system in fortplot, covering figure creation, sizing, layout management, and state handling. The implementation follows matplotlib's Figure class architecture and pyplot interface patterns to ensure familiar behavior and API compatibility.

## Problem Statement

The current figure management implementation has several limitations:
1. **Single global figure only** - No multi-figure support or figure numbering
2. **Fixed pixel sizing** - No DPI-based inch sizing like matplotlib
3. **No subplot management** - Missing grid layout and axes management
4. **No figure state tracking** - No current figure concept or figure registry
5. **Limited memory management** - No figure cleanup or disposal mechanisms
6. **Basic savefig functionality** - Missing format options and metadata

## Solution: Matplotlib-Compatible Figure Management

### Architecture

The solution consists of five main components:

1. **Enhanced figure_t type** - DPI-aware sizing and coordinate systems
2. **Figure manager** - Global figure registry and state management
3. **Subplot system** - Grid layout and axes management
4. **Enhanced savefig** - Complete format support and options
5. **Memory management** - Figure lifecycle and cleanup

### Key Components

#### 1. Enhanced Figure Type

Following matplotlib's Figure class in `figure.py` (lines 2464-2653):

```fortran
type :: figure_t
    ! Identity and numbering (matplotlib pattern)
    integer :: number = 0                    ! Figure ID for pyplot interface
    character(len=64) :: label = ''          ! Optional figure label
    
    ! Size and DPI (matplotlib defaults)
    real(wp) :: figsize(2) = [6.4_wp, 4.8_wp]  ! Width, height in inches
    integer :: dpi = 100                        ! Dots per inch
    integer :: width_px, height_px              ! Computed pixel dimensions
    
    ! Coordinate systems (following matplotlib's bbox system)
    type(bbox_t) :: bbox_inches              ! Figure bounds in inches
    type(bbox_t) :: bbox_pixels              ! Figure bounds in pixels
    type(transform_t) :: dpi_scale_trans     ! DPI scaling transform
    
    ! Colors and appearance
    character(len=20) :: facecolor = 'white'  ! Background color
    character(len=20) :: edgecolor = 'black'  ! Border color
    logical :: frameon = .true.               ! Show figure frame
    real(wp) :: alpha = 1.0                   ! Figure transparency
    
    ! Layout and margins (matplotlib rcParams pattern)
    real(wp) :: margin_left = 0.125_wp      ! rcParams['figure.subplot.left']
    real(wp) :: margin_right = 0.9_wp       ! rcParams['figure.subplot.right']
    real(wp) :: margin_bottom = 0.11_wp     ! rcParams['figure.subplot.bottom']
    real(wp) :: margin_top = 0.88_wp        ! rcParams['figure.subplot.top']
    
    ! Backend and rendering
    class(plot_backend), allocatable :: backend
    logical :: is_dirty = .true.             ! Needs redraw
    
    ! Axes management (matplotlib _AxesStack pattern)
    type(axes_stack_t) :: axstack            ! Track all axes and current
    type(axes_t), allocatable :: axes(:)     ! All axes in figure
    integer :: current_axes_index = 0        ! Current axes for gca()
    
    ! Plot data storage (deferred rendering)
    type(plot_data_t), allocatable :: plots(:)
    integer :: n_plots = 0
end type figure_t
```

#### 2. Figure Manager

Following matplotlib's `_pylab_helpers.Gcf` pattern in `pyplot.py`:

```fortran
module fortplot_figure_manager
    implicit none
    private
    
    ! Global figure registry (matplotlib Gcf pattern)
    type :: figure_manager_t
        type(figure_t), allocatable :: figures(:)
        integer, allocatable :: figure_ids(:)
        integer :: current_figure_id = 0
        integer :: next_id = 1
        integer :: max_figures = 20         ! Memory management limit
    contains
        procedure :: new_figure
        procedure :: get_current_figure
        procedure :: set_current_figure
        procedure :: close_figure
        procedure :: close_all
        procedure :: get_figure_by_id
    end type
    
    type(figure_manager_t), save :: global_manager
    
contains
    function new_figure(self, num, figsize, dpi, facecolor) result(fig)
        ! Implements matplotlib's figure() creation logic
        if (present(num)) then
            if (self%figure_exists(num)) then
                fig => self%get_figure_by_id(num)
                call self%set_current_figure(num)
                return
            end if
            new_id = num
        else
            new_id = self%next_id
            self%next_id = self%next_id + 1
        end if
        
        ! Memory management (matplotlib warning pattern)
        if (size(self%figures) >= self%max_figures) then
            call warn("Too many open figures. Consider closing figures.")
        end if
        
        ! Create new figure with matplotlib defaults
        call fig%initialize(figsize, dpi, facecolor, edgecolor, frameon)
        fig%number = new_id
        call self%register_figure(fig)
        call self%set_current_figure(new_id)
    end function
end module
```

#### 3. DPI-Aware Sizing System

Following matplotlib's coordinate system design (lines 2612-2632):

```fortran
subroutine initialize_figure_coordinates(fig, figsize, dpi)
    ! Set figure size in inches (matplotlib default)
    if (present(figsize)) then
        fig%figsize = figsize
    else
        fig%figsize = [6.4_wp, 4.8_wp]  ! matplotlib rcParams default
    end if
    
    if (present(dpi)) then
        fig%dpi = dpi
    else
        fig%dpi = 100  ! matplotlib rcParams default
    end if
    
    ! Calculate pixel dimensions
    fig%width_px = nint(fig%figsize(1) * fig%dpi)
    fig%height_px = nint(fig%figsize(2) * fig%dpi)
    
    ! Set up coordinate transforms (matplotlib pattern)
    fig%bbox_inches = bbox_from_bounds(0.0_wp, 0.0_wp, fig%figsize(1), fig%figsize(2))
    fig%dpi_scale_trans = affine2d_scale(real(fig%dpi, wp))
    fig%bbox_pixels = transform_bbox(fig%bbox_inches, fig%dpi_scale_trans)
end subroutine
```

#### 4. Subplot System

Following matplotlib's `add_gridspec()` and `subplots()` pattern (lines 785-921):

```fortran
type :: gridspec_t
    integer :: nrows, ncols
    real(wp) :: left, right, bottom, top     ! Margins
    real(wp) :: wspace, hspace               ! Spacing between subplots
    real(wp), allocatable :: width_ratios(:)  ! Relative column widths
    real(wp), allocatable :: height_ratios(:) ! Relative row heights
contains
    procedure :: get_subplot_params
    procedure :: get_subplot_geometry
end type

subroutine add_subplot(fig, nrows, ncols, index, axes)
    ! Implements matplotlib's add_subplot() functionality
    call fig%axstack%add_axes(axes)
    axes%figure => fig
    axes%position = calculate_subplot_position(nrows, ncols, index, fig%margins)
    fig%current_axes_index = fig%axstack%size()
end subroutine

function subplots(fig, nrows, ncols, sharex, sharey, squeeze) result(axes_array)
    ! Implements matplotlib's Figure.subplots() functionality
    gs = fig%add_gridspec(nrows, ncols)
    
    allocate(axes_array(nrows, ncols))
    do i = 1, nrows
        do j = 1, ncols
            call fig%add_subplot_from_gridspec(gs, i, j, axes_array(i, j))
            
            ! Axis sharing (matplotlib pattern)
            if (present(sharex) .and. sharex /= 'none') then
                call setup_shared_x_axis(axes_array, i, j, sharex)
            end if
            if (present(sharey) .and. sharey /= 'none') then
                call setup_shared_y_axis(axes_array, i, j, sharey)
            end if
        end do
    end do
    
    ! Squeeze single subplot (matplotlib behavior)
    if (present(squeeze) .and. squeeze .and. nrows == 1 .and. ncols == 1) then
        ! Return single axes instead of array
    end if
end function
```

#### 5. Enhanced Savefig Implementation

Following matplotlib's `Figure.savefig()` pattern (lines 3334-3450):

```fortran
subroutine savefig(fig, filename, dpi, bbox_inches, pad_inches, &
                   transparent, facecolor, edgecolor, format)
    character(*), intent(in) :: filename
    integer, intent(in), optional :: dpi
    character(*), intent(in), optional :: bbox_inches  ! 'tight', 'standard'
    real(wp), intent(in), optional :: pad_inches
    logical, intent(in), optional :: transparent
    character(*), intent(in), optional :: facecolor, edgecolor
    character(*), intent(in), optional :: format
    
    ! Auto-detect format from filename (matplotlib behavior)
    if (present(format)) then
        output_format = format
    else
        output_format = detect_format_from_extension(filename)
    end if
    
    ! Use figure DPI or override
    if (present(dpi)) then
        render_dpi = dpi
    else
        render_dpi = fig%dpi
    end if
    
    ! Handle tight bounding box (matplotlib bbox_inches='tight')
    if (present(bbox_inches) .and. bbox_inches == 'tight') then
        call calculate_tight_bbox(fig, tight_bbox, pad_inches)
        call render_with_bbox(fig, filename, output_format, render_dpi, tight_bbox)
    else
        call render_standard(fig, filename, output_format, render_dpi)
    end if
end subroutine
```

### Implementation Details

#### Pyplot Interface Functions

Following matplotlib's pyplot module patterns:

```fortran
! Global figure management (matplotlib pyplot pattern)
function figure(num, figsize, dpi, facecolor, edgecolor, frameon) result(fig)
    ! Implements matplotlib pyplot.figure() behavior
    fig => global_manager%new_figure(num, figsize, dpi, facecolor, edgecolor, frameon)
end function

function gcf() result(fig)
    ! Get current figure (create if none exists)
    fig => global_manager%get_current_figure()
    if (.not. associated(fig)) then
        fig => figure()  ! Create default figure
    end if
end function

function gca() result(ax)
    ! Get current axes (create if none exists)
    fig => gcf()
    ax => fig%get_current_axes()
    if (.not. associated(ax)) then
        call fig%add_subplot(1, 1, 1, ax)
    end if
end function

subroutine subplot(nrows, ncols, index, axes)
    ! Convenience function for adding subplot to current figure
    fig => gcf()
    call fig%add_subplot(nrows, ncols, index, axes)
end subroutine

function subplots(nrows, ncols, figsize, sharex, sharey, squeeze) result(fig_and_axes)
    ! Implements matplotlib pyplot.subplots() convenience function
    fig => figure(figsize=figsize)
    axes_array = fig%subplots(nrows, ncols, sharex, sharey, squeeze)
    fig_and_axes%fig => fig
    fig_and_axes%axes = axes_array
end function
```

#### Memory Management

Following matplotlib's figure cleanup patterns:

```fortran
subroutine close_figure(num)
    ! Close specific figure and free memory
    call global_manager%close_figure(num)
end subroutine

subroutine close_all()
    ! Close all figures (matplotlib plt.close('all'))
    call global_manager%close_all()
end subroutine

subroutine cleanup_figure(fig)
    ! Internal cleanup for figure disposal
    if (allocated(fig%backend)) deallocate(fig%backend)
    if (allocated(fig%plots)) deallocate(fig%plots)
    if (allocated(fig%axes)) deallocate(fig%axes)
    call fig%axstack%clear()
end subroutine
```

## Verification

### Test Cases

1. **Figure creation and numbering** - Verify matplotlib-compatible figure ID system
2. **DPI and sizing** - Test inch-based sizing and pixel calculation
3. **Multi-figure management** - Test figure switching and memory management
4. **Subplot creation** - Verify grid layout and axes positioning
5. **Savefig functionality** - Test format detection and output options
6. **Memory cleanup** - Verify proper figure disposal and memory release

### Expected Results

**Before (current implementation):**
- Single global figure only
- Fixed 640×480 pixel sizing
- No subplot support
- Basic savefig with format auto-detection
- No figure state management

**After (matplotlib-compatible):**
- Multiple numbered figures with pyplot interface
- DPI-aware inch-based sizing matching matplotlib defaults
- Complete subplot system with grid layout
- Enhanced savefig with tight bounding box and options
- Professional figure management with memory cleanup

## Performance Considerations

### Computational Complexity

- **Figure creation**: O(1) - constant time figure initialization
- **Subplot layout**: O(nrows × ncols) - linear in subplot count
- **Figure switching**: O(1) - hash table lookup by figure ID
- **Memory cleanup**: O(n_plots + n_axes) - linear in figure content

### Memory Usage

- **Figure registry**: O(n_figures) - figure manager overhead
- **Subplot grid**: O(nrows × ncols) - axes array storage
- **Plot data**: O(total_plot_points) - deferred rendering storage
- **Coordinate transforms**: Constant overhead per figure

### Optimization Opportunities

1. **Lazy subplot creation** - Create axes only when accessed
2. **Plot data compression** - Efficient storage for large datasets
3. **Backend caching** - Reuse backend instances across figures
4. **Memory pooling** - Reuse figure objects to reduce allocation

## Future Enhancements

### Matplotlib Feature Parity

1. **Figure events** - Implement figure resize and close callbacks
2. **Figure canvas** - Add interactive canvas with mouse/keyboard events
3. **Figure managers** - Support for different GUI backends
4. **Animation support** - Frame-based animation with figure updates

### Advanced Features

1. **Figure templates** - Predefined figure layouts and styles
2. **Multi-page output** - PDF with multiple figures per file
3. **Figure hierarchy** - Nested figures and subfigures
4. **Parallel rendering** - Multi-threaded figure generation

## References

1. **Matplotlib Figure class**: `thirdparty/matplotlib/lib/matplotlib/figure.py` (lines 2425-3800)
2. **Figure initialization**: `thirdparty/matplotlib/lib/matplotlib/figure.py` (lines 2464-2653)
3. **Figure.savefig()**: `thirdparty/matplotlib/lib/matplotlib/figure.py` (lines 3334-3450)
4. **pyplot interface**: `thirdparty/matplotlib/lib/matplotlib/pyplot.py` (lines 872-1782)
5. **Figure manager**: `thirdparty/matplotlib/lib/matplotlib/_pylab_helpers.py`
6. **pyplot-fortran**: `thirdparty/pyplot-fortran/src/pyplot_module.F90` (figure patterns)

## Implementation Files

- **`src/fortplot_figure_manager.f90`** - Global figure registry and management
- **`src/fortplot_figure_core.f90`** - Enhanced figure_t type and methods
- **`src/fortplot_gridspec.f90`** - Subplot grid layout system
- **`src/fortplot_axes_stack.f90`** - Axes management and tracking
- **`src/fortplot_coordinates.f90`** - DPI-aware coordinate systems
- **`src/fortplot.f90`** - Updated pyplot-style interface
- **`python/fortplot/fortplot.py`** - Enhanced Python figure interface
- **`test/test_figure_management.f90`** - Figure lifecycle and state tests
- **`test/test_subplot_system.f90`** - Subplot layout and sharing tests

This implementation ensures fortplot provides matplotlib-compatible figure management with professional multi-figure support, DPI-aware sizing, complete subplot systems, and robust memory management for production scientific visualization workflows.