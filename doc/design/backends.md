title: Backend Architecture Design
---

# Backend Architecture Implementation Design

## Overview

This document describes the matplotlib-compatible backend architecture in fortplot, covering rendering systems, output formats, and drawing primitives. The implementation follows matplotlib's three-layer backend architecture while leveraging Fortran's polymorphism and type safety for high-performance scientific visualization.

## Problem Statement

The current backend implementation has several limitations:
1. **No graphics context separation** - Drawing state embedded in backends instead of separate context objects
2. **Limited backend registration** - Simple file extension detection without dynamic backend loading
3. **Missing advanced primitives** - No path collections, gradient fills, or complex drawing operations
4. **No mixed-mode rendering** - Cannot optimize by switching between vector and raster for different objects
5. **Limited text capabilities** - Basic text rendering without advanced typography features
6. **No performance hooks** - Missing backend-specific optimization opportunities

## Solution: Matplotlib-Compatible Backend Architecture

### Architecture

The solution consists of six main components:

1. **Three-layer architecture** - Canvas, Renderer, and GraphicsContext separation
2. **Backend registry system** - Dynamic backend loading and selection
3. **Enhanced drawing primitives** - Complete matplotlib rendering API
4. **Graphics context management** - Sophisticated drawing state handling
5. **Mixed-mode rendering** - Adaptive vector/raster optimization
6. **Performance optimization** - Backend-specific acceleration hooks

### Key Components

#### 1. Three-Layer Backend Architecture

Following matplotlib's backend pattern in `backend_bases.py` (lines 130-599):

```fortran
! Graphics Context Layer - Drawing State Management
type :: graphics_context_t
    ! Line properties
    real(wp) :: linewidth = 1.0
    character(len=20) :: linestyle = '-'
    type(color_t) :: color
    character(len=20) :: linecap = 'round'      ! 'butt', 'round', 'square'
    character(len=20) :: linejoin = 'round'     ! 'miter', 'round', 'bevel'
    
    ! Fill properties
    type(color_t) :: facecolor
    logical :: fill = .false.
    
    ! Text properties
    type(font_properties_t) :: font
    
    ! Transparency and compositing
    real(wp) :: alpha = 1.0
    character(len=20) :: blend_mode = 'normal'
    
    ! Clipping and transforms
    logical :: clip_on = .true.
    type(bbox_t) :: clip_box
    type(transform_t) :: transform
    
    ! State stack for save/restore
    type(graphics_context_t), allocatable :: saved_states(:)
    integer :: stack_depth = 0
contains
    procedure :: save_state, restore_state
    procedure :: set_linewidth, set_color, set_alpha
    procedure :: copy => copy_graphics_context
end type

! Renderer Layer - Drawing Primitives
type, abstract :: renderer_base_t
    integer :: width, height                    ! Canvas dimensions
    real(wp) :: dpi = 100.0                    ! Resolution
    type(graphics_context_t) :: gc             ! Current graphics context
contains
    ! Required core drawing methods (matplotlib pattern)
    procedure(draw_path_interface), deferred :: draw_path
    procedure(draw_image_interface), deferred :: draw_image
    procedure(draw_text_interface), deferred :: draw_text
    
    ! Optional optimization methods
    procedure :: draw_markers => default_draw_markers
    procedure :: draw_path_collection => default_draw_path_collection
    procedure :: draw_quad_mesh => default_draw_quad_mesh
    
    ! Graphics context management
    procedure :: new_gc, copy_gc
    procedure :: save, restore
    
    ! Canvas management
    procedure :: clear, finalize
end type

! Canvas Layer - Backend Interface
type, abstract :: figure_canvas_base_t
    type(figure_t), pointer :: figure => null()
    class(renderer_base_t), allocatable :: renderer
contains
    procedure(print_figure_interface), deferred :: print_figure
    procedure :: draw => draw_figure
    procedure :: get_renderer
end type
```

#### 2. Backend Registry System

Following matplotlib's registry pattern in `registry.py` (lines 15-414):

```fortran
type :: backend_entry_t
    character(len=50) :: name                   ! Backend identifier
    character(len=100) :: module_name           ! Implementation module
    character(len=20) :: format_type            ! 'raster', 'vector', 'interactive'
    character(len=10), allocatable :: extensions(:) ! Supported file extensions
    logical :: builtin = .true.                 ! Built-in vs external backend
    character(len=200) :: description
end type

type :: backend_registry_t
    type(backend_entry_t), allocatable :: backends(:)
    integer :: n_backends = 0
contains
    procedure :: register_backend
    procedure :: get_backend_by_name
    procedure :: get_backend_by_extension
    procedure :: list_backends
    procedure :: load_external_backend
end type

type(backend_registry_t), save :: global_backend_registry

subroutine initialize_builtin_backends()
    ! Register built-in backends (matplotlib pattern)
    call global_backend_registry%register_backend( &
        name='agg', module_name='fortplot_agg', format_type='raster', &
        extensions=['png', 'jpg'], description='Anti-grain geometry raster backend')
    
    call global_backend_registry%register_backend( &
        name='pdf', module_name='fortplot_pdf', format_type='vector', &
        extensions=['pdf'], description='Portable Document Format backend')
    
    call global_backend_registry%register_backend( &
        name='svg', module_name='fortplot_svg', format_type='vector', &
        extensions=['svg'], description='Scalable Vector Graphics backend')
    
    call global_backend_registry%register_backend( &
        name='ascii', module_name='fortplot_ascii', format_type='text', &
        extensions=['txt'], description='ASCII art text backend')
end subroutine
```

#### 3. Enhanced Drawing Primitives

Following matplotlib's drawing API in `backend_bases.py`:

```fortran
! Path drawing with full matplotlib compatibility
subroutine draw_path(renderer, gc, path, transform)
    class(renderer_base_t), intent(inout) :: renderer
    type(graphics_context_t), intent(in) :: gc
    type(path_t), intent(in) :: path            ! Vector path with curves/lines
    type(transform_t), intent(in), optional :: transform
    
    ! Transform path coordinates
    if (present(transform)) then
        transformed_path = transform%transform_path(path)
    else
        transformed_path = path
    end if
    
    ! Render based on fill and stroke settings
    if (gc%fill) then
        call renderer%fill_path(transformed_path, gc%facecolor, gc%alpha)
    end if
    
    if (gc%linewidth > 0.0) then
        call renderer%stroke_path(transformed_path, gc%color, gc%linewidth, gc%linestyle)
    end if
end subroutine

! Optimized marker drawing (matplotlib pattern)
subroutine draw_markers(renderer, gc, marker_path, marker_trans, path, trans)
    ! High-performance marker rendering for scatter plots
    ! Reuses marker path for all instances with different transforms
    
    do i = 1, size(path%vertices, 2)
        point_trans = compose_transforms(marker_trans, translation_transform(path%vertices(:, i)))
        call renderer%draw_path(gc, marker_path, point_trans)
    end do
end subroutine

! Path collection for efficient multi-path rendering
subroutine draw_path_collection(renderer, gc, paths, transforms, facecolors, edgecolors)
    ! Batch rendering of multiple paths with different properties
    ! Used for contour plots, bar charts, etc.
    
    call renderer%begin_path_collection()
    do i = 1, size(paths)
        gc_copy = gc
        if (allocated(facecolors)) gc_copy%facecolor = facecolors(i)
        if (allocated(edgecolors)) gc_copy%color = edgecolors(i)
        call renderer%draw_path(gc_copy, paths(i), transforms(i))
    end do
    call renderer%end_path_collection()
end subroutine

! Advanced text rendering with layout
subroutine draw_text(renderer, gc, x, y, text, prop, angle, ismath)
    real(wp), intent(in) :: x, y, angle
    character(*), intent(in) :: text
    type(font_properties_t), intent(in) :: prop
    logical, intent(in), optional :: ismath      ! Math text rendering
    
    ! Calculate text metrics
    call get_text_width_height_descent(text, prop, width, height, descent)
    
    ! Create text layout
    layout = create_text_layout(text, prop, angle, x, y)
    
    ! Render with proper positioning
    call renderer%render_text_layout(gc, layout)
end subroutine
```

#### 4. Concrete Backend Implementations

Following matplotlib's backend specialization patterns:

```fortran
! AGG Raster Backend (matplotlib agg pattern)
type, extends(renderer_base_t) :: agg_renderer_t
    integer, allocatable :: pixel_buffer(:,:,:) ! RGBA pixel array
    type(antialiasing_t) :: aa_engine
contains
    procedure :: draw_path => agg_draw_path
    procedure :: draw_image => agg_draw_image
    procedure :: draw_text => agg_draw_text
    procedure :: finalize => agg_write_png
end type

! PDF Vector Backend (matplotlib pdf pattern)  
type, extends(renderer_base_t) :: pdf_renderer_t
    integer :: file_unit
    character(len=1000000) :: pdf_stream        ! PDF content stream
    integer :: object_count = 0
    type(pdf_graphics_state_t) :: pdf_state
contains
    procedure :: draw_path => pdf_draw_path
    procedure :: draw_image => pdf_draw_image
    procedure :: draw_text => pdf_draw_text
    procedure :: finalize => pdf_write_file
end type

! Mixed Mode Backend (matplotlib mixed pattern)
type, extends(renderer_base_t) :: mixed_renderer_t
    class(renderer_base_t), allocatable :: vector_renderer
    class(renderer_base_t), allocatable :: raster_renderer
    logical :: use_raster_for_complex = .true.
contains
    procedure :: draw_path => mixed_draw_path
    procedure :: draw_path_collection => mixed_draw_path_collection
end type

subroutine mixed_draw_path_collection(renderer, gc, paths, transforms, colors)
    ! Decide vector vs raster based on complexity
    if (size(paths) > renderer%raster_threshold) then
        ! Complex collection: rasterize
        call renderer%raster_renderer%draw_path_collection(gc, paths, transforms, colors)
    else
        ! Simple collection: vectorize
        call renderer%vector_renderer%draw_path_collection(gc, paths, transforms, colors)
    end if
end subroutine
```

#### 5. Graphics Context State Management

Following matplotlib's graphics context system:

```fortran
subroutine save_graphics_state(gc)
    ! Save current state to stack (matplotlib pattern)
    gc%stack_depth = gc%stack_depth + 1
    
    if (.not. allocated(gc%saved_states)) then
        allocate(gc%saved_states(10))  ! Initial stack size
    else if (gc%stack_depth > size(gc%saved_states)) then
        ! Grow stack if needed
        call resize_state_stack(gc)
    end if
    
    gc%saved_states(gc%stack_depth) = gc%copy()
end subroutine

subroutine restore_graphics_state(gc)
    ! Restore previous state from stack
    if (gc%stack_depth > 0) then
        gc = gc%saved_states(gc%stack_depth)
        gc%stack_depth = gc%stack_depth - 1
    end if
end subroutine
```

#### 6. Backend Selection and Factory

Following matplotlib's backend selection:

```fortran
function get_backend(format_or_filename) result(backend)
    character(*), intent(in) :: format_or_filename
    class(figure_canvas_base_t), allocatable :: backend
    
    ! Determine backend from format or file extension
    if (index(format_or_filename, '.') > 0) then
        ! Extract extension
        extension = extract_extension(format_or_filename)
        backend_name = global_backend_registry%get_backend_by_extension(extension)
    else
        ! Direct backend name
        backend_name = format_or_filename
    end if
    
    ! Create backend instance
    select case (trim(backend_name))
    case ('agg', 'png')
        allocate(agg_canvas_t :: backend)
    case ('pdf')
        allocate(pdf_canvas_t :: backend)
    case ('svg')
        allocate(svg_canvas_t :: backend)
    case ('ascii', 'txt')
        allocate(ascii_canvas_t :: backend)
    case default
        ! Try to load external backend
        backend = load_external_backend(backend_name)
    end select
end function
```

### Implementation Details

#### Performance Optimization Hooks

Following matplotlib's optimization patterns:

```fortran
! Backend-specific optimization for large datasets
subroutine optimize_for_large_data(renderer, data_size)
    select type (renderer)
    type is (agg_renderer_t)
        ! Use pixel-level optimizations for raster
        if (data_size > 10000) then
            call renderer%enable_fast_mode()
        end if
    type is (pdf_renderer_t)
        ! Use PDF streams for vector optimization
        if (data_size > 1000) then
            call renderer%enable_stream_compression()
        end if
    end select
end subroutine

! Level-of-detail rendering for performance
subroutine apply_level_of_detail(renderer, object_count, pixel_size)
    ! Adaptive detail reduction based on output resolution
    if (pixel_size < 2.0) then
        ! Very small features: simplify geometry
        call renderer%set_simplification_threshold(0.5)
    end if
    
    if (object_count > 10000) then
        ! Many objects: use instancing
        call renderer%enable_instanced_rendering()
    end if
end subroutine
```

#### Backend Extension System

Following matplotlib's entry point pattern:

```fortran
subroutine register_external_backend(name, factory_proc, extensions)
    character(*), intent(in) :: name
    procedure(backend_factory_interface) :: factory_proc
    character(*), intent(in) :: extensions(:)
    
    entry = backend_entry_t( &
        name=name, &
        module_name='external', &
        format_type='external', &
        extensions=extensions, &
        builtin=.false.)
    
    call global_backend_registry%register_backend(entry)
    call register_factory_procedure(name, factory_proc)
end subroutine
```

## Verification

### Test Cases

1. **Backend registry** - Verify registration, lookup, and dynamic loading
2. **Drawing primitives** - Test all required and optional drawing methods
3. **Graphics context** - Verify state save/restore and property management
4. **Format output** - Test all supported output formats for correctness
5. **Performance** - Benchmark rendering speed and memory usage
6. **Mixed-mode rendering** - Verify optimal vector/raster selection

### Expected Results

**Before (current implementation):**
- Simple polymorphic backend interface
- Basic PNG, PDF, ASCII output
- Embedded state management in backends
- File extension-based backend selection
- Limited drawing primitives

**After (matplotlib-compatible):**
- Three-layer architecture with separation of concerns
- Complete matplotlib drawing primitive API
- Sophisticated graphics context state management
- Dynamic backend registry with external support
- Mixed-mode rendering optimization
- Advanced text and path rendering capabilities

## Performance Considerations

### Computational Complexity

- **Backend selection**: O(1) - hash table lookup
- **Drawing operations**: O(n_primitives) - linear in drawing complexity
- **State management**: O(stack_depth) - proportional to nesting level
- **Mixed-mode decisions**: O(1) - constant time complexity heuristics

### Memory Usage

- **Graphics context stack**: O(max_nesting_depth) - state save/restore
- **Renderer buffers**: O(width × height × channels) - pixel/vector storage
- **Backend registry**: O(n_backends) - constant for typical usage
- **Path storage**: O(n_vertices + n_curves) - vector path complexity

### Optimization Opportunities

1. **Renderer pooling** - Reuse renderer instances to reduce allocation
2. **Graphics context flyweight** - Share immutable context state
3. **Batch rendering** - Group similar drawing operations
4. **Lazy evaluation** - Defer expensive operations until necessary

## Future Enhancements

### Matplotlib Feature Parity

1. **Interactive backends** - GUI integration with Qt, Tk, Web
2. **3D rendering** - OpenGL backend for 3D visualization
3. **Animation support** - Frame-based rendering with caching
4. **Multi-page output** - PDF with multiple figures per file

### Advanced Features

1. **GPU acceleration** - OpenCL/CUDA backend for large datasets
2. **Distributed rendering** - MPI backend for cluster visualization
3. **Streaming output** - Real-time rendering for live data
4. **Quality optimization** - Adaptive anti-aliasing and super-sampling

## References

1. **Backend registry**: `thirdparty/matplotlib/lib/matplotlib/backends/registry.py` (lines 15-414)
2. **Base classes**: `thirdparty/matplotlib/lib/matplotlib/backend_bases.py` (lines 130-599)
3. **Template backend**: `thirdparty/matplotlib/lib/matplotlib/backends/backend_template.py` (lines 39-214)
4. **AGG backend**: `thirdparty/matplotlib/lib/matplotlib/backends/backend_agg.py` (lines 57-100)
5. **PDF backend**: `thirdparty/matplotlib/lib/matplotlib/backends/backend_pdf.py` (lines 47-100)
6. **Mixed backend**: `thirdparty/matplotlib/lib/matplotlib/backends/backend_mixed.py` (lines 8-100)
7. **pyplot-fortran**: `thirdparty/pyplot-fortran/src/pyplot_module.F90` (backend patterns)

## Implementation Files

- **`src/fortplot_backend_registry.f90`** - Backend registration and discovery system
- **`src/fortplot_renderer_base.f90`** - Abstract renderer with drawing primitives
- **`src/fortplot_graphics_context.f90`** - Graphics state management
- **`src/fortplot_canvas_base.f90`** - Figure canvas interface
- **`src/fortplot_agg_backend.f90`** - Enhanced AGG raster renderer
- **`src/fortplot_pdf_backend.f90`** - Complete PDF vector renderer
- **`src/fortplot_svg_backend.f90`** - SVG vector backend
- **`src/fortplot_mixed_backend.f90`** - Adaptive vector/raster renderer
- **`src/fortplot_backend_factory.f90`** - Backend instantiation and management
- **`test/test_backend_architecture.f90`** - Backend system verification
- **`test/test_drawing_primitives.f90`** - Drawing API compliance tests
- **`test/test_performance_backends.f90`** - Backend performance benchmarks

This implementation ensures fortplot provides matplotlib-compatible backend architecture with professional rendering capabilities, flexible output format support, sophisticated graphics state management, and high-performance optimization for scientific visualization workflows.
