# Module Architecture Guide

## Overview

Fortplot's module architecture follows the Single Responsibility Principle (SRP) to maintain files under 1,000 lines while preserving backward compatibility through facade patterns.

## Refactored Module Structure

### PDF Backend Module Hierarchy

The original `fortplot_pdf.f90` (2,178 lines) was refactored into focused submodules:

```fortran
fortplot_pdf.f90                     (489 lines) - Main facade module
├── fortplot_pdf_core.f90            (122 lines) - Core types and constants  
├── fortplot_pdf_text.f90            (498 lines) - Text rendering and fonts
├── fortplot_pdf_drawing.f90         (232 lines) - Drawing primitives
├── fortplot_pdf_axes.f90            (395 lines) - Axes and tick rendering
└── fortplot_pdf_io.f90              (261 lines) - Stream I/O operations
```

**Usage Pattern:**
```fortran
! Client code remains unchanged - uses the facade
use fortplot_pdf, only: pdf_context, create_pdf_canvas

type(pdf_context) :: ctx
call create_pdf_canvas(ctx, "output.pdf", 800, 600)
```

### Animation Module (Simplified)

`fortplot_animation.f90` reduced from 1,060 lines through dependency injection:

```fortran
! Core animation type with minimal dependencies
type :: animation_t
    procedure(animate_interface), pointer, nopass :: animate_func => null()
    integer :: frames = 0
    integer :: interval_ms = DEFAULT_FRAME_INTERVAL_MS
contains
    procedure :: run
    procedure :: save
end type animation_t
```

### Main Module Structure

`fortplot.f90` (1,119 lines) maintains comprehensive API through selective imports:

```fortran
module fortplot
    ! Re-export core functionality
    use fortplot_figure_core, only: figure_t
    use fortplot_animation, only: animation_t, FuncAnimation
    use fortplot_colors, only: color_t
    
    ! Stateful API procedures
    public :: plot, scatter, contour, show, savefig
    public :: figure, xlabel, ylabel, title, legend
end module fortplot
```

## Facade Pattern Implementation

### Backward Compatibility Strategy

All refactored modules maintain 100% API compatibility:

```fortran
! Before refactoring - works unchanged
use fortplot_pdf
type(pdf_context) :: ctx
call create_pdf_canvas(ctx, "plot.pdf", 800, 600)

! After refactoring - identical usage
use fortplot_pdf  ! Now imports from facade
type(pdf_context) :: ctx
call create_pdf_canvas(ctx, "plot.pdf", 800, 600)
```

### Re-export Pattern

Main facade modules re-export functionality from submodules:

```fortran
module fortplot_pdf
    ! Import specialized functionality
    use fortplot_pdf_core
    use fortplot_pdf_text
    use fortplot_pdf_drawing
    use fortplot_pdf_axes
    use fortplot_pdf_io
    
    ! Re-export public interfaces
    public :: pdf_context, create_pdf_canvas
    public :: draw_pdf_axes_and_labels, draw_mixed_font_text
end module fortplot_pdf
```

## Developer Guidelines

### Working with Refactored Modules

**DO:**
```fortran
! Import from facade modules for compatibility
use fortplot_pdf, only: pdf_context

! Access specialized functionality when needed
use fortplot_pdf_text, only: draw_mixed_font_text
```

**DON'T:**
```fortran
! Avoid direct imports that bypass the facade
use fortplot_pdf_core, only: pdf_context_core  ! Too specific
```

### Adding New Functionality

1. **Identify appropriate submodule** based on responsibility
2. **Add to submodule** following SRP
3. **Re-export through facade** for public APIs
4. **Update tests** targeting the facade interface

Example:
```fortran
! Add to fortplot_pdf_drawing.f90
subroutine draw_new_primitive(ctx, ...)
end subroutine

! Re-export in fortplot_pdf.f90
module fortplot_pdf
    use fortplot_pdf_drawing, only: draw_new_primitive
    public :: draw_new_primitive
end module
```

### Size Monitoring

Keep modules under limits:
- **Target:** < 500 lines per module
- **Hard limit:** < 1,000 lines per module
- **Functions:** < 100 lines each (target < 50)

Check sizes:
```bash
wc -l src/fortplot_pdf*.f90
```

## Build System Integration

### FPM Compatibility

No build system changes required - FPM automatically handles:
- Module dependency resolution
- Compilation order
- Interface checking

### Module Dependencies

Dependencies resolved automatically through `use` statements:

```fortran
! fortplot_pdf.f90 depends on:
use fortplot_pdf_core      ! Core types
use fortplot_pdf_text      ! Text rendering  
use fortplot_pdf_drawing   ! Drawing ops
use fortplot_pdf_axes      ! Axes rendering
use fortplot_pdf_io        ! I/O operations
```

## Testing Strategy

### API Compatibility Tests

Verify facade pattern maintains compatibility:

```fortran
! Test original API still works
program test_api_compatibility
    use fortplot_pdf
    type(pdf_context) :: ctx
    
    call create_pdf_canvas(ctx, "test.pdf", 400, 300)
    ! ... existing test code unchanged
end program
```

### Module Boundary Tests

Ensure clean separation of concerns:

```fortran
! Each submodule tested independently
program test_pdf_text_module
    use fortplot_pdf_text, only: draw_mixed_font_text
    ! Test text functionality in isolation
end program
```

## Performance Considerations

### Compilation Speed

Smaller modules improve compilation performance:
- Parallel compilation of independent modules
- Reduced rebuild scope for changes
- Better compiler optimization opportunities

### Runtime Impact

Facade pattern adds minimal overhead:
- No additional function call layers
- Module imports resolved at compile time
- Same machine code generation as monolithic modules

## Migration Path

### For Library Users

**No changes required** - all existing code continues working unchanged.

### For Contributors

1. **Understand module responsibilities** before making changes
2. **Add functionality to appropriate submodule**
3. **Re-export through facade when needed**
4. **Test through facade interfaces**

## Future Refactoring Candidates

Monitor these files approaching size limits:

- `fortplot_figure_core.f90`: Currently 49 lines (safe)
- `fortplot.f90`: 1,119 lines (needs attention if growth continues)
- `fortplot_animation.f90`: 1,060 lines (monitor closely)

Apply same facade pattern when limits approached.