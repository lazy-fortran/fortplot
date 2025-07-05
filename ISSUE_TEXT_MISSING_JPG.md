# Text Elements Missing in JPEG Output

## Problem
Text elements such as axis labels, tick labels, titles, and legends are not rendered in JPEG output files. This affects the usability of JPEG as a backend for scientific plotting where text annotations are essential.

## Expected Behavior
JPEG output should include all text elements that appear in PNG output:
- Axis labels (xlabel, ylabel)
- Tick labels (numbers on axes)
- Plot titles
- Legend text
- Any other text annotations

## Current Behavior
JPEG files are generated but contain only the plot data (lines, markers, etc.) without any text elements.

## Root Cause Analysis
The issue appears to be in the text rendering pipeline for raster backends. While PNG backend properly renders text, the JPEG backend may be missing the text rendering calls or the text rendering may not be properly integrated with the JPEG raster buffer.

## Potential Solutions

### Option 1: Fix in fortplot_jpeg.f90 (Current Approach)
Add text rendering calls specifically to the JPEG backend. This would be a quick fix but creates backend-specific code duplication.

### Option 2: Generalize in fortplot_raster.f90 (Recommended)
Move text rendering logic to the common raster base class (`fortplot_raster.f90`) so that all raster backends (PNG, JPEG, future formats) automatically inherit proper text rendering. This follows the DRY principle and ensures consistency.

## Implementation Steps (Recommended Approach)

1. **Analyze PNG text rendering**
   - Identify where text rendering occurs in PNG backend
   - Document the text rendering pipeline

2. **Refactor to fortplot_raster.f90**
   - Move common text rendering logic to raster base class
   - Create abstract interface for backend-specific text operations
   - Ensure both PNG and JPEG inherit from common text rendering

3. **Update JPEG backend**
   - Implement JPEG-specific text rendering methods
   - Ensure proper integration with JPEG compression pipeline

4. **Add comprehensive tests**
   - Create tests that verify text elements in JPEG output
   - Compare JPEG and PNG text rendering for consistency

## Test Case
Create a simple test that generates both PNG and JPEG with text elements:

```fortran
program test_jpeg_text
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real :: x(10), y(10)
    integer :: i
    
    ! Generate test data
    do i = 1, 10
        x(i) = real(i)
        y(i) = real(i)**2
    end do
    
    call fig%initialize(640, 480)
    call fig%add_plot(x, y, label="y = x²")
    call fig%set_xlabel("X values")
    call fig%set_ylabel("Y values") 
    call fig%set_title("Test Plot with Text")
    
    ! Both should show identical text elements
    call fig%savefig("test_with_text.png")
    call fig%savefig("test_with_text.jpg")
end program
```

## Acceptance Criteria
- [ ] JPEG output contains all text elements present in PNG output
- [ ] Text positioning and formatting matches PNG output
- [ ] No performance regression in JPEG generation
- [ ] Text rendering code is shared between raster backends (no duplication)
- [ ] Comprehensive tests verify text rendering in JPEG

## Priority
**High** - Text elements are fundamental for scientific plotting. JPEG backend is incomplete without proper text support.

## Related Files
- `src/fortplot_jpeg.f90` - JPEG backend implementation
- `src/fortplot_png.f90` - PNG backend (working text rendering)
- `src/fortplot_raster.f90` - Common raster backend base
- `src/fortplot_text.f90` - Text rendering utilities
- `test/test_jpeg_*.f90` - JPEG test suite (needs text rendering tests)