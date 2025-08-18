# Surface Plot User Guide

## Overview

Surface plots visualize 3D data as a mesh surface over a 2D coordinate grid. This guide covers dimension requirements, validation behavior, and best practices.

## Basic Usage

```fortran
use fortplot
type(figure_t) :: fig

! Create coordinate arrays
real(wp), dimension(5) :: x = [0.0, 1.0, 2.0, 3.0, 4.0]
real(wp), dimension(3) :: y = [0.0, 1.0, 2.0]

! Create surface data - CRITICAL: dimensions must match coordinates
real(wp), dimension(5,3) :: z  ! size(z,1) must equal size(x)
                              ! size(z,2) must equal size(y)

! Calculate surface values
do i = 1, 5
    do j = 1, 3
        z(i,j) = x(i) * y(j)  ! Simple product surface
    end do
end do

call fig%initialize(800, 600)
call fig%add_surface(x, y, z, label="Product Surface")
call fig%savefig("surface.png")
```

## Dimension Requirements

**CRITICAL RULE**: For `add_surface(x, y, z)`:
- `z` must be a 2D array with dimensions `(m, n)`
- `x` must be a 1D array with `m` elements  
- `y` must be a 1D array with `n` elements
- Validation: `size(z,1) == size(x)` AND `size(z,2) == size(y)`

### Valid Examples

```fortran
! Square grid
real(wp), dimension(10) :: x, y
real(wp), dimension(10,10) :: z  ! ✓ Correct

! Rectangular grid  
real(wp), dimension(20) :: x_coords
real(wp), dimension(15) :: y_coords
real(wp), dimension(20,15) :: z_surface  ! ✓ Correct

! Single point
real(wp), dimension(1) :: x_single = [0.0]
real(wp), dimension(1) :: y_single = [0.0] 
real(wp), dimension(1,1) :: z_single = reshape([1.0], [1,1])  ! ✓ Correct
```

### Invalid Examples (Will Show Error)

```fortran
! Wrong z dimensions
real(wp), dimension(5) :: x
real(wp), dimension(3) :: y
real(wp), dimension(3,5) :: z_wrong  ! ✗ Wrong: should be (5,3), not (3,5)

! Mismatched x dimension
real(wp), dimension(4) :: x_wrong  ! ✗ Should be size 5
real(wp), dimension(3) :: y
real(wp), dimension(5,3) :: z

! Mismatched y dimension  
real(wp), dimension(5) :: x
real(wp), dimension(4) :: y_wrong  ! ✗ Should be size 3
real(wp), dimension(5,3) :: z
```

## Error Handling

When dimension validation fails, `add_surface` will:
1. Display clear error message showing expected vs actual dimensions
2. Return without adding the plot to prevent runtime errors
3. Continue execution (non-fatal error)

### Example Error Messages

```
Error: Surface z dimensions (3,5) must match x size (5) and y size (3)
Error: Surface z dimensions (4,3) must match x size (5) and y size (3)
```

## Edge Cases

### Empty Arrays
```fortran
! Empty arrays pass validation but create empty plot
real(wp), dimension(0) :: x_empty, y_empty
real(wp), dimension(0,0) :: z_empty
call fig%add_surface(x_empty, y_empty, z_empty)  ! ✓ Valid but empty
```

### Single Point Surfaces
```fortran
! Single point surfaces are valid
real(wp), dimension(1) :: x = [1.0]
real(wp), dimension(1) :: y = [2.0]
real(wp), dimension(1,1) :: z = reshape([3.0], [1,1])
call fig%add_surface(x, y, z)  ! ✓ Creates single-point surface
```

### Large Grids
```fortran
! Large grids work with correct dimensions
real(wp), dimension(1000) :: x_large
real(wp), dimension(500) :: y_large
real(wp), dimension(1000,500) :: z_large  ! ✓ Correct dimensions
```

## Best Practices

### 1. Initialize Arrays Properly
```fortran
! Good: Clear coordinate initialization
do i = 1, size(x)
    x(i) = x_min + (i-1) * (x_max - x_min) / (size(x) - 1)
end do
```

### 2. Use Array Constructors for Small Grids
```fortran
! Concise for small arrays
real(wp), dimension(3) :: x = [0.0, 0.5, 1.0]
real(wp), dimension(4) :: y = [0.0, 0.33, 0.67, 1.0]
```

### 3. Document Dimension Requirements
```fortran
! Document critical dimension relationships
real(wp), dimension(nx) :: x_coords      ! nx points
real(wp), dimension(ny) :: y_coords      ! ny points  
real(wp), dimension(nx, ny) :: z_data    ! surface data: z(i,j) = f(x_coords(i), y_coords(j))
```

### 4. Validate Before Calling
```fortran
! Optional: validate dimensions in your code
if (size(z,1) /= size(x) .or. size(z,2) /= size(y)) then
    error stop "Dimension mismatch in surface data"
end if
call fig%add_surface(x, y, z)
```

## Common Mistakes

### 1. Swapped Z Dimensions
```fortran
! WRONG - common mistake
real(wp), dimension(nx, ny) :: z_wrong
z_wrong = transpose(correct_z_data)  ! Don't do this

! CORRECT - match coordinate order
real(wp), dimension(nx, ny) :: z_correct  ! z(i,j) corresponds to x(i), y(j)
```

### 2. Inconsistent Grid Generation
```fortran
! WRONG - size mismatch
x = linspace(0.0, 1.0, 10)  ! 10 points
y = linspace(0.0, 1.0, 11)  ! 11 points  
allocate(z(10, 10))         ! Wrong size for y

! CORRECT - consistent sizes
nx = 10; ny = 11
allocate(x(nx), y(ny), z(nx, ny))
```

### 3. Matrix vs Grid Confusion
```fortran
! Surface plotting uses grid interpretation:
! z(i,j) = value at coordinate (x(i), y(j))
! NOT matrix multiplication or linear algebra interpretation
```

## Advanced Usage

### Mathematical Functions
```fortran
! Generate mathematical surface
do i = 1, nx
    do j = 1, ny
        r = sqrt(x(i)**2 + y(j)**2)
        z(i,j) = sin(r) * exp(-0.1*r)  ! Ripple function
    end do
end do
```

### Multiple Surfaces
```fortran
call fig%add_surface(x, y, z1, label="Surface 1")
call fig%add_surface(x, y, z2, label="Surface 2")  ! Same grid, different data
```

### Integration with Other Plots
```fortran
! Mix surface with contour plots
call fig%add_surface(x, y, z, label="3D Surface")
call fig%contour(x, y, z, levels=10)  ! Same data, different visualization
```

This guide ensures users understand surface plot dimension requirements and can avoid validation errors while creating effective 3D visualizations.