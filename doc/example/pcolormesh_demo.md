title: Pcolormesh Demo
---

# Pcolormesh Demo

This example demonstrates pseudocolor plots for efficient 2D data visualization.

## Source Files

## Source Code

üî∑ **Fortran:** [pcolormesh_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/pcolormesh_demo/pcolormesh_demo.f90)

üêç **Python:** [pcolormesh_demo.py](https://github.com/lazy-fortran/fortplot/blob/main/example/python/pcolormesh_demo/pcolormesh_demo.py)

```fortran
program pcolormesh_demo
    !! Comprehensive demo of pcolormesh functionality
    !! Shows different colormap and data patterns
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, pcolormesh, savefig, figure, xlabel, ylabel, title
    implicit none

    call demo_basic_gradient()
    call demo_sinusoidal_pattern()
    call demo_different_colormaps()

contains

    subroutine demo_basic_gradient()
        !! Basic pcolormesh with simple gradient
        real(wp) :: x(6), y(5)
        real(wp) :: c(4, 5)
        integer :: i, j

        ! Create coordinate arrays for regular grid
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.4_wp
        end do
        do i = 1, 5
            y(i) = real(i-1, wp) * 0.3_wp
        end do

        ! Create test data - simple gradient
        do i = 1, 4
            do j = 1, 5
                c(i, j) = real(i, wp) + real(j, wp) * 0.5_wp
            end do
        end do

        ! Create pcolormesh plot
        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Basic Pcolormesh - Linear Gradient')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='viridis')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_basic.png')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_basic.pdf')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_basic.txt')

    end subroutine demo_basic_gradient

    subroutine demo_sinusoidal_pattern()
        !! Pcolormesh with sinusoidal pattern
        real(wp) :: x(9), y(9)
        real(wp) :: c(8, 8)
        real(wp) :: xi, yj
        integer :: i, j
        real(wp), parameter :: pi = 3.14159265359_wp

        ! Create coordinate arrays
        do i = 1, 9
            x(i) = real(i-1, wp) * 0.2_wp
        end do
        do i = 1, 9
            y(i) = real(i-1, wp) * 0.15_wp
        end do

        ! Create sinusoidal pattern
        do i = 1, 8
            do j = 1, 8
                xi = (x(i) + x(i+1)) * 0.5_wp  ! Center of quad
                yj = (y(j) + y(j+1)) * 0.5_wp  ! Center of quad
                c(i, j) = sin(2.0_wp * pi * xi) * cos(3.0_wp * pi * yj)
            end do
        end do

        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Pcolormesh - Sinusoidal Pattern')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='coolwarm')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.png')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.pdf')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_sinusoidal.txt')

    end subroutine demo_sinusoidal_pattern

    subroutine demo_different_colormaps()
        !! Demo different colormaps
        real(wp) :: x(6), y(6)
        real(wp) :: c(5, 5)
        real(wp) :: r
        integer :: i, j
        real(wp), parameter :: pi = 3.14159265359_wp

        ! Create coordinate arrays
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.3_wp
        end do
        do i = 1, 6
            y(i) = real(i-1, wp) * 0.25_wp
        end do

        ! Create radial pattern
        do i = 1, 5
            do j = 1, 5
                r = sqrt((x(i) - 1.0_wp)**2 + (y(j) - 0.6_wp)**2)
                c(i, j) = exp(-r)
            end do
        end do

        call figure(figsize=[6.4_wp, 4.8_wp])
        call title('Pcolormesh - Radial Pattern (Plasma)')
        call xlabel('X coordinate')
        call ylabel('Y coordinate')
        call pcolormesh(x, y, c, colormap='plasma')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_plasma.png')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_plasma.pdf')
        call savefig('output/example/fortran/pcolormesh_demo/pcolormesh_plasma.txt')

    end subroutine demo_different_colormaps

end program pcolormesh_demo
```

## Features Demonstrated

- **Grid-based visualization**: Efficient for large 2D datasets
- **Colormap support**: All standard colormaps available
- **Cell-centered data**: Each cell shows one data value
- **Automatic scaling**: Data range mapped to colors

## Key Differences from Contour

- **Pcolormesh**: Shows actual data values as colored cells
- **Contour**: Interpolates and shows level curves
- **Performance**: Pcolormesh faster for large grids

## Output

### Pcolormesh Basic

![pcolormesh_basic.png](../../media/examples/pcolormesh_demo/pcolormesh_basic.png)

ASCII output:
```
%PDF-1.4
%ÄÅÇÉ
2 0 obj
<<
/Type /Catalog
/Pages 3 0 R
>>
endobj
3 0 obj
<<
/Type /Pages
/Kids [4 0 R]
/Count 1
>>
endobj
4 0 obj
<<
/Type /Page
/Parent 3 0 R
/MediaBox [0 0 595.0 842.0]
/Resources <<
  /Font <<
    /F5 5 0 R
    /F6 6 0 R
  >>
>>
/Contents 7 0 R
>>
endobj
5 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Helvetica
>>
endobj
6 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Symbol
>>
endobj
7 0 obj
<<
/Length 23
>>
stream
q
1 w
1 J
1 j
0 0 1 RG

endstream
endobj
xref
0 8
0000000000 65535 f
0000000000 00000 n
0000000013 00000 n
0000000056 00000 n
0000000106 00000 n
0000000244 00000 n
0000000307 00000 n
0000000367 00000 n
trailer
<<
/Size 8
/Root 2 0 R
>>
startxref
432
%%EOF
```

[Download PDF](../../media/examples/pcolormesh_demo/pcolormesh_basic.pdf                                                                                                                                                                                                                                            )

### Pcolormesh Plasma

![pcolormesh_plasma.png](../../media/examples/pcolormesh_demo/pcolormesh_plasma.png)

ASCII output:
```
%PDF-1.4
%ÄÅÇÉ
2 0 obj
<<
/Type /Catalog
/Pages 3 0 R
>>
endobj
3 0 obj
<<
/Type /Pages
/Kids [4 0 R]
/Count 1
>>
endobj
4 0 obj
<<
/Type /Page
/Parent 3 0 R
/MediaBox [0 0 595.0 842.0]
/Resources <<
  /Font <<
    /F5 5 0 R
    /F6 6 0 R
  >>
>>
/Contents 7 0 R
>>
endobj
5 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Helvetica
>>
endobj
6 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Symbol
>>
endobj
7 0 obj
<<
/Length 23
>>
stream
q
1 w
1 J
1 j
0 0 1 RG

endstream
endobj
xref
0 8
0000000000 65535 f
0000000000 00000 n
0000000013 00000 n
0000000056 00000 n
0000000106 00000 n
0000000244 00000 n
0000000307 00000 n
0000000367 00000 n
trailer
<<
/Size 8
/Root 2 0 R
>>
startxref
432
%%EOF
```

[Download PDF](../../media/examples/pcolormesh_demo/pcolormesh_plasma.pdf                                                                                                                                                                                                                                           )

### Pcolormesh Sinusoidal

![pcolormesh_sinusoidal.png](../../media/examples/pcolormesh_demo/pcolormesh_sinusoidal.png)

ASCII output:
```
%PDF-1.4
%ÄÅÇÉ
2 0 obj
<<
/Type /Catalog
/Pages 3 0 R
>>
endobj
3 0 obj
<<
/Type /Pages
/Kids [4 0 R]
/Count 1
>>
endobj
4 0 obj
<<
/Type /Page
/Parent 3 0 R
/MediaBox [0 0 595.0 842.0]
/Resources <<
  /Font <<
    /F5 5 0 R
    /F6 6 0 R
  >>
>>
/Contents 7 0 R
>>
endobj
5 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Helvetica
>>
endobj
6 0 obj
<<
/Type /Font
/Subtype /Type1
/BaseFont /Symbol
>>
endobj
7 0 obj
<<
/Length 23
>>
stream
q
1 w
1 J
1 j
0 0 1 RG

endstream
endobj
xref
0 8
0000000000 65535 f
0000000000 00000 n
0000000013 00000 n
0000000056 00000 n
0000000106 00000 n
0000000244 00000 n
0000000307 00000 n
0000000367 00000 n
trailer
<<
/Size 8
/Root 2 0 R
>>
startxref
432
%%EOF
```

[Download PDF](../../media/examples/pcolormesh_demo/pcolormesh_sinusoidal.pdf                                                                                                                                                                                                                                       )

