title: Contour Demo
---

# Contour Demo

This example demonstrates contour plotting capabilities, including basic contours, custom levels, and mixing contour plots with line plots.

## Source Files

## Source Code

**Python:**· **Fortran:** [contour_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/contour_demo/contour_demo.f90)

**Python:** **Python:** [contour_demo.py](https://github.com/lazy-fortran/fortplot/blob/main/example/python/contour_demo/contour_demo.py)

```fortran
program contour_demo
    !! Contour plotting examples with different functions
    use fortplot
    implicit none

    call gaussian_contours()
    call mixed_contour_line_plot()

contains

    subroutine gaussian_contours()
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        type(figure_t) :: fig
        integer :: i, j

        print *, "=== Contour Examples ==="

        ! Generate contour grid
        do i = 1, 30
            x_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
        end do

        ! Gaussian contour
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("2D Gaussian Function")
        call add_contour(x_grid, y_grid, z_grid, label="exp(-(x^2+y^2))")
        call savefig('output/example/fortran/contour_demo/contour_gaussian.png')
        call savefig('output/example/fortran/contour_demo/contour_gaussian.pdf')
        call savefig('output/example/fortran/contour_demo/contour_gaussian.txt')

        print *, "Created: contour_gaussian.png/pdf/txt"

    end subroutine gaussian_contours

    subroutine mixed_contour_line_plot()
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        real(wp), dimension(5) :: custom_levels
        type(figure_t) :: fig
        integer :: i, j

        ! Generate grid
        do i = 1, 30
            x_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
        end do

        ! Saddle function with custom levels
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = x_grid(i)**2 - y_grid(j)**2
            end do
        end do

        custom_levels = [-4.0_wp, -2.0_wp, 0.0_wp, 2.0_wp, 4.0_wp]
        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Mixed Plot: Contour + Line")
        call add_contour(x_grid, y_grid, z_grid, levels=custom_levels, label="x^2-y^2")
        call add_plot(x_grid, exp(-x_grid**2), label="Cross-section at y=0")
        call savefig('output/example/fortran/contour_demo/mixed_plot.png')
        call savefig('output/example/fortran/contour_demo/mixed_plot.pdf')
        call savefig('output/example/fortran/contour_demo/mixed_plot.txt')

        print *, "Created: mixed_plot.png/pdf/txt"

    end subroutine mixed_contour_line_plot

end program contour_demo
```

## Features Demonstrated

- **Basic contours**: Automatic level selection
- **Custom levels**: User-defined contour levels
- **Mixed plots**: Combining contours with line plots
- **Label formatting**: Contour level labels

## Output

### Contour Gaussian

![contour_gaussian.png](../../media/examples/contour_demo/contour_gaussian.png)

ASCII output:
```
%PDF-1.4
%
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

[Download PDF](../../media/examples/contour_demo/contour_gaussian.pdf                                                                                                                                                                                                                                            )

### Mixed Plot

![mixed_plot.png](../../media/examples/contour_demo/mixed_plot.png)

ASCII output:
```
%PDF-1.4
%
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

[Download PDF](../../media/examples/contour_demo/mixed_plot.pdf                                                                                                                                                                                                                                                  )

