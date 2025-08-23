title: Colored Contours
---

# Colored Contours

This example shows filled contour plots with customizable colormaps for visualizing 2D scalar fields.

## Source Files

## Source Code

ð· **Fortran:** [colored_contours.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/colored_contours/colored_contours.f90)

ð **Python:** [colored_contours.py](https://github.com/lazy-fortran/fortplot/blob/main/example/python/colored_contours/colored_contours.py)

```fortran
program colored_contours_example
    !! Demonstrates colored contour plots with different colormaps
    use fortplot
    implicit none

    call default_gaussian_example()
    call plasma_saddle_example()
    call mixed_colormap_comparison()

contains

    subroutine default_gaussian_example()
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        type(figure_t) :: fig
        integer :: i, j

        print *, "=== Default Colorblind-Safe Gaussian Example ==="

        ! Generate grid
        do i = 1, 30
            x_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
        end do

        ! 2D Gaussian
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("2D Gaussian - Default Colorblind-Safe Colormap")
        call add_contour_filled(x_grid, y_grid, z_grid)  ! Uses default 'crest' colormap
        call savefig('output/example/fortran/colored_contours/gaussian_default.png')
        call savefig('output/example/fortran/colored_contours/gaussian_default.pdf')
        call savefig('output/example/fortran/colored_contours/gaussian_default.txt')

        print *, "Created: gaussian_default.png/pdf/txt"
    end subroutine default_gaussian_example

    subroutine plasma_saddle_example()
        real(wp), dimension(25) :: x_grid, y_grid
        real(wp), dimension(25,25) :: z_grid
        real(wp), dimension(8) :: custom_levels
        type(figure_t) :: fig
        integer :: i, j

        print *, "=== Plasma Saddle Function Example ==="

        ! Generate grid
        do i = 1, 25
            x_grid(i) = -2.5_wp + (i-1) * 5.0_wp / 24.0_wp
            y_grid(i) = -2.5_wp + (i-1) * 5.0_wp / 24.0_wp
        end do

        ! Saddle function: x^2 - y^2
        do i = 1, 25
            do j = 1, 25
                z_grid(i,j) = x_grid(i)**2 - y_grid(j)**2
            end do
        end do

        ! Custom contour levels
        custom_levels = [-6.0_wp, -4.0_wp, -2.0_wp, -1.0_wp, 1.0_wp, 2.0_wp, 4.0_wp, 6.0_wp]

        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Saddle Function - Plasma Colormap")
        call add_contour_filled(x_grid, y_grid, z_grid, levels=custom_levels, colormap="plasma")
        call savefig('output/example/fortran/colored_contours/saddle_plasma.png')
        call savefig('output/example/fortran/colored_contours/saddle_plasma.pdf')
        call savefig('output/example/fortran/colored_contours/saddle_plasma.txt')

        print *, "Created: saddle_plasma.png/pdf/txt"
    end subroutine plasma_saddle_example

    subroutine mixed_colormap_comparison()
        real(wp), dimension(20) :: x_grid, y_grid
        real(wp), dimension(20,20) :: z_grid
        type(figure_t) :: fig1, fig2, fig3
        integer :: i, j

        print *, "=== Colormap Comparison ==="

        ! Generate grid
        do i = 1, 20
            x_grid(i) = -2.0_wp + (i-1) * 4.0_wp / 19.0_wp
            y_grid(i) = -2.0_wp + (i-1) * 4.0_wp / 19.0_wp
        end do

        ! Ripple function
        do i = 1, 20
            do j = 1, 20
                z_grid(i,j) = sin(sqrt(x_grid(i)**2 + y_grid(j)**2) * 3.0_wp) * exp(-0.3_wp * sqrt(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        ! Inferno colormap
        call figure(figsize=[6.4_wp, 4.8_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Ripple Function - Inferno Colormap")
        call add_contour_filled(x_grid, y_grid, z_grid, colormap="inferno")
        call savefig('output/example/fortran/colored_contours/ripple_inferno.png')
        call savefig('output/example/fortran/colored_contours/ripple_inferno.pdf')
        call savefig('output/example/fortran/colored_contours/ripple_inferno.txt')

        ! Coolwarm colormap
        call figure(figsize=[6.4_wp, 4.8_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Ripple Function - Coolwarm Colormap")
        call add_contour_filled(x_grid, y_grid, z_grid, colormap="coolwarm")
        call savefig('output/example/fortran/colored_contours/ripple_coolwarm.png')
        call savefig('output/example/fortran/colored_contours/ripple_coolwarm.pdf')
        call savefig('output/example/fortran/colored_contours/ripple_coolwarm.txt')

        ! Jet colormap
        call figure(figsize=[6.4_wp, 4.8_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Ripple Function - Jet Colormap")
        call add_contour_filled(x_grid, y_grid, z_grid, colormap="jet")
        call savefig('output/example/fortran/colored_contours/ripple_jet.png')
        call savefig('output/example/fortran/colored_contours/ripple_jet.pdf')
        call savefig('output/example/fortran/colored_contours/ripple_jet.txt')

        print *, "Created: ripple_inferno.png/pdf/txt, ripple_coolwarm.png/pdf/txt, ripple_jet.png/pdf/txt"
        print *, "Colormap comparison complete!"
    end subroutine mixed_colormap_comparison

end program colored_contours_example
```

## Features Demonstrated

- **Filled contours**: Continuous color gradients
- **Multiple colormaps**: viridis, jet, coolwarm, inferno, plasma
- **Custom levels**: Control contour density
- **Various functions**: Gaussian, ripple, saddle point patterns

## Available Colormaps

- `viridis` - Default, perceptually uniform
- `jet` - Classic rainbow colormap
- `coolwarm` - Blue to red diverging
- `inferno` - Black to yellow sequential
- `plasma` - Purple to yellow sequential

## Output

### Gaussian Default

![gaussian_default.png](../../media/examples/colored_contours/gaussian_default.png)

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

[Download PDF](../../media/examples/colored_contours/gaussian_default.pdf                                                                                                                                                                                                                                            )

### Ripple Jet

![ripple_jet.png](../../media/examples/colored_contours/ripple_jet.png)

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

[Download PDF](../../media/examples/colored_contours/ripple_jet.pdf                                                                                                                                                                                                                                                  )

### Ripple Coolwarm

![ripple_coolwarm.png](../../media/examples/colored_contours/ripple_coolwarm.png)

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

[Download PDF](../../media/examples/colored_contours/ripple_coolwarm.pdf                                                                                                                                                                                                                                             )

### Ripple Inferno

![ripple_inferno.png](../../media/examples/colored_contours/ripple_inferno.png)

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

[Download PDF](../../media/examples/colored_contours/ripple_inferno.pdf                                                                                                                                                                                                                                              )

### Saddle Plasma

![saddle_plasma.png](../../media/examples/colored_contours/saddle_plasma.png)

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

[Download PDF](../../media/examples/colored_contours/saddle_plasma.pdf                                                                                                                                                                                                                                               )

