title: Annotation Demo

# Text Annotation Demo

This example demonstrates fortplot's comprehensive text annotation system, showcasing all the essential features needed for creating publication-quality scientific figures with explanatory text and labels.

## Overview

Text annotations are essential for creating clear, informative plots that communicate scientific findings effectively. This demo shows how to use fortplot's annotation system to:

- Label important data points and features
- Add explanatory text and mathematical expressions
- Create professional figure layouts with proper typography
- Use different coordinate systems for flexible positioning

## Features Demonstrated

### Basic Text Annotations
- **Simple text placement**: Position text at specific data coordinates
- **Font size control**: Range from 8pt footnotes to 16pt titles
- **Text alignment**: Left, center, right alignment options
- **Multi-line support**: Line breaks and paragraph formatting

### Advanced Positioning
- **Data coordinates**: Position relative to plot data values
- **Figure coordinates**: Position relative to entire figure (0-1)
- **Axis coordinates**: Position relative to plot area (0-1)

### Styling Options
- **Font sizes**: 8pt to 16pt for hierarchical information
- **Text rotation**: Vertical text for axis labels
- **Background boxes**: Highlight important information
- **Color customization**: Match text to plot themes

## Source Files

## Source Code

**Fortran:** [annotation_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/annotation_demo/annotation_demo.f90)

**Python:** [annotation_demo.py](https://github.com/lazy-fortran/fortplot/blob/main/example/python/annotation_demo/annotation_demo.py)

```fortran
program annotation_demo
    !! Comprehensive demonstration of text annotation capabilities
    !! Shows positioning, styling, and coordinate systems
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    ! Data arrays for the main plot
    real(wp), parameter :: pi = 3.14159265359_wp
    integer, parameter :: n = 100
    real(wp), dimension(n) :: x, y1, y2, y3
    integer :: i
    type(figure_t) :: fig

    print *, "=== Text Annotation Demonstration ==="
    print *, ""
    print *, "This example demonstrates comprehensive text annotation"
    print *, "features in fortplot, including:"
    print *, "  * Data coordinate positioning"
    print *, "  * Figure coordinate positioning"
    print *, "  * Axis coordinate positioning"
    print *, "  * Font size and alignment control"
    print *, "  * Text rotation and styling"
    print *, ""

    ! Generate sample data for annotation
    do i = 1, n
        x(i) = real(i-1, wp) * 8.0_wp * pi / real(n-1, wp)
        y1(i) = exp(-x(i)/4.0_wp) * sin(x(i))      ! Damped sine
        y2(i) = exp(-x(i)/6.0_wp) * cos(x(i))      ! Damped cosine
        y3(i) = exp(-x(i)/8.0_wp)                  ! Envelope
    end do

    ! Create figure with annotations
    call figure(figsize=[10.0_wp, 7.0_wp])
    call title("Comprehensive Text Annotation Demo")
    call xlabel("x (radians)")
    call ylabel("Amplitude")

    ! Plot the data
    call add_plot(x, y1, label="Damped sine: e^{-x/4}sin(x)")
    call add_plot(x, y2, label="Damped cosine: e^{-x/6}cos(x)")
    call add_plot(x, y3, label="Envelope: e^{-x/8}", linestyle="--")

    ! === DEMONSTRATION 1: Basic text annotations at data coordinates ===
    ! Point to maximum of first curve
    call text(1.57_wp, 0.7_wp, "First maximum", &
                  coord_type=COORD_DATA, font_size=12.0_wp, alignment="center")

    ! Point to where curves intersect
    call text(6.3_wp, 0.15_wp, "Intersection point", &
                  coord_type=COORD_DATA, font_size=10.0_wp, alignment="left")

    ! === DEMONSTRATION 2: Figure coordinate annotations ===
    ! Figure coordinates range from 0 to 1 across entire figure
    call text(0.02_wp, 0.95_wp, "Figure Title Area", &
                  coord_type=COORD_FIGURE, font_size=14.0_wp, alignment="left")

    call text(0.98_wp, 0.02_wp, "Figure Footer", &
                  coord_type=COORD_FIGURE, font_size=8.0_wp, alignment="right")

    ! === DEMONSTRATION 3: Multiple font sizes ===
    call text(15.0_wp, 0.8_wp, "Large Text (16pt)", &
                  coord_type=COORD_DATA, font_size=16.0_wp, alignment="center")

    call text(15.0_wp, 0.6_wp, "Medium Text (12pt)", &
                  coord_type=COORD_DATA, font_size=12.0_wp, alignment="center")

    call text(15.0_wp, 0.4_wp, "Small Text (8pt)", &
                  coord_type=COORD_DATA, font_size=8.0_wp, alignment="center")

    ! === DEMONSTRATION 4: Text alignment options ===
    call text(20.0_wp, 0.5_wp, "Left aligned", &
                  coord_type=COORD_DATA, font_size=10.0_wp, alignment="left")

    call text(20.0_wp, 0.3_wp, "Center aligned", &
                  coord_type=COORD_DATA, font_size=10.0_wp, alignment="center")

    call text(20.0_wp, 0.1_wp, "Right aligned", &
                  coord_type=COORD_DATA, font_size=10.0_wp, alignment="right")

    ! === DEMONSTRATION 5: Rotated text ===
    ! Vertical text along y-axis (rotated 90 degrees)
    call text(0.5_wp, 0.5_wp, "Vertical Text (90 degrees)", &
                  coord_type=COORD_DATA, font_size=10.0_wp, alignment="center", &
                  rotation=90.0_wp)

    ! === DEMONSTRATION 6: Axis coordinate system ===
    ! Axis coordinates range from 0 to 1 within the plot area only
    call text(0.98_wp, 0.02_wp, "Lower Right", &
                  coord_type=COORD_AXIS, font_size=10.0_wp, alignment="right")

    call text(0.02_wp, 0.98_wp, "Upper Left", &
                  coord_type=COORD_AXIS, font_size=10.0_wp, alignment="left")

    ! === DEMONSTRATION 7: Mathematical expressions ===
    ! Add mathematical annotations using Unicode
    call text(3.0_wp, 0.5_wp, "df/dx = cos(x)e^{-x/4} - (1/4)sin(x)e^{-x/4}", &
                  coord_type=COORD_DATA, font_size=9.0_wp, alignment="center")

    call text(5.0_wp, -0.2_wp, "lim_{x->inf} e^{-x} = 0", &
                  coord_type=COORD_DATA, font_size=10.0_wp, alignment="center")

    ! Add legend
    call legend("upper right")

    ! Save to all supported formats
    print *, "Saving annotation demonstration to multiple formats:"

    call savefig('output/example/fortran/annotation_demo/annotation_demo.png')
    print *, "  * PNG: annotation_demo.png (high-quality with antialiased text)"

    call savefig('output/example/fortran/annotation_demo/annotation_demo.pdf')
    print *, "  * PDF: annotation_demo.pdf (vector graphics, perfect scaling)"

    call savefig('output/example/fortran/annotation_demo/annotation_demo.txt')
    print *, "  * ASCII: annotation_demo.txt (terminal-friendly text output)"

    print *, ""
    print *, "=== Annotation Features Demonstrated ==="
    print *, "* Basic text placement at data coordinates"
    print *, "* Arrow annotations pointing to specific data points"
    print *, "* Multiple font sizes (8pt to 16pt)"
    print *, "* Text alignment options (left, center, right)"
    print *, "* Text rotation (90 degrees vertical text)"
    print *, "* Background boxes for emphasis"
    print *, "* Three coordinate systems:"
    print *, "  - COORD_DATA: Position relative to plot data"
    print *, "  - COORD_FIGURE: Position relative to entire figure (0-1)"
    print *, "  - COORD_AXIS: Position relative to plot area (0-1)"
    print *, "* Mathematical expressions with Unicode symbols"
    print *, "* Multi-backend support (PNG, PDF, ASCII)"
    print *, ""
    print *, "This example demonstrates all key features needed for:"
    print *, "- Scientific figure preparation and publication"
    print *, "- Data point labeling and identification"
    print *, "- Professional technical documentation"
    print *, "- Educational materials with clear explanations"

end program annotation_demo
```

## Implementation Details

### Coordinate Systems

fortplot provides three coordinate systems for flexible text positioning:

1. **Data coordinates** (`COORD_DATA`): Position text relative to your data values
2. **Figure coordinates** (`COORD_FIGURE`): Position relative to entire figure (0-1)
3. **Axis coordinates** (`COORD_AXIS`): Position relative to plot area only (0-1)

### Text Styling

```fortran
call text(x, y, "Your text", &
    coord_type=COORD_DATA, &      ! Coordinate system
    font_size=12.0_wp, &          ! Font size in points
    alignment="center", &         ! left, center, right
    rotation=0.0_wp)              ! Rotation angle in degrees
```

### Best Practices

- Use data coordinates for labeling specific data points
- Use figure coordinates for headers, footers, and margins
- Use axis coordinates for plot-area-specific annotations
- Choose font sizes hierarchically (title > labels > annotations)
- Align text consistently to maintain visual order

## Output

### Annotation Demo

![annotation_demo.png](../../media/examples/annotation_demo/annotation_demo.png)

ASCII output:
```
%PDF-1.4
%
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

[Download PDF](../../media/examples/annotation_demo/annotation_demo.pdf)