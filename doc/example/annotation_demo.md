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
- **Text alignment**: Left, center, and right alignment options
- **Coordinate systems**: Data, figure, and axis coordinate positioning

### Advanced Annotation Features
- **Arrow annotations**: Point to specific data points with labeled arrows
- **Text rotation**: Vertical and angled text for space-efficient labeling
- **Background boxes**: Highlight important text with background colors
- **Mathematical expressions**: Unicode symbols for equations and formulas

### Professional Typography
- **Multiple font sizes**: Hierarchical text sizing for clear information structure
- **Alignment control**: Precise text positioning for clean layouts
- **Coordinate flexibility**: Choose the most appropriate coordinate system
- **Backend consistency**: Identical appearance across PNG, PDF, and ASCII output

## Code Structure

The demonstration follows a systematic approach:

1. **Data Generation**: Creates sample scientific data with interesting features
2. **Basic Plotting**: Sets up the main plot with multiple data series
3. **Annotation Examples**: Demonstrates each annotation feature systematically
4. **Output Generation**: Saves to all supported formats

## API Usage Examples

### Basic Text Placement
```fortran
! Simple text at data coordinates
call fig%text(x_pos, y_pos, "Label Text", coord_type=COORD_DATA)
```

### Arrow Annotations
```fortran
! Point to a specific data point
call fig%annotate("Maximum value", &
                  xy=[x_max, y_max], &          ! Arrow tip
                  xytext=[x_text, y_text], &    ! Text position
                  xy_coord_type=COORD_DATA)
```

### Typography Control
```fortran
! Customized text appearance
call fig%text(x, y, "Important Note", &
              font_size=14.0_wp, &
              alignment="center", &
              rotation=45.0_wp, &
              has_bbox=.true.)
```

### Coordinate Systems
```fortran
! Data coordinates (relative to plot data)
call fig%text(2.5, 1.0, "Data point", coord_type=COORD_DATA)

! Figure coordinates (0-1, relative to entire figure)
call fig%text(0.5, 0.95, "Figure title", coord_type=COORD_FIGURE)

! Axis coordinates (0-1, relative to plot area)
call fig%text(0.98, 0.02, "Corner label", coord_type=COORD_AXIS)
```

## Scientific Applications

This annotation system is designed for real-world scientific applications:

- **Research Papers**: Publication-quality figures with proper annotations
- **Conference Presentations**: Clear, readable plots for presentations
- **Educational Materials**: Well-labeled diagrams for teaching
- **Technical Reports**: Professional documentation with annotated data
- **Data Analysis**: Interactive exploration with on-the-fly annotations

## Performance

The annotation system is optimized for practical use:
- **Lightweight**: Minimal memory overhead per annotation
- **Fast rendering**: Efficient text positioning algorithms
- **Scalable**: Handles dozens of annotations without performance impact
- **Backend-optimized**: Each backend uses its optimal text rendering

## Source Files

## Source Code

🔷 **Fortran:** [annotation_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/annotation_demo/annotation_demo.f90)

```fortran
program annotation_demo
    !! Text annotation demonstration for fortplot
    !!
    !! This example demonstrates all the key text annotation features:
    !! - Basic text placement at specified coordinates
    !! - Text annotations with arrows pointing to data points
    !! - Font size and color customization
    !! - Text rotation support
    !! - Background boxes for text
    !! - Multiple text alignment options
    !! - Different coordinate systems (data, figure, axis)
    !!
    !! The example creates a plot with various data features and annotates
    !! them to demonstrate scientific figure preparation capabilities.

    use fortplot
    implicit none

    integer, parameter :: n = 100
    real(wp) :: x(n), y_sin(n), y_exp(n), y_quad(n)
    real(wp) :: x_max, y_max_sin, x_min, y_min_exp
    integer :: i, max_idx, min_idx
    type(figure_t) :: fig

    print *, "=== Text Annotation Demo ==="
    print *, "Generating scientific plot with comprehensive annotations..."
    print *, ""

    ! Generate sample data with interesting features to annotate
    do i = 1, n
        x(i) = real(i-1, wp) * 6.0_wp / real(n-1, wp)  ! 0 to 6
        y_sin(i) = sin(x(i)) * exp(-x(i)/4.0_wp)       ! Damped sine wave
        y_exp(i) = exp(-x(i)) - 0.5_wp                  ! Exponential decay
        y_quad(i) = 0.1_wp * (x(i) - 3.0_wp)**2 - 0.3_wp  ! Quadratic with minimum
    end do

    ! Find interesting data points to annotate
    max_idx = maxloc(y_sin, 1)
    x_max = x(max_idx)
    y_max_sin = y_sin(max_idx)

    min_idx = minloc(y_exp, 1)
    x_min = x(min_idx)
    y_min_exp = y_exp(min_idx)
    call fig%initialize(800, 600)

    ! Plot the data series
    call fig%add_plot(x, y_sin, label="Damped sine: sin(x)e^{-x/4}", linestyle="b-")
    call fig%add_plot(x, y_exp, label="Exponential: e^{-x} - 0.5", linestyle="r--")
    call fig%add_plot(x, y_quad, label="Quadratic: 0.1(x-3)^2 - 0.3", linestyle="g:")

    call fig%set_title("Scientific Data with Text Annotations")
    call fig%set_xlabel("Independent Variable (x)")
    call fig%set_ylabel("Dependent Variable (y)")

    ! === DEMONSTRATION 1: Basic text placement ===
    ! Simple text at data coordinates
    call fig%text(1.0_wp, 0.8_wp, "Peak Region", &
                  coord_type=COORD_DATA, font_size=12.0_wp)

    ! === DEMONSTRATION 2: Arrow annotations pointing to data ===
    ! Annotate the maximum of the sine wave
    call fig%annotate("Maximum: (" // trim(format_number(x_max)) // ", " // &
                      trim(format_number(y_max_sin)) // ")", &
                      xy=[x_max, y_max_sin], &
                      xytext=[x_max + 1.0_wp, y_max_sin + 0.3_wp], &
                      xy_coord_type=COORD_DATA, xytext_coord_type=COORD_DATA, &
                      font_size=10.0_wp, alignment="center")

    ! Annotate the minimum of the exponential
    call fig%annotate("Asymptotic approach", &
                      xy=[x_min, y_min_exp], &
                      xytext=[x_min - 1.5_wp, y_min_exp - 0.2_wp], &
                      xy_coord_type=COORD_DATA, xytext_coord_type=COORD_DATA, &
                      font_size=9.0_wp, alignment="right")

    ! === DEMONSTRATION 3: Font sizes and alignment ===
    ! Large title annotation in figure coordinates
    call fig%text(0.5_wp, 0.95_wp, "SCIENTIFIC ANALYSIS", &
                  coord_type=COORD_FIGURE, font_size=16.0_wp, alignment="center")

    ! Small footer note
    call fig%text(0.02_wp, 0.02_wp, "Data generated for annotation demonstration", &
                  coord_type=COORD_FIGURE, font_size=8.0_wp, alignment="left")

    ! === DEMONSTRATION 4: Rotated text ===
    ! Vertical label for special region
    call fig%text(4.5_wp, 0.0_wp, "Transition Zone", &
                  coord_type=COORD_DATA, font_size=11.0_wp, &
                  rotation=90.0_wp, alignment="center")

    ! === DEMONSTRATION 5: Background boxes ===
    ! Important note with background
    call fig%text(2.0_wp, -0.4_wp, "Critical Point", &
                  coord_type=COORD_DATA, font_size=12.0_wp, &
                  alignment="center", has_bbox=.true.)

    ! === DEMONSTRATION 6: Different coordinate systems ===
    ! Axis coordinates (0-1 normalized to plot area)
    call fig%text(0.98_wp, 0.98_wp, "Upper Right", &
                  coord_type=COORD_AXIS, font_size=10.0_wp, alignment="right")

    call fig%text(0.02_wp, 0.98_wp, "Upper Left", &
                  coord_type=COORD_AXIS, font_size=10.0_wp, alignment="left")

    ! === DEMONSTRATION 7: Mathematical expressions ===
    ! Add mathematical annotations using Unicode
    call fig%text(3.0_wp, 0.5_wp, "∂f/∂x = cos(x)e^{-x/4} - ¼sin(x)e^{-x/4}", &
                  coord_type=COORD_DATA, font_size=9.0_wp, alignment="center")

    call fig%text(5.0_wp, -0.2_wp, "lim_{x→∞} e^{-x} = 0", &
                  coord_type=COORD_DATA, font_size=10.0_wp, alignment="center")

    ! Add legend
    call fig%legend("upper right")

    ! Save to all supported formats
    print *, "Saving annotation demonstration to multiple formats:"

    call fig%savefig('output/example/fortran/annotation_demo/annotation_demo.png')
    print *, "  ✓ PNG: annotation_demo.png (high-quality with antialiased text)"

    call fig%savefig('output/example/fortran/annotation_demo/annotation_demo.pdf')
    print *, "  ✓ PDF: annotation_demo.pdf (vector graphics, perfect scaling)"

    call fig%savefig('output/example/fortran/annotation_demo/annotation_demo.txt')
    print *, "  ✓ ASCII: annotation_demo.txt (terminal-friendly text output)"

    print *, ""
    print *, "=== Annotation Features Demonstrated ==="
    print *, "✓ Basic text placement at data coordinates"
    print *, "✓ Arrow annotations pointing to specific data points"
    print *, "✓ Multiple font sizes (8pt to 16pt)"
    print *, "✓ Text alignment options (left, center, right)"
    print *, "✓ Text rotation (90° vertical text)"
    print *, "✓ Background boxes for emphasis"
    print *, "✓ Three coordinate systems:"
    print *, "  - COORD_DATA: Position relative to plot data"
    print *, "  - COORD_FIGURE: Position relative to entire figure (0-1)"
    print *, "  - COORD_AXIS: Position relative to plot area (0-1)"
    print *, "✓ Mathematical expressions with Unicode symbols"
    print *, "✓ Multi-backend support (PNG, PDF, ASCII)"
    print *, ""
    print *, "This example demonstrates all key features needed for:"
    print *, "- Scientific figure preparation and publication"
    print *, "- Data point labeling and identification"
    print *, "- Plot feature highlighting and explanation"
    print *, "- Educational and presentation materials"
    print *, ""
    print *, "Demo completed successfully! Check the generated files."

contains

    function format_number(value) result(formatted)
        !! Simple number formatting for annotations
        real(wp), intent(in) :: value
        character(len=16) :: formatted

        write(formatted, '(F0.2)') value
        formatted = adjustl(formatted)
    end function format_number

end program annotation_demo
```

## Output

## Integration with Other Features
