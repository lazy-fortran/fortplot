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
    call figure(figsize=[8.0_wp, 6.0_wp])
    
    ! Plot the data series
    call add_plot(x, y_sin, label="Damped sine: $sin(x)e^{-x/4}$", linestyle="b-")
    call add_plot(x, y_exp, label="Exponential: $e^{-x} - 0.5$", linestyle="r--")
    call add_plot(x, y_quad, label="Quadratic: $0.1(x-3)^2 - 0.3$", linestyle="g:")
    
    call title("Scientific Data with Text Annotations")
    call xlabel("Independent Variable (x)")
    call ylabel("Dependent Variable (y)")
    
    ! === DEMONSTRATION 1: Basic text placement ===
    ! Simple text at data coordinates
    call text(1.0_wp, 0.8_wp, "Peak Region", &
                  coord_type=COORD_DATA, font_size=12.0_wp)
    
    ! === DEMONSTRATION 2: Arrow annotations pointing to data ===
    ! Annotate the maximum of the sine wave
    call annotate("Maximum: (" // trim(format_number(x_max)) // ", " // &
                      trim(format_number(y_max_sin)) // ")", &
                      xy=[x_max, y_max_sin], &
                      xytext=[x_max + 1.0_wp, y_max_sin + 0.3_wp], &
                      xy_coord_type=COORD_DATA, xytext_coord_type=COORD_DATA, &
                      font_size=10.0_wp, alignment="center")
    
    ! Annotate the minimum of the exponential
    call annotate("Asymptotic approach", &
                      xy=[x_min, y_min_exp], &
                      xytext=[x_min - 1.5_wp, y_min_exp - 0.2_wp], &
                      xy_coord_type=COORD_DATA, xytext_coord_type=COORD_DATA, &
                      font_size=9.0_wp, alignment="right")
    
    ! === DEMONSTRATION 3: Font sizes and alignment ===
    ! Large title annotation in figure coordinates
    call text(0.5_wp, 0.95_wp, "SCIENTIFIC ANALYSIS", &
                  coord_type=COORD_FIGURE, font_size=16.0_wp, alignment="center")
    
    ! Small footer note
    call text(0.02_wp, 0.02_wp, "Data generated for annotation demonstration", &
                  coord_type=COORD_FIGURE, font_size=8.0_wp, alignment="left")
    
    ! === DEMONSTRATION 4: Rotated text ===
    ! Vertical label for special region
    call text(4.5_wp, 0.0_wp, "Transition Zone", &
                  coord_type=COORD_DATA, font_size=11.0_wp, &
                  rotation=90.0_wp, alignment="center")
    
    ! === DEMONSTRATION 5: Background boxes ===
    ! Important note with background
    call text(2.0_wp, -0.4_wp, "Critical Point", &
                  coord_type=COORD_DATA, font_size=12.0_wp, &
                  alignment="center", has_bbox=.true.)
    
    ! === DEMONSTRATION 6: Different coordinate systems ===
    ! Axis coordinates (0-1 normalized to plot area)
    call text(0.98_wp, 0.98_wp, "Upper Right", &
                  coord_type=COORD_AXIS, font_size=10.0_wp, alignment="right")
    
    call text(0.02_wp, 0.98_wp, "Upper Left", &
                  coord_type=COORD_AXIS, font_size=10.0_wp, alignment="left")
    
    ! === DEMONSTRATION 7: Mathematical expressions ===
    ! Add mathematical annotations using Unicode
    call text(3.0_wp, 0.5_wp, "$∂f/∂x = cos(x)e^{-x/4} - ¼sin(x)e^{-x/4}$", &
                  coord_type=COORD_DATA, font_size=9.0_wp, alignment="center")
    
    call text(5.0_wp, -0.2_wp, "$lim_{x→∞} e^{-x} = 0$", &
                  coord_type=COORD_DATA, font_size=10.0_wp, alignment="center")
    
    ! Add legend
    call legend("upper right")
    
    ! Save to all supported formats
    print *, "Saving annotation demonstration to multiple formats:"
    
    call savefig('output/example/fortran/annotation_demo/annotation_demo.png')
    print *, "  ✓ PNG: annotation_demo.png (high-quality with antialiased text)"
    
    call savefig('output/example/fortran/annotation_demo/annotation_demo.pdf')
    print *, "  ✓ PDF: annotation_demo.pdf (vector graphics, perfect scaling)"
    
    call savefig('output/example/fortran/annotation_demo/annotation_demo.txt')
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
