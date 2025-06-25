program test_matplotlib_text_positioning
    !! Test to understand current text positioning vs matplotlib expectations
    !! Following TDD: Write failing test first to understand the problem
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480
    real(wp) :: label_x, label_y
    
    ! Get exact plot area (x=80-576, y=58-427)
    margins = plot_margins_t()
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)
    
    print *, "=== Matplotlib Text Positioning Analysis ==="
    print *, "Canvas: 640x480"
    print *, "Plot area: x=", plot_area%left, "-", plot_area%left + plot_area%width, &
             " y=", plot_area%bottom, "-", plot_area%bottom + plot_area%height
    print *, ""
    
    ! TICK LABELS ANALYSIS
    print *, "=== TICK LABELS ==="
    print *, "X-axis tick labels should be:"
    print *, "  - Horizontally centered on tick marks"
    print *, "  - Positioned just below plot bottom edge"
    print *, "  - Text baseline determines Y position"
    print *, ""
    
    print *, "Y-axis tick labels should be:"
    print *, "  - Right-aligned to plot left edge"  
    print *, "  - Vertically centered on tick marks"
    print *, "  - Text baseline considerations for centering"
    print *, ""
    
    ! AXIS LABELS ANALYSIS  
    print *, "=== AXIS LABELS ==="
    
    ! Test current X-axis label positioning
    call calculate_x_label_position(real(plot_area%left + plot_area%width/2, wp), &
                                   real(plot_area%bottom, wp), real(plot_area%height, wp), &
                                   "x", label_x, label_y)
    
    print *, "Current X-axis label position:"
    print *, "  X:", int(label_x), " (should be centered at", plot_area%left + plot_area%width/2, ")"
    print *, "  Y:", int(label_y), " (plot bottom at", plot_area%bottom + plot_area%height, ")"
    print *, "  Gap from plot:", int(label_y) - (plot_area%bottom + plot_area%height), "pixels"
    
    ! Expected: matplotlib puts xlabel further below, with proper spacing from tick labels
    print *, "  Matplotlib expectation: xlabel should be ~40-50 pixels below plot"
    print *, ""
    
    ! Test current Y-axis label positioning  
    call calculate_y_label_position(real(plot_area%bottom + plot_area%height/2, wp), &
                                   real(plot_area%left, wp), "sin(x)", label_x, label_y)
    
    print *, "Current Y-axis label position:"
    print *, "  X:", int(label_x), " (plot left at", plot_area%left, ")"
    print *, "  Y:", int(label_y), " (should be centered at", plot_area%bottom + plot_area%height/2, ")"
    print *, "  Gap from plot:", plot_area%left - int(label_x), "pixels"
    
    ! Expected: matplotlib puts ylabel further left, accounting for tick label width + spacing
    print *, "  Matplotlib expectation: ylabel should be ~50-60 pixels left of plot"
    print *, ""
    
    ! TEXT ANCHOR POINTS
    print *, "=== TEXT ANCHOR POINT ISSUES ==="
    print *, "PNG text rendering:"
    print *, "  - render_text_to_image(x,y) - which corner is (x,y)?"
    print *, "  - Is it top-left, bottom-left, center?"
    print *, "  - After 90° rotation, how do coordinates change?"
    print *, ""
    
    print *, "PDF text rendering:"
    print *, "  - PDF text has baseline at (x,y)"
    print *, "  - After rotation matrix, how do coordinates change?"
    print *, "  - Coordinate system differences (Y-up vs Y-down)"
    print *, ""
    
    print *, "REQUIRED TESTS:"
    print *, "1. Test actual matplotlib tick label positions"
    print *, "2. Test actual matplotlib axis label positions"  
    print *, "3. Test text anchor behavior in PNG vs PDF"
    print *, "4. Test rotation effects on positioning"
    
end program test_matplotlib_text_positioning