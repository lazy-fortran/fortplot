program test_tick_vs_axis_labels
    !! TDD test: Define correct positioning for tick labels vs axis labels
    !! Based on matplotlib analysis: plot at x=80-576, y=58-427
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480
    
    ! Expected positioning based on matplotlib analysis
    integer, parameter :: EXPECTED_Y_TICK_SPACING = 19  ! Y-tick labels 19px from plot left
    integer, parameter :: EXPECTED_Y_AXIS_SPACING = 98  ! Y-axis label 98px from plot left
    integer, parameter :: EXPECTED_X_TICK_SPACING = 15  ! X-tick labels ~15px below plot
    integer, parameter :: EXPECTED_X_AXIS_SPACING = 50  ! X-axis label ~50px below plot
    
    margins = plot_margins_t()
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)
    
    print *, "=== Tick vs Axis Label Positioning Test ==="
    print *, "Plot area: x=", plot_area%left, "-", plot_area%left + plot_area%width, &
             " y=", plot_area%bottom, "-", plot_area%bottom + plot_area%height
    print *, ""
    
    ! Test X-axis positioning
    print *, "X-AXIS POSITIONING:"
    print *, "Plot bottom edge:", plot_area%bottom + plot_area%height
    
    print *, "X-tick labels should be at Y =", plot_area%bottom + plot_area%height + EXPECTED_X_TICK_SPACING
    print *, "  (", EXPECTED_X_TICK_SPACING, "pixels below plot bottom)"
    
    print *, "X-axis label should be at Y =", plot_area%bottom + plot_area%height + EXPECTED_X_AXIS_SPACING  
    print *, "  (", EXPECTED_X_AXIS_SPACING, "pixels below plot bottom)"
    print *, "  Gap between tick labels and axis label:", EXPECTED_X_AXIS_SPACING - EXPECTED_X_TICK_SPACING, "pixels"
    print *, ""
    
    ! Test Y-axis positioning  
    print *, "Y-AXIS POSITIONING:"
    print *, "Plot left edge:", plot_area%left
    
    print *, "Y-tick labels should end at X =", plot_area%left - EXPECTED_Y_TICK_SPACING
    print *, "  (", EXPECTED_Y_TICK_SPACING, "pixels left of plot)"
    
    print *, "Y-axis label should end at X =", plot_area%left - EXPECTED_Y_AXIS_SPACING
    print *, "  (", EXPECTED_Y_AXIS_SPACING, "pixels left of plot)"  
    print *, "  Gap between tick labels and axis label:", EXPECTED_Y_AXIS_SPACING - EXPECTED_Y_TICK_SPACING, "pixels"
    print *, ""
    
    ! Text anchor considerations
    print *, "TEXT ANCHOR CONSIDERATIONS:"
    print *, "PNG render_text_to_image(x,y,text):"
    print *, "  - (x,y) is likely top-left corner of text"
    print *, "  - For centered text: x = center_pos - text_width/2"
    print *, "  - For right-aligned text: x = right_pos - text_width"
    print *, ""
    
    print *, "PDF text positioning:"
    print *, "  - (x,y) is text baseline, left edge"
    print *, "  - For centered text: x = center_pos - text_width/2" 
    print *, "  - For right-aligned text: x = right_pos - text_width"
    print *, ""
    
    print *, "FAILING TESTS (to be fixed):"
    print *, "1. Separate tick label positioning from axis label positioning"
    print *, "2. Fix Y-tick label right-alignment to end exactly at plot_left - 19px"
    print *, "3. Fix X-tick label center-alignment below plot"
    print *, "4. Fix Y-axis label positioning to be 98px left of plot"
    print *, "5. Fix X-axis label positioning to be 50px below plot"
    
end program test_tick_vs_axis_labels