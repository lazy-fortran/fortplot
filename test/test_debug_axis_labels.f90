program test_debug_axis_labels
    !! Debug axis label positioning issues
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_label_positioning, only: calculate_x_axis_label_position, calculate_y_axis_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480
    real(wp) :: label_x, label_y
    
    margins = plot_margins_t()
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)
    
    print *, "=== Debug Axis Label Positioning ==="
    print *, "Canvas: 640x480"
    print *, "Plot area: x=", plot_area%left, "-", plot_area%left + plot_area%width, &
             " y=", plot_area%bottom, "-", plot_area%bottom + plot_area%height
    print *, ""
    
    ! Debug X-axis label
    call calculate_x_axis_label_position(real(plot_area%left + plot_area%width / 2, wp), &
                                       real(plot_area%bottom + plot_area%height, wp), &
                                       "x", label_x, label_y)
    
    print *, "X-axis label 'x':"
    print *, "  Position: X=", int(label_x), "Y=", int(label_y)
    print *, "  Plot bottom edge:", plot_area%bottom + plot_area%height  
    print *, "  Distance below plot:", int(label_y) - (plot_area%bottom + plot_area%height), "pixels"
    print *, "  Expected: ~40 pixels below plot"
    print *, ""
    
    ! Debug Y-axis label
    call calculate_y_axis_label_position(real(plot_area%bottom + plot_area%height / 2, wp), &
                                       real(plot_area%left, wp), &
                                       "sin(x)", label_x, label_y)
    
    print *, "Y-axis label 'sin(x)':"
    print *, "  Position: X=", int(label_x), "Y=", int(label_y)
    print *, "  Plot left edge:", plot_area%left
    print *, "  Distance left of plot:", plot_area%left - int(label_x), "pixels"
    print *, "  Expected: right edge ~58 pixels left of plot"
    print *, ""
    
    ! Check if Y-label is off-screen
    if (int(label_x) < 0) then
        print *, "WARNING: Y-axis label X position is NEGATIVE!"
        print *, "  This means the label is off the left edge of the canvas"
        print *, "  Label will not be visible in PNG output"
    end if
    
end program test_debug_axis_labels