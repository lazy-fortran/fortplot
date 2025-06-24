program test_label_positioning
    !! Test axis label positioning consistency
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480
    real(wp) :: label_x, label_y
    
    ! Get matplotlib-exact plot area
    margins = plot_margins_t()
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)
    
    print *, "=== Label Positioning Test ==="
    print *, "Plot area: left=", plot_area%left, "bottom=", plot_area%bottom
    print *, "Plot area: width=", plot_area%width, "height=", plot_area%height
    print *, ""
    
    ! Test X-axis label positioning (center of plot, below tick labels)
    call calculate_x_label_position(real(plot_area%left + plot_area%width/2, wp), &
                                   real(plot_area%bottom, wp), real(plot_area%height, wp), &
                                   "x", label_x, label_y)
    
    print *, "X-axis label 'x' at center:"
    print *, "Position: x=", int(label_x), "y=", int(label_y)
    
    ! Expected positioning for X label:
    ! - X: horizontally centered on plot area
    ! - Y: below plot area + spacing for tick labels
    print *, ""
    
    ! Test Y-axis label positioning (left of plot, vertically centered)
    call calculate_y_label_position(real(plot_area%bottom + plot_area%height/2, wp), &
                                   real(plot_area%left, wp), "sin(x)", label_x, label_y)
    
    print *, "Y-axis label 'sin(x)' at center:"
    print *, "Position: x=", int(label_x), "y=", int(label_y)
    
    ! Expected positioning for Y label:
    ! - X: left of plot area - spacing for tick labels - text width
    ! - Y: vertically centered with plot area
    
    print *, ""
    print *, "=== Spacing Analysis ==="
    print *, "X label spacing from plot bottom:", int(label_y) - (plot_area%bottom + plot_area%height)
    print *, "Y label spacing from plot left:", plot_area%left - int(label_x)
    
end program test_label_positioning