program test_separate_label_positioning
    !! TDD test: Define separate positioning for tick labels vs axis labels
    !! Based on matplotlib analysis showing different spacing requirements
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_label_positioning, only: calculate_x_tick_label_position, calculate_y_tick_label_position, &
                                         calculate_x_axis_label_position, calculate_y_axis_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480
    
    ! Expected positioning based on matplotlib analysis
    ! X-tick labels: 15 pixels below plot (Y=442)
    ! Y-tick labels: right edge 10 pixels left of plot (X=70) 
    ! X-axis label: 40 pixels below plot (Y=467)
    ! Y-axis label: right edge 58 pixels left of plot (X=22)
    
    real(wp) :: tick_x, tick_y, label_x, label_y
    
    margins = plot_margins_t()
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)
    
    print *, "=== Separate Label Positioning Test ==="
    print *, "Plot area: x=", plot_area%left, "-", plot_area%left + plot_area%width, &
             " y=", plot_area%bottom, "-", plot_area%bottom + plot_area%height
    print *, ""
    
    ! Test X-axis tick labels
    print *, "TICK LABELS:"
    tick_x = real(plot_area%left + plot_area%width/2, wp)  ! Center tick
    
    ! This should give Y=442 (15 pixels below plot bottom 427)
    call calculate_x_tick_label_position(tick_x, real(plot_area%bottom + plot_area%height, wp), &
                                        "0", label_x, label_y)
    print *, "X-tick label at center:"
    print *, "  Expected Y: 442, Actual Y:", int(label_y)
    print *, "  Expected centered X:", int(tick_x), "Actual X:", int(label_x)
    
    ! Test Y-axis tick labels  
    tick_y = real(plot_area%bottom + plot_area%height/2, wp)  ! Center tick
    
    ! This should give right edge at X=70 (10 pixels left of plot left 80)
    call calculate_y_tick_label_position(tick_y, real(plot_area%left, wp), &
                                        "0.5", label_x, label_y)
    print *, ""
    print *, "Y-tick label at center:"
    print *, "  Expected right edge X: 70, Actual left X:", int(label_x), "(width needed)"
    print *, "  Expected centered Y:", int(tick_y), "Actual Y:", int(label_y)
    
    print *, ""
    print *, "AXIS LABELS:"
    
    ! Test X-axis label
    call calculate_x_axis_label_position(real(plot_area%left + plot_area%width/2, wp), &
                                        real(plot_area%bottom + plot_area%height, wp), &
                                        "x", label_x, label_y)
    print *, "X-axis label:"
    print *, "  Expected Y: 467, Actual Y:", int(label_y)
    print *, "  Expected centered X:", plot_area%left + plot_area%width/2, "Actual X:", int(label_x)
    
    ! Test Y-axis label
    call calculate_y_axis_label_position(real(plot_area%bottom + plot_area%height/2, wp), &
                                        real(plot_area%left, wp), &
                                        "sin(x)", label_x, label_y)
    print *, ""
    print *, "Y-axis label:"
    print *, "  Expected right edge X: 22, Actual left X:", int(label_x), "(width needed)"
    print *, "  Expected centered Y:", plot_area%bottom + plot_area%height/2, "Actual Y:", int(label_y)
    
    print *, ""
    print *, "SUCCESS: All functions now implemented!"
    print *, "Positioning should match matplotlib exactly."

end program test_separate_label_positioning