program test_matplotlib_margins
    !! Test that our margin calculations match matplotlib exactly
    !! For 640x480: matplotlib produces x=80-576, y=58-427
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480
    
    ! Test current margins
    margins = plot_margins_t() ! Use defaults
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)
    
    print *, "=== Current Margin Test ==="
    print *, "Canvas size:", CANVAS_WIDTH, "x", CANVAS_HEIGHT
    print *, "Plot area left:", plot_area%left
    print *, "Plot area width:", plot_area%width  
    print *, "Plot area right:", plot_area%left + plot_area%width
    print *, "Plot area bottom:", plot_area%bottom
    print *, "Plot area height:", plot_area%height
    print *, "Plot area top:", plot_area%bottom + plot_area%height
    
    ! Expected: x=80-576, y=58-427
    print *, ""
    print *, "=== Expected (matplotlib) ==="
    print *, "X range: 80 to 576"
    print *, "Y range: 58 to 427"
    
    if (plot_area%left == 80 .and. (plot_area%left + plot_area%width) == 576 .and. &
        plot_area%bottom == 58 .and. (plot_area%bottom + plot_area%height) == 427) then
        print *, "PASS: MARGINS MATCH MATPLOTLIB"
    else
        print *, "FAIL: MARGINS DO NOT MATCH MATPLOTLIB"
    end if
    
end program test_matplotlib_margins