program debug_tick_positions
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_margins, only: get_axis_tick_positions
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    real(wp) :: x_positions(10), y_positions(10)
    integer :: num_x, num_y, i
    integer :: width = 640, height = 480
    
    ! Set up matplotlib-style margins
    margins = plot_margins_t()  ! Use defaults
    call calculate_plot_area(width, height, margins, plot_area)
    
    print *, "Plot area:"
    print *, "  Left:", plot_area%left
    print *, "  Bottom:", plot_area%bottom  
    print *, "  Width:", plot_area%width
    print *, "  Height:", plot_area%height
    print *, ""
    
    ! Get tick positions
    call get_axis_tick_positions(plot_area, 5, 5, x_positions, y_positions, num_x, num_y)
    
    print *, "Requested 5 ticks for both axes"
    print *, "Got num_x =", num_x, "and num_y =", num_y
    print *, ""
    
    print *, "X tick positions:"
    do i = 1, num_x
        print *, "  ", i, ":", x_positions(i)
    end do
    print *, ""
    
    print *, "Y tick positions:" 
    do i = 1, num_y
        print *, "  ", i, ":", y_positions(i)
    end do
    print *, ""
    
    ! Check if last position reaches the right edge
    print *, "Plot area left edge:", real(plot_area%left, wp)
    print *, "Plot area right edge:", real(plot_area%left + plot_area%width, wp)
    print *, "Last X tick position:", x_positions(num_x)
    print *, "Difference:", abs(x_positions(num_x) - real(plot_area%left + plot_area%width, wp))
    
end program debug_tick_positions