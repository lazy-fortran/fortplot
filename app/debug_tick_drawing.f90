program debug_tick_drawing
    use fortplot_raster, only: raster_context, create_raster_canvas
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_margins, only: get_axis_tick_positions
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(raster_context) :: ctx
    real(wp) :: x_positions(10), y_positions(10)
    integer :: num_x, num_y, i
    integer :: width = 640, height = 480
    
    ! Create raster canvas
    ctx = create_raster_canvas(width, height)
    
    ! Set up data range
    ctx%x_min = 0.0_wp
    ctx%x_max = 20.0_wp
    ctx%y_min = -1.0_wp
    ctx%y_max = 1.0_wp
    
    print *, "Plot area:"
    print *, "  Left:", ctx%plot_area%left
    print *, "  Bottom:", ctx%plot_area%bottom  
    print *, "  Width:", ctx%plot_area%width
    print *, "  Height:", ctx%plot_area%height
    print *, ""
    
    ! Get tick positions
    call get_axis_tick_positions(ctx%plot_area, 5, 5, x_positions, y_positions, num_x, num_y)
    
    print *, "Generated", num_x, "X tick positions and", num_y, "Y tick positions"
    print *, ""
    
    print *, "X tick positions:"
    do i = 1, num_x
        print *, "  Tick", i, "at pixel position:", x_positions(i)
    end do
    print *, ""
    
    print *, "Y tick positions:"
    do i = 1, num_y
        print *, "  Tick", i, "at pixel position:", y_positions(i)
    end do
    print *, ""
    
    ! Check plot area boundaries
    print *, "Plot area boundaries:"
    print *, "  Left edge:", real(ctx%plot_area%left, wp)
    print *, "  Right edge:", real(ctx%plot_area%left + ctx%plot_area%width, wp)
    print *, "  Bottom edge:", real(ctx%plot_area%bottom, wp)
    print *, "  Top edge:", real(ctx%plot_area%bottom + ctx%plot_area%height, wp)
    print *, ""
    
    ! Check if ticks are within plot area
    print *, "Tick position analysis:"
    print *, "X ticks:"
    do i = 1, num_x
        if (x_positions(i) >= real(ctx%plot_area%left, wp) .and. &
            x_positions(i) <= real(ctx%plot_area%left + ctx%plot_area%width, wp)) then
            print *, "  Tick", i, "is within plot area"
        else
            print *, "  Tick", i, "is OUTSIDE plot area!"
        end if
    end do
    
    print *, "Y ticks:"
    do i = 1, num_y
        if (y_positions(i) >= real(ctx%plot_area%bottom, wp) .and. &
            y_positions(i) <= real(ctx%plot_area%bottom + ctx%plot_area%height, wp)) then
            print *, "  Tick", i, "is within plot area"
        else
            print *, "  Tick", i, "is OUTSIDE plot area!"
        end if
    end do
    
end program debug_tick_drawing