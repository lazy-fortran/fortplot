program debug_tick_visibility
    use fortplot_raster, only: raster_context, create_raster_canvas, draw_line_distance_aa
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_margins, only: get_axis_tick_positions
    use fortplot_png, only: write_png_file
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
    
    ! Get tick positions
    call get_axis_tick_positions(ctx%plot_area, 5, 5, x_positions, y_positions, num_x, num_y)
    
    ! Draw only the X-axis tick marks (same as in the original code)
    print *, "Drawing", num_x, "X-axis tick marks:"
    do i = 1, num_x
        print *, "  Drawing tick", i, "at x =", x_positions(i)
        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  x_positions(i), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                  x_positions(i), &
                                  real(ctx%plot_area%bottom + ctx%plot_area%height + 5, wp), &
                                  0_1, 0_1, 0_1, 0.1_wp)
    end do
    
    ! Draw only the Y-axis tick marks (same as in the original code)
    print *, "Drawing", num_y, "Y-axis tick marks:"
    do i = 1, num_y
        print *, "  Drawing tick", i, "at y =", y_positions(i)
        call draw_line_distance_aa(ctx%raster%image_data, ctx%width, ctx%height, &
                                  real(ctx%plot_area%left, wp), y_positions(i), &
                                  real(ctx%plot_area%left - 5, wp), y_positions(i), &
                                  0_1, 0_1, 0_1, 0.1_wp)
    end do
    
    ! Save the image to see the tick marks
    call write_png_file("tick_marks_test.png", ctx%width, ctx%height, ctx%raster%image_data)
    print *, "Saved tick marks to tick_marks_test.png"
    
end program debug_tick_visibility