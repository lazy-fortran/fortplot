program test_raster_quiver_color
    !! The raster quiver shaft must retain the requested RGB colour.
    !!
    !! This is a pixel-level behavioural oracle for the signed-byte boundary:
    !! a saturated colour such as green is stored as int8(-1) in the image, but
    !! the antialiased shaft must still be drawn from the original [0,1] colour.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster, only: create_raster_canvas, raster_context
    implicit none

    integer, parameter :: width = 160, height = 120
    type(raster_context) :: context
    integer :: col, row, col_start, col_stop, row_center, pixel_index
    integer :: green_pixels

    context = create_raster_canvas(width, height, 100.0_wp)
    call context%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)
    call context%color(0.0_wp, 1.0_wp, 0.0_wp)
    call context%draw_arrow(0.75_wp, 0.5_wp, 0.5_wp, 0.0_wp, 1.0_wp, '->')

    col_start = nint((0.25_wp - context%x_min)/(context%x_max - context%x_min) &
        * real(context%plot_area%width, wp) + real(context%plot_area%left, wp)) + 2
    col_stop = nint((0.75_wp - context%x_min)/(context%x_max - context%x_min) &
        * real(context%plot_area%width, wp) + real(context%plot_area%left, wp)) - 12
    row_center = nint(real(context%plot_area%bottom + context%plot_area%height, wp) &
        - (0.5_wp - context%y_min)/(context%y_max - context%y_min) &
        * real(context%plot_area%height, wp))

    green_pixels = 0
    do row = max(0, row_center - 2), min(height - 1, row_center + 2)
        do col = max(0, col_start), min(width - 1, col_stop)
            pixel_index = (row*width + col)*3 + 1
            if (byte_value(context%raster%image_data(pixel_index + 1)) > &
                byte_value(context%raster%image_data(pixel_index)) + 30 &
                .and. byte_value(context%raster%image_data(pixel_index + 1)) > &
                byte_value(context%raster%image_data(pixel_index + 2)) + 30) then
                green_pixels = green_pixels + 1
            end if
        end do
    end do

    if (green_pixels < 5) then
        error stop 'raster quiver shaft lost its requested colour'
    end if

contains

    pure integer function byte_value(value) result(unsigned)
        integer(1), intent(in) :: value

        unsigned = iand(int(value), 255)
    end function byte_value

end program test_raster_quiver_color
