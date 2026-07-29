program test_point_marker_raster
    !! A pyplot point marker must leave visible raster ink.  This is the
    !! behavioral oracle for 3D scatter plots that use marker=".".
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster, only: raster_context, create_raster_canvas
    implicit none

    integer, parameter :: width = 80, height = 60
    type(raster_context) :: ctx
    integer :: i, ink_pixels

    ctx = create_raster_canvas(width, height, 100.0_wp)
    ctx%x_min = 0.0_wp
    ctx%x_max = 1.0_wp
    ctx%y_min = 0.0_wp
    ctx%y_max = 1.0_wp
    call ctx%set_marker_colors(0.0_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp)
    call ctx%draw_marker(0.5_wp, 0.5_wp, '.', 20.0_wp)

    ink_pixels = 0
    do i = 1, size(ctx%raster%image_data), 3
        if (iand(int(ctx%raster%image_data(i)), 255) < 250) then
            ink_pixels = ink_pixels + 1
        end if
    end do

    if (ink_pixels <= 0) then
        error stop 'point marker produced no visible raster pixels'
    end if
end program test_point_marker_raster
