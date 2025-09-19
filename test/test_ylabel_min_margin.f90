program test_ylabel_min_margin
    !! Ensure ylabel x-position enforces a sensible minimum left margin
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_axes, only: compute_ylabel_x_pos, y_tick_label_right_edge_at_axis
    implicit none

    type(plot_area_t) :: area
    integer :: rotated_width
    integer :: edge
    integer :: x_pos

    ! Craft a tight-left plot area with a wide rotated label to force clamping
    area%left = 10
    area%bottom = 0
    area%width = 400
    area%height = 300

    rotated_width = 60

    edge = y_tick_label_right_edge_at_axis(area, 0)
    x_pos = compute_ylabel_x_pos(edge, rotated_width, area)

    if (x_pos < 15) then
        print *, 'FAIL: ylabel minimum left margin not enforced:', x_pos
        stop 1
    end if

    print *, 'PASS: ylabel minimum left margin enforced (>=15px)'
end program test_ylabel_min_margin
