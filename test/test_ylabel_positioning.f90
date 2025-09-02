! Verify PNG ylabel placement math is width-invariant (no half-width bug)
program test_ylabel_positioning
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_layout,       only: plot_area_t
    use fortplot_raster_axes,  only: compute_ylabel_x_pos, y_tick_label_right_edge_at_axis
    implicit none

    type(plot_area_t) :: pa
    integer :: x1, x2, rw1, rw2, ytick_w
    integer :: right1, right2, r_edge_axis
    logical :: ok

    ! Choose a plot area with ample left margin to keep text within bounds
    pa%left = 200; pa%bottom = 50; pa%width = 400; pa%height = 300

    ! Simulate a reasonable maximum y-tick label width (in pixels)
    ytick_w = 30

    ! Two different rotated text widths (simulate short vs long ylabel)
    rw1 = 40
    rw2 = 80

    x1 = compute_ylabel_x_pos(pa, rw1, ytick_w)
    x2 = compute_ylabel_x_pos(pa, rw2, ytick_w)

    right1 = x1 + rw1
    right2 = x2 + rw2
    r_edge_axis = y_tick_label_right_edge_at_axis(pa)

    ok = .true.

    ! Property 1: Right edge of ylabel should not depend on rotated text width
    if (right1 /= right2) then
        print *, 'FAIL: ylabel right edge depends on text width (', right1, 'vs', right2, ')'
        ok = .false.
    end if

    ! Property 2: Ylabel should stay left of y-tick label right edge
    if (.not. (right1 <= r_edge_axis - 1)) then
        print *, 'FAIL: ylabel overlaps y-tick labels (right=', right1, ', r_edge_axis=', r_edge_axis, ')'
        ok = .false.
    end if

    if (ok) then
        print *, 'PASS: Ylabel positioning is width-invariant and clears tick labels'
        stop 0
    else
        stop 1
    end if
end program test_ylabel_positioning

