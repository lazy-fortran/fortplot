program test_ylabel_no_overlap_with_ticks
    !! Ensure ylabel is positioned left of y-tick labels (no overlap)
    use fortplot_layout,       only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_axes,         only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_text,         only: calculate_text_width, calculate_text_height
    use fortplot_constants,    only: TICK_MARK_LENGTH
    use fortplot_raster_axes,  only: compute_ylabel_x_pos, y_tick_label_right_edge_at_axis
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none

    type(plot_margins_t) :: margins
    type(plot_area_t)    :: plot_area
    integer :: width, height
    real(wp) :: y_ticks(MAX_TICKS)
    integer :: n, i, max_tick_width
    character(len=64) :: s
    character(len=*), parameter :: yscale = 'linear'
    real(wp), parameter :: y_min = 0.0_wp, y_max = 1000000.0_wp  ! wide labels
    real(wp), parameter :: symlog_threshold = 1.0_wp
    character(len=*), parameter :: ylabel = 'Y Label'
    integer :: rotated_width, x_ylabel, right_edge_ticks

    ! Canvas and plot area
    width = 640; height = 480
    call calculate_plot_area(width, height, margins, plot_area)

    ! Compute max y-tick label width for a wide numeric range
    call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, y_ticks, n)
    max_tick_width = 0
    do i = 1, n
        s = format_tick_label(y_ticks(i), yscale)
        max_tick_width = max(max_tick_width, calculate_text_width(trim(s)))
    end do

    ! Rotated ylabel width equals unrotated text height
    rotated_width = calculate_text_height(ylabel)

    ! Compute ylabel position using the helper
    x_ylabel = compute_ylabel_x_pos(plot_area, rotated_width, max_tick_width)
    right_edge_ticks = y_tick_label_right_edge_at_axis(plot_area)

    if (x_ylabel + rotated_width >= right_edge_ticks) then
        write(error_unit,*) 'ERROR: ylabel overlaps or touches y-tick labels area.'
        write(error_unit,*) '       x_ylabel+rot_w =', x_ylabel + rotated_width, ' >= right_edge_ticks =', right_edge_ticks
        stop 1
    end if

    print *, 'PASS: ylabel positioned left of y-tick labels (gap maintained).'
end program test_ylabel_no_overlap_with_ticks

