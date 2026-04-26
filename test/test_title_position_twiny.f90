program test_title_position_twiny
    !! Test that title positioning accounts for twiny top-axis elements
    !! Regression test for issue #1700: PNG backend twin axes tick label overlap
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_constants, only: TITLE_VERTICAL_OFFSET, REFERENCE_DPI
    use fortplot_raster_axes, only: compute_title_position

    implicit none

    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480

    character(len=600) :: escaped
    real(wp) :: title_px, baseline_py, twiny_py
    integer :: top_tick_h

    margins = plot_margins_t()
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)

    ! Test 1: without twiny
    call compute_title_position(plot_area, 'Test Title', escaped, &
                                title_px, baseline_py)

    ! Test 2: with twiny (top_tick_height > 0)
    top_tick_h = 14
    call compute_title_position(plot_area, 'Test Title', escaped, &
                                title_px, twiny_py, &
                                top_tick_height=top_tick_h)

    ! Verify twiny title is above baseline
    if (twiny_py >= baseline_py) then
        print *, 'FAIL: Twiny title not above baseline:', &
                 twiny_py, '>=', baseline_py
        stop 1
    end if

    print *, 'PASS: Title positioned correctly with and without twiny'
end program test_title_position_twiny
