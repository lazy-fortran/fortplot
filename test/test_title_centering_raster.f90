program test_title_centering_raster
    !! Verifies raster title centering over plot area
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_constants, only: TITLE_VERTICAL_OFFSET
    use fortplot_text, only: calculate_text_width_with_size, TITLE_FONT_SIZE
    use fortplot_raster_axes, only: compute_title_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    integer, parameter :: CANVAS_WIDTH = 640
    integer, parameter :: CANVAS_HEIGHT = 480

    character(len=*), parameter :: title_text = 'Simple Sine Wave'
    character(len=500) :: processed_text, escaped_text
    integer :: processed_len
    real(wp) :: title_px_r, title_py_r
    integer :: expected_px, expected_py, measured_width

    margins = plot_margins_t()
    call calculate_plot_area(CANVAS_WIDTH, CANVAS_HEIGHT, margins, plot_area)

    call compute_title_position(plot_area, title_text, processed_text, processed_len, &
                                escaped_text, title_px_r, title_py_r)

    ! Use the title font size for width calculation, matching the actual implementation
    measured_width = calculate_text_width_with_size(trim(escaped_text), real(TITLE_FONT_SIZE, wp))
    ! If width calculation fails, use the same fallback as the implementation
    if (measured_width <= 0) then
        measured_width = len_trim(escaped_text) * 12
    end if
    expected_px = plot_area%left + plot_area%width/2 - measured_width/2
    expected_py = plot_area%bottom - TITLE_VERTICAL_OFFSET

    if (int(title_px_r) /= expected_px) then
        print *, 'FAIL: Title X not centered; got ', int(title_px_r), ' expected ', expected_px
        stop 1
    end if

    if (int(title_py_r) /= expected_py) then
        print *, 'FAIL: Title Y offset incorrect; got ', int(title_py_r), ' expected ', expected_py
        stop 1
    end if

    print *, 'PASS: Title centering matches expected pixel position'
end program test_title_centering_raster

