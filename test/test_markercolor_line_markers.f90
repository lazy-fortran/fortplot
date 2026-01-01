program test_markercolor_line_markers
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    use fortplot_2d_plots, only: add_plot
    use fortplot_figure_core, only: figure_t

    implicit none

    integer, parameter :: width = 200
    integer, parameter :: height = 150

    real(dp) :: x(2), y_line(2), y_mark(2)
    real(dp) :: rgb(width, height, 3)
    integer :: i, j
    integer :: i1, i2
    integer :: j_top1, j_top2
    integer :: j_bottom1, j_bottom2
    integer :: red_top, red_bottom
    integer :: blue_top, blue_bottom
    integer :: marker_red, marker_blue
    integer :: line_red, line_blue
    real(dp) :: max_r, max_r_g, max_r_b
    real(dp) :: max_b, max_b_r, max_b_g
    type(figure_t) :: fig

    x = [0.2_dp, 0.8_dp]
    y_line = [0.9_dp, 0.7_dp]
    y_mark = [0.2_dp, 0.4_dp]

    call fig%initialize(width=width, height=height, backend='png')
    call fig%set_xlim(0.0_dp, 1.0_dp)
    call fig%set_ylim(0.0_dp, 1.0_dp)

    call add_plot(fig, x, y_line, linestyle='-', color_rgb=[0.0_dp, 0.0_dp, &
                                                            1.0_dp])

    call add_plot(fig, x, y_mark, linestyle='none', color_rgb=[0.0_dp, &
                                                               1.0_dp, &
                                                               0.0_dp], &
                  marker='o', markercolor=[1.0_dp, 0.0_dp, 0.0_dp])

    call fig%extract_rgb_data_for_animation(rgb)

    max_r = -1.0_dp
    max_r_g = 0.0_dp
    max_r_b = 0.0_dp
    max_b = -1.0_dp
    max_b_r = 0.0_dp
    max_b_g = 0.0_dp
    do j = 1, height
        do i = 1, width
            if (rgb(i, j, 1) > max_r) then
                max_r = rgb(i, j, 1)
                max_r_g = rgb(i, j, 2)
                max_r_b = rgb(i, j, 3)
            end if
            if (rgb(i, j, 3) > max_b) then
                max_b = rgb(i, j, 3)
                max_b_r = rgb(i, j, 1)
                max_b_g = rgb(i, j, 2)
            end if
        end do
    end do

    i1 = width/4
    i2 = 3*width/4
    j_top1 = height/6
    j_top2 = height/2 - 1
    j_bottom1 = height/2 + 1
    j_bottom2 = 5*height/6

    call count_dominant_pixels(rgb, i1, i2, j_top1, j_top2, red_top, blue_top)
    call count_dominant_pixels(rgb, i1, i2, j_bottom1, j_bottom2, red_bottom, &
                               blue_bottom)

    marker_red = max(red_top, red_bottom)
    marker_blue = merge(blue_top, blue_bottom, red_top >= red_bottom)

    line_blue = max(blue_top, blue_bottom)
    line_red = merge(red_top, red_bottom, blue_top >= blue_bottom)

    if (marker_red < 12) then
        write (error_unit, *) "FAIL: expected red marker pixels in ROI"
        write (error_unit, *) "INFO: red_top:", red_top, "red_bottom:", &
            red_bottom
        write (error_unit, *) "INFO: max R:", max_r, "at (R,G,B)=", max_r, &
            max_r_g, max_r_b
        stop 1
    end if

    if (line_blue < 12) then
        write (error_unit, *) "FAIL: expected blue line pixels in ROI"
        write (error_unit, *) "INFO: blue_top:", blue_top, "blue_bottom:", &
            blue_bottom
        write (error_unit, *) "INFO: max B:", max_b, "at (R,G,B)=", max_b_r, &
            max_b_g, max_b
        stop 1
    end if

    if (red_top >= red_bottom) then
        if (blue_top >= blue_bottom) then
            write (error_unit, *) "FAIL: red and blue dominate same ROI (top)"
            write (error_unit, *) "INFO: red_top:", red_top, "blue_top:", &
                blue_top
            stop 1
        end if
    else
        if (blue_bottom >= blue_top) then
            write (error_unit, *) "FAIL: red and blue dominate same ROI (bottom)"
            write (error_unit, *) "INFO: red_bottom:", red_bottom, "blue_bottom:", &
                blue_bottom
            stop 1
        end if
    end if

    if (marker_red <= marker_blue + 6) then
        write (error_unit, *) "FAIL: marker ROI is not red-dominant"
        write (error_unit, *) "INFO: marker_red:", marker_red, "marker_blue:", &
            marker_blue
        stop 1
    end if

    if (line_blue <= line_red + 6) then
        write (error_unit, *) "FAIL: line ROI is not blue-dominant"
        write (error_unit, *) "INFO: line_blue:", line_blue, "line_red:", line_red
        stop 1
    end if

    print *, "PASS: markercolor overrides marker rendering without changing line color"

contains

    pure logical function is_red_pixel(r, g, b) result(is_red)
        real(dp), intent(in) :: r, g, b

        is_red = r > 0.65_dp .and. r > g + 0.15_dp .and. r > b + 0.15_dp
    end function is_red_pixel

    pure logical function is_blue_pixel(r, g, b) result(is_blue)
        real(dp), intent(in) :: r, g, b

        is_blue = b > 0.65_dp .and. b > r + 0.15_dp .and. b > g + 0.15_dp
    end function is_blue_pixel

    subroutine count_dominant_pixels(rgb_in, ii1, ii2, jj1, jj2, red_count, &
                                     blue_count)
        real(dp), intent(in) :: rgb_in(width, height, 3)
        integer, intent(in) :: ii1, ii2, jj1, jj2
        integer, intent(out) :: red_count, blue_count
        integer :: ii, jj

        red_count = 0
        blue_count = 0

        do jj = jj1, jj2
            do ii = ii1, ii2
                if (is_red_pixel(rgb_in(ii, jj, 1), rgb_in(ii, jj, 2), &
                                 rgb_in(ii, jj, 3))) then
                    red_count = red_count + 1
                end if
                if (is_blue_pixel(rgb_in(ii, jj, 1), rgb_in(ii, jj, 2), &
                                  rgb_in(ii, jj, 3))) then
                    blue_count = blue_count + 1
                end if
            end do
        end do
    end subroutine count_dominant_pixels

end program test_markercolor_line_markers
