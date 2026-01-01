program test_markercolor_line_markers
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_figure_core, only: figure_t
    use fortplot_2d_plots, only: add_plot
    implicit none

    integer, parameter :: width = 200
    integer, parameter :: height = 150
    real(wp) :: x(2), y_line(2), y_mark(2)
    real(wp) :: rgb(width, height, 3)
    integer :: i, j
    integer :: red_count, blue_count
    real(wp) :: max_r, max_r_g, max_r_b
    real(wp) :: max_b, max_b_r, max_b_g
    type(figure_t) :: fig

    x = [0.2_wp, 0.8_wp]
    y_line = [0.9_wp, 0.7_wp]
    y_mark = [0.2_wp, 0.4_wp]

    call fig%initialize(width=width, height=height, backend='png')
    call fig%set_xlim(0.0_wp, 1.0_wp)
    call fig%set_ylim(0.0_wp, 1.0_wp)

    call add_plot(fig, x, y_line, linestyle='-', color_rgb=[0.0_wp, 0.0_wp, 1.0_wp])

    call add_plot(fig, x, y_mark, linestyle='none', color_rgb=[0.0_wp, &
                                                               1.0_wp, 0.0_wp], &
                  marker='o', markercolor=[1.0_wp, 0.0_wp, 0.0_wp])

    if (.not. fig%plots(2)%marker_color_set) then
        write (error_unit, *) "FAIL: markercolor did not set plot marker_color_set"
        stop 1
    end if

    call fig%extract_rgb_data_for_animation(rgb)

    red_count = 0
    blue_count = 0
    max_r = -1.0_wp
    max_r_g = 0.0_wp
    max_r_b = 0.0_wp
    max_b = -1.0_wp
    max_b_r = 0.0_wp
    max_b_g = 0.0_wp
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

            if (rgb(i, j, 1) > 0.45_wp .and. rgb(i, j, 1) > rgb(i, j, 2) + 0.10_wp &
                .and. rgb(i, j, 1) > rgb(i, j, 3) + 0.10_wp) then
                red_count = red_count + 1
            end if
            if (rgb(i, j, 3) > 0.45_wp .and. rgb(i, j, 3) > rgb(i, j, 1) + 0.10_wp &
                .and. rgb(i, j, 3) > rgb(i, j, 2) + 0.10_wp) then
                blue_count = blue_count + 1
            end if
        end do
    end do

    if (red_count < 10) then
        write (error_unit, *) "FAIL: expected red marker pixels, got:", red_count
        write (error_unit, *) "INFO: max R:", max_r, "at (R,G,B)=", max_r, &
            max_r_g, max_r_b
        stop 1
    end if

    if (blue_count < 10) then
        write (error_unit, *) "FAIL: expected blue line pixels, got:", blue_count
        write (error_unit, *) "INFO: max B:", max_b, "at (R,G,B)=", max_b_r, &
            max_b_g, max_b
        stop 1
    end if

    print *, "PASS: markercolor overrides marker rendering without changing line color"
end program test_markercolor_line_markers
