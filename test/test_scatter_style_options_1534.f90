program test_scatter_style_options_1534
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_figure_core, only: figure_t
    implicit none

    integer, parameter :: width = 220
    integer, parameter :: height = 180
    real(wp) :: rgb_opaque(width, height, 3)
    real(wp) :: rgb_transparent(width, height, 3)
    real(wp) :: rgb_thick(width, height, 3)
    real(wp) :: min_lum_opaque, min_lum_trans, min_lum_thick
    real(wp) :: max_redness_opaque, max_redness_dummy
    integer :: red_count_thin, red_count_thick

    call render_scatter(rgb_opaque, alpha=1.0_wp, linewidth=1.0_wp)
    call render_scatter(rgb_transparent, alpha=0.2_wp, linewidth=1.0_wp)
    call render_scatter(rgb_thick, alpha=1.0_wp, linewidth=5.0_wp)

    call analyze_roi(rgb_opaque, min_lum_opaque, max_redness_opaque, red_count_thin)
    call analyze_roi(rgb_transparent, min_lum_trans, max_redness_dummy, red_count_thick)
    call analyze_roi(rgb_thick, min_lum_thick, max_redness_dummy, red_count_thick)

    if (min_lum_opaque > 0.30_wp) then
        write (error_unit, *) "FAIL: expected opaque marker to include dark face pixels"
        write (error_unit, *) "INFO: min luminance:", min_lum_opaque
        stop 1
    end if

    if (min_lum_trans < 0.55_wp) then
        write (error_unit, *) "FAIL: expected alpha to increase face luminance"
        write (error_unit, *) "INFO: min luminance (alpha=0.2):", min_lum_trans
        stop 1
    end if

    if (max_redness_opaque < 0.30_wp) then
        write (error_unit, *) "FAIL: expected edgecolor to produce red-dominant pixels"
        write (error_unit, *) "INFO: max redness:", max_redness_opaque
        stop 1
    end if

    if (red_count_thick <= red_count_thin + 20) then
        write (error_unit, *) "FAIL: expected linewidth to increase red edge pixels"
        write (error_unit, *) "INFO: red_count thin:", red_count_thin
        write (error_unit, *) "INFO: red_count thick:", red_count_thick
        stop 1
    end if

    print *, "PASS: scatter alpha/edgecolor/facecolor/linewidth affect raster rendering"

contains

    subroutine render_scatter(rgb, alpha, linewidth)
        real(wp), intent(out) :: rgb(width, height, 3)
        real(wp), intent(in) :: alpha, linewidth
        type(figure_t) :: fig
        real(wp) :: x(1), y(1)

        x = [0.5_wp]
        y = [0.5_wp]

        call fig%initialize(width=width, height=height, backend='png')
        call fig%set_xlim(0.0_wp, 1.0_wp)
        call fig%set_ylim(0.0_wp, 1.0_wp)

        call fig%scatter(x, y, marker='o', facecolor=[0.0_wp, 0.0_wp, 0.0_wp], &
                         edgecolor=[1.0_wp, 0.0_wp, 0.0_wp], alpha=alpha, &
                         linewidth=linewidth)

        call fig%extract_rgb_data_for_animation(rgb)
    end subroutine render_scatter

    subroutine analyze_roi(rgb, min_lum, max_redness, red_count)
        real(wp), intent(in) :: rgb(width, height, 3)
        real(wp), intent(out) :: min_lum, max_redness
        integer, intent(out) :: red_count
        integer :: i, j
        integer :: i1, i2, j1, j2
        real(wp) :: lum, redness

        i1 = width/4
        i2 = 3*width/4
        j1 = height/4
        j2 = 3*height/4

        min_lum = 1.0_wp
        max_redness = -1.0_wp
        red_count = 0

        do j = j1, j2
            do i = i1, i2
                lum = (rgb(i, j, 1) + rgb(i, j, 2) + rgb(i, j, 3))/3.0_wp
                min_lum = min(min_lum, lum)

                redness = rgb(i, j, 1) - 0.5_wp*(rgb(i, j, 2) + rgb(i, j, 3))
                max_redness = max(max_redness, redness)

                if (rgb(i, j, 1) > 0.75_wp .and. rgb(i, j, 1) > rgb(i, j, 2) + 0.15_wp &
                    .and. rgb(i, j, 1) > rgb(i, j, 3) + 0.15_wp) then
                    red_count = red_count + 1
                end if
            end do
        end do
    end subroutine analyze_roi

end program test_scatter_style_options_1534
