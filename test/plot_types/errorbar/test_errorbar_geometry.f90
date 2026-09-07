program test_errorbar_geometry
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_errorbar_plots, only: errorbar_impl
    use fortplot_matplotlib, only: figure, errorbar, get_global_figure
    use fortplot_test_helpers, only: test_get_temp_path, &
        test_initialize_environment
    implicit none

    integer, parameter :: image_width = 640, image_height = 480
    real(wp) :: rgb(image_width, image_height, 3)
    real(wp), parameter :: red(3) = [1.0_wp, 0.0_wp, 0.0_wp]
    real(wp), parameter :: blue(3) = [0.0_wp, 0.0_wp, 1.0_wp]
    type(figure_t) :: fig
    integer :: horizontal_cap, vertical_cap, small_area, large_area
    logical :: red_pixels(image_width, image_height)

    call test_initialize_environment('errorbar_geometry')
    call check_cap_lengths()
    call check_independent_error_style()
    call check_marker_size()
    call check_format_string()
    print *, 'PASS: errorbar cap geometry, colors, solid stems, and marker sizes'

contains

    subroutine initialize_figure()
        call fig%initialize(width=image_width, height=image_height, backend='png')
        call fig%set_xlim(0.0_wp, 1.0_wp)
        call fig%set_ylim(0.0_wp, 1.0_wp)
    end subroutine initialize_figure

    subroutine extract_red_pixels()
        call fig%extract_rgb_data_for_animation(rgb)
        red_pixels = rgb(:, :, 1) > 0.8_wp
        red_pixels = red_pixels .and. rgb(:, :, 2) < 0.2_wp
        red_pixels = red_pixels .and. rgb(:, :, 3) < 0.2_wp
    end subroutine extract_red_pixels

    subroutine check_cap_lengths()
        real(wp), parameter :: capsize = 8.0_wp
        real(wp), parameter :: expected_pixels = 2.0_wp*capsize*100.0_wp/72.0_wp
        integer :: i

        ! Actual Matplotlib errorbar uses cap-marker diameter = 2*capsize points.
        ! Pixel spans, including AA rounding, must agree in both orientations.
        call initialize_figure()
        call errorbar_impl(fig, [0.5_wp], [0.5_wp], yerr=[0.25_wp], &
            capsize=capsize, color=red, linestyle='none')
        call extract_red_pixels()
        horizontal_cap = 0
        do i = 1, image_height
            horizontal_cap = max(horizontal_cap, count(red_pixels(:, i)))
        end do
        call assert_true(abs(real(horizontal_cap, wp) - expected_pixels) < 3.0_wp, &
            'Y-errorbar caps span twice capsize in points')
        call fig%savefig(test_get_temp_path('errorbar_caps_y.png'))
        call fig%savefig(test_get_temp_path('errorbar_caps_y.pdf'))

        call initialize_figure()
        call errorbar_impl(fig, [0.5_wp], [0.5_wp], xerr=[0.25_wp], &
            capsize=capsize, color=red, linestyle='none')
        call extract_red_pixels()
        vertical_cap = 0
        do i = 1, image_width
            vertical_cap = max(vertical_cap, count(red_pixels(i, :)))
        end do
        call assert_true(abs(real(vertical_cap, wp) - expected_pixels) < 3.0_wp, &
            'X-errorbar caps span twice capsize in points')
        call fig%savefig(test_get_temp_path('errorbar_caps_x.png'))
        call fig%savefig(test_get_temp_path('errorbar_caps_x.pdf'))
    end subroutine check_cap_lengths

    subroutine check_independent_error_style()
        integer :: i, column, max_count, first_row, last_row
        logical :: blue_pixels(image_width, image_height)

        call initialize_figure()
        call errorbar_impl(fig, [0.25_wp, 0.75_wp], [0.5_wp, 0.5_wp], &
            yerr=[0.25_wp, 0.25_wp], capsize=8.0_wp, color=blue, &
            ecolor=red, linestyle='--')
        call extract_red_pixels()
        blue_pixels = rgb(:, :, 3) > 0.8_wp
        blue_pixels = blue_pixels .and. rgb(:, :, 1) < 0.2_wp
        blue_pixels = blue_pixels .and. rgb(:, :, 2) < 0.2_wp
        call assert_true(count(blue_pixels) > 100, 'data line keeps blue color')
        call assert_true(count(red_pixels) > 100, 'error stems use separate red color')
        max_count = 0
        column = 1
        do i = 1, image_width
            if (count(red_pixels(i, :)) > max_count) then
                max_count = count(red_pixels(i, :))
                column = i
            end if
        end do
        first_row = findloc(red_pixels(column, :), .true., dim=1)
        last_row = findloc(red_pixels(column, :), .true., dim=1, back=.true.)
        call assert_true(max_count > 0.95_wp*(last_row - first_row + 1), &
            'error stems stay solid for dashed data lines')
        call fig%savefig(test_get_temp_path('errorbar_independent_style.png'))
        call fig%savefig(test_get_temp_path('errorbar_independent_style.pdf'))
    end subroutine check_independent_error_style

    subroutine check_marker_size()
        call initialize_figure()
        call errorbar_impl(fig, [0.5_wp], [0.5_wp], marker='o', &
            markersize=4.0_wp, color=red, linestyle='none')
        call extract_red_pixels()
        small_area = count(red_pixels)
        call initialize_figure()
        call errorbar_impl(fig, [0.5_wp], [0.5_wp], marker='o', &
            markersize=12.0_wp, color=red, linestyle='none')
        call extract_red_pixels()
        large_area = count(red_pixels)
        call assert_true(small_area > 0, 'small explicit marker is visible')
        call assert_true(large_area > 5*small_area, &
            'tripling marker diameter substantially increases area')
    end subroutine check_marker_size

    subroutine check_format_string()
        type(figure_t), pointer :: global_figure
        integer :: row, max_span

        call figure()
        call errorbar([0.25_wp, 0.75_wp], [0.5_wp, 0.5_wp], fmt='ro')
        global_figure => get_global_figure()
        call global_figure%set_xlim(0.0_wp, 1.0_wp)
        call global_figure%set_ylim(0.0_wp, 1.0_wp)
        call global_figure%extract_rgb_data_for_animation(rgb)
        red_pixels = rgb(:, :, 1) > 0.8_wp
        red_pixels = red_pixels .and. rgb(:, :, 2) < 0.2_wp
        red_pixels = red_pixels .and. rgb(:, :, 3) < 0.2_wp
        call assert_true(count(red_pixels) > 20, 'fmt=ro renders red circle markers')
        max_span = 0
        do row = 1, image_height
            max_span = max(max_span, count(red_pixels(:, row)))
        end do
        call assert_true(max_span < 40, 'marker-only fmt does not connect points')
    end subroutine check_format_string

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) return
        print *, 'FAIL: ', description
        error stop 1
    end subroutine assert_true

end program test_errorbar_geometry
