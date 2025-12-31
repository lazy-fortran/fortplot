program test_ylabel
    !! Comprehensive test suite for ylabel positioning and rendering
    !! Consolidates: test_ylabel_boundary_protection_1136, test_ylabel_comprehensive,
    !! test_ylabel_min_margin, test_ylabel_no_cutoff, test_ylabel_no_overlap_with_ticks,
    !! test_ylabel_positioning, test_ylabel_rotation_clipping
    use fortplot
    use fortplot_raster_axes, only: compute_ylabel_x_pos_old, compute_ylabel_x_pos, &
        y_tick_label_right_edge_at_axis_old, y_tick_label_right_edge_at_axis, &
        raster_render_ylabel
    use fortplot_layout, only: plot_area_t, plot_margins_t, calculate_plot_area
    use fortplot_margins, only: plot_area_t
    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_text, only: calculate_text_width, calculate_text_height
    use fortplot_constants, only: TICK_MARK_LENGTH
    use fortplot_raster_core, only: raster_image_t, create_raster_image, destroy_raster_image
    use test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    call test_min_margin()
    call test_no_cutoff()
    call test_no_overlap_with_ticks()
    call test_positioning_width_invariant()
    call test_rotation_clipping()
    call test_comprehensive_positioning()
    call test_boundary_protection_1136()

    print *, ''
    print *, '=== Y-label Test Summary ==='
    print *, 'Tests passed:', passed_tests, '/', total_tests

    if (passed_tests == total_tests) then
        print *, 'All ylabel tests PASSED!'
        stop 0
    else
        print *, 'FAIL: Some ylabel tests failed'
        stop 1
    end if

contains

    subroutine test_min_margin()
        !! Ensure ylabel x-position enforces a sensible minimum left margin
        type(plot_area_t) :: area
        integer :: rotated_width, edge, x_pos

        total_tests = total_tests + 1

        area%left = 10
        area%bottom = 0
        area%width = 400
        area%height = 300

        rotated_width = 60
        edge = y_tick_label_right_edge_at_axis(area, 0)
        x_pos = compute_ylabel_x_pos(edge, rotated_width, area)

        if (x_pos < 15) then
            print *, 'FAIL: test_min_margin - ylabel minimum left margin not enforced:', x_pos
        else
            print *, '  PASS: test_min_margin'
            passed_tests = passed_tests + 1
        end if
    end subroutine test_min_margin

    subroutine test_no_cutoff()
        !! Test that ylabel is not cut off at canvas edge with various configurations
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        integer :: ylabel_width, ytick_max_width, x_pos
        logical :: ok

        total_tests = total_tests + 1
        ok = .true.

        call calculate_plot_area(800, 600, margins, plot_area)
        ylabel_width = 40
        ytick_max_width = 60
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)

        if (x_pos < 1) then
            ok = .false.
        end if
        if (x_pos + ylabel_width > plot_area%left - 15) then
            ok = .false.
        end if

        call calculate_plot_area(400, 300, margins, plot_area)
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
        if (x_pos < 1) then
            ok = .false.
        end if

        call calculate_plot_area(800, 600, margins, plot_area)
        ylabel_width = 80
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
        if (x_pos < 1) then
            ok = .false.
        end if

        if (ok) then
            print *, '  PASS: test_no_cutoff'
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_no_cutoff - Y-label positioning prevents cutoff failed'
        end if
    end subroutine test_no_cutoff

    subroutine test_no_overlap_with_ticks()
        !! Ensure ylabel is positioned left of y-tick labels (no overlap)
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        integer :: width, height, n, i, max_tick_width
        real(wp) :: y_ticks(MAX_TICKS)
        character(len=64) :: s
        character(len=*), parameter :: yscale = 'linear'
        real(wp), parameter :: y_min = 0.0_wp, y_max = 1000000.0_wp
        real(wp), parameter :: symlog_threshold = 1.0_wp
        character(len=*), parameter :: ylabel_text = 'Y Label'
        integer :: rotated_width, x_ylabel, right_edge_ticks

        total_tests = total_tests + 1

        width = 640
        height = 480
        call calculate_plot_area(width, height, margins, plot_area)

        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, y_ticks, n)
        max_tick_width = 0
        do i = 1, n
            s = format_tick_label(y_ticks(i), yscale)
            max_tick_width = max(max_tick_width, calculate_text_width(trim(s)))
        end do

        rotated_width = calculate_text_height(ylabel_text)
        right_edge_ticks = y_tick_label_right_edge_at_axis(plot_area, max_tick_width)
        x_ylabel = compute_ylabel_x_pos(right_edge_ticks, rotated_width, plot_area)

        if (x_ylabel + rotated_width >= right_edge_ticks) then
            print *, 'FAIL: test_no_overlap_with_ticks - ylabel overlaps y-tick labels'
        else
            print *, '  PASS: test_no_overlap_with_ticks'
            passed_tests = passed_tests + 1
        end if
    end subroutine test_no_overlap_with_ticks

    subroutine test_positioning_width_invariant()
        !! Verify PNG ylabel placement math is width-invariant (no half-width bug)
        type(plot_area_t) :: pa
        integer :: x1, x2, rw1, rw2, ytick_w
        integer :: right1, right2, r_edge_axis
        logical :: ok

        total_tests = total_tests + 1

        pa%left = 200
        pa%bottom = 50
        pa%width = 400
        pa%height = 300

        ytick_w = 30
        rw1 = 40
        rw2 = 80

        x1 = compute_ylabel_x_pos_old(pa, rw1, ytick_w)
        x2 = compute_ylabel_x_pos_old(pa, rw2, ytick_w)

        right1 = x1 + rw1
        right2 = x2 + rw2
        r_edge_axis = y_tick_label_right_edge_at_axis_old(pa)

        ok = .true.
        if (right1 /= right2) then
            ok = .false.
        end if
        if (.not. (right1 <= r_edge_axis - 1)) then
            ok = .false.
        end if

        if (ok) then
            print *, '  PASS: test_positioning_width_invariant'
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_positioning_width_invariant - width dependency or overlap'
        end if
    end subroutine test_positioning_width_invariant

    subroutine test_rotation_clipping()
        !! Validate rotated Y-axis label renders without top-row clipping
        type(raster_image_t) :: raster
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        integer :: width, height
        integer :: left_band_x0, left_band_x1, y0, y1
        integer :: nonwhite_count
        character(len=*), parameter :: label = 'bdfgjqy'

        total_tests = total_tests + 1

        width = 400
        height = 300
        raster = create_raster_image(width, height)
        call calculate_plot_area(width, height, margins, plot_area)

        call raster_render_ylabel(raster, width, height, plot_area, label)

        left_band_x1 = max(1, plot_area%left - 1)
        left_band_x0 = max(1, plot_area%left - 80)
        y0 = max(1, plot_area%bottom)
        y1 = min(height, plot_area%bottom + plot_area%height)

        nonwhite_count = count_non_white_in_band(raster%image_data, width, height, &
            left_band_x0, left_band_x1, y0, y1)

        call destroy_raster_image(raster)

        if (nonwhite_count <= 0) then
            print *, 'FAIL: test_rotation_clipping - rotated ylabel appears missing'
        else
            print *, '  PASS: test_rotation_clipping'
            passed_tests = passed_tests + 1
        end if
    end subroutine test_rotation_clipping

    subroutine test_comprehensive_positioning()
        !! Comprehensive test for ylabel positioning (Issue #1136)
        type(plot_area_t) :: plot_area
        type(plot_margins_t) :: margins
        integer :: x_pos, ylabel_width, ytick_max_width
        integer :: sub_passed, sub_count

        total_tests = total_tests + 1
        sub_count = 0
        sub_passed = 0

        sub_count = sub_count + 1
        call calculate_plot_area(800, 600, margins, plot_area)
        ylabel_width = 20
        ytick_max_width = 30
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
        if (x_pos > 0) sub_passed = sub_passed + 1

        sub_count = sub_count + 1
        call calculate_plot_area(800, 600, margins, plot_area)
        ylabel_width = 30
        ytick_max_width = 40
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
        if (x_pos >= 15) sub_passed = sub_passed + 1

        sub_count = sub_count + 1
        ylabel_width = 50
        ytick_max_width = 100
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
        if (x_pos >= 15) sub_passed = sub_passed + 1

        sub_count = sub_count + 1
        call calculate_plot_area(400, 300, margins, plot_area)
        ylabel_width = 40
        ytick_max_width = 60
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
        if (x_pos >= 15) sub_passed = sub_passed + 1

        sub_count = sub_count + 1
        call calculate_plot_area(800, 600, margins, plot_area)
        ylabel_width = 100
        ytick_max_width = 50
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
        if (x_pos >= 15) sub_passed = sub_passed + 1

        sub_count = sub_count + 1
        ylabel_width = 40
        ytick_max_width = 50
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
        if (x_pos >= 15) sub_passed = sub_passed + 1

        sub_count = sub_count + 1
        ylabel_width = 30
        ytick_max_width = 0
        x_pos = compute_ylabel_x_pos_old(plot_area, ylabel_width, ytick_max_width)
        if (x_pos > 0) sub_passed = sub_passed + 1

        if (sub_passed == sub_count) then
            print *, '  PASS: test_comprehensive_positioning'
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_comprehensive_positioning -', sub_passed, '/', sub_count, 'subtests'
        end if
    end subroutine test_comprehensive_positioning

    subroutine test_boundary_protection_1136()
        !! Critical test for Issue #1136 - ylabel boundary protection
        type(plot_area_t) :: plot_area
        type(plot_margins_t) :: margins
        integer :: x_pos_long, x_pos_extreme
        logical :: ok
        real(wp) :: x(100), y_large(50)
        character(len=:), allocatable :: output_dir
        integer :: i

        total_tests = total_tests + 1
        ok = .true.

        call ensure_test_output_dir('ylabel_boundary_protection_1136', output_dir)

        do i = 1, 100
            x(i) = real(i-1, wp) * 0.05_wp
            if (i <= 50) then
                y_large(i) = sin(x(i)) * 123456.789_wp
            end if
        end do

        call figure(figsize=[8.0_8, 6.0_8])
        call plot(x(1:30), x(1:30)**2, 'b-')
        call title('Issue #1136 Critical Test - Long Y-label')
        call xlabel('X values')
        call ylabel('EXTREMELY LONG Y-AXIS LABEL THAT MUST REMAIN COMPLETELY ' // &
                    'VISIBLE AND NOT BE CUT OFF BY WHITE BLOCKS OR CANVAS BOUNDARIES')
        call savefig(trim(output_dir)//'test_ylabel_extreme_length_1136.png')

        call figure(figsize=[6.0_8, 8.0_8])
        call plot(x(1:50), y_large, 'r--')
        call title('Wide Tick Labels Test')
        call xlabel('X axis')
        call ylabel('Y-label Must Stay Visible With Wide Ticks')
        call savefig(trim(output_dir)//'test_ylabel_wide_ticks_1136.png')

        call figure(figsize=[4.0_8, 3.0_8])
        call plot(x(1:20), x(1:20)*100.0_wp, 'g:')
        call title('Small Canvas Y-label Test')
        call xlabel('X')
        call ylabel('Long Y-label In Cramped Space Should Still Show Completely')
        call savefig(trim(output_dir)//'test_ylabel_small_canvas_1136.png')

        call calculate_plot_area(800, 600, margins, plot_area)

        x_pos_long = compute_ylabel_x_pos_old(plot_area, 120, 50)
        if (.not. (x_pos_long >= 15 .and. x_pos_long + 120 <= 800)) then
            ok = .false.
        end if

        x_pos_extreme = compute_ylabel_x_pos_old(plot_area, 200, 80)
        if (.not. (x_pos_extreme >= 50 .and. x_pos_extreme + 200 <= 800)) then
            ok = .false.
        end if

        if (ok) then
            print *, '  PASS: test_boundary_protection_1136'
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_boundary_protection_1136 - boundary calculations failed'
        end if
    end subroutine test_boundary_protection_1136

    function count_non_white_in_band(img, w, h, x0, x1, y0, y1) result(cnt)
        integer(1), intent(in) :: img(*)
        integer, intent(in) :: w, h, x0, x1, y0, y1
        integer :: cnt, x, y, idx
        cnt = 0
        do y = y0, y1
            do x = x0, x1
                idx = ((y - 1) * w + (x - 1)) * 3 + 1
                if (idx >= 1 .and. idx + 2 <= w * h * 3) then
                    if (img(idx) /= -1_1 .or. img(idx+1) /= -1_1 .or. img(idx+2) /= -1_1) then
                        cnt = cnt + 1
                    end if
                end if
            end do
        end do
    end function count_non_white_in_band

end program test_ylabel
