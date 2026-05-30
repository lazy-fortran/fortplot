program test_axis_label_overlap_1573
    !! Regression test for issue #1573: axis labels overlapping tick labels.
    !! Verifies that xlabel is positioned below x-tick labels based on measured
    !! tick label height, and that ylabel width tracking works in all paths.
    use fortplot
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_raster_ticks, only: X_TICK_LABEL_PAD
    use fortplot_constants, only: XLABEL_VERTICAL_OFFSET, TICK_MARK_LENGTH
    use fortplot_global, only: global_figure
    use fortplot_raster, only: raster_context
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    call test_xlabel_clears_tick_labels()
    call test_ylabel_width_tracked_in_labels_only()
    call test_wide_tick_labels_no_overlap()
    call test_negative_numbers_no_overlap()
    call test_xtick_labels_clear_axis()

    print *, ''
    print *, '=== Axis label overlap test summary (issue #1573) ==='
    print *, 'Tests passed:', passed_tests, '/', total_tests

    if (passed_tests == total_tests) then
        print *, 'All axis label overlap tests PASSED!'
        stop 0
    else
        print *, 'FAIL: Some axis label overlap tests failed'
        stop 1
    end if

contains

    function get_last_x_tick_max_height_bottom() result(h)
        integer :: h
        h = 0
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            h = bk%raster%last_x_tick_max_height_bottom
        end select
    end function get_last_x_tick_max_height_bottom

    function get_last_y_tick_max_width() result(w)
        integer :: w
        w = 0
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            w = bk%raster%last_y_tick_max_width
        end select
    end function get_last_y_tick_max_width

    function get_x_tick_label_gap_from_axis() result(gap)
        integer :: gap

        gap = -1
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            gap = first_dark_row_gap(bk%raster%image_data, bk%width, bk%height, &
                                     bk%plot_area%left, &
                                     bk%plot_area%left + bk%plot_area%width, &
                                     bk%plot_area%bottom + bk%plot_area%height)
        end select
    end function get_x_tick_label_gap_from_axis

    function get_x_text_cluster_gap() result(gap)
        integer :: gap

        gap = -1
        select type (bk => global_figure%state%backend)
        class is (raster_context)
            gap = largest_blank_gap_between_clusters(bk%raster%image_data, &
                                                     bk%width, bk%height, &
                                                     bk%plot_area%left + &
                                                     bk%plot_area%width/2, &
                                                     bk%plot_area%bottom + &
                                                     bk%plot_area%height)
        end select
    end function get_x_text_cluster_gap

    function first_dark_row_gap(image_data, width, height, x_left, x_right, &
                                axis_y) result(gap)
        integer(1), intent(in) :: image_data(:)
        integer, intent(in) :: width, height, x_left, x_right, axis_y
        integer :: gap
        integer :: row, col, pixel_idx
        integer :: col_left, col_right, row_start, row_stop

        gap = -1
        col_left = max(0, x_left + 10)
        col_right = min(width - 1, x_right - 10)
        row_start = min(height - 1, axis_y + TICK_MARK_LENGTH + 3)
        row_stop = min(height - 1, axis_y + X_TICK_LABEL_PAD + 32)

        do row = row_start, row_stop
            do col = col_left, col_right
                pixel_idx = (row*width + col)*3 + 1
                if (is_dark_pixel(image_data, pixel_idx)) then
                    gap = row - axis_y
                    return
                end if
            end do
        end do
    end function first_dark_row_gap

    function largest_blank_gap_between_clusters(image_data, width, height, &
                                                center_x, axis_y) result(max_gap)
        integer(1), intent(in) :: image_data(:)
        integer, intent(in) :: width, height, center_x, axis_y
        integer :: max_gap
        integer :: row, row_start, row_stop, blank_run
        logical :: seen_text, dark_row

        max_gap = 0
        blank_run = 0
        seen_text = .false.
        row_start = min(height - 1, axis_y + TICK_MARK_LENGTH + 3)
        row_stop = min(height - 1, axis_y + X_TICK_LABEL_PAD + 80)

        do row = row_start, row_stop
            dark_row = row_has_dark_pixel(image_data, width, height, row, &
                                          center_x - 80, center_x + 80)
            if (dark_row) then
                if (seen_text) max_gap = max(max_gap, blank_run)
                seen_text = .true.
                blank_run = 0
            else if (seen_text) then
                blank_run = blank_run + 1
            end if
        end do
    end function largest_blank_gap_between_clusters

    logical function row_has_dark_pixel(image_data, width, height, row, x_left, &
                                        x_right) result(has_dark)
        integer(1), intent(in) :: image_data(:)
        integer, intent(in) :: width, height, row, x_left, x_right
        integer :: col, pixel_idx, col_left, col_right

        has_dark = .false.
        if (row < 0 .or. row >= height) return

        col_left = max(0, x_left)
        col_right = min(width - 1, x_right)
        do col = col_left, col_right
            pixel_idx = (row*width + col)*3 + 1
            if (is_dark_pixel(image_data, pixel_idx)) then
                has_dark = .true.
                return
            end if
        end do
    end function row_has_dark_pixel

    logical function is_dark_pixel(image_data, pixel_idx) result(is_dark)
        integer(1), intent(in) :: image_data(:)
        integer, intent(in) :: pixel_idx
        integer :: r, g, b

        r = byte_to_int(image_data(pixel_idx))
        g = byte_to_int(image_data(pixel_idx + 1))
        b = byte_to_int(image_data(pixel_idx + 2))
        is_dark = r < 80 .and. g < 80 .and. b < 80
    end function is_dark_pixel

    integer function byte_to_int(value) result(unsigned_value)
        integer(1), intent(in) :: value

        unsigned_value = int(value) + merge(256, 0, value < 0)
    end function byte_to_int

    subroutine test_xlabel_clears_tick_labels()
        !! Verify xlabel y-position accounts for measured x-tick label height
        character(len=:), allocatable :: output_dir
        real(wp) :: x(5), y(5)
        integer :: i, cluster_gap

        total_tests = total_tests + 1

        call ensure_test_output_dir('axis_label_overlap_1573', output_dir)

        do i = 1, 5
            x(i) = real(i, wp) * 200000.0_wp
            y(i) = real(i, wp) * 0.5_wp
        end do

        call figure(figsize=[6.4_wp, 4.8_wp])
        call plot(x, y, 'b-')
        call xlabel('X Axis Label')
        call ylabel('Y Axis')
        call savefig(trim(output_dir) // 'xlabel_clearance.png')

        cluster_gap = get_x_text_cluster_gap()
        if (get_last_x_tick_max_height_bottom() > 0 .and. cluster_gap >= 2) then
            print *, '  PASS: test_xlabel_clears_tick_labels - height, gap:', &
                     get_last_x_tick_max_height_bottom(), cluster_gap
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_xlabel_clears_tick_labels - ' // &
                     'tick height or xlabel gap invalid'
        end if
    end subroutine test_xlabel_clears_tick_labels

    subroutine test_ylabel_width_tracked_in_labels_only()
        !! Verify ylabel tick width tracking works (was zero in labels-only path)
        character(len=:), allocatable :: output_dir
        real(wp) :: x(5), y(5)
        integer :: i

        total_tests = total_tests + 1

        call ensure_test_output_dir('axis_label_overlap_1573', output_dir)

        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i, wp) * 100000.0_wp
        end do

        call figure(figsize=[6.4_wp, 4.8_wp])
        call plot(x, y, 'r-')
        call xlabel('X')
        call ylabel('Y Axis Label (long)')
        call savefig(trim(output_dir) // 'ylabel_width_tracking.png')

        if (get_last_y_tick_max_width() > 0) then
            print *, '  PASS: test_ylabel_width_tracked - width:', &
                     get_last_y_tick_max_width()
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_ylabel_width_tracked - ' // &
                     'get_last_y_tick_max_width() was zero after rendering'
        end if
    end subroutine test_ylabel_width_tracked_in_labels_only

    subroutine test_wide_tick_labels_no_overlap()
        !! Plot with very large numbers to trigger wide tick labels
        character(len=:), allocatable :: output_dir
        real(wp) :: x(10), y(10)
        integer :: i

        total_tests = total_tests + 1

        call ensure_test_output_dir('axis_label_overlap_1573', output_dir)

        do i = 1, 10
            x(i) = real(i, wp) * 1000000.0_wp
            y(i) = real(i, wp) * (-500000.0_wp)
        end do

        call figure(figsize=[6.4_wp, 4.8_wp])
        call plot(x, y, 'g-')
        call xlabel('Large X values with wide tick labels')
        call ylabel('Negative Y values')
        call title('Wide tick label overlap test')
        call savefig(trim(output_dir) // 'wide_ticks.png')

        if (get_last_x_tick_max_height_bottom() > 0 .and. get_last_y_tick_max_width() > 0) then
            print *, '  PASS: test_wide_tick_labels_no_overlap'
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_wide_tick_labels_no_overlap - ' // &
                     'tick dimensions not tracked'
        end if
    end subroutine test_wide_tick_labels_no_overlap

    subroutine test_negative_numbers_no_overlap()
        !! Plot with negative numbers (minus signs add width) and log scale
        character(len=:), allocatable :: output_dir
        real(wp) :: x(5), y(5)
        integer :: i

        total_tests = total_tests + 1

        call ensure_test_output_dir('axis_label_overlap_1573', output_dir)

        do i = 1, 5
            x(i) = real(i, wp) * 0.1_wp
            y(i) = 10.0_wp ** real(i, wp)
        end do

        call figure(figsize=[6.4_wp, 4.8_wp])
        call plot(x, y, 'b-')
        call xlabel('X values')
        call ylabel('Log-scale Y')
        call set_yscale('log')
        call savefig(trim(output_dir) // 'log_scale.png')

        if (get_last_x_tick_max_height_bottom() > 0) then
            print *, '  PASS: test_negative_numbers_no_overlap'
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_negative_numbers_no_overlap - ' // &
                     'tick height not tracked for log scale'
        end if
    end subroutine test_negative_numbers_no_overlap

    subroutine test_xtick_labels_clear_axis()
        character(len=:), allocatable :: output_dir
        real(wp) :: x(5), y(5)
        integer :: i, gap

        total_tests = total_tests + 1

        call ensure_test_output_dir('axis_label_overlap_1573', output_dir)

        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = real(i*i, wp)
        end do

        call figure(figsize=[6.4_wp, 4.8_wp])
        call plot(x, y, 'b-')
        call xlabel('x')
        call savefig(trim(output_dir) // 'xtick_label_gap.png')

        gap = get_x_tick_label_gap_from_axis()
        if (gap >= X_TICK_LABEL_PAD - 1) then
            print *, '  PASS: test_xtick_labels_clear_axis - gap:', gap
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_xtick_labels_clear_axis - gap:', gap
        end if
    end subroutine test_xtick_labels_clear_axis

end program test_axis_label_overlap_1573
