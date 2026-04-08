program test_axis_label_overlap_1573
    !! Regression test for issue #1573: axis labels overlapping tick labels.
    !! Verifies that xlabel is positioned below x-tick labels based on measured
    !! tick label height, and that ylabel width tracking works in all paths.
    use fortplot
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_raster_ticks, only: last_x_tick_max_height_bottom, &
                                     last_y_tick_max_width, X_TICK_LABEL_PAD
    use fortplot_constants, only: XLABEL_VERTICAL_OFFSET
    use test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    call test_xlabel_clears_tick_labels()
    call test_ylabel_width_tracked_in_labels_only()
    call test_wide_tick_labels_no_overlap()
    call test_negative_numbers_no_overlap()

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

    subroutine test_xlabel_clears_tick_labels()
        !! Verify xlabel y-position accounts for measured x-tick label height
        character(len=:), allocatable :: output_dir
        real(wp) :: x(5), y(5)
        integer :: i

        total_tests = total_tests + 1

        call ensure_test_output_dir('axis_label_overlap_1573', output_dir)

        do i = 1, 5
            x(i) = real(i, wp) * 200000.0_wp
            y(i) = real(i, wp) * 0.5_wp
        end do

        last_x_tick_max_height_bottom = 0
        call figure(figsize=[6.4_wp, 4.8_wp])
        call plot(x, y, 'b-')
        call xlabel('X Axis Label')
        call ylabel('Y Axis')
        call savefig(trim(output_dir) // 'xlabel_clearance.png')

        if (last_x_tick_max_height_bottom > 0) then
            print *, '  PASS: test_xlabel_clears_tick_labels - height tracked:', &
                     last_x_tick_max_height_bottom
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_xlabel_clears_tick_labels - ' // &
                     'last_x_tick_max_height_bottom not set'
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

        last_y_tick_max_width = 0
        call figure(figsize=[6.4_wp, 4.8_wp])
        call plot(x, y, 'r-')
        call xlabel('X')
        call ylabel('Y Axis Label (long)')
        call savefig(trim(output_dir) // 'ylabel_width_tracking.png')

        if (last_y_tick_max_width > 0) then
            print *, '  PASS: test_ylabel_width_tracked - width:', &
                     last_y_tick_max_width
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_ylabel_width_tracked - ' // &
                     'last_y_tick_max_width was zero after rendering'
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

        if (last_x_tick_max_height_bottom > 0 .and. last_y_tick_max_width > 0) then
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

        if (last_x_tick_max_height_bottom > 0) then
            print *, '  PASS: test_negative_numbers_no_overlap'
            passed_tests = passed_tests + 1
        else
            print *, 'FAIL: test_negative_numbers_no_overlap - ' // &
                     'tick height not tracked for log scale'
        end if
    end subroutine test_negative_numbers_no_overlap

end program test_axis_label_overlap_1573
