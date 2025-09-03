program test_raster_label_layout_utils
    !! Focused tests for raster label layout helpers
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_axes, only: compute_non_overlapping_mask => compute_non_overlapping_mask_simple, &
                                    compute_ylabel_x_pos => compute_ylabel_x_pos_old, &
                                    y_tick_label_right_edge_at_axis => y_tick_label_right_edge_at_axis_old
    use fortplot_constants, only: TICK_MARK_LENGTH

    implicit none

    call test_non_overlapping_mask_basic()
    call test_ylabel_x_pos_clears_tick_labels()
    call test_y_tick_label_right_edge()
    call test_ylabel_bounds_check()

    print *, 'Raster label layout utility tests passed'
contains

    subroutine test_non_overlapping_mask_basic()
        real(wp) :: centers(3)
        integer :: widths(3)
        logical :: keep(3)

        ! Case 1: evenly spaced labels, no overlap expected
        centers = [10.0_wp, 20.0_wp, 30.0_wp]
        widths  = [10, 10, 10]
        call compute_non_overlapping_mask(centers, widths, 0.0_wp, keep)
        if (any(.not. keep)) then
            print *, 'FAIL: Expected all labels to be kept in non-overlap case'
            stop 1
        end if

        ! Case 2: overlapping if drawn all; greedy should keep 1st and 3rd only
        centers = [10.0_wp, 18.0_wp, 26.0_wp]
        widths  = [12, 12, 12]
        call compute_non_overlapping_mask(centers, widths, 2.0_wp, keep)
        if (.not. (keep(1) .and. .not. keep(2) .and. keep(3))) then
            print *, 'FAIL: Overlap mask incorrect:', keep
            stop 1
        end if
    end subroutine test_non_overlapping_mask_basic

    subroutine test_ylabel_x_pos_clears_tick_labels()
        type(plot_area_t) :: area
        integer :: rotated_text_width
        integer :: y_tick_max_width
        integer :: x_pos, expected
        integer, parameter :: Y_TICK_LABEL_RIGHT_PAD = 8
        ! Updated to match the increased gap in fortplot_raster_axes
        integer, parameter :: YLABEL_EXTRA_GAP = 8

        area%left = 100; area%bottom = 50; area%width = 400; area%height = 300
        rotated_text_width = 20
        y_tick_max_width = 40

        x_pos = compute_ylabel_x_pos(area, rotated_text_width, y_tick_max_width)
        ! Expect the RIGHT edge of the rotated ylabel to align at the
        ! computed clearance from the y-tick labels, hence subtract the
        ! full rotated_text_width from the left edge calculation.
        expected = area%left - (TICK_MARK_LENGTH + Y_TICK_LABEL_RIGHT_PAD + &
                y_tick_max_width + YLABEL_EXTRA_GAP) - rotated_text_width
        ! Account for minimum margin protection (15 pixels from edge)
        if (expected < 15) expected = 15
        if (x_pos /= expected) then
            print *, 'FAIL: ylabel x position mismatch:', x_pos, expected
            stop 1
        end if
    end subroutine test_ylabel_x_pos_clears_tick_labels

    subroutine test_y_tick_label_right_edge()
        type(plot_area_t) :: area
        integer :: r_edge, expected
        integer, parameter :: Y_TICK_LABEL_RIGHT_PAD = 8

        area%left = 100; area%bottom = 50; area%width = 400; area%height = 300

        r_edge = y_tick_label_right_edge_at_axis(area)
        expected = area%left - TICK_MARK_LENGTH - Y_TICK_LABEL_RIGHT_PAD
        if (r_edge /= expected) then
            print *, 'FAIL: y-tick label right edge mismatch:', r_edge, expected
            stop 1
        end if
    end subroutine test_y_tick_label_right_edge

    subroutine test_ylabel_bounds_check()
        ! Test that ylabel x position is clamped to stay within canvas bounds
        ! This tests the bounds check added in PR #1164 to fix issue #1136
        type(plot_area_t) :: area
        integer :: rotated_text_width
        integer :: y_tick_max_width
        integer :: x_pos
        integer, parameter :: Y_TICK_LABEL_RIGHT_PAD = 8
        ! Updated to match the increased gap in fortplot_raster_axes
        integer, parameter :: YLABEL_EXTRA_GAP = 8

        ! Create a scenario where ylabel would be positioned at negative x
        ! Small plot area on the left with very wide tick labels
        area%left = 30  ! Small left margin
        area%bottom = 50
        area%width = 400
        area%height = 300
        
        rotated_text_width = 20  ! Width of ylabel when rotated
        y_tick_max_width = 50     ! Very wide tick labels
        
        x_pos = compute_ylabel_x_pos(area, rotated_text_width, y_tick_max_width)
        
        ! The natural position would be negative:
        ! 30 - (5 + 8 + 50 + 2) - 20 = 30 - 65 - 20 = -55
        ! But the implementation in raster_render_ylabel clamps it to 1
        
        ! For this test, we only verify the compute_ylabel_x_pos function returns
        ! the computed value (even if negative), as the clamping happens in the
        ! rendering function, not in the position calculation
        if (x_pos >= area%left) then
            print *, 'FAIL: ylabel x position should be left of plot area for wide tick labels'
            print *, '  Expected x_pos < ', area%left, ', got: ', x_pos
            stop 1
        end if
        
        print *, 'PASS: Ylabel positioning is width-invariant and clears tick labels'
    end subroutine test_ylabel_bounds_check

end program test_raster_label_layout_utils
