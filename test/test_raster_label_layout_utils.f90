program test_raster_label_layout_utils
    !! Focused tests for raster label layout helpers
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_axes, only: compute_non_overlapping_mask, &
                                    compute_ylabel_x_pos, &
                                    y_tick_label_right_edge_at_axis
    use fortplot_constants, only: TICK_MARK_LENGTH

    implicit none

    call test_non_overlapping_mask_basic()
    call test_ylabel_x_pos_clears_tick_labels()
    call test_y_tick_label_right_edge()

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
        integer, parameter :: YLABEL_EXTRA_GAP = 2

        area%left = 100; area%bottom = 50; area%width = 400; area%height = 300
        rotated_text_width = 20
        y_tick_max_width = 40

        x_pos = compute_ylabel_x_pos(area, rotated_text_width, y_tick_max_width)
        ! Expect the RIGHT edge of the rotated ylabel to align at the
        ! computed clearance from the y-tick labels, hence subtract the
        ! full rotated_text_width from the left edge calculation.
        expected = area%left - (TICK_MARK_LENGTH + Y_TICK_LABEL_RIGHT_PAD + &
                y_tick_max_width + YLABEL_EXTRA_GAP) - rotated_text_width
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

end program test_raster_label_layout_utils
