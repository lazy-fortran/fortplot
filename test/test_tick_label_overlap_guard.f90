program test_tick_label_overlap_guard
    !! Guard to prevent overlapping X-axis tick labels in raster backend
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster_axes, only: compute_non_overlapping_mask => compute_non_overlapping_mask_simple
    implicit none

    real(wp), dimension(8) :: centers
    integer,  dimension(8) :: widths
    logical,  dimension(8) :: keep
    real(wp) :: min_gap
    integer :: i, last_kept
    real(wp) :: last_right, left_i, right_i
    logical :: ok

    ! Simulate 8 ticks 20px apart with 18px-wide labels (would overlap without guard)
    do i = 1, 8
        centers(i) = real((i - 1) * 20, wp)
        widths(i) = 18
    end do
    min_gap = 2.0_wp

    call compute_non_overlapping_mask(centers, widths, min_gap, keep)

    ! Verify that kept labels do not overlap horizontally
    ok = .true.
    last_right = -1.0e30_wp
    do i = 1, size(centers)
        if (keep(i)) then
            left_i  = centers(i) - 0.5_wp * real(widths(i), wp)
            right_i = centers(i) + 0.5_wp * real(widths(i), wp)
            if (left_i < last_right + min_gap - 1.0e-9_wp) then
                ok = .false.
            end if
            last_right = right_i
        end if
    end do

    if (ok) then
        print *, 'PASS: X-axis tick label overlap guard (raster)'
    else
        print *, 'FAIL: Overlap detected in selected tick labels'
        stop 1
    end if
end program test_tick_label_overlap_guard

