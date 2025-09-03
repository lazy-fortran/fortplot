program test_tick_label_overlap_guard_varied
    !! Additional guard test: varied spacing and widths
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster_axes, only: compute_non_overlapping_mask => compute_non_overlapping_mask_simple
    implicit none

    real(wp), dimension(10) :: centers
    integer,  dimension(10) :: widths
    logical,  dimension(10) :: keep
    real(wp) :: min_gap
    integer :: i
    real(wp) :: last_right, left_i, right_i
    logical :: ok

    ! Mixed spacing: clusters and gaps; mixed label widths
    centers = [ real([0, 12, 24, 60, 80, 100, 101, 140, 200, 260], wp) ]
    widths  = [  18,  22,  16,  28,  30,   8,   8,  24,  20,  18 ]
    min_gap = 2.0_wp

    call compute_non_overlapping_mask(centers, widths, min_gap, keep)

    ! Verify greedily kept labels do not overlap
    ok = .true.
    last_right = -1.0e30_wp
    do i = 1, size(centers)
        if (keep(i)) then
            left_i  = centers(i) - 0.5_wp * real(widths(i), wp)
            right_i = centers(i) + 0.5_wp * real(widths(i), wp)
            if (left_i < last_right + min_gap - 1.0e-9_wp) ok = .false.
            last_right = right_i
        end if
    end do

    if (ok) then
        print *, 'PASS: Varied tick label overlap guard (raster)'
    else
        print *, 'FAIL: Overlap detected in varied-case tick labels'
        stop 1
    end if
end program test_tick_label_overlap_guard_varied

