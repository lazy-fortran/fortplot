program test_tick_label_overlap_guard
    !! Guard against overlapping X-axis tick labels in the raster backend.
    !! Covers a uniform dense layout and a varied spacing/width layout.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster_axes, only: &
        compute_non_overlapping_mask => compute_non_overlapping_mask_simple
    implicit none

    call test_uniform_dense()
    call test_varied_spacing()

contains

    subroutine test_uniform_dense()
        !! 8 ticks 20px apart with 18px labels (would overlap without the guard).
        real(wp) :: centers(8)
        integer :: widths(8)
        logical :: keep(8)
        integer :: i

        do i = 1, 8
            centers(i) = real((i - 1)*20, wp)
            widths(i) = 18
        end do

        call compute_non_overlapping_mask(centers, widths, 2.0_wp, keep)
        call assert_no_overlap(centers, widths, keep, 2.0_wp, &
                               'X-axis tick label overlap guard (raster)')
    end subroutine test_uniform_dense

    subroutine test_varied_spacing()
        !! Mixed spacing (clusters and gaps) and mixed label widths.
        real(wp) :: centers(10)
        integer :: widths(10)
        logical :: keep(10)

        centers = real([0, 12, 24, 60, 80, 100, 101, 140, 200, 260], wp)
        widths = [18, 22, 16, 28, 30, 8, 8, 24, 20, 18]

        call compute_non_overlapping_mask(centers, widths, 2.0_wp, keep)
        call assert_no_overlap(centers, widths, keep, 2.0_wp, &
                               'Varied tick label overlap guard (raster)')
    end subroutine test_varied_spacing

    subroutine assert_no_overlap(centers, widths, keep, min_gap, label)
        !! Fail if any greedily kept label overlaps the previous kept one.
        real(wp), intent(in) :: centers(:), min_gap
        integer, intent(in) :: widths(:)
        logical, intent(in) :: keep(:)
        character(len=*), intent(in) :: label
        integer :: i
        real(wp) :: last_right, left_i, right_i

        last_right = -1.0e30_wp
        do i = 1, size(centers)
            if (keep(i)) then
                left_i = centers(i) - 0.5_wp*real(widths(i), wp)
                right_i = centers(i) + 0.5_wp*real(widths(i), wp)
                if (left_i < last_right + min_gap - 1.0e-9_wp) then
                    print *, 'FAIL: overlap detected in selected tick labels: '//label
                    stop 1
                end if
                last_right = right_i
            end if
        end do
        print *, 'PASS: '//label
    end subroutine assert_no_overlap

end program test_tick_label_overlap_guard
