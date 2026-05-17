program test_raster_axes_overlap_mask
    !! Focused tests for compute_non_overlapping_mask in raster axes
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster_axes, only: compute_non_overlapping_mask => compute_non_overlapping_mask_simple
    implicit none

    real(wp), allocatable :: centers(:)
    integer, allocatable :: widths(:)
    logical, allocatable :: keep(:)
    real(wp) :: gap

    print *, '=== Testing raster axes overlap mask ==='

    ! Case 1: Non-overlapping labels should all be kept (touching allowed)
    allocate(centers(3), widths(3), keep(3))
    centers = [10.0_wp, 20.0_wp, 40.0_wp]
    widths  = [10, 10, 10]
    gap = 0.0_wp
    call compute_non_overlapping_mask(centers, widths, gap, keep)
    if (all(keep .eqv. [.true., .true., .true.])) then
        print *, 'PASS: Non-overlapping sequence all kept'
    else
        print *, 'ERROR: Expected all labels to be kept in non-overlap case'
        stop 1
    end if

    ! Case 2: Overlapping middle label should be dropped by greedy selection
    centers = [10.0_wp, 14.0_wp, 28.0_wp]
    widths  = [10, 10, 10]
    call compute_non_overlapping_mask(centers, widths, gap, keep)
    if (all(keep .eqv. [.true., .false., .true.])) then
        print *, 'PASS: Overlapping middle label dropped as expected'
    else
        print *, 'ERROR: Expected mask [T,F,T] for overlapping middle label'
        stop 2
    end if

    ! Case 3: Positive min_gap enforces additional spacing
    centers = [10.0_wp, 20.0_wp, 30.0_wp]
    widths  = [10, 10, 10]
    gap = 4.0_wp
    call compute_non_overlapping_mask(centers, widths, gap, keep)
    if (all(keep .eqv. [.true., .false., .true.])) then
        print *, 'PASS: min_gap enforced correctly'
    else
        print *, 'ERROR: Expected mask [T,F,T] with min_gap=4'
        stop 3
    end if

    ! Case 4: Widths length mismatch should result in no labels kept
    deallocate(widths); allocate(widths(2))
    widths = [10, 10]
    call compute_non_overlapping_mask(centers, widths, gap, keep)
    if (all(.not. keep)) then
        print *, 'PASS: Mismatched widths length handled safely'
    else
        print *, 'ERROR: Expected all .false. for mismatched widths length'
        stop 4
    end if

    ! Case 5: Negative min_gap is clamped to zero (touching allowed)
    deallocate(centers, widths, keep)
    allocate(centers(2), widths(2), keep(2))
    centers = [10.0_wp, 20.0_wp]
    widths  = [10, 10]
    gap = -5.0_wp
    call compute_non_overlapping_mask(centers, widths, gap, keep)
    if (all(keep .eqv. [.true., .true.])) then
        print *, 'PASS: Negative min_gap treated as zero (touching kept)'
    else
        print *, 'ERROR: Expected [T,T] when min_gap is negative (clamped to 0)'
        stop 5
    end if

    ! Case 6: Zero-width labels never overlap; all should be kept
    deallocate(centers, widths, keep)
    allocate(centers(3), widths(3), keep(3))
    centers = [10.0_wp, 10.0_wp, 10.0_wp]
    widths  = [0, 0, 0]
    gap = 0.0_wp
    call compute_non_overlapping_mask(centers, widths, gap, keep)
    if (all(keep)) then
        print *, 'PASS: Zero-width labels all kept'
    else
        print *, 'ERROR: Expected all .true. for zero-width labels'
        stop 6
    end if

    print *, '=== Raster axes overlap mask tests PASSED ==='
end program test_raster_axes_overlap_mask
