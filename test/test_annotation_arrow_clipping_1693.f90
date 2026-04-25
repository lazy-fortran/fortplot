program test_annotation_arrow_clipping
    !! Regression test for Issue #1693 / #1716:
    !! Annotation arrows must be clipped to the plot data boundary
    !! so they do not extend outside the visible axes frame.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_annotation_rendering, only: clip_to_data_bounds
    implicit none

    real(wp) :: cx, cy

    ! Clip point inside bounds stays unchanged
    cx = 3.0_wp; cy = 0.5_wp
    call clip_to_data_bounds(cx, cy, 0.0_wp, 6.0_wp, -1.0_wp, 1.0_wp)
    if (cx /= 3.0_wp .or. cy /= 0.5_wp) then
        print *, 'FAIL: interior point moved'
        stop 1
    end if

    ! Clip point below y_min
    cx = 3.0_wp; cy = -2.0_wp
    call clip_to_data_bounds(cx, cy, 0.0_wp, 6.0_wp, -1.0_wp, 1.0_wp)
    if (cy /= -1.0_wp) then
        print *, 'FAIL: point below y_min not clipped'
        stop 1
    end if

    ! Clip point above y_max
    cx = 3.0_wp; cy = 5.0_wp
    call clip_to_data_bounds(cx, cy, 0.0_wp, 6.0_wp, -1.0_wp, 1.0_wp)
    if (cy /= 1.0_wp) then
        print *, 'FAIL: point above y_max not clipped'
        stop 1
    end if

    ! Clip point left of x_min
    cx = -5.0_wp; cy = 0.0_wp
    call clip_to_data_bounds(cx, cy, 0.0_wp, 6.0_wp, -1.0_wp, 1.0_wp)
    if (cx /= 0.0_wp) then
        print *, 'FAIL: point left of x_min not clipped'
        stop 1
    end if

    ! Clip point right of x_max
    cx = 10.0_wp; cy = 0.0_wp
    call clip_to_data_bounds(cx, cy, 0.0_wp, 6.0_wp, -1.0_wp, 1.0_wp)
    if (cx /= 6.0_wp) then
        print *, 'FAIL: point right of x_max not clipped'
        stop 1
    end if

    ! Clip point beyond both x_max and y_min
    cx = 20.0_wp; cy = -10.0_wp
    call clip_to_data_bounds(cx, cy, 0.0_wp, 6.0_wp, -1.0_wp, 1.0_wp)
    if (cx /= 6.0_wp .or. cy /= -1.0_wp) then
        print *, 'FAIL: point beyond two bounds not clipped'
        stop 1
    end if

    ! End-to-end: annotation with text outside data range renders without error
    call figure(figsize=[6.4_wp, 4.8_wp])
    call plot([0.0_wp, 6.0_wp], [0.0_wp, 1.0_wp])

    call annotate("Outside text", &
                  xy=[3.0_wp, 0.5_wp], &
                  xytext=[8.0_wp, -2.0_wp], &
                  xy_coord_type=COORD_DATA, xytext_coord_type=COORD_DATA)

    call savefig('build/test/output/test_annotation_arrow_clipping.pdf')

    print *, 'PASS: annotation arrow clipping works (Issue #1693/#1716)'

end program test_annotation_arrow_clipping
