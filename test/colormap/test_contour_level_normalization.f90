program test_contour_level_normalization
    !! Regression test for matplotlib-style contour colour normalization.
    !!
    !! matplotlib spans the full colormap across the contour LEVEL range, not the
    !! raw data range. With levels [-4, -2, 0, 2, 4] over data spanning [-9, 9],
    !! normalising over the data range compresses every line into a narrow
    !! mid-colormap band. The endpoints must instead land on the colormap
    !! extremes (viridis(0) dark purple, viridis(1) yellow).
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_colormap, only: colormap_value_to_color
    implicit none

    real(wp), parameter :: levels(5) = &
        [-4.0_wp, -2.0_wp, 0.0_wp, 2.0_wp, 4.0_wp]
    real(wp) :: lev_min, lev_max
    real(wp) :: c_lo(3), c_hi(3), c_end0(3), c_end1(3)
    real(wp) :: tol

    tol = 1.0e-6_wp
    lev_min = minval(levels)
    lev_max = maxval(levels)

    ! Colour the lowest and highest levels normalised over the level range.
    call colormap_value_to_color(levels(1), lev_min, lev_max, 'viridis', c_lo)
    call colormap_value_to_color(levels(5), lev_min, lev_max, 'viridis', c_hi)

    ! Reference colormap endpoints.
    call colormap_value_to_color(0.0_wp, 0.0_wp, 1.0_wp, 'viridis', c_end0)
    call colormap_value_to_color(1.0_wp, 0.0_wp, 1.0_wp, 'viridis', c_end1)

    if (sum(abs(c_lo - c_end0)) > tol) then
        print *, 'FAIL: lowest level does not map to colormap minimum'
        print *, '  got ', c_lo, ' expected ', c_end0
        stop 1
    end if

    if (sum(abs(c_hi - c_end1)) > tol) then
        print *, 'FAIL: highest level does not map to colormap maximum'
        print *, '  got ', c_hi, ' expected ', c_end1
        stop 2
    end if

    ! The narrow-band failure mode: normalising over the wider data range
    ! [-9, 9] must NOT reproduce the colormap endpoints (sanity that the test
    ! distinguishes the two behaviours).
    call colormap_value_to_color(levels(1), -9.0_wp, 9.0_wp, 'viridis', c_lo)
    if (sum(abs(c_lo - c_end0)) <= tol) then
        print *, 'FAIL: data-range normalization unexpectedly hit colormap min'
        stop 3
    end if

    print *, 'PASS: contour colours span full colormap over the level range'
end program test_contour_level_normalization
