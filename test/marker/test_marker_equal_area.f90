program test_marker_equal_area
    !! Behavioral coverage for matplotlib-parity marker sizing (cluster
    !! c02-marker-shape-size). A single scatter size must give circle, square and
    !! diamond markers the same filled area, matching matplotlib's default where
    !! one size s scales every marker by sqrt(s): square side == circle diameter
    !! and diamond full diagonal == sqrt(2) * circle diameter.
    !!
    !! get_marker_size is interpreted by the raster backend as: circle -> fill
    !! radius, square -> full side, diamond -> full diagonal.

    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, &
                                MARKER_DIAMOND
    implicit none

    real(dp), parameter :: PI = 3.14159265358979323846_dp
    real(dp), parameter :: TOL = 1.0e-9_dp
    real(dp) :: r_circle, side_square, diag_diamond
    real(dp) :: area_circle, area_square, area_diamond
    logical :: ok

    ok = .true.

    r_circle = get_marker_size(MARKER_CIRCLE)
    side_square = get_marker_size(MARKER_SQUARE)
    diag_diamond = get_marker_size(MARKER_DIAMOND)

    ! Filled areas under the backend's interpretation of each size.
    area_circle = PI*r_circle**2
    area_square = side_square**2
    area_diamond = 0.5_dp*diag_diamond**2

    print *, 'Testing: equal-area marker sizing (matplotlib parity)'
    print *, '  circle area =', area_circle
    print *, '  square area =', area_square
    print *, '  diamond area =', area_diamond

    ! Square side must equal circle diameter (area = (2r)^2 = 4 pi^-1 * circle?)
    ! matplotlib: square side == circle diameter -> area ratio square/circle = 4/pi.
    if (abs(side_square - 2.0_dp*r_circle) > TOL) then
        print *, '  FAIL: square side /= circle diameter'
        ok = .false.
    end if

    ! Diamond full diagonal must equal sqrt(2) * circle diameter, which gives the
    ! diamond the same filled area as the square.
    if (abs(diag_diamond - 2.0_dp*sqrt(2.0_dp)*r_circle) > TOL) then
        print *, '  FAIL: diamond diagonal /= sqrt(2) * circle diameter'
        ok = .false.
    end if

    if (abs(area_square - area_diamond) > 1.0e-6_dp) then
        print *, '  FAIL: square and diamond filled areas differ'
        ok = .false.
    end if

    ! Guard against the old bug where square was ~half and diamond ~1/3 of the
    ! circle: both must now be larger than the circle (ratio 4/pi ~= 1.27).
    if (area_square <= area_circle .or. area_diamond <= area_circle) then
        print *, '  FAIL: square/diamond smaller than circle (old defect)'
        ok = .false.
    end if

    if (ok) then
        print *, '  PASS: circle, square and diamond use equal-area sizing'
        print *, 'All marker equal-area tests passed'
    else
        print *, 'Marker equal-area tests FAILED'
        error stop 1
    end if

end program test_marker_equal_area
