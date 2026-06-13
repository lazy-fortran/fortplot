program test_marker_equal_area
    !! Behavioral coverage for matplotlib-parity marker sizing (cluster
    !! c02-marker-shape-size). Renders circle, square and diamond markers to a
    !! raster buffer and counts the filled pixels. matplotlib's default scatter
    !! renders the square and diamond slightly larger than the circle and roughly
    !! equal to each other (square ~1.17x circle, diamond ~1.23x circle). A
    !! constants-only check cannot catch the rendering-convention mismatch that
    !! made the rendered square/diamond far smaller than the circle, so this test
    !! asserts the measured filled-pixel ratios directly.

    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, &
                                MARKER_DIAMOND
    use fortplot_raster_markers, only: draw_circle_with_edge_face, &
                                       draw_square_with_edge_face, &
                                       draw_diamond_with_edge_face
    implicit none

    ! Render at a larger scale than the default marker so the filled-pixel count
    ! is not dominated by edge quantization; the area ratios are scale-invariant.
    integer, parameter :: W = 161, H = 161
    real(dp), parameter :: CX = 81.0_dp, CY = 81.0_dp
    real(dp), parameter :: SCALE = 6.0_dp
    real(dp) :: area_circle, area_square, area_diamond
    real(dp) :: ratio_sq, ratio_di, ratio_sq_di
    logical :: ok

    ok = .true.

    area_circle = filled_pixels_circle()
    area_square = filled_pixels_square()
    area_diamond = filled_pixels_diamond()

    print *, 'Testing: rendered equal-area marker sizing (matplotlib parity)'
    print *, '  circle filled pixels  =', area_circle
    print *, '  square filled pixels  =', area_square
    print *, '  diamond filled pixels =', area_diamond

    ratio_sq = area_square/area_circle
    ratio_di = area_diamond/area_circle
    ratio_sq_di = area_square/area_diamond

    print *, '  square/circle  =', ratio_sq
    print *, '  diamond/circle =', ratio_di
    print *, '  square/diamond =', ratio_sq_di

    ! matplotlib: square and diamond both rendered LARGER than the circle.
    if (ratio_sq < 1.0_dp .or. ratio_di < 1.0_dp) then
        print *, '  FAIL: square/diamond not larger than circle (old defect)'
        ok = .false.
    end if

    ! Square ~1.17x circle, diamond ~1.23x circle. Allow a calibration band.
    if (ratio_sq < 1.05_dp .or. ratio_sq > 1.30_dp) then
        print *, '  FAIL: square/circle ratio outside matplotlib band [1.05,1.30]'
        ok = .false.
    end if
    if (ratio_di < 1.10_dp .or. ratio_di > 1.36_dp) then
        print *, '  FAIL: diamond/circle ratio outside matplotlib band [1.10,1.36]'
        ok = .false.
    end if

    ! Square and diamond within ~10% of each other.
    if (ratio_sq_di < 0.90_dp .or. ratio_sq_di > 1.10_dp) then
        print *, '  FAIL: square and diamond differ by more than 10%'
        ok = .false.
    end if

    if (ok) then
        print *, '  PASS: rendered circle, square, diamond match matplotlib ratios'
        print *, 'All marker equal-area tests passed'
    else
        print *, 'Marker equal-area tests FAILED'
        error stop 1
    end if

contains

    function count_filled(image_data) result(n)
        !! Sum the fractional ink coverage over all pixels. Background is white
        !! (255), marker fill is black (0), so per-pixel coverage is
        !! 1 - brightness/255. Summing fractional coverage (rather than
        !! thresholding) gives the true filled area independent of the AA edge
        !! convention, which is exactly the quantity matplotlib's area ratios
        !! compare.
        integer(1), intent(in) :: image_data(:)
        real(dp) :: n
        integer :: idx, r255
        n = 0.0_dp
        do idx = 1, size(image_data), 3
            r255 = iand(int(image_data(idx)), 255)
            n = n + (255.0_dp - real(r255, dp))/255.0_dp
        end do
    end function count_filled

    subroutine fresh_buffer(image_data)
        integer(1), intent(out) :: image_data(:)
        image_data = -1_1  ! 255 -> white background
    end subroutine fresh_buffer

    function filled_pixels_circle() result(n)
        real(dp) :: n
        integer(1) :: image_data(W*H*3)
        real(dp) :: radius
        call fresh_buffer(image_data)
        radius = SCALE*get_marker_size(MARKER_CIRCLE)
        call draw_circle_with_edge_face(image_data, W, H, CX, CY, radius, &
                                        0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, &
                                        0.0_dp, 0.0_dp, 0.0_dp, 1.0_dp, 0.0_dp)
        n = count_filled(image_data)
    end function filled_pixels_circle

    function filled_pixels_square() result(n)
        real(dp) :: n
        integer(1) :: image_data(W*H*3)
        real(dp) :: side
        call fresh_buffer(image_data)
        side = SCALE*get_marker_size(MARKER_SQUARE)
        call draw_square_with_edge_face(image_data, W, H, CX, CY, side, &
                                        0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, &
                                        0.0_dp, 0.0_dp, 0.0_dp, 1.0_dp, 0.0_dp)
        n = count_filled(image_data)
    end function filled_pixels_square

    function filled_pixels_diamond() result(n)
        real(dp) :: n
        integer(1) :: image_data(W*H*3)
        real(dp) :: diag
        call fresh_buffer(image_data)
        diag = SCALE*get_marker_size(MARKER_DIAMOND)
        call draw_diamond_with_edge_face(image_data, W, H, CX, CY, diag, &
                                         0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, &
                                         0.0_dp, 0.0_dp, 0.0_dp, 1.0_dp, 0.0_dp)
        n = count_filled(image_data)
    end function filled_pixels_diamond

end program test_marker_equal_area
