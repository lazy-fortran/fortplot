program test_marker_boundary_clipping
    !! Regression test for issue #1683: markers at data range boundary
    !! should not be clipped by the plot border.
    !!
    !! The fix increases DATA_RANGE_MARGIN from 2% to 5% so that
    !! markers at the exact data boundary are pushed far enough inside
    !! the plot area to avoid overlap with the 1-pixel axes frame.
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use fortplot_figure_core, only: figure_t
    implicit none

    integer, parameter :: w = 400
    integer, parameter :: h = 300
    real(wp) :: rgb(w, h, 3)
    integer :: corner_pixels(4)
    integer :: i

    ! Render four markers, one at each data-range corner
    call render_four_corners(rgb)

    ! Count dark pixels in each corner region of the plot area.
    ! The plot area is roughly central; corners are offset from image edges.
    ! With sufficient margin, each corner marker should have a full set
    ! of pixels.  With insufficient margin, the marker near the axes
    ! frame would have fewer visible pixels (clipped by the frame line).
    call count_corner_pixels(rgb, corner_pixels)

    do i = 1, 4
        if (corner_pixels(i) < 200) then
            write (error_unit, *) "FAIL: corner ", i, &
                " has only ", corner_pixels(i), &
                " marker pixels (expected >= 200)"
            write (error_unit, *) "INFO: marker at data boundary may be clipped by plot border"
            stop 1
        end if
    end do

    print *, "PASS: all four corner markers fully visible"
    print *, "  corner pixel counts:", corner_pixels

contains

    subroutine render_four_corners(rgb)
        real(wp), intent(out) :: rgb(w, h, 3)
        type(figure_t) :: fig
        real(wp) :: x(4), y(4)

        ! Markers at all four corners of the data range [0,1] x [0,1]
        x = [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp]
        y = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp]

        call fig%initialize(width=w, height=h, backend='png')
        call fig%set_xlim(0.0_wp, 1.0_wp)
        call fig%set_ylim(0.0_wp, 1.0_wp)

        call fig%scatter(x, y, marker='o', facecolor=[0.0_wp, 0.0_wp, 0.0_wp], &
                         edgecolor=[0.0_wp, 0.0_wp, 0.0_wp])

        call fig%extract_rgb_data_for_animation(rgb)
    end subroutine render_four_corners

    subroutine count_corner_pixels(rgb, counts)
        real(wp), intent(in) :: rgb(w, h, 3)
        integer, intent(out) :: counts(4)
        integer :: i, j
        real(wp) :: lum

        ! Define search regions for each corner marker.
        ! These are approximate regions inside the plot area where the
        ! corner markers should appear after margin expansion.
        !
        ! Corner 1: bottom-left  (data 0,0) -> image bottom-left of plot area
        ! Corner 2: bottom-right (data 1,0) -> image bottom-right of plot area
        ! Corner 3: top-left     (data 0,1) -> image top-left of plot area
        ! Corner 4: top-right    (data 1,1) -> image top-right of plot area

        counts = 0

        ! Corner 1: bottom-left region
        do j = 3*h/4, h - 10
            do i = 10, w/4
                lum = (rgb(i, j, 1) + rgb(i, j, 2) + rgb(i, j, 3)) / 3.0_wp
                if (lum < 0.5_wp) counts(1) = counts(1) + 1
            end do
        end do

        ! Corner 2: bottom-right region
        do j = 3*h/4, h - 10
            do i = 3*w/4, w - 10
                lum = (rgb(i, j, 1) + rgb(i, j, 2) + rgb(i, j, 3)) / 3.0_wp
                if (lum < 0.5_wp) counts(2) = counts(2) + 1
            end do
        end do

        ! Corner 3: top-left region
        do j = 10, h/4
            do i = 10, w/4
                lum = (rgb(i, j, 1) + rgb(i, j, 2) + rgb(i, j, 3)) / 3.0_wp
                if (lum < 0.5_wp) counts(3) = counts(3) + 1
            end do
        end do

        ! Corner 4: top-right region
        do j = 10, h/4
            do i = 3*w/4, w - 10
                lum = (rgb(i, j, 1) + rgb(i, j, 2) + rgb(i, j, 3)) / 3.0_wp
                if (lum < 0.5_wp) counts(4) = counts(4) + 1
            end do
        end do
    end subroutine count_corner_pixels

end program test_marker_boundary_clipping
