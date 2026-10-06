program test_polygon_clip
    !! clip_polygon against analytic oracles: the clipped polygon's shoelace
    !! area equals the hand-computed area of the visible part, created
    !! vertices lie on the rectangle, and non-finite vertices are dropped.
    use fortplot_polygon_clip, only: clip_polygon, clip_polygon_capacity
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
        ieee_positive_inf
    implicit none

    real(wp), parameter :: lo(2) = [0.0_wp, 0.0_wp], hi(2) = [1.0_wp, 1.0_wp]
    real(wp), parameter :: big = 1.0e21_wp
    real(wp) :: nan, inf
    integer :: failures

    failures = 0
    nan = ieee_value(1.0_wp, ieee_quiet_nan)
    inf = ieee_value(1.0_wp, ieee_positive_inf)

    ! Inside: unchanged area.
    call expect([0.2_wp, 0.8_wp, 0.8_wp, 0.2_wp], [0.2_wp, 0.2_wp, 0.6_wp, &
                0.6_wp], 0.24_wp, 'inside')
    ! Huge square around the box: the whole unit box.
    call expect([-big, big, big, -big], [-big, -big, big, big], 1.0_wp, &
                'huge square')
    ! Band y in [-1e21 x, 1e21 x] for x in [0.5, 1]: the box half x >= 0.5.
    call expect([0.5_wp, 1.0_wp, 1.0_wp, 0.5_wp], &
                [0.5_wp*big, big, -big, -0.5_wp*big], 0.5_wp, 'huge band')
    ! Triangle x + y <= 1.5 in the first quadrant: the box minus its
    ! corner x + y > 1.5, area 1 - 0.5**3 = 0.875.
    call expect([0.0_wp, 1.5_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.5_wp], &
                0.875_wp, 'cut triangle')
    ! Outside: empty.
    call expect([2.0_wp, 3.0_wp, 3.0_wp], [2.0_wp, 2.0_wp, 3.0_wp], 0.0_wp, &
                'outside')
    ! NaN and Inf vertices dropped: square minus the bad corners -> the
    ! remaining triangle (0,0),(1,0),(1,1) of area 1/2.
    call expect([0.0_wp, 1.0_wp, 1.0_wp, nan, 0.0_wp], &
                [0.0_wp, 0.0_wp, 1.0_wp, 0.5_wp, inf], &
                0.5_wp, 'non-finite dropped')

    if (failures > 0) then
        print '(a,i0,a)', ' FAIL: ', failures, ' check(s) failed'
        stop 1
    end if
    print *, 'PASS: polygon clip areas match the analytic visible parts'

contains

    subroutine expect(px, py, area_ref, what)
        real(wp), intent(in) :: px(:), py(:), area_ref
        character(len=*), intent(in) :: what
        real(wp) :: cx(clip_polygon_capacity(size(px)))
        real(wp) :: cy(clip_polygon_capacity(size(px))), area
        integer :: m

        call clip_polygon(px, py, lo, hi, cx, cy, m)
        area = 0.0_wp
        if (m > 0) area = 0.5_wp*abs(sum(cx(1:m)*cshift(cy(1:m), 1) - &
                                         cshift(cx(1:m), 1)*cy(1:m)))
        print '(1x,a,a,i0,a,es12.4)', what, ': m = ', m, ' area ', area
        if (abs(area - area_ref) > 1.0e-12_wp) then
            print *, 'FAIL: wrong clipped area in ', what
            failures = failures + 1
        end if
        if (m > 0) then
            if (any(cx(1:m) < lo(1) .or. cx(1:m) > hi(1) .or. &
                    cy(1:m) < lo(2) .or. cy(1:m) > hi(2))) then
                print *, 'FAIL: vertex outside the box in ', what
                failures = failures + 1
            end if
        end if
    end subroutine expect

end program test_polygon_clip
