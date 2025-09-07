program test_colormap_interpolation_regression
    !! Regression test: verify colormap interpolation varies across the low end
    !! Previously, the first interval was constant, flattening all negatives.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_colormap, only: colormap_value_to_color
    implicit none

    real(wp) :: c1(3), c2(3), c3(3)
    real(wp) :: diff12, diff23

    ! Map values in [-1, 1] with a diverging colormap (coolwarm)
    call colormap_value_to_color(-0.9_wp, -1.0_wp, 1.0_wp, 'coolwarm', c1)
    call colormap_value_to_color(-0.5_wp, -1.0_wp, 1.0_wp, 'coolwarm', c2)
    call colormap_value_to_color(-0.1_wp, -1.0_wp, 1.0_wp, 'coolwarm', c3)

    diff12 = abs(c1(1)-c2(1)) + abs(c1(2)-c2(2)) + abs(c1(3)-c2(3))
    diff23 = abs(c2(1)-c3(1)) + abs(c2(2)-c3(2)) + abs(c2(3)-c3(3))

    if (diff12 <= 1.0e-6_wp .or. diff23 <= 1.0e-6_wp) then
        print *, 'FAIL: coolwarm interpolation is flat on negative side'
        stop 1
    end if

    ! Also sanity-check a sequential colormap (viridis) is not flat near min
    call colormap_value_to_color(-0.9_wp, -1.0_wp, 1.0_wp, 'viridis', c1)
    call colormap_value_to_color(-0.5_wp, -1.0_wp, 1.0_wp, 'viridis', c2)
    diff12 = abs(c1(1)-c2(1)) + abs(c1(2)-c2(2)) + abs(c1(3)-c2(3))
    if (diff12 <= 1.0e-6_wp) then
        print *, 'FAIL: viridis interpolation is unexpectedly flat near min'
        stop 2
    end if

    print *, 'PASS: colormap interpolation varies across negative range'
end program test_colormap_interpolation_regression

