program test_colormap_lut_accuracy
    !! Verify viridis, plasma, inferno LUT colors match matplotlib reference values
    !! at key interpolation points (0.0, 0.25, 0.5, 0.75, 1.0).
    !! Maximum allowed deviation: 0.005 per channel (3-decimal precision of LUT).
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_colormap, only: get_colormap_color
    implicit none

    real(wp), parameter :: TOLERANCE = 0.005_wp
    integer :: failures

    ! Viridis reference values (matplotlib 256-entry LUT, interpolated)
    ! Reshape fills column-major: first 5 values -> column 1 (R), next 5 -> column 2 (G), etc.
    real(wp), parameter :: viridis_ref(5, 3) = reshape([ &
         0.267_wp, 0.230_wp, 0.128_wp, 0.363_wp, 0.993_wp, &   ! R channel
         0.005_wp, 0.321_wp, 0.565_wp, 0.787_wp, 0.906_wp, &   ! G channel
         0.329_wp, 0.545_wp, 0.551_wp, 0.387_wp, 0.144_wp &    ! B channel
         ], [5, 3])

    ! Plasma reference values (matplotlib 256-entry LUT, interpolated)
    real(wp), parameter :: plasma_ref(5, 3) = reshape([ &
         0.050_wp, 0.493_wp, 0.796_wp, 0.972_wp, 0.940_wp, &   ! R channel
         0.030_wp, 0.012_wp, 0.278_wp, 0.582_wp, 0.975_wp, &   ! G channel
         0.528_wp, 0.658_wp, 0.471_wp, 0.254_wp, 0.131_wp &    ! B channel
         ], [5, 3])

    ! Inferno reference values (matplotlib 256-entry LUT, interpolated)
    real(wp), parameter :: inferno_ref(5, 3) = reshape([ &
         0.001_wp, 0.340_wp, 0.733_wp, 0.977_wp, 0.988_wp, &   ! R channel
         0.000_wp, 0.062_wp, 0.214_wp, 0.553_wp, 0.998_wp, &   ! G channel
         0.014_wp, 0.429_wp, 0.332_wp, 0.038_wp, 0.645_wp &    ! B channel
         ], [5, 3])

    real(wp) :: color(3)
    integer :: i, cmap_idx

    failures = 0

    ! Test viridis
    do i = 1, 5
        call get_colormap_color(real(i - 1, wp) / 4.0_wp, 'viridis', color)
        do cmap_idx = 1, 3
            if (abs(color(cmap_idx) - viridis_ref(i, cmap_idx)) > TOLERANCE) then
                print *, 'FAIL: viridis t=', real(i - 1, wp) / 4.0_wp, &
                     ' channel=', cmap_idx, ' got=', color(cmap_idx), &
                     ' expected=', viridis_ref(i, cmap_idx)
                failures = failures + 1
            end if
        end do
    end do

    ! Test plasma
    do i = 1, 5
        call get_colormap_color(real(i - 1, wp) / 4.0_wp, 'plasma', color)
        do cmap_idx = 1, 3
            if (abs(color(cmap_idx) - plasma_ref(i, cmap_idx)) > TOLERANCE) then
                print *, 'FAIL: plasma t=', real(i - 1, wp) / 4.0_wp, &
                     ' channel=', cmap_idx, ' got=', color(cmap_idx), &
                     ' expected=', plasma_ref(i, cmap_idx)
                failures = failures + 1
            end if
        end do
    end do

    ! Test inferno
    do i = 1, 5
        call get_colormap_color(real(i - 1, wp) / 4.0_wp, 'inferno', color)
        do cmap_idx = 1, 3
            if (abs(color(cmap_idx) - inferno_ref(i, cmap_idx)) > TOLERANCE) then
                print *, 'FAIL: inferno t=', real(i - 1, wp) / 4.0_wp, &
                     ' channel=', cmap_idx, ' got=', color(cmap_idx), &
                     ' expected=', inferno_ref(i, cmap_idx)
                failures = failures + 1
            end if
        end do
    end do

    ! Verify endpoints are exact (no interpolation needed)
    call get_colormap_color(0.0_wp, 'viridis', color)
    if (abs(color(1) - 0.267_wp) > TOLERANCE .or. &
        abs(color(2) - 0.005_wp) > TOLERANCE .or. &
        abs(color(3) - 0.329_wp) > TOLERANCE) then
        print *, 'FAIL: viridis t=0.0 endpoint mismatch'
        failures = failures + 1
    end if

    call get_colormap_color(1.0_wp, 'viridis', color)
    if (abs(color(1) - 0.993_wp) > TOLERANCE .or. &
        abs(color(2) - 0.906_wp) > TOLERANCE .or. &
        abs(color(3) - 0.144_wp) > TOLERANCE) then
        print *, 'FAIL: viridis t=1.0 endpoint mismatch'
        failures = failures + 1
    end if

    if (failures > 0) then
        print *, 'FAIL: ', failures, ' colormap accuracy test(s) failed'
        stop 1
    end if

    print *, 'PASS: viridis/plasma/inferno LUT colors match matplotlib within ', TOLERANCE
end program test_colormap_lut_accuracy
