program test_polar_angular_labels
    !! Verify polar angular tick labels match matplotlib defaults:
    !! 8 spokes at 45-degree spacing and labels terminated with the
    !! degree sign (U+00B0) rather than the literal " deg" suffix.
    use fortplot_polar, only: compute_angular_ticks, format_angle_label, &
                              RAD_TO_DEG
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    real(wp), parameter :: tol = 1.0e-9_wp
    character(len=2), parameter :: degree_sign = achar(194)//achar(176)

    call test_format_uses_degree_sign()
    call test_eight_spokes_at_45_degrees()

    print *, 'All polar angular label tests passed!'

contains

    subroutine test_format_uses_degree_sign()
        character(len=8) :: label

        label = format_angle_label(45)
        if (trim(label) /= '45'//degree_sign) then
            print *, 'FAIL: expected 45 with degree sign, got [', trim(label), ']'
            stop 1
        end if

        ! The literal " deg" suffix must be gone.
        if (index(label, 'deg') /= 0) then
            print *, 'FAIL: label must not contain literal "deg"'
            stop 1
        end if
    end subroutine test_format_uses_degree_sign

    subroutine test_eight_spokes_at_45_degrees()
        integer, parameter :: n = 8
        real(wp) :: angles(n)
        character(len=8) :: labels(n)
        integer :: i, deg

        call compute_angular_ticks(n, angles, labels)

        do i = 1, n
            deg = nint(angles(i)*RAD_TO_DEG)
            if (deg /= (i - 1)*45) then
                print *, 'FAIL: spoke', i, 'expected', (i-1)*45, 'deg got', deg
                stop 1
            end if
            if (index(labels(i), degree_sign) == 0) then
                print *, 'FAIL: label', i, 'missing degree sign'
                stop 1
            end if
        end do
    end subroutine test_eight_spokes_at_45_degrees

end program test_polar_angular_labels
