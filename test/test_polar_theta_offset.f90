program test_polar_theta_offset
    !! Verify polar_to_cartesian uses theta_offset=0 by default (matplotlib compat)
    !! Issue #1742: polar default theta_offset was pi/2 (0 at top) instead of 0 (0 at east)
    use fortplot_polar, only: polar_to_cartesian, PI, TWO_PI
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    real(wp) :: x, y
    real(wp), parameter :: tol = 1.0e-10_wp
    integer :: test_count = 0, passed_count = 0
    logical :: all_pass = .true.

    call test_default_offset_zero()
    call test_pi_over_two()
    call test_pi()
    call test_three_pi_over_two()
    call test_custom_offset()

    write(*, '(/A)') '=== Test Summary ==='
    write(*, '(A, I0, A, I0)') 'Passed: ', passed_count, ' / ', test_count

    if (all_pass) then
        write(*, '(A)') 'PASS: All tests PASSED'
    else
        write(*, '(A)') 'FAIL: Some tests FAILED'
        error stop 1
    end if

contains

    subroutine test_default_offset_zero()
        !! (theta=0, r=1) with default offset => (1, 0) i.e. east
        real(wp) :: dx, dy

        call polar_to_cartesian(0.0_wp, 1.0_wp, x, y)

        dx = abs(x - 1.0_wp)
        dy = abs(y - 0.0_wp)

        test_count = test_count + 1
        if (dx < tol .and. dy < tol) then
            passed_count = passed_count + 1
            write(*, '(A)') 'PASS: theta=0, r=1 => (1, 0) [east]'
        else
            all_pass = .false.
            write(*, '(A, F12.6, A, F12.6, A)') &
                'FAIL: theta=0, r=1 => (', x, ', ', y, ') expected (1, 0)'
        end if
    end subroutine

    subroutine test_pi_over_two()
        !! (theta=pi/2, r=1) with default offset => (0, 1) i.e. north
        real(wp) :: dx, dy

        call polar_to_cartesian(0.5_wp*PI, 1.0_wp, x, y)

        dx = abs(x - 0.0_wp)
        dy = abs(y - 1.0_wp)

        test_count = test_count + 1
        if (dx < tol .and. dy < tol) then
            passed_count = passed_count + 1
            write(*, '(A)') 'PASS: theta=pi/2, r=1 => (0, 1) [north]'
        else
            all_pass = .false.
            write(*, '(A, F12.6, A, F12.6, A)') &
                'FAIL: theta=pi/2, r=1 => (', x, ', ', y, ') expected (0, 1)'
        end if
    end subroutine

    subroutine test_pi()
        !! (theta=pi, r=1) => (-1, 0) i.e. west
        real(wp) :: dx, dy

        call polar_to_cartesian(PI, 1.0_wp, x, y)

        dx = abs(x - (-1.0_wp))
        dy = abs(y - 0.0_wp)

        test_count = test_count + 1
        if (dx < tol .and. dy < tol) then
            passed_count = passed_count + 1
            write(*, '(A)') 'PASS: theta=pi, r=1 => (-1, 0) [west]'
        else
            all_pass = .false.
            write(*, '(A, F12.6, A, F12.6, A)') &
                'FAIL: theta=pi, r=1 => (', x, ', ', y, ') expected (-1, 0)'
        end if
    end subroutine

    subroutine test_three_pi_over_two()
        !! (theta=3pi/2, r=1) => (0, -1) i.e. south
        real(wp) :: dx, dy

        call polar_to_cartesian(1.5_wp*PI, 1.0_wp, x, y)

        dx = abs(x - 0.0_wp)
        dy = abs(y - (-1.0_wp))

        test_count = test_count + 1
        if (dx < tol .and. dy < tol) then
            passed_count = passed_count + 1
            write(*, '(A)') 'PASS: theta=3pi/2, r=1 => (0, -1) [south]'
        else
            all_pass = .false.
            write(*, '(A, F12.6, A, F12.6, A)') &
                'FAIL: theta=3pi/2, r=1 => (', x, ', ', y, ') expected (0, -1)'
        end if
    end subroutine

    subroutine test_custom_offset()
        !! Explicit theta_offset=pi/2 gives old behavior: theta=0 => (0, 1)
        real(wp) :: dx, dy

        call polar_to_cartesian(0.0_wp, 1.0_wp, x, y, &
                                theta_offset=0.5_wp*PI)

        dx = abs(x - 0.0_wp)
        dy = abs(y - 1.0_wp)

        test_count = test_count + 1
        if (dx < tol .and. dy < tol) then
            passed_count = passed_count + 1
            write(*, '(A)') 'PASS: custom offset=pi/2, theta=0 => (0, 1) [top]'
        else
            all_pass = .false.
            write(*, '(A, F12.6, A, F12.6, A)') &
                'FAIL: custom offset=pi/2, theta=0 => (', x, ', ', y, ') expected (0, 1)'
        end if
    end subroutine

end program test_polar_theta_offset
