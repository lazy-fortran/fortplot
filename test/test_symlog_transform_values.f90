program test_symlog_transform_values
    !! Verify symlog forward/inverse transform against matplotlib reference values
    !! Issue #1738: symlog transform missing linscale/log(base) factor
    use fortplot_scales, only: apply_scale_transform, apply_inverse_scale_transform
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    real(wp), parameter :: thr = 1.0_wp
    real(wp), parameter :: tol = 1.0d-10
    real(wp) :: fwd, inv
    logical :: ok
    integer :: failures = 0

    ! matplotlib SymmetricalLogTransform(10, 1.0, 1.0).transform([0.5, 1.0, 10.0, 100.0])
    ! => [0.55555556, 1.11111111, 2.11111111, 3.11111111]
    ! Exact: 5/9, 10/9, 19/9, 28/9

    call check_fwd( 0.5_wp,  5.0_wp/9.0_wp,  "T(0.5)  linear region", failures)
    call check_fwd( 1.0_wp, 10.0_wp/9.0_wp,  "T(1)    boundary",     failures)
    call check_fwd( 10.0_wp,19.0_wp/9.0_wp,  "T(10)   log region",   failures)
    call check_fwd(100.0_wp,28.0_wp/9.0_wp,  "T(100)  log region",   failures)

    ! Negative values (symmetric)
    call check_fwd(-0.5_wp, -5.0_wp/9.0_wp,  "T(-0.5)  linear region",  failures)
    call check_fwd(-1.0_wp,-10.0_wp/9.0_wp,  "T(-1)    boundary",      failures)
    call check_fwd(-10.0_wp,-19.0_wp/9.0_wp, "T(-10)   log region",    failures)

    ! Round-trip: inverse(T(x)) == x
    call check_roundtrip( 0.5_wp,  "inv(T(0.5))",   failures)
    call check_roundtrip( 1.0_wp,  "inv(T(1))",     failures)
    call check_roundtrip(10.0_wp,  "inv(T(10))",    failures)
    call check_roundtrip(100.0_wp, "inv(T(100))",   failures)
    call check_roundtrip(-0.5_wp,  "inv(T(-0.5))",  failures)
    call check_roundtrip(-10.0_wp, "inv(T(-10))",   failures)

    ! Zero
    fwd = apply_scale_transform(0.0_wp, 'symlog', thr)
    ok = abs(fwd) < tol
    if (.not. ok) then
        write(*, '(A, F12.8)') "  FAIL: T(0) should be 0, got ", fwd
        failures = failures + 1
    else
        write(*, '(A)') "  PASS: T(0) = 0"
    end if

    if (failures > 0) then
        write(*, '(/A, I0, A)') "FAILED: ", failures, " test(s)"
        error stop 1
    else
        write(*, '(/A)') "All symlog transform tests passed"
    end if

contains

    subroutine check_fwd(x, expected, label, failures)
        real(wp), intent(in) :: x, expected
        character(len=*), intent(in) :: label
        integer, intent(inout) :: failures
        real(wp) :: got

        got = apply_scale_transform(x, 'symlog', thr)
        if (abs(got - expected) > tol) then
            write(*, '(A, F12.8, A, F12.8, A, A, A)') &
                "  FAIL: ", got, " ~= ", expected, " (", trim(label), ")"
            failures = failures + 1
        else
            write(*, '(A)') "  PASS: " // trim(label)
        end if
    end subroutine check_fwd

    subroutine check_roundtrip(x, label, failures)
        real(wp), intent(in) :: x
        character(len=*), intent(in) :: label
        integer, intent(inout) :: failures
        real(wp) :: fwd_val, back

        fwd_val = apply_scale_transform(x, 'symlog', thr)
        back = apply_inverse_scale_transform(fwd_val, 'symlog', thr)
        if (abs(back - x) > tol) then
            write(*, '(A, F12.8, A, F12.8, A, A, A)') &
                "  FAIL: inv(T(", x, ")) = ", back, " (", trim(label), ")"
            failures = failures + 1
        else
            write(*, '(A)') "  PASS: " // trim(label)
        end if
    end subroutine check_roundtrip

end program test_symlog_transform_values
