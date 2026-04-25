program test_symlog_transform_values
    !! Verify symlog forward/inverse transform with balanced linear/log regions
    !! Issue #1717: symlog tick labels bunched due to oversized linear region
    use fortplot_scales, only: apply_scale_transform, apply_inverse_scale_transform
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    real(wp), parameter :: thr = 1.0_wp
    real(wp), parameter :: tol = 1.0d-10
    real(wp) :: fwd, inv
    logical :: ok
    integer :: failures = 0

    ! Balanced symlog transform (linscale=1, continuity-adjusted):
    ! Linear region: T(x) = x
    ! Log region: T(x) = sign(x) * threshold * (1 + log10(|x|/threshold))
    ! With threshold=1: T(0.5)=0.5, T(1)=1, T(10)=2, T(100)=3

    call check_fwd( 0.5_wp,  0.5_wp,  "T(0.5)  linear region", failures)
    call check_fwd( 1.0_wp,  1.0_wp,  "T(1)    boundary",     failures)
    call check_fwd( 10.0_wp, 2.0_wp,  "T(10)   log region",   failures)
    call check_fwd(100.0_wp, 3.0_wp,  "T(100)  log region",   failures)

    ! Negative values (symmetric)
    call check_fwd(-0.5_wp, -0.5_wp,  "T(-0.5)  linear region",  failures)
    call check_fwd(-1.0_wp, -1.0_wp,  "T(-1)    boundary",      failures)
    call check_fwd(-10.0_wp,-2.0_wp,  "T(-10)   log region",    failures)

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
