program test_pcolormesh_issue_698_repro
    use fortplot_pcolormesh, only: pcolormesh_t
    use fortplot_errors, only: fortplot_error_t, ERROR_DIMENSION_MISMATCH
    use iso_fortran_env, only: wp => real64
    implicit none

    type(pcolormesh_t) :: mesh
    type(fortplot_error_t) :: error
    real(wp), dimension(3,3) :: z
    real(wp), dimension(3) :: x, y
    real(wp), dimension(4) :: x_correct, y_correct
    integer :: i, j
    logical :: ok1, ok2

    do i = 1, 3
        do j = 1, 3
            z(i, j) = real(i + j, wp)
        end do
    end do

    x = [1.0_wp, 2.0_wp, 3.0_wp]
    y = [1.0_wp, 2.0_wp, 3.0_wp]
    x_correct = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    y_correct = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]

    call mesh%initialize_regular_grid(x, y, z, error=error)
    ok1 = error%is_error() .and. (error%status == ERROR_DIMENSION_MISMATCH)

    call mesh%initialize_regular_grid(x_correct, y_correct, z, error=error)
    ok2 = .not. error%is_error()

    if (ok1 .and. ok2) then
        print *, "✓ PASS: Issue #698 reproduction and positive control"
        stop 0
    else
        if (.not. ok1) print *, "✗ FAIL: Expected dimension mismatch not detected"
        if (.not. ok2) print *, "✗ FAIL: Correct dimensions should succeed"
        stop 1
    end if
end program test_pcolormesh_issue_698_repro

