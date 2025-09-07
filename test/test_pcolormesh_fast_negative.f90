program test_pcolormesh_fast_negative
    !! Fast, minimal pcolormesh test covering negative coords/values
    !! Verifies ASCII output contains negative tick labels (no external tools)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, pcolormesh, savefig, title
    implicit none

    integer, parameter :: nx = 16, ny = 12
    real(wp) :: x(nx), y(ny)
    real(wp) :: c(nx-1, ny-1)
    integer :: i, j
    real(wp) :: xc, yc
    character(len=*), parameter :: out_txt = 'test/output/test_pcolormesh_fast_negative.txt'
    integer :: unit, ios
    character(len=8192) :: buf
    logical :: ok

    ! Negative to positive coordinates (small grid for speed)
    do i = 1, nx
        x(i) = -1.0_wp + 2.0_wp * real(i-1, wp) / real(nx-1, wp)
    end do
    do j = 1, ny
        y(j) = -0.8_wp + 1.6_wp * real(j-1, wp) / real(ny-1, wp)
    end do

    ! Cell-centered values spanning negative/positive
    do i = 1, nx-1
        do j = 1, ny-1
            xc = 0.5_wp * (x(i) + x(i+1))
            yc = 0.5_wp * (y(j) + y(j+1))
            c(i, j) = xc - yc  ! simple saddle: negative and positive
        end do
    end do

    call figure()
    call title('fast pcolormesh negative test')
    call pcolormesh(x, y, c, colormap='coolwarm')
    call savefig(out_txt)

    ! Read ASCII file and verify negative ticks appear
    ok = .false.
    open(newunit=unit, file=out_txt, status='old', action='read', iostat=ios)
    if (ios == 0) then
        do
            read(unit, '(A)', iostat=ios) buf
            if (ios /= 0) exit
            if (index(buf, '-') > 0) then
                ok = .true.
                exit
            end if
        end do
        close(unit)
    end if

    if (.not. ok) then
        print *, 'FAIL: negative tick label not found in ASCII output'
        stop 1
    end if

    print *, 'PASS: fast negative pcolormesh ASCII output contains negative tick labels'
end program test_pcolormesh_fast_negative

