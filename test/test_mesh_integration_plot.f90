program test_pcolormesh_integration
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none

    real(wp) :: x_coords(4), y_coords(3), c_data(2,3)
    integer :: i, j
    logical :: file_exists

    x_coords = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    y_coords = [0.0_wp, 0.5_wp, 1.0_wp]

    do i = 1, 2
        do j = 1, 3
            c_data(i, j) = real(i * j, wp)
        end do
    end do

    call figure()
    call pcolormesh(x_coords, y_coords, c_data)
    call title('Pcolormesh Integration Test')
    call savefig("test/output/test_pcolormesh_integration.png")

    inquire(file="test/output/test_pcolormesh_integration.png", exist=file_exists)
    if (file_exists) then
        print *, "✓ PASS: Integration produced output image"
        stop 0
    else
        print *, "✗ FAIL: Integration did not create expected image"
        stop 1
    end if
end program test_pcolormesh_integration

