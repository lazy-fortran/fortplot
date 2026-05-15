program test_svg_quiver
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, quiver, xlabel, ylabel, title, savefig
    implicit none
    integer, parameter :: nx = 3, ny = 3
    real(wp), dimension(nx*ny) :: x, y, u, v
    integer :: i, j, k
    real(wp) :: xi, yj

    k = 0
    do j = 1, ny
        do i = 1, nx
            k = k + 1
            xi = real(i-1, wp)
            yj = real(j-1, wp)
            x(k) = xi
            y(k) = yj
            u(k) = 1.0_wp
            v(k) = 0.0_wp
        end do
    end do

    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v)
    call xlabel('X')
    call ylabel('Y')
    call title('SVG Quiver Test')
    call savefig('/tmp/quiver_test/quiver.svg')
    print *, 'SVG quiver test completed'
end program test_svg_quiver
