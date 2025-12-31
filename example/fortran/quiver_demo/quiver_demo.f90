program quiver_demo
    !! Demonstrate quiver plot for discrete vector field arrows
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, quiver, xlabel, ylabel, title, savefig
    implicit none

    integer, parameter :: nx = 10, ny = 10
    real(wp), dimension(nx*ny) :: x, y, u, v
    integer :: i, j, k
    real(wp) :: xi, yj

    ! Create grid of points for vector field
    k = 0
    do j = 1, ny
        do i = 1, nx
            k = k + 1
            xi = -2.0_wp + 4.0_wp * real(i-1, wp) / real(nx-1, wp)
            yj = -2.0_wp + 4.0_wp * real(j-1, wp) / real(ny-1, wp)
            x(k) = xi
            y(k) = yj
            ! Circular flow field (same as streamplot demo)
            u(k) = -yj
            v(k) = xi
        end do
    end do

    ! Basic quiver plot
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v)
    call xlabel('X')
    call ylabel('Y')
    call title('Quiver Plot Demo - Circular Flow')

    call savefig('output/example/fortran/quiver_demo/quiver_demo.png')
    call savefig('output/example/fortran/quiver_demo/quiver_demo.pdf')
    call savefig('output/example/fortran/quiver_demo/quiver_demo.txt')

    ! Scaled quiver plot
    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, scale=0.5_wp)
    call xlabel('X')
    call ylabel('Y')
    call title('Quiver Plot Demo - Smaller Arrows')

    call savefig('output/example/fortran/quiver_demo/quiver_scaled.png')
    call savefig('output/example/fortran/quiver_demo/quiver_scaled.pdf')
    call savefig('output/example/fortran/quiver_demo/quiver_scaled.txt')

    print *, 'Quiver demo completed!'

end program quiver_demo
