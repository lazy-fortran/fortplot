program streamplot_demo
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, streamplot, xlabel, ylabel, title, savefig
    implicit none

    integer, parameter :: nx = 20, ny = 20
    real(wp), dimension(nx) :: x
    real(wp), dimension(ny) :: y
    real(wp), dimension(nx, ny) :: u, v
    integer :: i, j

    ! Create grid
    do i = 1, nx
        x(i) = -2.0_wp + 4.0_wp * real(i-1, wp) / real(nx-1, wp)
    end do

    do j = 1, ny
        y(j) = -2.0_wp + 4.0_wp * real(j-1, wp) / real(ny-1, wp)
    end do

    ! Create circular flow field
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -y(j)
            v(i,j) = x(i)
        end do
    end do

    ! Base streamline plot (no arrows)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call streamplot(x, y, u, v, density=1.0_wp)
    call xlabel('X')
    call ylabel('Y')
    call title('Streamline Plot Demo - Circular Flow')

    ! Save base figure
    call savefig('output/example/fortran/streamplot_demo/streamplot_demo.png')
    call savefig('output/example/fortran/streamplot_demo/streamplot_demo.pdf')
    call savefig('output/example/fortran/streamplot_demo/streamplot_demo.txt')

    ! Arrow variant: emphasize direction with arrowheads
    call figure(figsize=[8.0_wp, 6.0_wp])
    call streamplot(x, y, u, v, density=1.0_wp, arrowsize=1.5_wp, arrowstyle='->')
    call xlabel('X')
    call ylabel('Y')
    call title('Streamline Plot Demo - With Arrows')

    ! Save arrow variant
    call savefig('output/example/fortran/streamplot_demo/streamplot_arrows.png')
    call savefig('output/example/fortran/streamplot_demo/streamplot_arrows.pdf')
    call savefig('output/example/fortran/streamplot_demo/streamplot_arrows.txt')

    print *, 'Streamplot demo completed!'

end program streamplot_demo
