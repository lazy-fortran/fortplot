program contour_filled_demo
    !! Demonstrates filled contours using backend heatmap fill with overlaid lines
    use fortplot
    implicit none

    integer, parameter :: nx = 40, ny = 30
    real(wp) :: x(nx), y(ny)
    real(wp) :: z(ny, nx)
    real(wp) :: r
    integer :: i, j

    ! Build a smooth radial function
    do i = 1, nx
        x(i) = -3.0_wp + real(i-1, wp) * 6.0_wp / real(nx-1, wp)
    end do
    do j = 1, ny
        y(j) = -2.5_wp + real(j-1, wp) * 5.0_wp / real(ny-1, wp)
    end do

    do j = 1, ny
        do i = 1, nx
            r = sqrt(x(i)**2 + y(j)**2)
            z(j, i) = sin(3.0_wp * r) * exp(-0.35_wp * r)
        end do
    end do

    call figure(figsize=[7.2_wp, 5.4_wp])
    call title('Filled Contour Demo (plasma)')
    call xlabel('x')
    call ylabel('y')

    ! No explicit levels: backend fills smoothly; lines overlay at defaults
    call add_contour_filled(x, y, z, colormap='plasma', show_colorbar=.true.)

    call savefig('output/example/fortran/contour_filled_demo/contour_filled_demo.png')
    call savefig('output/example/fortran/contour_filled_demo/contour_filled_demo.pdf')
    call savefig('output/example/fortran/contour_filled_demo/contour_filled_demo.txt')
end program contour_filled_demo

