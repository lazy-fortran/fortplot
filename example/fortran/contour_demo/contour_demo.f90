program contour_demo
    !! Comprehensive contour plotting examples demonstrating line contours,
    !! filled contours, custom levels, and various colormaps.
    use fortplot
    implicit none

    call demo_basic_contour()
    call demo_mixed_contour_line()
    call demo_filled_contour()
    call demo_colormap_comparison()

contains

    subroutine demo_basic_contour()
        !! Basic contour line plot with Gaussian function
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30, 30) :: z_grid
        integer :: i, j

        print *, "=== Basic Contour Demo ==="

        do i = 1, 30
            x_grid(i) = -3.0_wp + (i - 1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i - 1) * 6.0_wp / 29.0_wp
        end do

        do i = 1, 30
            do j = 1, 30
                z_grid(i, j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("2D Gaussian Function")
        call add_contour(x_grid, y_grid, z_grid, label="exp(-(x^2+y^2))")
        call savefig('output/example/fortran/contour_demo/contour_gaussian.png')
        call savefig('output/example/fortran/contour_demo/contour_gaussian.pdf')
        call savefig('output/example/fortran/contour_demo/contour_gaussian.txt')

        print *, "Created: contour_gaussian.png/pdf/txt"
    end subroutine demo_basic_contour

    subroutine demo_mixed_contour_line()
        !! Contour plot combined with a line plot overlay
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30, 30) :: z_grid
        real(wp), dimension(5) :: custom_levels
        integer :: i, j

        print *, "=== Mixed Contour + Line Demo ==="

        do i = 1, 30
            x_grid(i) = -3.0_wp + (i - 1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i - 1) * 6.0_wp / 29.0_wp
        end do

        do i = 1, 30
            do j = 1, 30
                z_grid(i, j) = x_grid(i)**2 - y_grid(j)**2
            end do
        end do

        custom_levels = [-4.0_wp, -2.0_wp, 0.0_wp, 2.0_wp, 4.0_wp]
        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Mixed Plot: Contour + Line")
        call add_contour(x_grid, y_grid, z_grid, levels=custom_levels, label="x^2-y^2")
        call add_plot(x_grid, exp(-x_grid**2), label="Cross-section at y=0")
        call savefig('output/example/fortran/contour_demo/mixed_plot.png')
        call savefig('output/example/fortran/contour_demo/mixed_plot.pdf')
        call savefig('output/example/fortran/contour_demo/mixed_plot.txt')

        print *, "Created: mixed_plot.png/pdf/txt"
    end subroutine demo_mixed_contour_line

    subroutine demo_filled_contour()
        !! Filled contour plots with colorbar
        integer, parameter :: nx = 40, ny = 30
        real(wp) :: x(nx), y(ny)
        real(wp) :: z(ny, nx)
        real(wp) :: r
        integer :: i, j

        print *, "=== Filled Contour Demo ==="

        do i = 1, nx
            x(i) = -3.0_wp + real(i - 1, wp) * 6.0_wp / real(nx - 1, wp)
        end do
        do j = 1, ny
            y(j) = -2.5_wp + real(j - 1, wp) * 5.0_wp / real(ny - 1, wp)
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
        call add_contour_filled(x, y, z, colormap='plasma', show_colorbar=.true.)
        call savefig('output/example/fortran/contour_demo/contour_filled.png')
        call savefig('output/example/fortran/contour_demo/contour_filled.pdf')
        call savefig('output/example/fortran/contour_demo/contour_filled.txt')

        print *, "Created: contour_filled.png/pdf/txt"
    end subroutine demo_filled_contour

    subroutine demo_colormap_comparison()
        !! Compare different colormaps with ripple function
        real(wp), dimension(80) :: x_grid, y_grid
        real(wp), dimension(80, 80) :: z_grid
        integer :: i, j

        print *, "=== Colormap Comparison ==="

        do i = 1, 80
            x_grid(i) = -2.0_wp + (i - 1) * 4.0_wp / 79.0_wp
            y_grid(i) = -2.0_wp + (i - 1) * 4.0_wp / 79.0_wp
        end do

        do i = 1, 80
            do j = 1, 80
                z_grid(i, j) = sin(sqrt(x_grid(i)**2 + y_grid(j)**2) * 3.0_wp) &
                    * exp(-0.3_wp * sqrt(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call figure(figsize=[6.4_wp, 4.8_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Ripple Function - Inferno Colormap")
        call add_contour_filled(x_grid, y_grid, z_grid, colormap='inferno')
        call savefig('output/example/fortran/contour_demo/ripple_inferno.png')
        call savefig('output/example/fortran/contour_demo/ripple_inferno.pdf')
        call savefig('output/example/fortran/contour_demo/ripple_inferno.txt')

        call figure(figsize=[6.4_wp, 4.8_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Ripple Function - Coolwarm Colormap")
        call add_contour_filled(x_grid, y_grid, z_grid, colormap='coolwarm')
        call savefig('output/example/fortran/contour_demo/ripple_coolwarm.png')
        call savefig('output/example/fortran/contour_demo/ripple_coolwarm.pdf')
        call savefig('output/example/fortran/contour_demo/ripple_coolwarm.txt')

        call figure(figsize=[6.4_wp, 4.8_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Ripple Function - Jet Colormap")
        call add_contour_filled(x_grid, y_grid, z_grid, colormap='jet')
        call savefig('output/example/fortran/contour_demo/ripple_jet.png')
        call savefig('output/example/fortran/contour_demo/ripple_jet.pdf')
        call savefig('output/example/fortran/contour_demo/ripple_jet.txt')

        print *, "Created: ripple_inferno/coolwarm/jet.png/pdf/txt"
        print *, "Contour demo complete!"
    end subroutine demo_colormap_comparison

end program contour_demo