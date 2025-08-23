program contour_demo
    !! Contour plotting examples with different functions
    use fortplot
    implicit none

    call gaussian_contours()
    call mixed_contour_line_plot()

contains

    subroutine gaussian_contours()
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        type(figure_t) :: fig
        integer :: i, j

        print *, "=== Contour Examples ==="
        
        ! Generate contour grid
        do i = 1, 30
            x_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
        end do

        ! Gaussian contour
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("2D Gaussian Function")
        call add_contour(x_grid, y_grid, z_grid, label="exp(-(x²+y²))")
        call savefig('output/example/fortran/contour_demo/contour_gaussian.png')
        call savefig('output/example/fortran/contour_demo/contour_gaussian.pdf')
        call savefig('output/example/fortran/contour_demo/contour_gaussian.txt')
        
        print *, "Created: contour_gaussian.png/pdf/txt"
        
    end subroutine gaussian_contours

    subroutine mixed_contour_line_plot()
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        real(wp), dimension(5) :: custom_levels
        type(figure_t) :: fig
        integer :: i, j

        ! Generate grid
        do i = 1, 30
            x_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
        end do
        
        ! Saddle function with custom levels
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = x_grid(i)**2 - y_grid(j)**2
            end do
        end do

        custom_levels = [-4.0_wp, -2.0_wp, 0.0_wp, 2.0_wp, 4.0_wp]
        call figure(figsize=[8.0_wp, 6.0_wp])
        call xlabel("x")
        call ylabel("y")
        call title("Mixed Plot: Contour + Line")
        call add_contour(x_grid, y_grid, z_grid, levels=custom_levels, label="x²-y²")
        call add_plot(x_grid, exp(-x_grid**2), label="Cross-section at y=0")
        call savefig('output/example/fortran/contour_demo/mixed_plot.png')
        call savefig('output/example/fortran/contour_demo/mixed_plot.pdf')
        call savefig('output/example/fortran/contour_demo/mixed_plot.txt')
        
        print *, "Created: mixed_plot.png/pdf/txt"
        
    end subroutine mixed_contour_line_plot

end program contour_demo