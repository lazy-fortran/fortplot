program colored_contours_example
    !! Demonstrates colored contour plots with different colormaps
    use fortplot
    implicit none

    call default_gaussian_example()
    call plasma_saddle_example()
    call mixed_colormap_comparison()

contains

    subroutine default_gaussian_example()
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        type(figure_t) :: fig
        integer :: i, j

        print *, "=== Default Colorblind-Safe Gaussian Example ==="
        
        ! Generate grid
        do i = 1, 30
            x_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
        end do

        ! 2D Gaussian
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call fig%initialize(640, 480)
        call fig%set_xlabel("x")
        call fig%set_ylabel("y")
        call fig%set_title("2D Gaussian - Default Colorblind-Safe Colormap")
        call fig%add_contour_filled(x_grid, y_grid, z_grid)  ! Uses default 'crest' colormap
        call fig%savefig('output/example/fortran/colored_contours/gaussian_default.png')
        call fig%savefig('output/example/fortran/colored_contours/gaussian_default.pdf')
        call fig%savefig('output/example/fortran/colored_contours/gaussian_default.txt')
        
        print *, "Created: gaussian_default.png/pdf/txt"
    end subroutine default_gaussian_example

    subroutine plasma_saddle_example()
        real(wp), dimension(25) :: x_grid, y_grid
        real(wp), dimension(25,25) :: z_grid
        real(wp), dimension(8) :: custom_levels
        type(figure_t) :: fig
        integer :: i, j

        print *, "=== Plasma Saddle Function Example ==="
        
        ! Generate grid
        do i = 1, 25
            x_grid(i) = -2.5_wp + (i-1) * 5.0_wp / 24.0_wp
            y_grid(i) = -2.5_wp + (i-1) * 5.0_wp / 24.0_wp
        end do

        ! Saddle function: x^2 - y^2
        do i = 1, 25
            do j = 1, 25
                z_grid(i,j) = x_grid(i)**2 - y_grid(j)**2
            end do
        end do

        ! Custom contour levels
        custom_levels = [-6.0_wp, -4.0_wp, -2.0_wp, -1.0_wp, 1.0_wp, 2.0_wp, 4.0_wp, 6.0_wp]

        call fig%initialize(640, 480)
        call fig%set_xlabel("x")
        call fig%set_ylabel("y")
        call fig%set_title("Saddle Function - Plasma Colormap")
        call fig%add_contour_filled(x_grid, y_grid, z_grid, levels=custom_levels, colormap="plasma")
        call fig%savefig('output/example/fortran/colored_contours/saddle_plasma.png')
        call fig%savefig('output/example/fortran/colored_contours/saddle_plasma.pdf')
        call fig%savefig('output/example/fortran/colored_contours/saddle_plasma.txt')
        
        print *, "Created: saddle_plasma.png/pdf/txt"
    end subroutine plasma_saddle_example

    subroutine mixed_colormap_comparison()
        real(wp), dimension(20) :: x_grid, y_grid
        real(wp), dimension(20,20) :: z_grid
        type(figure_t) :: fig1, fig2, fig3
        integer :: i, j

        print *, "=== Colormap Comparison ==="
        
        ! Generate grid
        do i = 1, 20
            x_grid(i) = -2.0_wp + (i-1) * 4.0_wp / 19.0_wp
            y_grid(i) = -2.0_wp + (i-1) * 4.0_wp / 19.0_wp
        end do

        ! Ripple function
        do i = 1, 20
            do j = 1, 20
                z_grid(i,j) = sin(sqrt(x_grid(i)**2 + y_grid(j)**2) * 3.0_wp) * exp(-0.3_wp * sqrt(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        ! Inferno colormap
        call fig1%initialize(640, 480)
        call fig1%set_xlabel("x")
        call fig1%set_ylabel("y")
        call fig1%set_title("Ripple Function - Inferno Colormap")
        call fig1%add_contour_filled(x_grid, y_grid, z_grid, colormap="inferno")
        call fig1%savefig('output/example/fortran/colored_contours/ripple_inferno.png')
        call fig1%savefig('output/example/fortran/colored_contours/ripple_inferno.pdf')
        call fig1%savefig('output/example/fortran/colored_contours/ripple_inferno.txt')

        ! Coolwarm colormap
        call fig2%initialize(640, 480)
        call fig2%set_xlabel("x")
        call fig2%set_ylabel("y")
        call fig2%set_title("Ripple Function - Coolwarm Colormap")
        call fig2%add_contour_filled(x_grid, y_grid, z_grid, colormap="coolwarm")
        call fig2%savefig('output/example/fortran/colored_contours/ripple_coolwarm.png')
        call fig2%savefig('output/example/fortran/colored_contours/ripple_coolwarm.pdf')
        call fig2%savefig('output/example/fortran/colored_contours/ripple_coolwarm.txt')

        ! Jet colormap
        call fig3%initialize(640, 480)
        call fig3%set_xlabel("x")
        call fig3%set_ylabel("y")
        call fig3%set_title("Ripple Function - Jet Colormap")
        call fig3%add_contour_filled(x_grid, y_grid, z_grid, colormap="jet")
        call fig3%savefig('output/example/fortran/colored_contours/ripple_jet.png')
        call fig3%savefig('output/example/fortran/colored_contours/ripple_jet.pdf')
        call fig3%savefig('output/example/fortran/colored_contours/ripple_jet.txt')
        
        print *, "Created: ripple_inferno.png/pdf/txt, ripple_coolwarm.png/pdf/txt, ripple_jet.png/pdf/txt"
        print *, "Colormap comparison complete!"
    end subroutine mixed_colormap_comparison

end program colored_contours_example