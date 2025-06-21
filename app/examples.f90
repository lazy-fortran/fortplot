program main
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    implicit none

    call line_plot_example()
    call contour_plot_example()

contains

    subroutine line_plot_example()
        real(wp), dimension(100) :: x, sx, cx
        type(figure_t) :: fig
        integer :: i

        print *, "=== Line Plot Example ==="
        
        x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
        sx = sin(x)
        cx = cos(x)
        
        call fig%initialize(640, 480)
        call fig%set_xlabel("x")
        call fig%set_ylabel("y")
        call fig%set_title("Sine and Cosine Functions")

        call fig%add_plot(x, sx, label="sin(x)")
        call fig%add_plot(x, cx, label="cos(x)")

        call fig%savefig('line_plot.png')
        call fig%savefig('line_plot.pdf')
        call fig%savefig('line_plot.txt')
        
        call fig%show()
        
        print *, "Line plot files created: line_plot.png, line_plot.pdf, line_plot.txt"
        print *

    end subroutine line_plot_example

    subroutine contour_plot_example()
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        real(wp), dimension(5) :: custom_levels
        type(figure_t) :: fig1, fig2, fig3
        integer :: i, j

        print *, "=== Contour Plot Examples ==="
        
        do i = 1, 30
            x_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
        end do
        print *, "Creating 2D Gaussian contour plot..."
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call fig1%initialize(640, 480)
        call fig1%set_xlabel("x")
        call fig1%set_ylabel("y")
        call fig1%set_title("2D Gaussian Function")
        call fig1%add_contour(x_grid, y_grid, z_grid, label="exp(-(x²+y²))")
        call fig1%savefig('contour_gaussian.png')
        call fig1%savefig('contour_gaussian.pdf')
        call fig1%savefig('contour_gaussian.txt')

        print *, "Creating saddle function contour plot with custom levels..."
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = x_grid(i)**2 - y_grid(j)**2
            end do
        end do

        custom_levels = [-4.0_wp, -2.0_wp, 0.0_wp, 2.0_wp, 4.0_wp]
        call fig2%initialize(640, 480)
        call fig2%set_xlabel("x")
        call fig2%set_ylabel("y")
        call fig2%set_title("Saddle Function (x²-y²)")
        call fig2%add_contour(x_grid, y_grid, z_grid, levels=custom_levels, label="x²-y²")
        call fig2%savefig('contour_saddle.png')
        call fig2%savefig('contour_saddle.pdf')
        call fig2%savefig('contour_saddle.txt')

        print *, "Creating mixed plot with contour and line overlay..."
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do

        call fig3%initialize(640, 480)
        call fig3%set_xlabel("x")
        call fig3%set_ylabel("y")
        call fig3%set_title("Mixed Plot: Contour + Line")
        call fig3%add_contour(x_grid, y_grid, z_grid, label="2D Gaussian")
        call fig3%add_plot(x_grid, exp(-x_grid**2), label="Cross-section at y=0")
        call fig3%savefig('mixed_plot.png')
        call fig3%savefig('mixed_plot.pdf')
        call fig3%savefig('mixed_plot.txt')

        print *, "Contour plot files created:"
        print *, "  - contour_gaussian.png/pdf/txt (2D Gaussian)"
        print *, "  - contour_saddle.png/pdf/txt (Saddle function)"
        print *, "  - mixed_plot.png/pdf/txt (Contour + Line)"
        print *

        print *, "=== Summary ==="
        print *, "Successfully demonstrated:"
        print *, "  ✓ Line plots with multiple backends"
        print *, "  ✓ Contour plots with default levels"
        print *, "  ✓ Contour plots with custom levels"
        print *, "  ✓ Mixed plots (contour + line)"
        print *, "  ✓ Unified architecture across PNG, PDF, ASCII"

    end subroutine contour_plot_example

end program main
