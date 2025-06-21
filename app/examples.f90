program main
    use fortplot
    implicit none

    call simple_api_examples()
    call object_oriented_examples()

contains

    subroutine simple_api_examples()
        real(wp), dimension(100) :: x, sx, cx
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        integer :: i, j

        print *, "=== Pyplot-Style API Examples ==="
        
        x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
        sx = sin(x)
        cx = cos(x)
        
        ! Pyplot-style line plot
        call figure()
        call plot(x, sx, label='sin(x)')
        call title('Simple Sine Plot')
        call xlabel('x')
        call ylabel('y')
        call savefig('simple_sine.png')
        
        ! Pyplot-style terminal display
        call figure()
        call plot(x, sx, label='sin(x)')
        call title('Sine Function')
        call show()
        
        ! Simple multi-line plot  
        call figure()
        call plot(x, sx, label='sin(x)')
        call plot(x, cx, label='cos(x)')
        call title('Sine and Cosine')
        call xlabel('x')
        call ylabel('y')
        call savefig('multi_trig.png')
        
        ! Clear line style demonstration - all styles on simple data
        call figure()
        call plot(x, sx + 2.0_wp, label='solid (-)', linestyle='-')
        call plot(x, sx + 1.0_wp, label='dashed (--)', linestyle='--')
        call plot(x, sx, label='dotted (:)', linestyle=':')
        call plot(x, sx - 1.0_wp, label='dash-dot (-.)', linestyle='-.')
        call title('All Line Styles')
        call xlabel('x')
        call ylabel('y')
        call savefig('line_styles.png')
        
        ! Generate contour data
        do i = 1, 30
            x_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
            y_grid(i) = -3.0_wp + (i-1) * 6.0_wp / 29.0_wp
        end do
        
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do
        
        ! Pyplot-style contour plot
        call figure()
        call contour(x_grid, y_grid, z_grid)
        call title('Simple Gaussian')
        call xlabel('x')
        call ylabel('y')
        call savefig('simple_contour.png')
        
        print *, "Pyplot-style API files created:"
        print *, "  - simple_sine.png (basic plot)"
        print *, "  - multi_trig.png (multiple lines)"  
        print *, "  - line_styles.png (all line styles)"
        print *, "  - simple_contour.png (contour plot)"
        print *

    end subroutine simple_api_examples

    subroutine object_oriented_examples()
        real(wp), dimension(100) :: x, sx, cx
        real(wp), dimension(30) :: x_grid, y_grid
        real(wp), dimension(30,30) :: z_grid
        real(wp), dimension(5) :: custom_levels
        type(figure_t) :: fig1, fig2, fig3
        integer :: i, j

        print *, "=== Object-Oriented API Examples ==="
        
        x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
        sx = sin(x)
        cx = cos(x)
        
        ! Multi-line plot using OO interface
        call fig1%initialize(640, 480)
        call fig1%set_xlabel("x")
        call fig1%set_ylabel("y")
        call fig1%set_title("Sine and Cosine Functions")
        call fig1%add_plot(x, sx, label="sin(x)")
        call fig1%add_plot(x, cx, label="cos(x)")
        call fig1%savefig('multi_line.png')
        call fig1%savefig('multi_line.pdf')
        call fig1%savefig('multi_line.txt')
        
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

        call fig2%initialize(640, 480)
        call fig2%set_xlabel("x")
        call fig2%set_ylabel("y")
        call fig2%set_title("2D Gaussian Function")
        call fig2%add_contour(x_grid, y_grid, z_grid, label="exp(-(x²+y²))")
        call fig2%savefig('contour_gaussian.png')
        call fig2%savefig('contour_gaussian.pdf')

        ! Saddle function with custom levels
        do i = 1, 30
            do j = 1, 30
                z_grid(i,j) = x_grid(i)**2 - y_grid(j)**2
            end do
        end do

        custom_levels = [-4.0_wp, -2.0_wp, 0.0_wp, 2.0_wp, 4.0_wp]
        call fig3%initialize(640, 480)
        call fig3%set_xlabel("x")
        call fig3%set_ylabel("y")
        call fig3%set_title("Mixed Plot: Contour + Line")
        call fig3%add_contour(x_grid, y_grid, z_grid, levels=custom_levels, label="x²-y²")
        call fig3%add_plot(x_grid, exp(-x_grid**2), label="Cross-section at y=0")
        call fig3%savefig('mixed_plot.png')
        call fig3%savefig('mixed_plot.pdf')

        print *, "Object-oriented API files created:"
        print *, "  - multi_line.png/pdf/txt (Multiple line plots)"
        print *, "  - contour_gaussian.png/pdf (2D Gaussian contours)"
        print *, "  - mixed_plot.png/pdf (Contour + line combination)"
        print *

    end subroutine object_oriented_examples

end program main
