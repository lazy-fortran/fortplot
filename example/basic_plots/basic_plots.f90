program basic_plots
    !! Basic plotting examples using both simple and OO APIs
    use fortplot
    implicit none

    call simple_plots()
    call multi_line_plot()

contains

    subroutine simple_plots()
        real(wp), dimension(50) :: x, y
        integer :: i
        
        print *, "=== Basic Plots ==="
        
        ! Generate simple sine data
        x = [(real(i-1, wp) * 0.2_wp, i=1, 50)]
        y = sin(x)
        
        ! Simple plot using functional API
        call figure()
        call plot(x, y, label='sin(x)')
        call title('Simple Sine Wave')
        call xlabel('x')
        call ylabel('sin(x)')
        call savefig('example/basic_plots/simple_plot.png')
        call savefig('example/basic_plots/simple_plot.pdf')
        call savefig('example/basic_plots/simple_plot.txt')
        
        print *, "Created: simple_plot.png/pdf/txt"
        
    end subroutine simple_plots

    subroutine multi_line_plot()
        real(wp), dimension(100) :: x, sx, cx
        type(figure_t) :: fig
        integer :: i

        x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
        sx = sin(x)
        cx = cos(x)
        
        ! Multi-line plot using OO interface
        call fig%initialize(640, 480)
        call fig%set_xlabel("x")
        call fig%set_ylabel("y")
        call fig%set_title("Sine and Cosine Functions")
        call fig%add_plot(x, sx, label="sin(x)")
        call fig%add_plot(x, cx, label="cos(x)")
        call fig%savefig('example/basic_plots/multi_line.png')
        call fig%savefig('example/basic_plots/multi_line.pdf')
        call fig%savefig('example/basic_plots/multi_line.txt')
        
        print *, "Created: multi_line.png/pdf/txt"
        
    end subroutine multi_line_plot

end program basic_plots