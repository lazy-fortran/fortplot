program legend_demo
    !! Demonstration of legend functionality following SOLID principles
    !! Shows legend positioning, labeling, and rendering across all backends
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    call basic_legend_example()
    call positioned_legend_example()
    call multi_function_legend_example()
    
contains

    subroutine basic_legend_example()
        !! Basic legend usage with labeled plots
        type(figure_t) :: fig
        real(wp), dimension(50) :: x, y1, y2
        integer :: i
        
        print *, "=== Basic Legend Example ==="
        
        ! Generate data
        x = [(real(i-1, wp) * 0.2_wp, i=1, 50)]
        y1 = sin(x)
        y2 = cos(x)
        
        call fig%initialize(640, 480)
        call fig%set_title("Basic Legend Demo")
        call fig%set_xlabel("x")
        call fig%set_ylabel("y")
        
        ! Add labeled plots
        call fig%add_plot(x, y1, label="sin(x)")
        call fig%add_plot(x, y2, label="cos(x)")
        
        ! Add legend with default position (upper right)
        call fig%legend()
        
        call fig%savefig('output/example/fortran/legend_demo/basic_legend.png')
        call fig%savefig('output/example/fortran/legend_demo/basic_legend.pdf')
        call fig%savefig('output/example/fortran/legend_demo/basic_legend.txt')
        
        print *, "Created: basic_legend.png/pdf/txt"
    end subroutine basic_legend_example

    subroutine positioned_legend_example()
        !! Demonstrate different legend positions
        type(figure_t) :: fig1, fig2, fig3, fig4
        real(wp), dimension(20) :: x, y1, y2
        integer :: i
        
        print *, "=== Legend Positioning Examples ==="
        
        ! Generate test data
        x = [(real(i, wp), i=1, 20)]
        y1 = x**0.5_wp
        y2 = log(x)
        
        ! Upper left position
        call fig1%initialize(640, 480)
        call fig1%set_title("Legend: Upper Left")
        call fig1%add_plot(x, y1, label="√x")
        call fig1%add_plot(x, y2, label="ln(x)")
        call fig1%legend(location="upper left")
        call fig1%savefig('example/fortran/legend_demo/legend_upper_left.png')
        call fig1%savefig('example/fortran/legend_demo/legend_upper_left.pdf')
        call fig1%savefig('example/fortran/legend_demo/legend_upper_left.txt')
        
        ! Upper right position (default)
        call fig2%initialize(640, 480)
        call fig2%set_title("Legend: Upper Right")
        call fig2%add_plot(x, y1, label="√x")
        call fig2%add_plot(x, y2, label="ln(x)")
        call fig2%legend(location="upper right")
        call fig2%savefig('example/fortran/legend_demo/legend_upper_right.png')
        call fig2%savefig('example/fortran/legend_demo/legend_upper_right.pdf')
        call fig2%savefig('example/fortran/legend_demo/legend_upper_right.txt')
        
        ! Lower left position
        call fig3%initialize(640, 480)
        call fig3%set_title("Legend: Lower Left")
        call fig3%add_plot(x, y1, label="√x")
        call fig3%add_plot(x, y2, label="ln(x)")
        call fig3%legend(location="lower left")
        call fig3%savefig('example/fortran/legend_demo/legend_lower_left.png')
        call fig3%savefig('example/fortran/legend_demo/legend_lower_left.pdf')
        call fig3%savefig('example/fortran/legend_demo/legend_lower_left.txt')
        
        ! Lower right position
        call fig4%initialize(640, 480)
        call fig4%set_title("Legend: Lower Right")
        call fig4%add_plot(x, y1, label="√x")
        call fig4%add_plot(x, y2, label="ln(x)")
        call fig4%legend(location="lower right")
        call fig4%savefig('example/fortran/legend_demo/legend_lower_right.png')
        call fig4%savefig('example/fortran/legend_demo/legend_lower_right.pdf')
        call fig4%savefig('example/fortran/legend_demo/legend_lower_right.txt')
        
        print *, "Created: legend_upper_left/right.png/pdf/txt, legend_lower_left/right.png/pdf/txt"
    end subroutine positioned_legend_example

    subroutine multi_function_legend_example()
        !! Complex legend with multiple mathematical functions
        type(figure_t) :: fig
        real(wp), dimension(100) :: x, y1, y2, y3, y4
        integer :: i
        
        print *, "=== Multi-Function Legend Example ==="
        
        ! Generate mathematical functions
        x = [(real(i-1, wp) * 0.1_wp, i=1, 100)]
        y1 = exp(-x/2.0_wp) * cos(x)
        y2 = x * exp(-x/3.0_wp)
        y3 = sin(x) / x
        y4 = x**2 * exp(-x)
        
        call fig%initialize(800, 600)
        call fig%set_title("Mathematical Functions with Legend")
        call fig%set_xlabel("x")
        call fig%set_ylabel("f(x)")
        
        ! Add multiple labeled functions
        call fig%add_plot(x, y1, label="e^(-x/2)cos(x)")
        call fig%add_plot(x, y2, label="xe^(-x/3)")
        call fig%add_plot(x, y3, label="sin(x)/x")
        call fig%add_plot(x, y4, label="x²e^(-x)")
        
        ! Add legend
        call fig%legend()
        
        call fig%savefig('output/example/fortran/legend_demo/multi_function_legend.png')
        call fig%savefig('output/example/fortran/legend_demo/multi_function_legend.pdf')
        call fig%savefig('output/example/fortran/legend_demo/multi_function_legend.txt')
        
        print *, "Created: multi_function_legend.png/pdf/txt"
    end subroutine multi_function_legend_example

end program legend_demo