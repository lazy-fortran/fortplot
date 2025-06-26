program marker_demo
    !! Demonstrates different marker types and styles
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call demo_scatter_plot()
    call demo_all_marker_types()
    call demo_marker_colors()
    
    print *, "=== Marker Examples ==="
    print *, "Created: scatter_plot.png/pdf/txt"
    print *, "Created: all_marker_types.png/pdf/txt"
    print *, "Created: marker_colors.png/pdf/txt"
    print *, "All marker examples completed!"

contains


    subroutine demo_scatter_plot()
        !! Creates a scatter plot to demonstrate markers in practical use
        type(figure_t) :: fig
        real(wp) :: x(20), y(20)
        integer :: i
        
        ! Generate scatter data
        do i = 1, 20
            x(i) = real(i, wp) * 0.3_wp
            y(i) = sin(x(i)) + 0.1_wp * real(i - 10, wp) / 10.0_wp  ! Sine wave with trend
        end do
        
        call fig%initialize(600, 450)
        call fig%set_title("Scatter Plot with Antialiased Markers")
        call fig%set_xlabel("X Values")
        call fig%set_ylabel("Y Values")
        
        ! Scatter plot with markers
        call fig%add_plot(x, y, marker='o', linestyle='None', label='Data Points')
        
        ! Add trend line for context
        call fig%add_plot(x, sin(x), linestyle='-', label='Sin(x) Reference')
        
        call fig%legend()
        call fig%savefig('example/marker_demo/scatter_plot.png')
        call fig%savefig('example/marker_demo/scatter_plot.pdf')
        call fig%savefig('example/marker_demo/scatter_plot.txt')
    end subroutine demo_scatter_plot

    subroutine demo_all_marker_types()
        !! Demonstrates all available marker types with realistic data
        type(figure_t) :: fig
        real(wp) :: x1(10), y1(10), x2(10), y2(10), x3(10), y3(10), x4(10), y4(10)
        integer :: i
        
        ! Generate realistic data for each marker type
        do i = 1, 10
            x1(i) = real(i, wp) * 0.5_wp
            y1(i) = sin(x1(i)) + 3.0_wp
            x2(i) = real(i, wp) * 0.5_wp
            y2(i) = cos(x2(i)) + 2.0_wp
            x3(i) = real(i, wp) * 0.5_wp
            y3(i) = sin(x3(i) * 2.0_wp) + 1.0_wp
            x4(i) = real(i, wp) * 0.5_wp
            y4(i) = cos(x4(i) * 1.5_wp)
        end do
        
        call fig%initialize(600, 400)
        call fig%set_title("All Marker Types")
        call fig%set_xlabel("X Values")
        call fig%set_ylabel("Y Values")
        
        ! Draw each marker type with data
        call fig%add_plot(x1, y1, marker='o', linestyle='None', label='Circle')
        call fig%add_plot(x2, y2, marker='s', linestyle='None', label='Square')
        call fig%add_plot(x3, y3, marker='D', linestyle='None', label='Diamond')
        call fig%add_plot(x4, y4, marker='x', linestyle='None', label='Cross')
        
        call fig%legend()
        call fig%savefig('example/marker_demo/all_marker_types.png')
        call fig%savefig('example/marker_demo/all_marker_types.pdf')
        call fig%savefig('example/marker_demo/all_marker_types.txt')
    end subroutine demo_all_marker_types

    subroutine demo_marker_colors()
        !! Demonstrates different colored markers with realistic data
        type(figure_t) :: fig
        real(wp) :: x1(8), y1(8), x2(8), y2(8), x3(8), y3(8)
        integer :: i
        
        ! Generate realistic data for color demonstration
        do i = 1, 8
            x1(i) = real(i, wp) * 0.6_wp
            y1(i) = exp(-x1(i) * 0.3_wp) * cos(x1(i)) + 2.5_wp
            x2(i) = real(i, wp) * 0.6_wp
            y2(i) = exp(-x2(i) * 0.2_wp) * sin(x2(i)) + 1.5_wp
            x3(i) = real(i, wp) * 0.6_wp
            y3(i) = exp(-x3(i) * 0.4_wp) * sin(x3(i) * 1.5_wp) + 0.5_wp
        end do
        
        call fig%initialize(500, 400)
        call fig%set_title("Marker Colors and Styles")
        call fig%set_xlabel("X Position")
        call fig%set_ylabel("Y Position")
        
        ! Different marker types with automatic color cycling
        call fig%add_plot(x1, y1, marker='o', linestyle='None', label='Blue circles')
        call fig%add_plot(x2, y2, marker='s', linestyle='None', label='Green squares')
        call fig%add_plot(x3, y3, marker='D', linestyle='None', label='Orange diamonds')
        
        call fig%legend()
        call fig%savefig('example/marker_demo/marker_colors.png')
        call fig%savefig('example/marker_demo/marker_colors.pdf')
        call fig%savefig('example/marker_demo/marker_colors.txt')
    end subroutine demo_marker_colors

end program marker_demo