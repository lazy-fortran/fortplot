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
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call title("Scatter Plot with Antialiased Markers")
        call xlabel("X Values")
        call ylabel("Y Values")
        
        ! Scatter plot with markers (using proper scatter function)
        call scatter(x, y, label='Data Points', marker='o')
        
        ! Add trend line for context
        call add_plot(x, sin(x), linestyle='-', label='Sin(x) Reference')
        
        call legend()
        call savefig('output/example/fortran/marker_demo/scatter_plot.png')
        call savefig('output/example/fortran/marker_demo/scatter_plot.pdf')
        call savefig('output/example/fortran/marker_demo/scatter_plot.txt')
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
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call title("All Marker Types")
        call xlabel("X Values")
        call ylabel("Y Values")
        
        ! Draw each marker type with data (using proper scatter function)
        call scatter(x1, y1, label='Circle', marker='o')
        call scatter(x2, y2, label='Square', marker='s')
        call scatter(x3, y3, label='Diamond', marker='D')
        call scatter(x4, y4, label='Cross', marker='x')
        
        call legend()
        call savefig('output/example/fortran/marker_demo/all_marker_types.png')
        call savefig('output/example/fortran/marker_demo/all_marker_types.pdf')
        call savefig('output/example/fortran/marker_demo/all_marker_types.txt')
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
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call title("Marker Colors and Styles")
        call xlabel("X Position")
        call ylabel("Y Position")
        
        ! Different marker types with automatic color cycling (using proper scatter function)
        call scatter(x1, y1, label='Blue circles', marker='o')
        call scatter(x2, y2, label='Green squares', marker='s')
        call scatter(x3, y3, label='Orange diamonds', marker='D')
        
        call legend()
        call savefig('output/example/fortran/marker_demo/marker_colors.png')
        call savefig('output/example/fortran/marker_demo/marker_colors.pdf')
        call savefig('output/example/fortran/marker_demo/marker_colors.txt')
    end subroutine demo_marker_colors

end program marker_demo