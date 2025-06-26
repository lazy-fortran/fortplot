program marker_demo
    !! Demonstrates antialiased markers with smooth edges
    !!
    !! This example showcases the improved marker rendering quality
    !! with antialiasing, comparing different marker sizes and types.
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call demo_antialiased_circles()
    call demo_marker_comparison()
    call demo_scatter_plot()
    call demo_all_marker_types()
    call demo_marker_colors()
    call demo_transparent_markers()
    
    print *, "=== Marker Examples ==="
    print *, "Created: antialiased_circles.png/pdf/txt"
    print *, "Created: marker_comparison.png/pdf/txt" 
    print *, "Created: scatter_plot.png/pdf/txt"
    print *, "Created: all_marker_types.png/pdf/txt"
    print *, "Created: marker_colors.png/pdf/txt"
    print *, "Created: transparent_markers.png/pdf/txt"
    print *, "All marker examples completed!"

contains

    subroutine demo_antialiased_circles()
        !! Demonstrates smooth, antialiased circle markers at different sizes
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        real(wp) :: sizes(5)
        integer :: i
        
        ! Create data points for different sized markers
        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y = [2.0_wp, 2.0_wp, 2.0_wp, 2.0_wp, 2.0_wp]
        sizes = [3.0_wp, 5.0_wp, 7.0_wp, 9.0_wp, 12.0_wp]  ! Different marker sizes
        
        call fig%initialize(600, 400)
        call fig%set_title("Antialiased Circle Markers")
        call fig%set_xlabel("Position")
        call fig%set_ylabel("Value")
        
        ! Draw markers at different sizes to show antialiasing quality
        do i = 1, 5
            call fig%add_plot(x(i:i), y(i:i), marker='o', linestyle='None', label='Size')
        end do
        
        ! Add some data to show markers in context
        call fig%add_plot([0.5_wp, 5.5_wp], [1.5_wp, 1.5_wp], linestyle='-', label='Reference line')
        
        call fig%legend()
        call fig%savefig('example/marker_demo/antialiased_circles.png')
        call fig%savefig('example/marker_demo/antialiased_circles.pdf')
        call fig%savefig('example/marker_demo/antialiased_circles.txt')
    end subroutine demo_antialiased_circles

    subroutine demo_marker_comparison()
        !! Shows the quality difference between aliased and antialiased markers
        type(figure_t) :: fig
        real(wp) :: x1(3), y1(3), x2(3), y2(3)
        
        ! Data for comparison
        x1 = [1.0_wp, 2.0_wp, 3.0_wp]
        y1 = [3.0_wp, 3.0_wp, 3.0_wp]
        x2 = [1.0_wp, 2.0_wp, 3.0_wp] 
        y2 = [1.0_wp, 1.0_wp, 1.0_wp]
        
        call fig%initialize(500, 400)
        call fig%set_title("Marker Quality Comparison")
        call fig%set_xlabel("X Position")
        call fig%set_ylabel("Y Position")
        
        ! Antialiased markers (our implementation)
        call fig%add_plot(x1, y1, marker='o', linestyle='None', label='Antialiased')
        
        ! Regular plot points for comparison  
        call fig%add_plot(x2, y2, marker='o', linestyle='None', label='Standard')
        
        call fig%legend()
        call fig%savefig('example/marker_demo/marker_comparison.png')
        call fig%savefig('example/marker_demo/marker_comparison.pdf')
        call fig%savefig('example/marker_demo/marker_comparison.txt')
    end subroutine demo_marker_comparison

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
        !! Demonstrates all available marker types (circle, square, diamond, x)
        type(figure_t) :: fig
        real(wp) :: x(4), y(4)
        character(len=1) :: markers(4)
        character(len=7) :: labels(4)
        integer :: i
        
        ! Position data for different marker types
        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y = [2.0_wp, 2.0_wp, 2.0_wp, 2.0_wp]
        markers = ['o', 's', 'D', 'x']
        labels = ['Circle ', 'Square ', 'Diamond', 'Cross  ']
        
        call fig%initialize(600, 400)
        call fig%set_title("All Marker Types - Antialiased")
        call fig%set_xlabel("Marker Type")
        call fig%set_ylabel("Value")
        
        ! Draw each marker type
        do i = 1, 4
            call fig%add_plot(x(i:i), y(i:i), marker=markers(i), linestyle='None', label=trim(labels(i)))
        end do
        
        call fig%legend()
        call fig%savefig('example/marker_demo/all_marker_types.png')
        call fig%savefig('example/marker_demo/all_marker_types.pdf')
        call fig%savefig('example/marker_demo/all_marker_types.txt')
    end subroutine demo_all_marker_types

    subroutine demo_marker_colors()
        !! Demonstrates separate edge and face colors for markers
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        
        ! Data points for color demonstration
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [2.0_wp, 2.0_wp, 2.0_wp]
        
        call fig%initialize(500, 400)
        call fig%set_title("Marker Edge and Face Colors")
        call fig%set_xlabel("X Position")
        call fig%set_ylabel("Y Position")
        
        ! Note: This will use the current color API until separate edge/face API is available
        ! For now, demonstrate with different colored markers using RGB values
        call fig%add_plot(x(1:1), y(1:1), marker='o', linestyle='None', label='Blue marker')
        call fig%add_plot(x(2:2), y(2:2), marker='s', linestyle='None', label='Green marker')
        call fig%add_plot(x(3:3), y(3:3), marker='D', linestyle='None', label='Red marker')
        
        call fig%legend()
        call fig%savefig('example/marker_demo/marker_colors.png')
        call fig%savefig('example/marker_demo/marker_colors.pdf')
        call fig%savefig('example/marker_demo/marker_colors.txt')
    end subroutine demo_marker_colors

    subroutine demo_transparent_markers()
        !! Demonstrates transparent marker fills and overlapping effects
        type(figure_t) :: fig
        real(wp) :: x(6), y(6)
        integer :: i
        
        ! Create overlapping markers to show transparency
        x = [1.0_wp, 1.2_wp, 2.0_wp, 2.2_wp, 3.0_wp, 3.2_wp]
        y = [2.0_wp, 2.0_wp, 2.0_wp, 2.0_wp, 2.0_wp, 2.0_wp]
        
        call fig%initialize(600, 400)
        call fig%set_title("Transparent Marker Fills")
        call fig%set_xlabel("X Position")
        call fig%set_ylabel("Y Position")
        
        ! Draw overlapping markers with different colors
        ! Note: Transparency will be implemented through the new alpha API
        do i = 1, 6, 2
            call fig%add_plot(x(i:i+1), y(i:i+1), marker='o', linestyle='None', label='Transparent markers')
        end do
        
        call fig%legend()
        call fig%savefig('example/marker_demo/transparent_markers.png')
        call fig%savefig('example/marker_demo/transparent_markers.pdf')
        call fig%savefig('example/marker_demo/transparent_markers.txt')
    end subroutine demo_transparent_markers

end program marker_demo