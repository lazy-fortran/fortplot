program styling_demo
    !! Comprehensive demonstration of line and marker styling in FortPlot
    !!
    !! Combines demonstrations of:
    !! - Line styles (solid, dashed, dotted, dash-dot)
    !! - Format strings (matplotlib-style)
    !! - Marker types (circle, square, diamond, cross)
    !! - Scatter plots with markers

    use fortplot
    implicit none

    print *, "=== FortPlot Styling Examples ==="
    call demo_line_styles()
    call demo_format_strings()
    call demo_scatter_markers()
    call demo_all_marker_types()
    print *, "All styling examples completed!"

contains

    subroutine demo_line_styles()
        !! Demonstrates all available line styles
        real(wp), dimension(50) :: x, y1, y2, y3, y4, y5, y6
        integer :: i

        do i = 1, 50
            x(i) = real(i-1, wp) * 0.2_wp
            y1(i) = sin(x(i)) + 2.5_wp
            y2(i) = cos(x(i)) + 1.5_wp
            y3(i) = sin(x(i) * 2.0_wp) + 0.5_wp
            y4(i) = cos(x(i) * 3.0_wp) - 0.5_wp
            y5(i) = sin(x(i) * 0.5_wp) - 1.5_wp
            y6(i) = cos(x(i) * 0.5_wp) - 2.5_wp
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call plot(x, y1, label='Solid (-)', linestyle=LINESTYLE_SOLID)
        call plot(x, y2, label='Dashed (--)', linestyle=LINESTYLE_DASHED)
        call plot(x, y3, label='Dotted (:)', linestyle=LINESTYLE_DOTTED)
        call plot(x, y4, label='Dash-dot (-.)', linestyle=LINESTYLE_DASHDOT)
        call plot(x, y5, label='None (invisible)', linestyle=LINESTYLE_NONE)
        call plot(x, y6, label='Markers only', linestyle='o')
        call title('Line Style Reference')
        call xlabel('X values')
        call ylabel('Y values')
        call legend()
        call savefig('output/example/fortran/styling_demo/line_styles.png')
        call savefig('output/example/fortran/styling_demo/line_styles.pdf')
        call savefig('output/example/fortran/styling_demo/line_styles.txt')
        print *, "Created: line_styles.png/pdf/txt"
    end subroutine demo_line_styles

    subroutine demo_format_strings()
        !! Demonstrates matplotlib-style format strings
        integer, parameter :: n = 50
        real(wp) :: x(n), y1(n), y2(n), y3(n), y4(n)
        integer :: i

        do i = 1, n
            x(i) = real(i-1, wp) * 0.2_wp
            y1(i) = sin(x(i))
            y2(i) = cos(x(i))
            y3(i) = sin(x(i) * 0.5_wp) * 0.8_wp
            y4(i) = cos(x(i) * 0.5_wp) * 0.6_wp
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title('Matplotlib-style Format Strings')
        call xlabel('X values')
        call ylabel('Y values')
        call add_plot(x, y1, label='sin(x) - solid line', linestyle='-')
        call add_plot(x, y2, label='cos(x) - dashed line', linestyle='--')
        call add_plot(x, y3, label='sin(x/2) - circles only', linestyle='o')
        call add_plot(x, y4, label='cos(x/2) - x markers with line', linestyle='x-')
        call legend()
        call savefig('output/example/fortran/styling_demo/format_strings.png')
        call savefig('output/example/fortran/styling_demo/format_strings.pdf')
        call savefig('output/example/fortran/styling_demo/format_strings.txt')
        print *, "Created: format_strings.png/pdf/txt"
    end subroutine demo_format_strings

    subroutine demo_scatter_markers()
        !! Creates a scatter plot demonstrating markers in practical use
        real(wp) :: x(20), y(20)
        integer :: i

        do i = 1, 20
            x(i) = real(i, wp) * 0.3_wp
            y(i) = sin(x(i)) + 0.1_wp * real(i - 10, wp) / 10.0_wp
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title("Scatter Plot with Markers")
        call xlabel("X Values")
        call ylabel("Y Values")
        call scatter(x, y, label='Data Points', marker='o')
        call add_plot(x, sin(x), linestyle='-', label='Sin(x) Reference')
        call legend()
        call savefig('output/example/fortran/styling_demo/scatter_plot.png')
        call savefig('output/example/fortran/styling_demo/scatter_plot.pdf')
        call savefig('output/example/fortran/styling_demo/scatter_plot.txt')
        print *, "Created: scatter_plot.png/pdf/txt"
    end subroutine demo_scatter_markers

    subroutine demo_all_marker_types()
        !! Demonstrates all available marker types
        real(wp) :: x1(10), y1(10), x2(10), y2(10), x3(10), y3(10), x4(10), y4(10)
        integer :: i

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
        call scatter(x1, y1, label='Circle', marker='o')
        call scatter(x2, y2, label='Square', marker='s')
        call scatter(x3, y3, label='Diamond', marker='D')
        call scatter(x4, y4, label='Cross', marker='x')
        call legend()
        call savefig('output/example/fortran/styling_demo/marker_types.png')
        call savefig('output/example/fortran/styling_demo/marker_types.pdf')
        call savefig('output/example/fortran/styling_demo/marker_types.txt')
        print *, "Created: marker_types.png/pdf/txt"
    end subroutine demo_all_marker_types

end program styling_demo
