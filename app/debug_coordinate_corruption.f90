program debug_coordinate_corruption
    !! Emergency test to reproduce catastrophic PNG coordinate system regression
    !!
    !! Expected: Coordinate system corruption with diagonal streaking
    !! This test generates various plot types to identify the corruption
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    print *, "=== Emergency Coordinate Corruption Debug ==="
    print *, ""
    
    call test_basic_line_plot()
    call test_scatter_plot()
    call test_multiple_series()
    call test_dense_data()
    call test_geometric_shapes()
    
    print *, ""
    print *, "Coordinate corruption debug completed."
    print *, "Check output files for diagonal streaking or skewed coordinates"

contains

    subroutine test_basic_line_plot()
        !! Test basic line plot for coordinate corruption
        type(figure_t) :: fig
        real(wp) :: x(10), y(10)
        integer :: i
        
        print *, "Testing basic line plot..."
        
        do i = 1, 10
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp) ** 2
        end do
        
        call fig%initialize(width=400, height=300)
        call fig%add_plot(x, y, label="y=x²")
        call fig%set_xlabel("X")
        call fig%set_ylabel("Y")
        call fig%set_title("Basic Line Test")
        call fig%savefig("debug_basic_line.png")
        print *, "  → debug_basic_line.png"
    end subroutine test_basic_line_plot

    subroutine test_scatter_plot()
        !! Test scatter plot for coordinate corruption
        type(figure_t) :: fig
        real(wp) :: x(20), y(20)
        integer :: i
        
        print *, "Testing scatter plot..."
        
        do i = 1, 20
            x(i) = real(i-1, wp) * 0.5_wp
            y(i) = sin(x(i))
        end do
        
        call fig%initialize(width=400, height=300)
        call fig%add_scatter(x, y, marker='o', label="sin(x)")
        call fig%set_xlabel("X")
        call fig%set_ylabel("Y")
        call fig%set_title("Scatter Test")
        call fig%savefig("debug_scatter.png")
        print *, "  → debug_scatter.png"
    end subroutine test_scatter_plot

    subroutine test_multiple_series()
        !! Test multiple series for coordinate corruption
        type(figure_t) :: fig
        real(wp) :: x(15), y1(15), y2(15), y3(15)
        integer :: i
        
        print *, "Testing multiple series..."
        
        do i = 1, 15
            x(i) = real(i-1, wp) * 0.2_wp
            y1(i) = x(i)
            y2(i) = x(i) ** 2
            y3(i) = x(i) ** 3
        end do
        
        call fig%initialize(width=500, height=400)
        call fig%add_plot(x, y1, label="linear")
        call fig%add_plot(x, y2, label="quadratic") 
        call fig%add_plot(x, y3, label="cubic")
        call fig%set_xlabel("X")
        call fig%set_ylabel("Y")
        call fig%set_title("Multiple Series Test")
        call fig%legend()
        call fig%savefig("debug_multiple.png")
        print *, "  → debug_multiple.png"
    end subroutine test_multiple_series

    subroutine test_dense_data()
        !! Test dense data for coordinate corruption
        type(figure_t) :: fig
        real(wp) :: x(100), y(100)
        integer :: i
        
        print *, "Testing dense data..."
        
        do i = 1, 100
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = sin(x(i) * 2.0_wp) * exp(-x(i) * 0.1_wp)
        end do
        
        call fig%initialize(width=600, height=400)
        call fig%add_plot(x, y, label="dense wave")
        call fig%set_xlabel("X")
        call fig%set_ylabel("Y")
        call fig%set_title("Dense Data Test")
        call fig%savefig("debug_dense.png")
        print *, "  → debug_dense.png"
    end subroutine test_dense_data

    subroutine test_geometric_shapes()
        !! Test geometric shapes for coordinate corruption
        type(figure_t) :: fig
        real(wp) :: x_line(2), y_line(2)
        real(wp) :: x_scatter(4), y_scatter(4)
        
        print *, "Testing geometric shapes..."
        
        ! Diagonal line
        x_line = [0.0_wp, 10.0_wp]
        y_line = [0.0_wp, 10.0_wp]
        
        ! Rectangle corners
        x_scatter = [2.0_wp, 8.0_wp, 8.0_wp, 2.0_wp]
        y_scatter = [2.0_wp, 2.0_wp, 8.0_wp, 8.0_wp]
        
        call fig%initialize(width=400, height=400)
        call fig%add_plot(x_line, y_line, label="diagonal")
        call fig%add_scatter(x_scatter, y_scatter, marker='s', label="corners")
        call fig%set_xlabel("X")
        call fig%set_ylabel("Y")
        call fig%set_title("Geometric Shapes Test")
        call fig%legend()
        call fig%savefig("debug_geometric.png")
        print *, "  → debug_geometric.png"
    end subroutine test_geometric_shapes

end program debug_coordinate_corruption