program test_png_yaxis_reference
    !! Reference test for Y-axis label positioning using PNG backend
    !! Given: Various Y-tick positions and data ranges
    !! When: Using PNG backend for label positioning
    !! Then: Generate reference behavior for comparison with PDF backend
    !! 
    !! This serves as reference implementation to verify correct Y-axis behavior
    !! PNG backend should demonstrate proper label distribution without clustering

    use fortplot
    use fortplot_label_positioning, only: calculate_y_tick_label_position
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_png_reference_distribution()
    call test_png_coordinate_consistency()
    call test_png_multi_range_behavior()
    call test_png_negative_range_handling()
    
    print *, "=== PNG Y-axis reference tests completed ==="

contains

    subroutine test_png_reference_distribution()
        !! Given: Multiple evenly spaced Y-tick positions
        !! When: Using PNG backend label positioning
        !! Then: Demonstrate expected proper distribution (reference for PDF comparison)
        
        real(wp), parameter :: PLOT_TOP = 50.0_wp    ! PNG: Y=0 at top
        real(wp), parameter :: PLOT_HEIGHT = 300.0_wp
        real(wp), parameter :: PLOT_RIGHT = 400.0_wp
        real(wp), parameter :: EXPECTED_SPACING = 50.0_wp
        integer, parameter :: NUM_TICKS = 5
        
        real(wp) :: tick_positions(NUM_TICKS)
        real(wp) :: label_x, label_y
        real(wp) :: label_y_positions(NUM_TICKS)
        real(wp) :: spacing_differences(NUM_TICKS-1)
        character(len=10) :: labels(NUM_TICKS) = ["0.0", "1.0", "2.0", "3.0", "4.0"]
        integer :: i
        
        print *, "=== PNG Reference: Y-label distribution ==="
        print *, "PNG coordinates: Y=0 at top, Y increases downward"
        
        ! Create evenly spaced tick positions for PNG (Y increases downward)
        do i = 1, NUM_TICKS
            tick_positions(i) = PLOT_TOP + real(i-1, wp) * EXPECTED_SPACING
        end do
        
        ! Calculate all label positions using PNG positioning
        do i = 1, NUM_TICKS
            call calculate_y_tick_label_position(tick_positions(i), PLOT_RIGHT, &
                                               trim(labels(i)), label_x, label_y)
            label_y_positions(i) = label_y
            print *, "PNG: Tick Y=", tick_positions(i), "-> Label Y=", label_y
        end do
        
        ! Measure spacing consistency (reference behavior)
        print *, "PNG spacing analysis (reference):"
        do i = 1, NUM_TICKS-1
            spacing_differences(i) = label_y_positions(i+1) - label_y_positions(i)
            print *, "  Spacing", i, "to", i+1, ":", spacing_differences(i), "px"
        end do
        
        ! Generate test plot for visual verification
        call create_png_reference_plot()
        
        print *, "PNG reference behavior documented for PDF comparison"
        print *, ""
        
    end subroutine test_png_reference_distribution

    subroutine test_png_coordinate_consistency()
        !! Given: Known tick positions in PNG coordinate system
        !! When: Calculating PNG label positions
        !! Then: Document reference coordinate transformation behavior
        
        real(wp), parameter :: PLOT_TOP = 50.0_wp
        real(wp), parameter :: PLOT_HEIGHT = 300.0_wp  
        real(wp), parameter :: PLOT_RIGHT = 400.0_wp
        
        real(wp) :: test_positions(4) = [75.0_wp, 125.0_wp, 175.0_wp, 225.0_wp]
        real(wp) :: label_x, label_y, position_offset
        character(len=10) :: test_label = "ref"
        integer :: i
        
        print *, "=== PNG Reference: Coordinate consistency ==="
        
        do i = 1, 4
            call calculate_y_tick_label_position(test_positions(i), PLOT_RIGHT, &
                                               test_label, label_x, label_y)
            
            position_offset = label_y - test_positions(i)
            print *, "PNG: Tick Y=", test_positions(i), "-> Label Y=", label_y, &
                    "Offset=", position_offset, "px"
        end do
        
        print *, "PNG coordinate transformation documented as reference"
        print *, ""
        
    end subroutine test_png_coordinate_consistency

    subroutine test_png_multi_range_behavior()
        !! Given: Various data ranges (small, medium, large)
        !! When: Creating PNG plots with different Y-ranges
        !! Then: Document how PNG handles different scales (reference for PDF)
        
        type(figure_t) :: fig
        real(wp) :: x(10), y_small(10), y_medium(10), y_large(10)
        integer :: i
        
        print *, "=== PNG Reference: Multi-range behavior ==="
        
        ! Create test data with different scales
        do i = 1, 10
            x(i) = real(i, wp)
            y_small(i) = real(i, wp) * 0.1_wp - 0.5_wp     ! Range: [-0.4, 0.5]
            y_medium(i) = real(i, wp) * 5.0_wp - 25.0_wp   ! Range: [-20, 25]  
            y_large(i) = real(i, wp) * 100.0_wp - 500.0_wp ! Range: [-400, 500]
        end do
        
        ! Small range test
        call fig%initialize(400, 300)
        call fig%add_plot(x, y_small, label="small_range")
        call fig%savefig("png_ref_small_range.png")
        print *, "Created PNG reference: small range [-0.4, 0.5]"
        
        ! Medium range test  
        call fig%initialize(400, 300)
        call fig%add_plot(x, y_medium, label="medium_range")
        call fig%savefig("png_ref_medium_range.png")
        print *, "Created PNG reference: medium range [-20, 25]"
        
        ! Large range test
        call fig%initialize(400, 300) 
        call fig%add_plot(x, y_large, label="large_range")
        call fig%savefig("png_ref_large_range.png")
        print *, "Created PNG reference: large range [-400, 500]"
        
        print *, "PNG multi-range reference plots created"
        print *, ""
        
    end subroutine test_png_multi_range_behavior

    subroutine test_png_negative_range_handling()
        !! Given: Data ranges with negative values
        !! When: Creating PNG plots with negative Y-ranges  
        !! Then: Document PNG negative value handling (reference)
        
        type(figure_t) :: fig
        real(wp) :: x(8), y_negative(8), y_mixed(8)
        integer :: i
        
        print *, "=== PNG Reference: Negative range handling ==="
        
        ! Create negative and mixed range data
        do i = 1, 8
            x(i) = real(i, wp)
            y_negative(i) = -real(i, wp) * 2.0_wp          ! Range: [-2, -16]
            y_mixed(i) = real(i - 4, wp) * 3.0_wp          ! Range: [-9, 12]
        end do
        
        ! Purely negative range
        call fig%initialize(400, 300)
        call fig%add_plot(x, y_negative, label="negative_only")
        call fig%savefig("png_ref_negative_range.png")
        print *, "Created PNG reference: negative range [-2, -16]"
        
        ! Mixed positive/negative range  
        call fig%initialize(400, 300)
        call fig%add_plot(x, y_mixed, label="mixed_range")
        call fig%savefig("png_ref_mixed_range.png")
        print *, "Created PNG reference: mixed range [-9, 12]"
        
        print *, "PNG negative range reference plots created"
        print *, ""
        
    end subroutine test_png_negative_range_handling

    subroutine create_png_reference_plot()
        !! Create a comprehensive PNG reference plot for visual comparison
        
        type(figure_t) :: fig
        real(wp) :: x(20), y(20)
        integer :: i
        
        print *, "Creating comprehensive PNG reference plot..."
        
        ! Create data that spans multiple quadrants
        do i = 1, 20
            x(i) = real(i, wp)
            y(i) = sin(real(i, wp) * 0.3_wp) * 10.0_wp
        end do
        
        call fig%initialize(600, 400)
        call fig%add_plot(x, y, label="PNG_reference")
        call fig%savefig("png_yaxis_reference.png")
        
        print *, "Created png_yaxis_reference.png for visual comparison"
        
    end subroutine create_png_reference_plot

end program test_png_yaxis_reference