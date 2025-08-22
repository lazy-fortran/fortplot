program test_ascii_pcolormesh_edge_cases
    !! Test ASCII pcolormesh edge cases and boundary conditions
    !!
    !! Tests robustness of ASCII mesh rendering for edge cases like
    !! empty meshes, single quadrilaterals, degenerate cases, and
    !! boundary conditions.
    !!
    !! Given: ASCII backend with various edge case scenarios
    !! When: Rendering pcolormesh plots
    !! Then: Should handle gracefully without crashes or artifacts

    use fortplot, only: figure_t, wp
    use fortplot_ascii, only: ascii_context, create_ascii_canvas
    use fortplot_security, only: get_test_output_path
    implicit none
    
    logical :: all_tests_passed = .true.
    
    ! Test edge cases
    call test_empty_mesh_rendering()
    call test_single_quad_rendering()  
    call test_degenerate_quad_cases()
    call test_zero_area_quads()
    call test_very_small_canvas()
    call test_very_large_canvas()
    call test_extreme_coordinate_ranges()
    call test_mesh_precision_limits()
    
    if (all_tests_passed) then
        print *, "All ASCII pcolormesh edge case tests PASSED"
    else
        print *, "ASCII pcolormesh edge case tests completed with failures"
        call exit(1)
    end if

contains

    subroutine test_empty_mesh_rendering()
        !! Given: Empty mesh data (0x0 quads)
        !! When: Rendering to ASCII backend
        !! Then: Should handle without crashing, produce empty or minimal output
        
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), data(:,:)
        
        print *, "Testing empty mesh rendering..."
        
        ! Create minimal grid (2 vertices = 0 quads)
        allocate(x(1), y(1))
        allocate(data(0, 0))  ! Empty data array
        
        x(1) = 0.0_wp
        y(1) = 0.0_wp
        
        call fig%initialize(20, 10)
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")  ! Should not crash
        
        print *, "PASS: Empty mesh handled without crash"
        
        ! Test completely empty arrays
        block
            real(wp), allocatable :: empty_x(:), empty_y(:), empty_data(:,:)
            allocate(empty_x(0), empty_y(0), empty_data(0, 0))
            
            call fig%initialize(10, 5)
            ! This might fail with dimension error - should be handled gracefully
            call fig%add_pcolormesh(empty_x, empty_y, empty_data)
            call fig%savefig("terminal")
            
            print *, "PASS: Completely empty arrays handled"
        end block
    end subroutine test_empty_mesh_rendering

    subroutine test_single_quad_rendering()
        !! Given: Single quadrilateral mesh (2x2 vertices, 1x1 quad)
        !! When: Rendering to ASCII backend
        !! Then: Should render single quad with appropriate character pattern
        
        type(figure_t) :: fig
        real(wp) :: x(2) = [0.0_wp, 1.0_wp]
        real(wp) :: y(2) = [0.0_wp, 1.0_wp]
        real(wp) :: data(1, 1) = reshape([0.6_wp], [1, 1])
        
        print *, "Testing single quad rendering..."
        
        call fig%initialize(12, 8)
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")
        
        print *, "EXPECTED FAIL: Single quad renders as solid block instead of pattern"
        all_tests_passed = .false.
        
        ! Test with different single values
        data(1, 1) = 0.0_wp  ! Minimum
        call fig%initialize(12, 8)  
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")
        
        data(1, 1) = 1.0_wp  ! Maximum
        call fig%initialize(12, 8)
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")
        
        print *, "Single quad test completed"
    end subroutine test_single_quad_rendering

    subroutine test_degenerate_quad_cases()
        !! Given: Degenerate quadrilaterals (zero width/height, inverted)
        !! When: Rendering to ASCII backend  
        !! Then: Should handle without artifacts or crashes
        
        type(ascii_context) :: ctx
        real(wp) :: x_quad(4), y_quad(4)
        integer :: i, j, rendered_count
        
        print *, "Testing degenerate quad cases..."
        
        ctx = create_ascii_canvas(20, 15)
        call ctx%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)
        call ctx%color(0.5_wp, 0.5_wp, 0.5_wp)
        
        ! Test zero-width quad (vertical line)
        x_quad = [0.5_wp, 0.5_wp, 0.5_wp, 0.5_wp]  
        y_quad = [0.2_wp, 0.2_wp, 0.8_wp, 0.8_wp]
        call ctx%fill_quad(x_quad, y_quad)  ! Should not crash
        
        rendered_count = 0
        do i = 1, ctx%plot_height
            do j = 1, ctx%plot_width
                if (ctx%canvas(i, j) /= ' ') rendered_count = rendered_count + 1
            end do
        end do
        print '(A,I0)', "  Zero-width quad: ", rendered_count, " characters rendered"
        
        ! Clear canvas
        ctx%canvas = ' '
        
        ! Test zero-height quad (horizontal line)
        x_quad = [0.2_wp, 0.8_wp, 0.8_wp, 0.2_wp]
        y_quad = [0.5_wp, 0.5_wp, 0.5_wp, 0.5_wp]
        call ctx%fill_quad(x_quad, y_quad)  ! Should not crash
        
        rendered_count = 0
        do i = 1, ctx%plot_height
            do j = 1, ctx%plot_width
                if (ctx%canvas(i, j) /= ' ') rendered_count = rendered_count + 1
            end do
        end do
        print '(A,I0)', "  Zero-height quad: ", rendered_count, " characters rendered"
        
        print *, "PASS: Degenerate quads handled without crash"
    end subroutine test_degenerate_quad_cases

    subroutine test_zero_area_quads()
        !! Given: Quadrilaterals with zero area (collapsed to points/lines)
        !! When: Rendering to ASCII backend
        !! Then: Should handle without infinite loops or errors
        
        type(ascii_context) :: ctx
        real(wp) :: x_quad(4), y_quad(4)
        
        print *, "Testing zero-area quads..."
        
        ctx = create_ascii_canvas(15, 10)
        call ctx%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp)
        call ctx%color(0.7_wp, 0.7_wp, 0.7_wp)
        
        ! Test point-collapsed quad (all vertices same)
        x_quad = [0.5_wp, 0.5_wp, 0.5_wp, 0.5_wp]
        y_quad = [0.5_wp, 0.5_wp, 0.5_wp, 0.5_wp]
        call ctx%fill_quad(x_quad, y_quad)  ! Should not hang or crash
        
        print *, "PASS: Point-collapsed quad handled"
        
        ! Test triangle-collapsed quad (3 vertices same)
        x_quad = [0.3_wp, 0.7_wp, 0.3_wp, 0.3_wp]
        y_quad = [0.3_wp, 0.3_wp, 0.3_wp, 0.7_wp]
        call ctx%fill_quad(x_quad, y_quad)  ! Should not hang or crash
        
        print *, "PASS: Triangle-collapsed quad handled"
    end subroutine test_zero_area_quads

    subroutine test_very_small_canvas()
        !! Given: Very small ASCII canvas (minimal dimensions)
        !! When: Rendering pcolormesh
        !! Then: Should handle without buffer overflows or errors
        
        type(figure_t) :: fig
        real(wp) :: x(3) = [0.0_wp, 0.5_wp, 1.0_wp]
        real(wp) :: y(3) = [0.0_wp, 0.5_wp, 1.0_wp]
        real(wp) :: data(2, 2) = reshape([0.1_wp, 0.9_wp, 0.3_wp, 0.7_wp], [2, 2])
        
        print *, "Testing very small canvas..."
        
        ! Test minimal canvas size
        call fig%initialize(3, 2)  ! Very small
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")  ! Should not crash
        
        print *, "PASS: Very small canvas handled"
        
        ! Test single character canvas
        call fig%initialize(1, 1)  
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")
        
        print *, "PASS: Single character canvas handled"
    end subroutine test_very_small_canvas

    subroutine test_very_large_canvas()
        !! Given: Very large ASCII canvas
        !! When: Rendering pcolormesh
        !! Then: Should handle without performance issues or memory errors
        
        type(figure_t) :: fig
        real(wp) :: x(4) = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(4) = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: data(3, 3) = reshape([0.1_wp, 0.2_wp, 0.3_wp, 0.4_wp, 0.5_wp, &
                                         0.6_wp, 0.7_wp, 0.8_wp, 0.9_wp], [3, 3])
        
        print *, "Testing very large canvas..."
        
        ! Test large canvas (but not too large to avoid memory issues in tests)
        call fig%initialize(200, 100)  
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig(get_test_output_path('/tmp/test_large_canvas.txt'))
        
        print *, "PASS: Large canvas handled"
    end subroutine test_very_large_canvas

    subroutine test_extreme_coordinate_ranges()
        !! Given: Extremely large or small coordinate values
        !! When: Rendering pcolormesh
        !! Then: Should handle without numerical overflow or precision loss
        
        type(figure_t) :: fig
        real(wp) :: x_large(3), y_large(3), data(2, 2)
        real(wp) :: x_small(3), y_small(3)
        
        print *, "Testing extreme coordinate ranges..."
        
        ! Test very large coordinates
        x_large = [1.0e6_wp, 2.0e6_wp, 3.0e6_wp]
        y_large = [1.0e6_wp, 2.0e6_wp, 3.0e6_wp]
        data = reshape([0.2_wp, 0.8_wp, 0.4_wp, 0.6_wp], [2, 2])
        
        call fig%initialize(20, 15)
        call fig%add_pcolormesh(x_large, y_large, data)
        call fig%savefig("terminal")  ! Should not overflow
        
        print *, "PASS: Large coordinates handled"
        
        ! Test very small coordinates  
        x_small = [1.0e-6_wp, 2.0e-6_wp, 3.0e-6_wp]
        y_small = [1.0e-6_wp, 2.0e-6_wp, 3.0e-6_wp]
        
        call fig%initialize(20, 15)
        call fig%add_pcolormesh(x_small, y_small, data)
        call fig%savefig("terminal")  # Should not underflow
        
        print *, "PASS: Small coordinates handled"
    end subroutine test_extreme_coordinate_ranges

    subroutine test_mesh_precision_limits()
        !! Given: Mesh coordinates with precision near floating-point limits
        !! When: Rendering pcolormesh
        !! Then: Should maintain precision and not produce artifacts
        
        type(figure_t) :: fig
        real(wp) :: x(3), y(3), data(2, 2)
        real(wp) :: epsilon_val
        
        print *, "Testing mesh precision limits..."
        
        epsilon_val = epsilon(1.0_wp)
        
        ! Test coordinates very close together (near machine precision)
        x = [0.0_wp, epsilon_val, 2.0_wp * epsilon_val]
        y = [0.0_wp, epsilon_val, 2.0_wp * epsilon_val]
        data = reshape([0.3_wp, 0.7_wp, 0.1_wp, 0.9_wp], [2, 2])
        
        call fig%initialize(20, 15)
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")  ! Should handle precision limits
        
        print *, "PASS: Precision limits handled"
        
        ! Test nearly identical coordinates (should not cause division by zero)
        x = [0.0_wp, epsilon_val * 0.1_wp, epsilon_val * 0.2_wp]
        y = [0.0_wp, epsilon_val * 0.1_wp, epsilon_val * 0.2_wp]
        
        call fig%initialize(15, 10)
        call fig%add_pcolormesh(x, y, data)
        call fig%savefig("terminal")
        
        print *, "PASS: Near-identical coordinates handled"
    end subroutine test_mesh_precision_limits

end program test_ascii_pcolormesh_edge_cases