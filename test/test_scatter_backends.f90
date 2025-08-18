program test_scatter_backends
    !! Backend rendering consistency tests for Issue #56 Enhanced Scatter Plot
    !! RED phase tests for PNG/PDF/ASCII backend marker rendering
    !! 
    !! Backend Requirements (from DESIGN.md):
    !! - PNG/PDF: High-quality vector marker rendering with antialiasing
    !! - ASCII: Character-based marker representation with size/color mapping
    !! - Consistent visual output across all backends
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    write(error_unit, '(A)') '=== RED PHASE: Scatter Plot Backend Rendering Tests ==='
    write(error_unit, '(A)') 'These backend tests should FAIL and drive rendering implementation'
    write(error_unit, '(A)') ''
    
    ! Backend rendering tests (should FAIL)
    call test_png_marker_geometry_rendering()
    call test_pdf_vector_marker_quality()
    call test_ascii_character_marker_mapping()
    call test_marker_size_consistency_across_backends()
    call test_colormap_rendering_consistency()
    call test_complex_marker_shapes_backends()
    
    write(error_unit, '(A)') 'Backend rendering tests completed - all FAILED as expected'
    write(error_unit, '(A)') 'These tests will drive backend implementation in GREEN phase'
    
contains

    subroutine test_png_marker_geometry_rendering()
        !! Given: Scatter plot with various marker shapes
        !! When: I render to PNG backend
        !! Then: Markers should appear as proper geometric shapes with antialiasing
        
        type(figure_t) :: fig
        real(wp) :: x(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: y(6) = [1.0_wp, 2.0_wp, 3.0_wp, 2.5_wp, 1.5_wp, 3.5_wp]
        real(wp) :: sizes(6) = [20.0_wp, 30.0_wp, 40.0_wp, 35.0_wp, 25.0_wp, 45.0_wp]
        
        write(error_unit, '(A)') 'Testing PNG marker geometry rendering...'
        
        call fig%initialize(600, 400)
        
        ! Test different marker shapes (will FAIL - not implemented)
        call fig%scatter(x(1:2), y(1:2), s=sizes(1:2), marker='circle', &
                        facecolor=[1.0_wp, 0.0_wp, 0.0_wp], &
                        edgecolor=[0.0_wp, 0.0_wp, 0.0_wp], &
                        linewidth=2.0_wp, label='Circles')
        
        call fig%scatter(x(3:4), y(3:4), s=sizes(3:4), marker='square', &
                        facecolor=[0.0_wp, 1.0_wp, 0.0_wp], &
                        edgecolor=[0.0_wp, 0.0_wp, 0.0_wp], &
                        linewidth=2.0_wp, label='Squares')
        
        call fig%savefig('/tmp/test_png_markers.png')
        
        error stop 'PNG marker geometry rendering not implemented'
    end subroutine test_png_marker_geometry_rendering
    
    subroutine test_pdf_vector_marker_quality()
        !! Given: Scatter plot with complex marker shapes
        !! When: I render to PDF backend
        !! Then: Markers should be true vector graphics with perfect scaling
        
        type(figure_t) :: fig
        real(wp) :: x(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: y(4) = [1.0_wp, 2.0_wp, 3.0_wp, 2.0_wp]
        real(wp) :: sizes(4) = [25.0_wp, 50.0_wp, 75.0_wp, 40.0_wp]
        real(wp) :: colors(4) = [0.1_wp, 0.4_wp, 0.7_wp, 0.9_wp]
        
        write(error_unit, '(A)') 'Testing PDF vector marker quality...'
        
        call fig%initialize(800, 600)
        
        ! Test complex marker shapes for vector rendering (will FAIL)
        call fig%scatter(x, y, s=sizes, c=colors, marker='star', &
                        colormap='plasma', alpha=0.8_wp, &
                        edgecolor=[0.2_wp, 0.2_wp, 0.2_wp], &
                        linewidth=1.5_wp, label='Vector Stars')
        
        call fig%savefig('/tmp/test_pdf_markers.pdf')
        
        error stop 'PDF vector marker rendering not implemented'
    end subroutine test_pdf_vector_marker_quality
    
    subroutine test_ascii_character_marker_mapping()
        !! Given: Scatter plot with size and color mapping
        !! When: I render to ASCII backend
        !! Then: Markers should be represented with appropriate characters
        
        type(figure_t) :: fig
        real(wp) :: x(8) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp]
        real(wp) :: y(8) = [1.0_wp, 3.0_wp, 2.0_wp, 4.0_wp, 2.5_wp, 3.5_wp, 1.5_wp, 4.5_wp]
        real(wp) :: sizes(8) = [10.0_wp, 20.0_wp, 30.0_wp, 40.0_wp, 50.0_wp, 25.0_wp, 35.0_wp, 45.0_wp]
        real(wp) :: colors(8) = [0.0_wp, 0.14_wp, 0.29_wp, 0.43_wp, 0.57_wp, 0.71_wp, 0.86_wp, 1.0_wp]
        
        write(error_unit, '(A)') 'Testing ASCII character marker mapping...'
        
        call fig%initialize(80, 24)  ! ASCII terminal dimensions
        
        ! Test ASCII marker representation (will FAIL)
        call fig%scatter(x, y, s=sizes, c=colors, marker='circle', &
                        colormap='grayscale', label='ASCII Markers')
        
        call fig%savefig('/tmp/test_ascii_markers.txt')
        
        error stop 'ASCII character marker mapping not implemented'
    end subroutine test_ascii_character_marker_mapping
    
    subroutine test_marker_size_consistency_across_backends()
        !! Given: Identical scatter plot with size mapping
        !! When: I render to PNG, PDF, and ASCII backends
        !! Then: Relative marker sizes should be consistent
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [1.0_wp, 2.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]
        real(wp) :: sizes(5) = [15.0_wp, 30.0_wp, 60.0_wp, 45.0_wp, 20.0_wp]
        
        write(error_unit, '(A)') 'Testing marker size consistency across backends...'
        
        ! Create identical plots for all backends
        call fig%initialize(400, 300)
        call fig%scatter(x, y, s=sizes, marker='diamond', &
                        facecolor=[0.5_wp, 0.2_wp, 0.8_wp], &
                        label='Size Consistency Test')
        
        ! Render to all backends
        call fig%savefig('/tmp/size_consistency.png')
        call fig%savefig('/tmp/size_consistency.pdf')
        call fig%savefig('/tmp/size_consistency.txt')
        
        error stop 'Marker size consistency not implemented'
    end subroutine test_marker_size_consistency_across_backends
    
    subroutine test_colormap_rendering_consistency()
        !! Given: Scatter plot with color mapping and colorbar
        !! When: I render to different backends
        !! Then: Color representation should be consistent
        
        type(figure_t) :: fig
        real(wp) :: x(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: y(6) = [1.0_wp, 2.0_wp, 3.0_wp, 2.5_wp, 1.5_wp, 3.0_wp]
        real(wp) :: colors(6) = [0.0_wp, 0.2_wp, 0.4_wp, 0.6_wp, 0.8_wp, 1.0_wp]
        
        write(error_unit, '(A)') 'Testing colormap rendering consistency...'
        
        call fig%initialize(500, 400)
        
        ! Create color-mapped scatter plot (will FAIL)
        call fig%scatter(x, y, c=colors, marker='hexagon', &
                        colormap='viridis', show_colorbar=.true., &
                        label='Colormap Consistency Test')
        
        call fig%add_colorbar(position='right', label='Color Values')
        
        ! Render to backends
        call fig%savefig('/tmp/colormap_consistency.png')
        call fig%savefig('/tmp/colormap_consistency.pdf')
        call fig%savefig('/tmp/colormap_consistency.txt')
        
        error stop 'Colormap rendering consistency not implemented'
    end subroutine test_colormap_rendering_consistency
    
    subroutine test_complex_marker_shapes_backends()
        !! Given: Scatter plot with complex marker shapes
        !! When: I render complex geometries (star, pentagon, etc.)
        !! Then: Complex shapes should render correctly in each backend
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [1.0_wp, 2.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]
        real(wp) :: sizes(5) = [35.0_wp, 40.0_wp, 45.0_wp, 38.0_wp, 42.0_wp]
        
        write(error_unit, '(A)') 'Testing complex marker shapes across backends...'
        
        call fig%initialize(600, 300)
        
        ! Test complex geometric shapes (will FAIL)
        call fig%scatter([x(1)], [y(1)], s=[sizes(1)], marker='star', &
                        facecolor=[1.0_wp, 0.8_wp, 0.0_wp], label='Star')
        
        call fig%scatter([x(2)], [y(2)], s=[sizes(2)], marker='pentagon', &
                        facecolor=[0.8_wp, 0.0_wp, 1.0_wp], label='Pentagon')
        
        call fig%scatter([x(3)], [y(3)], s=[sizes(3)], marker='hexagon', &
                        facecolor=[0.0_wp, 0.8_wp, 1.0_wp], label='Hexagon')
        
        ! Render to all backends
        call fig%savefig('/tmp/complex_shapes.png')
        call fig%savefig('/tmp/complex_shapes.pdf')
        call fig%savefig('/tmp/complex_shapes.txt')
        
        error stop 'Complex marker shapes not implemented'
    end subroutine test_complex_marker_shapes_backends

end program test_scatter_backends