program test_scatter_enhanced
    !! Comprehensive RED phase test suite for Issue #56 Enhanced Scatter Plot functionality
    !! Following TDD methodology - these tests should FAIL initially and drive implementation
    !! 
    !! Architecture: Tests complete scatter plot enhancement including:
    !! - Size mapping for bubble charts
    !! - Color mapping with colormaps  
    !! - Advanced marker shapes
    !! - Colorbar integration
    !! - Performance optimization for large datasets
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    integer :: test_count = 0, failed_count = 0
    
    write(error_unit, '(A)') '=== RED PHASE: Enhanced Scatter Plot Tests ==='
    write(error_unit, '(A)') 'These tests should FAIL initially and drive implementation'
    write(error_unit, '(A)') ''
    
    ! Core Enhanced Scatter API Tests (should FAIL)
    call test_enhanced_scatter_basic_api()
    call test_scatter_size_mapping()
    call test_scatter_color_mapping()
    call test_scatter_combined_size_color_mapping()
    
    ! Advanced Marker System Tests (should FAIL)  
    call test_marker_shape_system()
    call test_marker_edge_face_colors()
    call test_marker_transparency()
    
    ! Colorbar Integration Tests (should FAIL)
    call test_colorbar_automatic_generation()
    call test_colorbar_positioning()
    
    write(error_unit, '(A,I0,A,I0,A)') 'RED PHASE COMPLETE: ', failed_count, ' of ', test_count, ' tests failed (EXPECTED)'
    write(error_unit, '(A)') 'Tests are ready to drive GREEN phase implementation'
    
contains

    subroutine test_enhanced_scatter_basic_api()
        !! Given: A figure with enhanced scatter API
        !! When: I call scatter() with basic parameters  
        !! Then: Enhanced scatter plot should be created with proper defaults
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]
        
        call fig%initialize(400, 300)
        
        ! This should use enhanced scatter API (will FAIL - not implemented)
        call fig%scatter(x, y, marker='circle', label='Basic Scatter')
        
        error stop 'Enhanced scatter API not implemented'
    end subroutine test_enhanced_scatter_basic_api
    
    subroutine test_scatter_size_mapping()
        !! Given: Data points with size values for bubble chart
        !! When: I create scatter plot with size mapping
        !! Then: Markers should have variable sizes based on data
        
        type(figure_t) :: fig
        real(wp) :: x(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: y(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: sizes(4) = [10.0_wp, 50.0_wp, 100.0_wp, 25.0_wp]
        
        call fig%initialize(400, 300)
        
        ! This should create bubble chart with variable marker sizes (will FAIL)
        call fig%scatter(x, y, s=sizes, label='Bubble Chart')
        
        error stop 'Size mapping not implemented'
    end subroutine test_scatter_size_mapping
    
    subroutine test_scatter_color_mapping()
        !! Given: Data points with color values for visualization
        !! When: I create scatter plot with color mapping
        !! Then: Markers should have colors based on colormap
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: colors(5) = [0.1_wp, 0.3_wp, 0.7_wp, 0.9_wp, 0.5_wp]
        
        call fig%initialize(400, 300)
        
        ! This should create color-mapped scatter plot (will FAIL)
        call fig%scatter(x, y, c=colors, colormap='viridis', label='Color Mapped')
        
        error stop 'Color mapping not implemented'
    end subroutine test_scatter_color_mapping
    
    subroutine test_scatter_combined_size_color_mapping()
        !! Given: Data with both size and color values
        !! When: I create scatter plot with both mappings
        !! Then: Markers should vary in both size and color
        
        type(figure_t) :: fig
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: sizes(3) = [20.0_wp, 60.0_wp, 40.0_wp]
        real(wp) :: colors(3) = [0.2_wp, 0.8_wp, 0.5_wp]
        
        call fig%initialize(400, 300)
        
        ! This should create combined size/color mapping (will FAIL)
        call fig%scatter(x, y, s=sizes, c=colors, colormap='plasma', &
                        label='Combined Mapping')
        
        error stop 'Combined size/color mapping not implemented'
    end subroutine test_scatter_combined_size_color_mapping

    subroutine test_marker_shape_system()
        !! Given: Different marker shape specifications
        !! When: I create scatter plots with various markers
        !! Then: Each plot should use the correct marker geometry
        
        type(figure_t) :: fig
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        
        call fig%initialize(600, 400)
        
        ! Test comprehensive marker shapes (will FAIL)
        call fig%scatter(x, y, marker='circle', label='Circles')
        call fig%scatter(x+0.5_wp, y, marker='square', label='Squares')
        call fig%scatter(x, y+0.5_wp, marker='triangle', label='Triangles')
        call fig%scatter(x+0.5_wp, y+0.5_wp, marker='diamond', label='Diamonds')
        call fig%scatter(x+1.0_wp, y, marker='star', label='Stars')
        
        error stop 'Advanced marker shapes not implemented'
    end subroutine test_marker_shape_system
    
    subroutine test_marker_edge_face_colors()
        !! Given: Scatter plot with custom edge and face colors
        !! When: I specify edge and face colors separately
        !! Then: Markers should render with correct edge/face styling
        
        type(figure_t) :: fig
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: edge_color(3) = [1.0_wp, 0.0_wp, 0.0_wp]  ! Red edges
        real(wp) :: face_color(3) = [0.0_wp, 0.0_wp, 1.0_wp]  ! Blue faces
        
        call fig%initialize(400, 300)
        
        ! This should create markers with custom edge/face colors (will FAIL)
        call fig%scatter(x, y, marker='circle', &
                        edgecolor=edge_color, facecolor=face_color, &
                        linewidth=2.0_wp, label='Custom Colors')
        
        error stop 'Edge/face color control not implemented'
    end subroutine test_marker_edge_face_colors
    
    subroutine test_marker_transparency()
        !! Given: Scatter plot with transparency setting
        !! When: I specify alpha value for markers
        !! Then: Markers should render with correct transparency
        
        type(figure_t) :: fig
        real(wp) :: x(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: y(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        
        call fig%initialize(400, 300)
        
        ! This should create semi-transparent markers (will FAIL)
        call fig%scatter(x, y, marker='circle', alpha=0.5_wp, label='Transparent')
        
        error stop 'Marker transparency not implemented'
    end subroutine test_marker_transparency

    subroutine test_colorbar_automatic_generation()
        !! Given: Scatter plot with color mapping
        !! When: Colorbar is enabled (default)
        !! Then: Colorbar should be automatically generated
        
        type(figure_t) :: fig
        real(wp) :: x(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: y(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: colors(4) = [0.0_wp, 0.33_wp, 0.67_wp, 1.0_wp]
        
        call fig%initialize(500, 400)
        
        ! This should automatically generate colorbar (will FAIL)
        call fig%scatter(x, y, c=colors, colormap='viridis', label='Auto Colorbar')
        
        error stop 'Automatic colorbar generation not implemented'
    end subroutine test_colorbar_automatic_generation
    
    subroutine test_colorbar_positioning()
        !! Given: Scatter plot with color mapping
        !! When: I specify colorbar position
        !! Then: Colorbar should appear in specified location
        
        type(figure_t) :: fig
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: colors(3) = [0.2_wp, 0.5_wp, 0.8_wp]
        
        call fig%initialize(400, 500)
        
        ! This should position colorbar at bottom (will FAIL)
        call fig%scatter(x, y, c=colors, colormap='plasma', label='Bottom Colorbar')
        call fig%add_colorbar(position='bottom', label='Color Values')
        
        error stop 'Colorbar positioning not implemented'
    end subroutine test_colorbar_positioning

end program test_scatter_enhanced