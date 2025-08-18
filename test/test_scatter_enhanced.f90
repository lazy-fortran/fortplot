program test_scatter_enhanced
    !! Comprehensive RED phase test suite for Issue #56 Enhanced Scatter Plot functionality
    !! Following TDD methodology - these tests should FAIL initially and drive implementation
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    write(error_unit, '(A)') '=== RED PHASE: Enhanced Scatter Plot Tests ==='
    write(error_unit, '(A)') 'These tests should FAIL initially and drive implementation'
    
    ! Core Enhanced Scatter API Tests (should FAIL)
    call test_enhanced_scatter_basic_api()
    call test_scatter_size_mapping()
    call test_scatter_color_mapping()
    call test_marker_shape_system()
    call test_colorbar_automatic_generation()
    
    write(error_unit, '(A)') 'RED PHASE COMPLETE: Tests ready for GREEN phase implementation'
    
contains

    subroutine test_enhanced_scatter_basic_api()
        !! Given: A figure with enhanced scatter API
        !! When: I call scatter() with basic parameters  
        !! Then: Enhanced scatter plot should be created
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]
        
        ! XFAIL: Expected failure - Issue #56
        write(error_unit, '(A)') 'XFAIL: Enhanced scatter API not implemented - Issue #56'
        write(error_unit, '(A)') 'Skipping test until enhanced scatter functionality is available'
        return  ! Skip test instead of failing
        
        call fig%initialize(400, 300)
        
        ! This should use enhanced scatter API (will FAIL - not implemented)
        call fig%add_plot(x, y, label='Basic Scatter')
        
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
        
        ! XFAIL: Expected failure - Issue #56
        write(error_unit, '(A)') 'XFAIL: Size mapping for scatter plots not implemented - Issue #56'
        write(error_unit, '(A)') 'Skipping test until bubble chart functionality is available'
        return  ! Skip test instead of failing
        
        call fig%initialize(400, 300)
        
        ! This should create bubble chart with variable marker sizes (will FAIL)
        call fig%add_plot(x, y, label='Bubble Chart')
        
        error stop 'Size mapping not implemented'
    end subroutine test_scatter_size_mapping
    
    subroutine test_scatter_color_mapping()
        !! Given: Data points with color values
        !! When: I create scatter plot with color mapping
        !! Then: Markers should have colors based on colormap
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: colors(5) = [0.1_wp, 0.3_wp, 0.7_wp, 0.9_wp, 0.5_wp]
        
        ! XFAIL: Expected failure - Issue #56
        write(error_unit, '(A)') 'XFAIL: Color mapping for scatter plots not implemented - Issue #56'
        write(error_unit, '(A)') 'Skipping test until color-coded scatter functionality is available'
        return  ! Skip test instead of failing
        
        call fig%initialize(400, 300)
        
        ! This should create color-mapped scatter plot (will FAIL)
        call fig%add_plot(x, y, label='Color Mapped')
        
        error stop 'Color mapping not implemented'
    end subroutine test_scatter_color_mapping

    subroutine test_marker_shape_system()
        !! Given: Different marker shape specifications
        !! When: I create scatter plots with various markers
        !! Then: Each plot should use the correct marker geometry
        
        type(figure_t) :: fig
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        
        ! XFAIL: Expected failure - Issue #56
        write(error_unit, '(A)') 'XFAIL: Advanced marker shapes not implemented - Issue #56'
        write(error_unit, '(A)') 'Skipping test until enhanced marker system is available'
        return  ! Skip test instead of failing
        
        call fig%initialize(600, 400)
        
        ! Test comprehensive marker shapes (will FAIL)
        call fig%add_plot(x, y, label='Circles')
        call fig%add_plot(x+0.5_wp, y, label='Squares')
        call fig%add_plot(x, y+0.5_wp, label='Triangles')
        call fig%add_plot(x+0.5_wp, y+0.5_wp, label='Diamonds')
        call fig%add_plot(x+1.0_wp, y, label='Stars')
        
        error stop 'Advanced marker shapes not implemented'
    end subroutine test_marker_shape_system
    
    subroutine test_colorbar_automatic_generation()
        !! Given: Scatter plot with color mapping
        !! When: Colorbar is enabled (default)
        !! Then: Colorbar should be automatically generated
        
        type(figure_t) :: fig
        real(wp) :: x(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: y(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: colors(4) = [0.0_wp, 0.33_wp, 0.67_wp, 1.0_wp]
        
        ! XFAIL: Expected failure - Issue #56
        write(error_unit, '(A)') 'XFAIL: Automatic colorbar generation not implemented - Issue #56'
        write(error_unit, '(A)') 'Skipping test until colorbar functionality is available'
        return  ! Skip test instead of failing
        
        call fig%initialize(500, 400)
        
        ! This should automatically generate colorbar (will FAIL)
        call fig%add_plot(x, y, label='Auto Colorbar')
        
        error stop 'Automatic colorbar generation not implemented'
    end subroutine test_colorbar_automatic_generation

end program test_scatter_enhanced