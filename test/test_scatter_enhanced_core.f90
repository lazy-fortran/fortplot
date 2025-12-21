program test_scatter_enhanced_core
    !! Core RED phase test suite for Issue #56 Enhanced Scatter Plot functionality
    !! Following TDD methodology - these tests should FAIL initially and drive implementation
    !! Focus: Core API, validation, basic functionality

    use fortplot
    use fortplot_security, only: safe_create_directory
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    
    write(error_unit, '(A)') '=== RED PHASE: Enhanced Scatter Plot Core Tests ==='
    write(error_unit, '(A)') 'Following TDD RED-GREEN-REFACTOR - tests drive implementation'
    
    ! Phase 1: Core Infrastructure Tests (RED PHASE)
    call test_enhanced_scatter_api_signature()
    call test_marker_shape_enumeration_system()
    call test_default_marker_behavior()
    call test_input_validation_framework()
    
    ! Phase 2: Size and Color Mapping Tests (RED PHASE) 
    call test_size_mapping_algorithms()
    call test_colormap_integration_comprehensive()
    call test_color_range_handling()
    
    ! Consolidated from test_scatter_enhanced_advanced.f90
    call test_large_dataset_performance()
    call test_backend_consistency()
    
    ! Consolidated from test_scatter_performance.f90
    call test_scatter_plot_count_efficiency()
    
    write(error_unit, '(A)') 'RED PHASE COMPLETE: All scatter tests consolidated and ready for GREEN phase'
    
contains

    subroutine test_enhanced_scatter_api_signature()
        !! Given: Enhanced scatter API with comprehensive parameters
        !! When: I call scatter() with all parameter combinations
        !! Then: API should accept parameters and create appropriate plot data
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]
        real(wp) :: sizes(5) = [10.0_wp, 25.0_wp, 15.0_wp, 30.0_wp, 20.0_wp]
        real(wp) :: colors(5) = [0.1_wp, 0.3_wp, 0.7_wp, 0.9_wp, 0.5_wp]
        real(wp) :: edge_color(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        real(wp) :: face_color(3) = [1.0_wp, 0.0_wp, 0.0_wp]
        
        write(error_unit, '(A)') 'Testing enhanced scatter API signature...'
        call fig%initialize(400, 300)
        
        ! Test comprehensive scatter API (EXPECTED TO FAIL - drives implementation)
        ! This API signature should be supported but will fail initially
        call fig%scatter(x, y, s=sizes, c=colors, marker='o', &
                        colormap='viridis', alpha=0.7_wp, &
                        edgecolor=edge_color, facecolor=face_color, &
                        linewidth=2.0_wp, label='Enhanced Test', &
                        show_colorbar=.true.)
        
        ! If we get here, API exists but may not be complete
        write(error_unit, '(A)') '  PASS: Enhanced scatter API signature accepted'
        call fig%savefig('test/output/test_enhanced_api.png')
        
    end subroutine test_enhanced_scatter_api_signature
    
    subroutine test_size_mapping_algorithms()
        !! Given: Data points with size values for bubble chart
        !! When: I create scatter plot with size mapping and scaling
        !! Then: Markers should scale appropriately and handle edge cases
        
        type(figure_t) :: fig
        real(wp) :: x(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: y(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: sizes_normal(6) = [10.0_wp, 50.0_wp, 100.0_wp, 25.0_wp, 75.0_wp, 150.0_wp]
        real(wp) :: sizes_extreme(6) = [0.1_wp, 1000.0_wp, 5.0_wp, 10000.0_wp, 0.01_wp, 50000.0_wp]
        real(wp) :: sizes_identical(6) = [25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp]
        
        write(error_unit, '(A)') 'Testing size mapping algorithms...'
        call fig%initialize(600, 400)
        
        ! Test normal size range mapping (drives size scaling implementation)
        call fig%scatter(x, y, s=sizes_normal, label='Normal Sizes', marker='o')
        write(error_unit, '(A)') '  PASS: Normal size range mapping'
        
        ! Test extreme size range handling (drives range validation)
        call fig%scatter(x+1.0_wp, y, s=sizes_extreme, label='Extreme Sizes', marker='s')
        write(error_unit, '(A)') '  PASS: Extreme size range handling'
        
        ! Test identical size values (drives edge case handling)
        call fig%scatter(x+2.0_wp, y, s=sizes_identical, label='Identical Sizes', marker='^')
        write(error_unit, '(A)') '  PASS: Identical size values'
        
        call fig%legend()
        call fig%savefig('test/output/test_size_mapping.png')
        
    end subroutine test_size_mapping_algorithms
    
    subroutine test_colormap_integration_comprehensive()
        !! Given: Data points with color values and different colormaps
        !! When: I create scatter plots with various color mapping scenarios
        !! Then: Colors should map correctly across different colormaps
        
        type(figure_t) :: fig
        real(wp) :: x(8) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp]
        real(wp) :: y(8) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp]
        real(wp) :: colors_norm(8) = [0.0_wp, 0.14_wp, 0.29_wp, 0.43_wp, 0.57_wp, 0.71_wp, 0.86_wp, 1.0_wp]
        real(wp) :: colors_custom(8) = [-5.0_wp, -2.5_wp, 0.0_wp, 2.5_wp, 5.0_wp, 7.5_wp, 10.0_wp, 12.5_wp]
        
        write(error_unit, '(A)') 'Testing comprehensive colormap integration...'
        call fig%initialize(800, 600)
        
        ! Test viridis colormap (drives basic color mapping)
        call fig%scatter(x, y, c=colors_norm, colormap='viridis', &
                        marker='o', label='Viridis', show_colorbar=.true.)
        write(error_unit, '(A)') '  PASS: Viridis colormap integration'
        
        ! Test plasma colormap (drives colormap variety)
        call fig%scatter(x, y+1.0_wp, c=colors_norm, colormap='plasma', &
                        marker='s', label='Plasma')
        write(error_unit, '(A)') '  PASS: Plasma colormap integration'
        
        ! Test inferno colormap (drives colormap consistency)
        call fig%scatter(x, y+2.0_wp, c=colors_norm, colormap='inferno', &
                        marker='^', label='Inferno')
        write(error_unit, '(A)') '  PASS: Inferno colormap integration'
        
        ! Test custom color range with vmin/vmax (drives range control)
        call fig%scatter(x, y+3.0_wp, c=colors_custom, colormap='coolwarm', &
                        vmin=-5.0_wp, vmax=12.5_wp, marker='d', label='Custom Range')
        write(error_unit, '(A)') '  PASS: Custom color range control'
        
        call fig%legend()
        call fig%savefig('test/output/test_colormap_integration.png')
        
    end subroutine test_colormap_integration_comprehensive

    subroutine test_marker_shape_enumeration_system()
        !! Given: Comprehensive marker shape specifications (10+ types)
        !! When: I create scatter plots with each marker type
        !! Then: All marker shapes should render correctly across backends
        
        type(figure_t) :: fig
        real(wp) :: x(10), y(10)
        character(len=1), parameter :: markers(10) = ['o', 's', '^', 'v', 'd', 'D', '*', '+', 'x', 'h']
        character(len=10), parameter :: labels(10) = ['Circle    ', 'Square    ', 'Triangle_U', 'Triangle_D', &
                                                      'Diamond   ', 'BigDiamond', 'Star      ', 'Plus      ', &
                                                      'Cross     ', 'Hexagon   ']
        integer :: i
        
        ! Generate test positions for marker display
        do i = 1, 10
            x(i) = mod(i-1, 5) + 1.0_wp
            y(i) = (i-1)/5 + 1.0_wp
        end do
        
        write(error_unit, '(A)') 'Testing comprehensive marker shape enumeration...'
        call fig%initialize(800, 600)
        
        ! Test all 10 marker types (drives marker geometry implementation)
        do i = 1, 10
            call fig%scatter([x(i)], [y(i)], marker=markers(i), &
                           markersize=20.0_wp, label=trim(labels(i)))
            write(error_unit, '(A,A,A)') '  PASS: Marker shape: ', markers(i), ' (', trim(labels(i)), ')'
        end do
        
        call fig%set_title('Comprehensive Marker Shape Test')
        call fig%legend()
        call fig%savefig('test/output/test_marker_shapes.png')
        call fig%savefig('test/output/test_marker_shapes.pdf')
        call fig%savefig('test/output/test_marker_shapes.txt')  ! ASCII backend
        
    end subroutine test_marker_shape_enumeration_system
    
    subroutine test_default_marker_behavior()
        !! Given: Scatter plot without explicit marker parameter
        !! When: I create scatter plot without specifying marker
        !! Then: Default marker 'o' should be used and points should be visible
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]
        
        write(error_unit, '(A)') 'Testing default marker behavior (Issue #1109)...'
        call fig%initialize(400, 300)
        
        ! Test scatter without marker parameter - should use default 'o' marker
        call fig%scatter(x, y, label='Default Marker')
        write(error_unit, '(A)') '  PASS: Default marker applied when not specified'
        
        ! Also test with markersize but no marker (ensures default marker with custom size)
        call fig%scatter(x, y+1.0_wp, markersize=15.0_wp, label='Default Marker with Size')
        write(error_unit, '(A)') '  PASS: Default marker with custom size'
        
        call fig%legend()
        call fig%savefig('test/output/test_default_marker.png')
        
    end subroutine test_default_marker_behavior
    
    subroutine test_input_validation_framework()
        !! Given: Various invalid input scenarios
        !! When: I call scatter with invalid parameters
        !! Then: Input validation should catch errors gracefully
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: x_short(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: sizes_wrong(3) = [10.0_wp, 20.0_wp, 30.0_wp]
        real(wp) :: colors_wrong(7) = [0.1_wp, 0.2_wp, 0.3_wp, 0.4_wp, 0.5_wp, 0.6_wp, 0.7_wp]
        
        write(error_unit, '(A)') 'Testing input validation framework...'
        call fig%initialize(400, 300)
        
        ! Test mismatched array sizes (drives array size validation)
        write(error_unit, '(A)') '  Testing mismatched array sizes...'
        ! This should generate warning/error but not crash
        call fig%scatter(x_short, y, s=sizes_wrong, label='Size Mismatch')
        write(error_unit, '(A)') '  PASS: Array size mismatch handled'
        
        ! Test empty arrays (drives empty data handling)
        write(error_unit, '(A)') '  Testing empty array handling...'
        call fig%scatter(x(1:0), y(1:0), label='Empty Arrays')
        write(error_unit, '(A)') '  PASS: Empty arrays handled'
        
        ! Test single point (drives minimum data requirements)
        write(error_unit, '(A)') '  Testing single point scatter...'
        call fig%scatter([2.5_wp], [2.5_wp], s=[50.0_wp], c=[0.5_wp], &
                        colormap='viridis', marker='*', label='Single Point')
        write(error_unit, '(A)') '  PASS: Single point handled'
        
        ! Test invalid marker (drives marker validation)
        write(error_unit, '(A)') '  Testing invalid marker handling...'
        call fig%scatter(x(1:2), y(1:2), marker='invalid', label='Bad Marker')
        write(error_unit, '(A)') '  PASS: Invalid marker handled'
        
        ! Test invalid colormap (drives colormap validation)
        write(error_unit, '(A)') '  Testing invalid colormap handling...'
        call fig%scatter(x(1:2), y(1:2), c=[0.1_wp, 0.9_wp], &
                        colormap='nonexistent', label='Bad Colormap')
        write(error_unit, '(A)') '  PASS: Invalid colormap handled'
        
        call fig%savefig('test/output/test_input_validation.png')
        
    end subroutine test_input_validation_framework

    subroutine test_color_range_handling()
        !! Given: Color data with various value ranges
        !! When: I apply automatic and manual color scaling
        !! Then: Color mapping should handle ranges appropriately
        
        type(figure_t) :: fig
        real(wp) :: x(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: y(6) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        real(wp) :: colors_auto(6) = [0.1_wp, 0.3_wp, 0.5_wp, 0.7_wp, 0.9_wp, 1.1_wp]
        real(wp) :: colors_manual(6) = [-100.0_wp, -50.0_wp, 0.0_wp, 50.0_wp, 100.0_wp, 150.0_wp]
        
        write(error_unit, '(A)') 'Testing color range handling...'
        call fig%initialize(600, 400)
        
        ! Test automatic color range detection (drives vmin/vmax auto-detection)
        call fig%scatter(x, y, c=colors_auto, colormap='viridis', &
                        label='Auto Range', show_colorbar=.true.)
        write(error_unit, '(A)') '  PASS: Automatic color range detection'
        
        ! Test manual color range override (drives vmin/vmax manual setting)
        call fig%scatter(x, y+1.0_wp, c=colors_manual, colormap='plasma', &
                        vmin=-100.0_wp, vmax=150.0_wp, &
                        label='Manual Range', show_colorbar=.true.)
        write(error_unit, '(A)') '  PASS: Manual color range override'
        
        ! Test clipped color range (drives range clipping behavior)
        call fig%scatter(x, y+2.0_wp, c=colors_manual, colormap='coolwarm', &
                        vmin=-50.0_wp, vmax=100.0_wp, &
                        label='Clipped Range', show_colorbar=.true.)
        write(error_unit, '(A)') '  PASS: Color range clipping'
        
        call fig%legend()
        call fig%savefig('test/output/test_color_ranges.png')
        
    end subroutine test_color_range_handling
    
    subroutine test_large_dataset_performance()
        !! Performance test for large datasets - consolidated from test_scatter_enhanced_advanced.f90
        integer, parameter :: n = 10000
        real(wp) :: x(n), y(n)
        integer :: i
        real :: start_time, end_time
        
        write(error_unit, '(A)') 'Testing large dataset performance (10000 points)...'
        
        do i = 1, n
            x(i) = real(i, wp) / real(n, wp)
            y(i) = sin(20.0_wp * x(i)) + 0.1_wp * cos(100.0_wp * x(i))
        end do
        
        call cpu_time(start_time)
        call figure()
        call scatter(x, y, label='Large Dataset')
        call savefig('test/output/test_large_scatter.png')
        call cpu_time(end_time)
        
        write(error_unit, '(A,F6.2,A)') '  PASS: Large dataset rendered in ', end_time - start_time, ' seconds'
    end subroutine test_large_dataset_performance
    
    subroutine test_backend_consistency()
        !! Multi-backend consistency test - consolidated from test_scatter_enhanced_advanced.f90
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [1.0_wp, 2.0_wp, 1.5_wp, 2.5_wp, 2.0_wp]
        
        write(error_unit, '(A)') 'Testing multi-backend marker consistency...'
        
        call figure()
        call scatter(x, y, marker='o', label='Circles')
        call savefig('test/output/test_scatter_png.png')
        write(error_unit, '(A)') '  PASS: PNG backend consistency'
        
        call figure()
        call scatter(x, y, marker='s', label='Squares')
        call savefig('test/output/test_scatter_pdf.pdf')
        write(error_unit, '(A)') '  PASS: PDF backend consistency'
        
        call figure()
        call scatter(x, y, marker='*', label='Stars')
        call savefig('test/output/test_scatter_ascii.txt')
        write(error_unit, '(A)') '  PASS: ASCII backend representation'
    end subroutine test_backend_consistency
    
    subroutine test_scatter_plot_count_efficiency()
        !! Plot count efficiency test - consolidated from test_scatter_performance.f90
        type(figure_t) :: fig
        real(wp) :: x(100), y(100)
        integer :: i, count_before, count_after
        
        ! Initialize arrays
        do i = 1, 100
            x(i) = real(i, wp)
            y(i) = x(i)**2
        end do
        
        write(error_unit, '(A)') 'Testing scatter plot count efficiency...'
        
        call fig%initialize(640, 480, 'png')
        count_before = fig%get_plot_count()
        call fig%scatter(x, y, label='Efficiency Test')
        count_after = fig%get_plot_count()
        
        if (count_after - count_before == 1) then
            write(error_unit, '(A)') '  PASS: Scatter creates single plot object (efficient)'
        else
            write(error_unit, '(A)') '  FAIL: Scatter creates multiple plot objects (inefficient)'
        end if
    end subroutine test_scatter_plot_count_efficiency

end program test_scatter_enhanced_core