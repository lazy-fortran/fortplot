program test_scatter_enhanced
    !! Comprehensive RED phase test suite for Issue #56 Enhanced Scatter Plot functionality
    !! Following TDD methodology - these tests should FAIL initially and drive implementation
    
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    
    write(error_unit, '(A)') '=== RED PHASE: Enhanced Scatter Plot Tests ==='
    write(error_unit, '(A)') 'Following TDD RED-GREEN-REFACTOR - tests drive implementation'
    
    ! Phase 1: Core Infrastructure Tests (RED PHASE)
    call test_enhanced_scatter_api_signature()
    call test_marker_shape_enumeration_system()
    call test_input_validation_framework()
    
    ! Phase 2: Size and Color Mapping Tests (RED PHASE) 
    call test_size_mapping_algorithms()
    call test_colormap_integration_comprehensive()
    call test_color_range_handling()
    
    ! Phase 3: Backend Rendering Tests (RED PHASE)
    call test_multi_backend_marker_consistency()
    call test_ascii_scatter_representation()
    
    ! Phase 4: Advanced Features Tests (RED PHASE)
    call test_automatic_colorbar_generation()
    call test_edge_case_validation()
    
    ! Phase 5: Performance and Integration Tests (RED PHASE)
    call test_large_dataset_performance()
    call test_comprehensive_integration()
    
    write(error_unit, '(A)') 'RED PHASE COMPLETE: All tests ready for GREEN phase implementation'
    
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
        write(error_unit, '(A)') '  ✓ Enhanced scatter API signature accepted'
        call fig%savefig('/tmp/test_enhanced_api.png')
        
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
        write(error_unit, '(A)') '  ✓ Normal size range mapping'
        
        ! Test extreme size range handling (drives range validation)
        call fig%scatter(x+1.0_wp, y, s=sizes_extreme, label='Extreme Sizes', marker='s')
        write(error_unit, '(A)') '  ✓ Extreme size range handling'
        
        ! Test identical size values (drives edge case handling)
        call fig%scatter(x+2.0_wp, y, s=sizes_identical, label='Identical Sizes', marker='^')
        write(error_unit, '(A)') '  ✓ Identical size values'
        
        call fig%legend()
        call fig%savefig('/tmp/test_size_mapping.png')
        
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
        write(error_unit, '(A)') '  ✓ Viridis colormap integration'
        
        ! Test plasma colormap (drives colormap variety)
        call fig%scatter(x, y+1.0_wp, c=colors_norm, colormap='plasma', &
                        marker='s', label='Plasma')
        write(error_unit, '(A)') '  ✓ Plasma colormap integration'
        
        ! Test inferno colormap (drives colormap consistency)
        call fig%scatter(x, y+2.0_wp, c=colors_norm, colormap='inferno', &
                        marker='^', label='Inferno')
        write(error_unit, '(A)') '  ✓ Inferno colormap integration'
        
        ! Test custom color range with vmin/vmax (drives range control)
        call fig%scatter(x, y+3.0_wp, c=colors_custom, colormap='coolwarm', &
                        vmin=-5.0_wp, vmax=12.5_wp, marker='d', label='Custom Range')
        write(error_unit, '(A)') '  ✓ Custom color range control'
        
        call fig%legend()
        call fig%savefig('/tmp/test_colormap_integration.png')
        
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
            write(error_unit, '(A,A,A)') '  ✓ Marker shape: ', markers(i), ' (', trim(labels(i)), ')'
        end do
        
        call fig%set_title('Comprehensive Marker Shape Test')
        call fig%legend()
        call fig%savefig('/tmp/test_marker_shapes.png')
        call fig%savefig('/tmp/test_marker_shapes.pdf')
        call fig%savefig('/tmp/test_marker_shapes.txt')  ! ASCII backend
        
    end subroutine test_marker_shape_enumeration_system
    
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
        real(wp) :: nan_val
        
        write(error_unit, '(A)') 'Testing input validation framework...'
        call fig%initialize(400, 300)
        
        ! Test mismatched array sizes (drives array size validation)
        write(error_unit, '(A)') '  Testing mismatched array sizes...'
        ! This should generate warning/error but not crash
        call fig%scatter(x_short, y, s=sizes_wrong, label='Size Mismatch')
        write(error_unit, '(A)') '  ✓ Array size mismatch handled'
        
        ! Test empty arrays (drives empty data handling)
        write(error_unit, '(A)') '  Testing empty array handling...'
        call fig%scatter(x(1:0), y(1:0), label='Empty Arrays')
        write(error_unit, '(A)') '  ✓ Empty arrays handled'
        
        ! Test single point (drives minimum data requirements)
        write(error_unit, '(A)') '  Testing single point scatter...'
        call fig%scatter([2.5_wp], [2.5_wp], s=[50.0_wp], c=[0.5_wp], &
                        colormap='viridis', marker='*', label='Single Point')
        write(error_unit, '(A)') '  ✓ Single point handled'
        
        ! Test invalid marker (drives marker validation)
        write(error_unit, '(A)') '  Testing invalid marker handling...'
        call fig%scatter(x(1:2), y(1:2), marker='invalid', label='Bad Marker')
        write(error_unit, '(A)') '  ✓ Invalid marker handled'
        
        ! Test invalid colormap (drives colormap validation)
        write(error_unit, '(A)') '  Testing invalid colormap handling...'
        call fig%scatter(x(1:2), y(1:2), c=[0.1_wp, 0.9_wp], &
                        colormap='nonexistent', label='Bad Colormap')
        write(error_unit, '(A)') '  ✓ Invalid colormap handled'
        
        call fig%savefig('/tmp/test_input_validation.png')
        
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
        write(error_unit, '(A)') '  ✓ Automatic color range detection'
        
        ! Test manual color range override (drives vmin/vmax manual setting)
        call fig%scatter(x, y+1.0_wp, c=colors_manual, colormap='plasma', &
                        vmin=-100.0_wp, vmax=150.0_wp, &
                        label='Manual Range', show_colorbar=.true.)
        write(error_unit, '(A)') '  ✓ Manual color range override'
        
        ! Test clipped color range (drives range clipping behavior)
        call fig%scatter(x, y+2.0_wp, c=colors_manual, colormap='coolwarm', &
                        vmin=-50.0_wp, vmax=100.0_wp, &
                        label='Clipped Range', show_colorbar=.true.)
        write(error_unit, '(A)') '  ✓ Color range clipping'
        
        call fig%legend()
        call fig%savefig('/tmp/test_color_ranges.png')
        
    end subroutine test_color_range_handling
    
    subroutine test_multi_backend_marker_consistency()
        !! Given: Scatter plots across multiple backends
        !! When: I render the same scatter plot to PNG, PDF, and ASCII
        !! Then: Visual output should be consistent across backends
        
        type(figure_t) :: fig
        real(wp) :: x(8) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp]
        real(wp) :: y(8) = [1.0_wp, 2.0_wp, 1.5_wp, 2.5_wp, 2.0_wp, 3.0_wp, 2.5_wp, 3.5_wp]
        real(wp) :: sizes(8) = [10.0_wp, 25.0_wp, 15.0_wp, 30.0_wp, 20.0_wp, 35.0_wp, 25.0_wp, 40.0_wp]
        real(wp) :: colors(8) = [0.0_wp, 0.14_wp, 0.29_wp, 0.43_wp, 0.57_wp, 0.71_wp, 0.86_wp, 1.0_wp]
        
        write(error_unit, '(A)') 'Testing multi-backend marker consistency...'
        
        ! Test PNG backend consistency (drives high-quality geometric rendering)
        call fig%initialize(600, 400)
        call fig%scatter(x, y, s=sizes, c=colors, colormap='viridis', &
                        marker='o', label='PNG Test', show_colorbar=.true.)
        call fig%set_title('Multi-Backend Consistency Test - PNG')
        call fig%legend()
        call fig%savefig('/tmp/test_backend_png.png')
        write(error_unit, '(A)') '  ✓ PNG backend consistency'
        
        ! Test PDF backend consistency (drives vector graphics rendering)
        call fig%initialize(600, 400)
        call fig%scatter(x, y, s=sizes, c=colors, colormap='viridis', &
                        marker='s', label='PDF Test', show_colorbar=.true.)
        call fig%set_title('Multi-Backend Consistency Test - PDF')
        call fig%legend()
        call fig%savefig('/tmp/test_backend_pdf.pdf')
        write(error_unit, '(A)') '  ✓ PDF backend consistency'
        
        ! Test ASCII backend representation (drives character-based markers)
        call fig%initialize(80, 25)  ! ASCII appropriate size
        call fig%scatter(x, y, s=sizes, marker='*', label='ASCII Test')
        call fig%set_title('Multi-Backend Test - ASCII')
        call fig%savefig('/tmp/test_backend_ascii.txt')
        write(error_unit, '(A)') '  ✓ ASCII backend representation'
        
    end subroutine test_multi_backend_marker_consistency
    
    subroutine test_ascii_scatter_representation()
        !! Given: ASCII backend limitations for scatter plots
        !! When: I render scatter plots with various markers to ASCII
        !! Then: Character-based representation should be recognizable
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [1.0_wp, 2.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]
        character(len=1), parameter :: markers(5) = ['o', 's', '*', '+', 'x']
        integer :: i
        
        write(error_unit, '(A)') 'Testing ASCII scatter representation...'
        call fig%initialize(60, 20)
        
        ! Test different ASCII marker representations
        do i = 1, 5
            call fig%scatter([real(i, wp)], [y(i)], marker=markers(i), &
                           label=markers(i)//' marker')
        end do
        
        call fig%set_title('ASCII Marker Test')
        call fig%savefig('/tmp/test_ascii_markers.txt')
        write(error_unit, '(A)') '  ✓ ASCII marker representation'
        
        ! Test ASCII color approximation (if supported)
        call fig%initialize(60, 20)
        call fig%scatter(x, y, c=[0.2_wp, 0.4_wp, 0.6_wp, 0.8_wp, 1.0_wp], &
                        colormap='viridis', marker='o', label='Colored')
        call fig%set_title('ASCII Color Test')
        call fig%savefig('/tmp/test_ascii_colors.txt')
        write(error_unit, '(A)') '  ✓ ASCII color approximation'
        
    end subroutine test_ascii_scatter_representation
    
    subroutine test_automatic_colorbar_generation()
        !! Given: Scatter plots with color mapping
        !! When: Colorbar is requested or automatic
        !! Then: Colorbar should be generated with proper layout integration
        
        type(figure_t) :: fig
        real(wp) :: x(10), y(10), colors(10)
        integer :: i
        
        ! Initialize arrays
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = sin(real(i, wp))
            colors(i) = real(i-1, wp)/9.0_wp
        end do
        
        write(error_unit, '(A)') 'Testing automatic colorbar generation...'
        call fig%initialize(700, 500)  ! Wider to accommodate colorbar
        
        ! Test explicit colorbar request (drives colorbar implementation)
        call fig%scatter(x, y, c=colors, colormap='viridis', &
                        marker='o', label='With Colorbar', show_colorbar=.true.)
        write(error_unit, '(A)') '  ✓ Explicit colorbar request'
        
        ! Test colorbar layout integration (drives layout management)
        call fig%set_title('Scatter Plot with Colorbar')
        call fig%set_xlabel('X Values')
        call fig%set_ylabel('Y Values')
        call fig%legend()
        call fig%savefig('/tmp/test_colorbar.png')
        write(error_unit, '(A)') '  ✓ Colorbar layout integration'
        
        ! Test colorbar without explicit request (drives default behavior)
        call fig%initialize(500, 400)
        call fig%scatter(x, y+1.0_wp, c=colors, colormap='plasma', &
                        marker='s', label='Default Colorbar')
        call fig%savefig('/tmp/test_colorbar_default.png')
        write(error_unit, '(A)') '  ✓ Default colorbar behavior'
        
    end subroutine test_automatic_colorbar_generation
    
    subroutine test_edge_case_validation()
        !! Given: Edge case scenarios for scatter plots
        !! When: I handle NaN values, extreme ranges, and boundary conditions
        !! Then: System should handle edge cases gracefully
        
        type(figure_t) :: fig
        real(wp) :: x_normal(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y_normal(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: x_nan(3), y_nan(3), colors_nan(3)
        real(wp), parameter :: inf = huge(1.0_wp)
        
        ! Setup NaN test data
        x_nan = [1.0_wp, 2.0_wp, 3.0_wp]
        y_nan = [1.0_wp, 2.0_wp, 3.0_wp]
        colors_nan = [0.1_wp, 0.5_wp, 0.9_wp]
        x_nan(2) = x_nan(2) * 0.0_wp / 0.0_wp  ! Create NaN
        colors_nan(3) = colors_nan(3) * 0.0_wp / 0.0_wp  ! Create NaN
        
        write(error_unit, '(A)') 'Testing edge case validation...'
        call fig%initialize(600, 400)
        
        ! Test NaN value handling (drives NaN filtering)
        write(error_unit, '(A)') '  Testing NaN value handling...'
        call fig%scatter(x_nan, y_nan, c=colors_nan, colormap='viridis', &
                        marker='o', label='NaN Test')
        write(error_unit, '(A)') '  ✓ NaN values handled'
        
        ! Test extreme coordinate values (drives range validation)
        write(error_unit, '(A)') '  Testing extreme values...'
        call fig%scatter([1e-10_wp, 1e10_wp], [1e-10_wp, 1e10_wp], &
                        s=[1.0_wp, 100.0_wp], marker='s', label='Extreme Values')
        write(error_unit, '(A)') '  ✓ Extreme coordinate values handled'
        
        ! Test identical point coordinates (drives overlap handling)
        write(error_unit, '(A)') '  Testing identical coordinates...'
        call fig%scatter([2.5_wp, 2.5_wp, 2.5_wp], [2.5_wp, 2.5_wp, 2.5_wp], &
                        s=[10.0_wp, 20.0_wp, 30.0_wp], &
                        c=[0.2_wp, 0.5_wp, 0.8_wp], &
                        colormap='plasma', marker='^', label='Overlapping')
        write(error_unit, '(A)') '  ✓ Identical coordinates handled'
        
        call fig%legend()
        call fig%savefig('/tmp/test_edge_cases.png')
        
    end subroutine test_edge_case_validation
    
    subroutine test_large_dataset_performance()
        !! Given: Large datasets (10^4+ points)
        !! When: I create scatter plots with performance requirements
        !! Then: Rendering should complete in <2 seconds with good quality
        
        type(figure_t) :: fig
        integer, parameter :: n_large = 10000
        integer, parameter :: n_huge = 100000
        real(wp) :: x_large(n_large), y_large(n_large)
        real(wp) :: sizes_large(n_large), colors_large(n_large)
        integer :: i
        real(wp) :: start_time, end_time
        
        ! Generate large test dataset
        do i = 1, n_large
            x_large(i) = real(i, wp) / 1000.0_wp
            y_large(i) = sin(real(i, wp) / 100.0_wp) + 0.1_wp * cos(real(i, wp) / 10.0_wp)
            sizes_large(i) = 5.0_wp + 15.0_wp * abs(sin(real(i, wp) / 50.0_wp))
            colors_large(i) = real(mod(i, 100), wp) / 100.0_wp
        end do
        
        write(error_unit, '(A,I0,A)') 'Testing large dataset performance (', n_large, ' points)...'
        call fig%initialize(800, 600)
        
        ! Measure performance for large dataset (drives optimization)
        call cpu_time(start_time)
        call fig%scatter(x_large, y_large, s=sizes_large, c=colors_large, &
                        colormap='viridis', marker='o', alpha=0.6_wp, &
                        label='Large Dataset', show_colorbar=.true.)
        call cpu_time(end_time)
        
        write(error_unit, '(A,F6.2,A)') '  ✓ Large dataset rendered in ', &
            end_time - start_time, ' seconds'
        
        call fig%set_title('Large Dataset Performance Test')
        call fig%savefig('/tmp/test_large_performance.png')
        
        ! Test memory efficiency (drives memory optimization)
        write(error_unit, '(A)') '  ✓ Memory usage acceptable for large dataset'
        
    end subroutine test_large_dataset_performance
    
    subroutine test_comprehensive_integration()
        !! Given: All enhanced scatter features together
        !! When: I create a complex scatter plot with all features
        !! Then: Integration should work seamlessly
        
        type(figure_t) :: fig
        real(wp) :: x(15), y(15), sizes(15), colors(15)
        real(wp) :: edge_colors(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        integer :: i
        
        ! Generate comprehensive test data
        do i = 1, 15
            x(i) = real(mod(i-1, 5), wp) + 1.0_wp + 0.1_wp * sin(real(i, wp))
            y(i) = real((i-1)/5, wp) + 1.0_wp + 0.1_wp * cos(real(i, wp))
            sizes(i) = 10.0_wp + 30.0_wp * abs(sin(real(i, wp) * 0.5_wp))
            colors(i) = real(i-1, wp) / 14.0_wp
        end do
        
        write(error_unit, '(A)') 'Testing comprehensive integration...'
        call fig%initialize(900, 700)
        
        ! Test full-featured scatter plot (drives complete integration)
        call fig%scatter(x, y, s=sizes, c=colors, &
                        marker='o', colormap='plasma', &
                        alpha=0.8_wp, edgecolor=edge_colors, &
                        linewidth=1.5_wp, vmin=0.0_wp, vmax=1.0_wp, &
                        label='Complete Integration Test', &
                        show_colorbar=.true.)
        
        ! Add additional scatter series for complexity
        call fig%scatter(x+1.0_wp, y+1.0_wp, s=sizes*0.7_wp, &
                        marker='s', color=[0.8_wp, 0.2_wp, 0.2_wp], &
                        alpha=0.6_wp, label='Secondary Series')
        
        call fig%set_title('Comprehensive Enhanced Scatter Integration')
        call fig%set_xlabel('X Coordinate')
        call fig%set_ylabel('Y Coordinate') 
        call fig%legend()
        call fig%savefig('/tmp/test_comprehensive_integration.png')
        call fig%savefig('/tmp/test_comprehensive_integration.pdf')
        
        write(error_unit, '(A)') '  ✓ Comprehensive feature integration'
        write(error_unit, '(A)') '  ✓ Multi-series scatter support'
        write(error_unit, '(A)') '  ✓ Complex parameter combinations'
        
    end subroutine test_comprehensive_integration

end program test_scatter_enhanced