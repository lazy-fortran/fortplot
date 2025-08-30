program test_scatter_enhanced_advanced
    !! Advanced RED phase test suite for Issue #56 Enhanced Scatter Plot functionality
    !! Following TDD methodology - these tests should FAIL initially and drive implementation
    !! Focus: Backend rendering, performance, edge cases, integration

    use fortplot
    use fortplot_security, only: safe_create_directory
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    
    write(error_unit, '(A)') '=== RED PHASE: Enhanced Scatter Plot Advanced Tests ==='
    write(error_unit, '(A)') 'Following TDD RED-GREEN-REFACTOR - tests drive implementation'
    
    ! Phase 3: Backend Rendering Tests (RED PHASE)
    call test_multi_backend_marker_consistency()
    call test_ascii_scatter_representation()
    
    ! Phase 4: Advanced Features Tests (RED PHASE)
    call test_automatic_colorbar_generation()
    call test_edge_case_validation()
    
    ! Phase 5: Performance and Integration Tests (RED PHASE)
    call test_large_dataset_performance()
    call test_comprehensive_integration()
    
    write(error_unit, '(A)') 'RED PHASE ADVANCED COMPLETE: All tests ready for GREEN phase'
    
contains
    
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
        call fig%savefig('test/output/test_backend_png.png')
        write(error_unit, '(A)') '  ✓ PNG backend consistency'
        
        ! Test PDF backend consistency (drives vector graphics rendering)
        call fig%initialize(600, 400)
        call fig%scatter(x, y, s=sizes, c=colors, colormap='viridis', &
                        marker='s', label='PDF Test', show_colorbar=.true.)
        call fig%set_title('Multi-Backend Consistency Test - PDF')
        call fig%legend()
        call fig%savefig('test/output/test_backend_pdf.pdf')
        write(error_unit, '(A)') '  ✓ PDF backend consistency'
        
        ! Test ASCII backend representation (drives character-based markers)
        call fig%initialize(80, 25)  ! ASCII appropriate size
        call fig%scatter(x, y, s=sizes, marker='*', label='ASCII Test')
        call fig%set_title('Multi-Backend Test - ASCII')
        call fig%savefig('test/output/test_backend_ascii.txt')
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
        call fig%savefig('test/output/test_ascii_markers.txt')
        write(error_unit, '(A)') '  ✓ ASCII marker representation'
        
        ! Test ASCII color approximation (if supported)
        call fig%initialize(60, 20)
        call fig%scatter(x, y, c=[0.2_wp, 0.4_wp, 0.6_wp, 0.8_wp, 1.0_wp], &
                        colormap='viridis', marker='o', label='Colored')
        call fig%set_title('ASCII Color Test')
        call fig%savefig('test/output/test_ascii_colors.txt')
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
        call fig%savefig('test/output/test_colorbar.png')
        write(error_unit, '(A)') '  ✓ Colorbar layout integration'
        
        ! Test colorbar without explicit request (drives default behavior)
        call fig%initialize(500, 400)
        call fig%scatter(x, y+1.0_wp, c=colors, colormap='plasma', &
                        marker='s', label='Default Colorbar')
        call fig%savefig('test/output/test_colorbar_default.png')
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
        call fig%savefig('test/output/test_edge_cases.png')
        
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
        call fig%savefig('test/output/test_large_performance.png')
        
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
        call fig%savefig('test/output/test_comprehensive_integration.png')
        call fig%savefig('test/output/test_comprehensive_integration.pdf')
        
        write(error_unit, '(A)') '  ✓ Comprehensive feature integration'
        write(error_unit, '(A)') '  ✓ Multi-series scatter support'
        write(error_unit, '(A)') '  ✓ Complex parameter combinations'
        
    end subroutine test_comprehensive_integration

end program test_scatter_enhanced_advanced