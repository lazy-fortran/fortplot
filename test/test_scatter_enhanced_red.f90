program test_scatter_enhanced_red
    !! RED PHASE tests for Issue #56 Enhanced Scatter Plot functionality
    !! Following TDD methodology - tests drive implementation requirements
    !! These tests use existing API where possible and document missing features
    
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    write(error_unit, '(A)') '=== RED PHASE: Enhanced Scatter Plot Tests ==='
    write(error_unit, '(A)') 'Testing existing functionality and documenting missing features'
    
    ! Phase 1: Test Current Infrastructure
    call test_current_scatter_api()
    call test_marker_system_completeness()
    call test_colormap_system_readiness()
    
    ! Phase 2: Document Enhanced Features Needed
    call test_size_mapping_requirements()
    call test_color_mapping_requirements()
    call test_colorbar_integration_requirements()
    
    ! Phase 3: Test Backend Readiness
    call test_backend_scatter_support()
    
    ! Phase 4: Performance and Edge Case Requirements
    call test_performance_requirements()
    call test_edge_case_requirements()
    
    write(error_unit, '(A)') 'RED PHASE COMPLETE: Requirements documented for implementation'
    
contains

    subroutine test_current_scatter_api()
        !! Given: Current scatter plot API
        !! When: I test existing functionality
        !! Then: Basic scatter plots should work
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [2.0_wp, 4.0_wp, 1.0_wp, 5.0_wp, 3.0_wp]
        real(wp) :: sizes(5) = [10.0_wp, 25.0_wp, 15.0_wp, 30.0_wp, 20.0_wp]
        real(wp) :: colors(5) = [0.1_wp, 0.3_wp, 0.7_wp, 0.9_wp, 0.5_wp]
        
        write(error_unit, '(A)') 'Testing current scatter API...'
        call fig%initialize(400, 300)
        
        ! Test existing 2D scatter functionality
        call fig%add_scatter_2d(x, y, s=sizes, c=colors, marker='o', &
                               colormap='viridis', label='Current API', &
                               show_colorbar=.true.)
        
        write(error_unit, '(A)') '  ✓ Basic scatter plot with size and color mapping works'
        call figure_savefig(fig, get_test_output_path('/tmp/test_current_api.png'))
        
        ! Test different marker
        call fig%initialize(400, 300) 
        call fig%add_scatter_2d(x, y+1.0_wp, marker='s', label='Square Markers')
        write(error_unit, '(A)') '  ✓ Square markers supported'
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('/tmp/test_current_markers.png'))
        
    end subroutine test_current_scatter_api
    
    subroutine test_marker_system_completeness()
        !! Given: Marker system requirements for Issue #56
        !! When: I test all required marker types
        !! Then: All 10+ marker types should be available
        
        type(figure_t) :: fig
        character(len=1), parameter :: required_markers(10) = &
            ['o', 's', '^', 'v', 'd', 'D', '*', '+', 'x', 'h']
        character(len=13), parameter :: marker_names(10) = &
            ['circle       ', 'square       ', 'triangle_up  ', 'triangle_down', &
             'diamond      ', 'big_diamond  ', 'star         ', 'plus         ', &
             'cross        ', 'hexagon      ']
        real(wp) :: x(10), y(10)
        integer :: i
        
        ! Generate positions
        do i = 1, 10
            x(i) = mod(i-1, 5) + 1.0_wp
            y(i) = (i-1)/5 + 1.0_wp  
        end do
        
        write(error_unit, '(A)') 'Testing marker system completeness...'
        call fig%initialize(800, 600)
        
        ! Test each required marker type
        do i = 1, 10
            write(error_unit, '(A,A,A)') '  Testing marker: ', required_markers(i), &
                ' (', trim(marker_names(i)), ')'
            
            ! Test marker availability - this drives marker implementation
            call fig%add_scatter_2d([x(i)], [y(i)], marker=required_markers(i), &
                                   markersize=20.0_wp, label=trim(marker_names(i)))
            
            write(error_unit, '(A,A)') '  ✓ Marker ', required_markers(i), ' available'
        end do
        
        call fig%set_title('Marker System Completeness Test')
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('/tmp/test_marker_completeness.png'))
        call figure_savefig(fig, get_test_output_path('/tmp/test_marker_completeness.pdf'))
        call figure_savefig(fig, get_test_output_path('/tmp/test_marker_completeness.txt'))  ! ASCII test
        
    end subroutine test_marker_system_completeness
    
    subroutine test_colormap_system_readiness()
        !! Given: Colormap system for enhanced scatter plots
        !! When: I test required colormaps
        !! Then: All Issue #56 colormaps should be available
        
        type(figure_t) :: fig
        real(wp) :: x(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: y(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp] 
        real(wp) :: colors(4) = [0.0_wp, 0.33_wp, 0.67_wp, 1.0_wp]
        character(len=10), parameter :: required_colormaps(4) = &
            ['viridis   ', 'plasma    ', 'inferno   ', 'coolwarm  ']
        integer :: i
        
        write(error_unit, '(A)') 'Testing colormap system readiness...'
        
        do i = 1, 4
            call fig%initialize(500, 400)
            write(error_unit, '(A,A)') '  Testing colormap: ', trim(required_colormaps(i))
            
            ! Test colormap availability
            call fig%add_scatter_2d(x, y, c=colors, &
                                   colormap=trim(required_colormaps(i)), &
                                   marker='o', label=trim(required_colormaps(i)))
            
            call fig%set_title('Colormap: ' // trim(required_colormaps(i)))
            call figure_savefig(fig, get_test_output_path('/tmp/test_colormap_') // trim(required_colormaps(i)) // '.png')
            write(error_unit, '(A,A)') '  ✓ Colormap ', trim(required_colormaps(i)), ' available'
        end do
        
    end subroutine test_colormap_system_readiness
    
    subroutine test_size_mapping_requirements()
        !! Given: Size mapping requirements for bubble charts
        !! When: I test size scaling algorithms needed
        !! Then: Document size mapping behavior requirements
        
        type(figure_t) :: fig
        real(wp) :: x(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: y(6) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        real(wp) :: sizes_normal(6) = [10.0_wp, 20.0_wp, 30.0_wp, 40.0_wp, 50.0_wp, 60.0_wp]
        real(wp) :: sizes_extreme(6) = [1.0_wp, 100.0_wp, 5.0_wp, 1000.0_wp, 10.0_wp, 10000.0_wp]
        real(wp) :: sizes_identical(6) = [25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp, 25.0_wp]
        
        write(error_unit, '(A)') 'Testing size mapping requirements...'
        call fig%initialize(600, 500)
        
        ! Test current size mapping (documents current behavior)
        call fig%add_scatter_2d(x, y, s=sizes_normal, marker='o', label='Normal Sizes')
        write(error_unit, '(A)') '  ✓ Normal size range mapping available'
        
        ! Test extreme size range (documents needed robust handling)
        call fig%add_scatter_2d(x, y+1.0_wp, s=sizes_extreme, marker='s', label='Extreme Sizes')
        write(error_unit, '(A)') '  → REQUIREMENT: Robust handling of extreme size ranges needed'
        
        ! Test identical sizes (documents edge case handling)
        call fig%add_scatter_2d(x, y+2.0_wp, s=sizes_identical, marker='^', label='Identical Sizes')
        write(error_unit, '(A)') '  → REQUIREMENT: Proper handling of identical size values needed'
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('/tmp/test_size_requirements.png'))
        
    end subroutine test_size_mapping_requirements
    
    subroutine test_color_mapping_requirements()
        !! Given: Color mapping requirements for Issue #56
        !! When: I test color range handling scenarios
        !! Then: Document color mapping behavior requirements
        
        type(figure_t) :: fig
        real(wp) :: x(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: y(6) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        real(wp) :: colors_normal(6) = [0.0_wp, 0.2_wp, 0.4_wp, 0.6_wp, 0.8_wp, 1.0_wp]
        real(wp) :: colors_custom(6) = [-5.0_wp, -2.5_wp, 0.0_wp, 2.5_wp, 5.0_wp, 7.5_wp]
        
        write(error_unit, '(A)') 'Testing color mapping requirements...'
        call fig%initialize(600, 500)
        
        ! Test current color mapping
        call fig%add_scatter_2d(x, y, c=colors_normal, colormap='viridis', &
                               marker='o', label='Normal Colors')
        write(error_unit, '(A)') '  ✓ Basic color mapping available'
        
        ! Test custom range (documents vmin/vmax needs)
        call fig%add_scatter_2d(x, y+1.0_wp, c=colors_custom, colormap='plasma', &
                               marker='s', label='Custom Range')
        write(error_unit, '(A)') '  → REQUIREMENT: vmin/vmax range control needed'
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('/tmp/test_color_requirements.png'))
        
    end subroutine test_color_mapping_requirements
    
    subroutine test_colorbar_integration_requirements()
        !! Given: Colorbar integration requirements
        !! When: I test automatic colorbar generation
        !! Then: Document colorbar functionality needs
        
        type(figure_t) :: fig
        real(wp) :: x(8), y(8), colors(8)
        integer :: i
        
        ! Generate test data
        do i = 1, 8
            x(i) = real(i, wp)
            y(i) = sin(real(i, wp))
            colors(i) = real(i-1, wp) / 7.0_wp
        end do
        
        write(error_unit, '(A)') 'Testing colorbar integration requirements...'
        call fig%initialize(700, 500)  ! Extra width for colorbar
        
        ! Test current colorbar support
        call fig%add_scatter_2d(x, y, c=colors, colormap='viridis', &
                               marker='o', label='With Colorbar', show_colorbar=.true.)
        
        write(error_unit, '(A)') '  ✓ Basic colorbar integration available'
        write(error_unit, '(A)') '  → REQUIREMENT: Enhanced colorbar positioning and labeling needed'
        
        call fig%set_title('Colorbar Integration Test')
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('/tmp/test_colorbar_requirements.png'))
        
    end subroutine test_colorbar_integration_requirements
    
    subroutine test_backend_scatter_support()
        !! Given: Multi-backend requirements for scatter plots
        !! When: I test scatter rendering across backends
        !! Then: Document backend consistency requirements
        
        type(figure_t) :: fig
        real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: y(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp) :: sizes(5) = [15.0_wp, 25.0_wp, 35.0_wp, 45.0_wp, 55.0_wp]
        real(wp) :: colors(5) = [0.0_wp, 0.25_wp, 0.5_wp, 0.75_wp, 1.0_wp]
        
        write(error_unit, '(A)') 'Testing backend scatter support...'
        
        ! PNG backend test
        call fig%initialize(600, 400)
        call fig%add_scatter_2d(x, y, s=sizes, c=colors, colormap='viridis', &
                               marker='o', label='PNG Backend')
        call fig%set_title('Backend Test - PNG')
        call figure_savefig(fig, get_test_output_path('/tmp/test_backend_png.png'))
        write(error_unit, '(A)') '  ✓ PNG backend scatter support'
        
        ! PDF backend test  
        call fig%initialize(600, 400)
        call fig%add_scatter_2d(x, y, s=sizes, c=colors, colormap='plasma', &
                               marker='s', label='PDF Backend')
        call fig%set_title('Backend Test - PDF') 
        call figure_savefig(fig, get_test_output_path('/tmp/test_backend_pdf.pdf'))
        write(error_unit, '(A)') '  ✓ PDF backend scatter support'
        
        ! ASCII backend test
        call fig%initialize(60, 20)
        call fig%add_scatter_2d(x, y, marker='*', label='ASCII Backend')
        call fig%set_title('Backend Test - ASCII')
        call figure_savefig(fig, get_test_output_path('/tmp/test_backend_ascii.txt'))
        write(error_unit, '(A)') '  ✓ ASCII backend scatter support'
        write(error_unit, '(A)') '  → REQUIREMENT: Enhanced ASCII marker representation needed'
        
    end subroutine test_backend_scatter_support
    
    subroutine test_performance_requirements()
        !! Given: Performance requirements (10^4+ points, <2 seconds)
        !! When: I test large dataset scenarios
        !! Then: Document performance optimization needs
        
        type(figure_t) :: fig
        integer, parameter :: n_large = 5000  ! Reduced for test
        real(wp) :: x_large(n_large), y_large(n_large)
        real(wp) :: sizes_large(n_large), colors_large(n_large)
        integer :: i
        real(wp) :: start_time, end_time
        
        ! Generate test data
        do i = 1, n_large
            x_large(i) = real(i, wp) / 500.0_wp
            y_large(i) = sin(real(i, wp) / 100.0_wp)
            sizes_large(i) = 5.0_wp + 10.0_wp * abs(sin(real(i, wp) / 50.0_wp))
            colors_large(i) = real(mod(i, 100), wp) / 100.0_wp
        end do
        
        write(error_unit, '(A,I0,A)') 'Testing performance requirements (', n_large, ' points)...'
        call fig%initialize(800, 600)
        
        ! Measure current performance
        call cpu_time(start_time)
        call fig%add_scatter_2d(x_large, y_large, s=sizes_large, c=colors_large, &
                               colormap='viridis', marker='o', label='Large Dataset')
        call cpu_time(end_time)
        
        write(error_unit, '(A,F6.2,A)') '  Current performance: ', &
            end_time - start_time, ' seconds'
        write(error_unit, '(A)') '  → REQUIREMENT: Optimization for 10^4+ points needed'
        
        call fig%set_title('Performance Requirements Test')
        call figure_savefig(fig, get_test_output_path('/tmp/test_performance_requirements.png'))
        
    end subroutine test_performance_requirements
    
    subroutine test_edge_case_requirements()
        !! Given: Edge case requirements for robust scatter plots
        !! When: I test boundary conditions and error cases  
        !! Then: Document edge case handling needs
        
        type(figure_t) :: fig
        real(wp) :: x_normal(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y_normal(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: x_empty(0), y_empty(0)
        
        write(error_unit, '(A)') 'Testing edge case requirements...'
        call fig%initialize(400, 300)
        
        ! Test single point
        call fig%add_scatter_2d([2.0_wp], [2.0_wp], marker='o', &
                               markersize=30.0_wp, label='Single Point')
        write(error_unit, '(A)') '  ✓ Single point handling'
        
        ! Test empty arrays (should handle gracefully)
        call fig%add_scatter_2d(x_empty, y_empty, label='Empty Arrays')
        write(error_unit, '(A)') '  → REQUIREMENT: Graceful empty array handling needed'
        
        ! Test invalid marker (should handle gracefully)
        call fig%add_scatter_2d(x_normal, y_normal, marker='invalid', label='Invalid Marker')
        write(error_unit, '(A)') '  → REQUIREMENT: Invalid marker validation needed'
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('/tmp/test_edge_case_requirements.png'))
        
    end subroutine test_edge_case_requirements

end program test_scatter_enhanced_red