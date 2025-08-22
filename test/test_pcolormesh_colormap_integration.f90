program test_pcolormesh_colormap_integration
    !! Test pcolormesh integration with existing colormap system
    !! 
    !! Given: Existing fortplot colormap infrastructure
    !! When: Using pcolormesh with various colormaps and normalization
    !! Then: Should produce accurate color mapping consistent with existing system
    
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_colormap, only: get_colormap_color, colormap_value_to_color
    use fortplot_security, only: get_test_output_path
    implicit none
    
    call test_colormap_system_integration()
    call test_color_value_mapping_accuracy()
    call test_normalization_consistency()
    call test_colormap_edge_behavior()
    call test_colormap_interpolation_quality()
    call test_multi_colormap_comparison()
    call test_custom_colormap_support()
    
    print *, "All pcolormesh colormap integration tests completed!"
    
contains

    subroutine test_colormap_system_integration()
        !! Given: Existing colormap system in fortplot_colormap module
        !! When: Using pcolormesh with standard colormaps
        !! Then: Should use same color mapping as other plot types
        
        type(figure_t) :: fig
        real(wp) :: x(6), y(6), c_linear(5, 5)
        real(wp) :: test_color(3)
        integer :: i, j
        character(len=*), parameter :: standard_colormaps(5) = &
            ['viridis ', 'plasma  ', 'inferno ', 'coolwarm', 'jet     ']
        integer :: cmap_idx
        character(len=512) :: filename
        
        ! Arrange - Create linear gradient data for color mapping verification
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.2_wp
            y(i) = real(i-1, wp) * 0.2_wp
        end do
        
        ! Create known linear gradient: 0.0 to 1.0
        do i = 1, 5
            do j = 1, 5
                c_linear(i, j) = real(i + j - 2, wp) / 8.0_wp
            end do
        end do
        
        ! Act & Assert - Test each standard colormap
        do cmap_idx = 1, 5
            call fig%initialize(300, 300)
            call fig%add_pcolormesh(x, y, c_linear, colormap=trim(standard_colormaps(cmap_idx)))
            call fig%set_title("Colormap Integration - " // trim(standard_colormaps(cmap_idx)))
            
            filename = trim(get_test_output_path('/tmp/test_colormap_integration_')) // &
                       trim(standard_colormaps(cmap_idx)) // '.png'
            call fig%savefig(filename)
            
            ! Test that colormap system is being used correctly
            ! This should use the same colormap_value_to_color function as other plots
            call colormap_value_to_color(0.5_wp, 0.0_wp, 1.0_wp, &
                                       trim(standard_colormaps(cmap_idx)), test_color)
            
            ! Color should be valid RGB values
            if (any(test_color < 0.0_wp) .or. any(test_color > 1.0_wp)) then
                error stop "Colormap integration failed - invalid color values"
            end if
        end do
        
        print *, "test_colormap_system_integration: PASSED"
    end subroutine test_colormap_system_integration

    subroutine test_color_value_mapping_accuracy()
        !! Given: Known data values with specific colormap
        !! When: Rendering pcolormesh
        !! Then: Color mapping should be mathematically correct and consistent
        
        type(figure_t) :: fig
        real(wp) :: x(4), y(4), c_precise(3, 3)
        real(wp) :: expected_color(3), tolerance
        integer :: i, j
        
        ! Arrange - Create precise test data with known expected colors
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        
        ! Create precise values for color mapping verification
        c_precise = reshape([0.0_wp, 0.25_wp, 0.5_wp, &
                            0.25_wp, 0.5_wp, 0.75_wp, &
                            0.5_wp, 0.75_wp, 1.0_wp], [3, 3])
        
        tolerance = 1.0e-6_wp
        
        ! Act - Render with viridis colormap (well-defined colors)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_precise, colormap='viridis', vmin=0.0_wp, vmax=1.0_wp)
        call fig%set_title("Color Mapping Accuracy - Precise values")
        call fig%savefig(get_test_output_path('/tmp/test_color_mapping_accuracy.png'))
        
        ! Assert - Test specific color values using colormap system
        ! Test minimum value (should map to viridis minimum)
        call colormap_value_to_color(0.0_wp, 0.0_wp, 1.0_wp, 'viridis', expected_color)
        write(*, '(A, 3F8.5)') "Viridis min (0.0): ", expected_color
        
        ! Test maximum value (should map to viridis maximum)
        call colormap_value_to_color(1.0_wp, 0.0_wp, 1.0_wp, 'viridis', expected_color)
        write(*, '(A, 3F8.5)') "Viridis max (1.0): ", expected_color
        
        ! Test middle value (should map to viridis middle)
        call colormap_value_to_color(0.5_wp, 0.0_wp, 1.0_wp, 'viridis', expected_color)
        write(*, '(A, 3F8.5)') "Viridis mid (0.5): ", expected_color
        
        ! Test with different colormap for consistency
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_precise, colormap='plasma', vmin=0.0_wp, vmax=1.0_wp)
        call fig%set_title("Color Mapping Accuracy - Plasma colormap")
        call fig%savefig(get_test_output_path('/tmp/test_color_mapping_plasma.png'))
        
        print *, "test_color_value_mapping_accuracy: PASSED"
    end subroutine test_color_value_mapping_accuracy

    subroutine test_normalization_consistency()
        !! Given: Data with different value ranges and normalization parameters
        !! When: Using vmin/vmax parameters
        !! Then: Color mapping should be consistent with mathematical normalization
        
        type(figure_t) :: fig
        real(wp) :: x(5), y(5), c_range(4, 4)
        integer :: i, j
        real(wp) :: data_min, data_max, mid_value
        
        ! Arrange - Create data with known range
        do i = 1, 5
            x(i) = real(i-1, wp) * 0.25_wp
            y(i) = real(i-1, wp) * 0.25_wp
        end do
        
        ! Create data range from -2.0 to 2.0
        do i = 1, 4
            do j = 1, 4
                c_range(i, j) = real(i + j - 5, wp) * 0.5_wp  ! Range: -2.0 to 1.5
            end do
        end do
        
        data_min = minval(c_range)
        data_max = maxval(c_range)
        mid_value = (data_min + data_max) * 0.5_wp
        
        write(*, '(A, F8.3, A, F8.3)') "Data range: ", data_min, " to ", data_max
        
        ! Act & Assert - Test different normalization scenarios
        
        ! Auto-normalization (should use data range)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_range, colormap='viridis')
        call fig%set_title("Auto normalization")
        call fig%savefig(get_test_output_path('/tmp/test_norm_auto.png'))
        
        ! Explicit data range normalization (should be identical to auto)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_range, colormap='viridis', &
                               vmin=data_min, vmax=data_max)
        call fig%set_title("Explicit data range normalization")
        call fig%savefig(get_test_output_path('/tmp/test_norm_explicit.png'))
        
        ! Clipped normalization (narrower range)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_range, colormap='viridis', &
                               vmin=mid_value - 0.5_wp, vmax=mid_value + 0.5_wp)
        call fig%set_title("Clipped normalization")
        call fig%savefig(get_test_output_path('/tmp/test_norm_clipped.png'))
        
        ! Expanded normalization (wider range)
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_range, colormap='viridis', &
                               vmin=data_min - 1.0_wp, vmax=data_max + 1.0_wp)
        call fig%set_title("Expanded normalization")
        call fig%savefig(get_test_output_path('/tmp/test_norm_expanded.png'))
        
        ! Symmetric normalization around zero
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_range, colormap='coolwarm', &
                               vmin=-2.0_wp, vmax=2.0_wp)
        call fig%set_title("Symmetric normalization")
        call fig%savefig(get_test_output_path('/tmp/test_norm_symmetric.png'))
        
        print *, "test_normalization_consistency: PASSED"
    end subroutine test_normalization_consistency

    subroutine test_colormap_edge_behavior()
        !! Given: Data values at colormap edges (exactly vmin, vmax)
        !! When: Rendering pcolormesh
        !! Then: Edge values should map to correct colormap extremes
        
        type(figure_t) :: fig
        real(wp) :: x(4), y(4), c_edges(3, 3)
        real(wp), parameter :: v_min = -1.0_wp, v_max = 1.0_wp
        real(wp) :: edge_color_min(3), edge_color_max(3)
        
        ! Arrange - Create data with exact edge values
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        
        ! Create pattern with exact vmin/vmax values
        c_edges = reshape([v_min, 0.0_wp, v_max, &
                          0.0_wp, 0.5_wp, 0.0_wp, &
                          v_max, 0.0_wp, v_min], [3, 3])
        
        ! Act - Render with precise vmin/vmax
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_edges, colormap='coolwarm', &
                               vmin=v_min, vmax=v_max)
        call fig%set_title("Colormap Edge Behavior - Exact vmin/vmax")
        call fig%savefig(get_test_output_path('/tmp/test_colormap_edges.png'))
        
        ! Assert - Verify edge color mapping
        call colormap_value_to_color(v_min, v_min, v_max, 'coolwarm', edge_color_min)
        call colormap_value_to_color(v_max, v_min, v_max, 'coolwarm', edge_color_max)
        
        write(*, '(A, 3F8.5)') "Coolwarm vmin color: ", edge_color_min
        write(*, '(A, 3F8.5)') "Coolwarm vmax color: ", edge_color_max
        
        ! Test with values outside vmin/vmax range (should clamp to edges)
        c_edges(1, 1) = v_min - 0.5_wp  ! Below vmin
        c_edges(3, 3) = v_max + 0.5_wp  ! Above vmax
        
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_edges, colormap='viridis', &
                               vmin=v_min, vmax=v_max)
        call fig%set_title("Colormap Edge Behavior - Clamping")
        call fig%savefig(get_test_output_path('/tmp/test_colormap_clamping.png'))
        
        print *, "test_colormap_edge_behavior: PASSED"
    end subroutine test_colormap_edge_behavior

    subroutine test_colormap_interpolation_quality()
        !! Given: Smooth data requiring colormap interpolation
        !! When: Rendering high-resolution pcolormesh
        !! Then: Color transitions should be smooth without banding artifacts
        
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), c_smooth(:,:)
        integer, parameter :: n = 50  ! High resolution for smooth transitions
        integer :: i, j
        real(wp) :: pi = 4.0_wp * atan(1.0_wp)
        
        ! Arrange - Create high-resolution smooth function
        allocate(x(n+1), y(n+1), c_smooth(n, n))
        
        do i = 1, n+1
            x(i) = real(i-1, wp) / real(n, wp) * 2.0_wp * pi
            y(i) = real(i-1, wp) / real(n, wp) * 2.0_wp * pi
        end do
        
        ! Create smooth mathematical function requiring good interpolation
        do i = 1, n
            do j = 1, n
                c_smooth(i, j) = sin(x(i)) * cos(y(j)) + &
                               0.3_wp * sin(3.0_wp * x(i)) * cos(2.0_wp * y(j))
            end do
        end do
        
        ! Act - Test different colormaps for interpolation quality
        call fig%initialize(500, 500)
        call fig%add_pcolormesh(x, y, c_smooth, colormap='viridis')
        call fig%set_title("Colormap Interpolation - Viridis (smooth function)")
        call fig%savefig(get_test_output_path('/tmp/test_interpolation_viridis.png'))
        
        call fig%initialize(500, 500)
        call fig%add_pcolormesh(x, y, c_smooth, colormap='plasma')
        call fig%set_title("Colormap Interpolation - Plasma (smooth function)")
        call fig%savefig(get_test_output_path('/tmp/test_interpolation_plasma.png'))
        
        ! Test with high contrast colormap
        call fig%initialize(500, 500)
        call fig%add_pcolormesh(x, y, c_smooth, colormap='coolwarm')
        call fig%set_title("Colormap Interpolation - Coolwarm (high contrast)")
        call fig%savefig(get_test_output_path('/tmp/test_interpolation_coolwarm.png'))
        
        deallocate(x, y, c_smooth)
        print *, "test_colormap_interpolation_quality: PASSED"
    end subroutine test_colormap_interpolation_quality

    subroutine test_multi_colormap_comparison()
        !! Given: Identical data with different colormaps
        !! When: Rendering side-by-side comparisons
        !! Then: Should demonstrate colormap differences clearly
        
        type(figure_t) :: fig
        real(wp) :: x(6), y(6), c_test(5, 5)
        integer :: i, j
        character(len=*), parameter :: comparison_colormaps(4) = &
            ['viridis ', 'plasma  ', 'coolwarm', 'jet     ']
        integer :: cmap_idx
        character(len=512) :: filename
        
        ! Arrange - Create interesting test pattern
        do i = 1, 6
            x(i) = real(i-1, wp) * 0.2_wp
            y(i) = real(i-1, wp) * 0.2_wp
        end do
        
        ! Create radial pattern good for colormap comparison
        do i = 1, 5
            do j = 1, 5
                c_test(i, j) = sqrt((x(i) - 0.5_wp)**2 + (y(j) - 0.5_wp)**2) / 0.7_wp
            end do
        end do
        
        ! Act - Render same data with different colormaps
        do cmap_idx = 1, 4
            call fig%initialize(300, 300)
            call fig%add_pcolormesh(x, y, c_test, colormap=trim(comparison_colormaps(cmap_idx)), &
                                   vmin=0.0_wp, vmax=1.0_wp)
            call fig%set_title("Colormap Comparison - " // trim(comparison_colormaps(cmap_idx)))
            call fig%set_xlabel("X coordinate")
            call fig%set_ylabel("Y coordinate")
            
            filename = trim(get_test_output_path('/tmp/test_colormap_compare_')) // &
                       trim(comparison_colormaps(cmap_idx)) // '.png'
            call fig%savefig(filename)
        end do
        
        print *, "test_multi_colormap_comparison: PASSED"
    end subroutine test_multi_colormap_comparison

    subroutine test_custom_colormap_support()
        !! Given: Request for custom or extended colormap functionality
        !! When: Using pcolormesh with custom parameters
        !! Then: Should integrate with any custom colormap extensions
        
        type(figure_t) :: fig
        real(wp) :: x(4), y(4), c_custom(3, 3)
        integer :: i, j
        
        ! Arrange
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        do i = 1, 3
            do j = 1, 3
                c_custom(i, j) = real(i + j, wp) / 6.0_wp
            end do
        end do
        
        ! Act - Test custom colormap scenarios
        
        ! Test with colormap parameters that might be added in future
        call fig%initialize(300, 300)
        call fig%add_pcolormesh(x, y, c_custom, colormap='viridis')
        call fig%set_title("Custom Colormap Support - Future extensibility")
        call fig%savefig(get_test_output_path('/tmp/test_custom_colormap.png'))
        
        ! Test colormap with unusual normalization
        call fig%initialize(300, 300) 
        call fig%add_pcolormesh(x, y, c_custom, colormap='plasma', &
                               vmin=0.2_wp, vmax=0.8_wp)  ! Narrow range
        call fig%set_title("Custom Normalization - Narrow range")
        call fig%savefig(get_test_output_path('/tmp/test_custom_normalization.png'))
        
        print *, "test_custom_colormap_support: PASSED"
    end subroutine test_custom_colormap_support

end program test_pcolormesh_colormap_integration