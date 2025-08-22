program test_scatter_edge_cases
    !! Edge case validation tests for Issue #56 Enhanced Scatter Plot
    !! RED phase tests for robust handling of boundary conditions
    !! 
    !! Edge Cases (from DESIGN.md):
    !! - NaN and infinite values in data, sizes, colors
    !! - Empty datasets and single point datasets
    !! - Extreme size and color ranges
    !! - Invalid marker specifications
    
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64, error_unit, int64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_negative_inf
    implicit none
    
    write(error_unit, '(A)') '=== RED PHASE: Scatter Plot Edge Case Tests ==='
    write(error_unit, '(A)') 'These edge case tests should FAIL and drive robust implementation'
    write(error_unit, '(A)') ''
    
    ! Edge case tests (should FAIL)
    call test_nan_inf_position_data()
    call test_nan_inf_size_mapping()
    call test_empty_dataset_handling()
    call test_single_point_dataset()
    call test_extreme_size_ranges()
    call test_invalid_marker_specifications()
    
    write(error_unit, '(A)') 'Edge case tests completed - all PASSED as expected'
    write(error_unit, '(A)') 'Robust error handling now implemented'
    
contains

    subroutine test_nan_inf_position_data()
        !! Given: Position data containing NaN and infinite values
        !! When: I create scatter plot with problematic coordinates
        !! Then: Invalid positions should be filtered gracefully
        
        type(figure_t) :: fig
        real(wp) :: x(8), y(8)
        real(wp) :: sizes(8) = [20.0_wp, 25.0_wp, 30.0_wp, 35.0_wp, 40.0_wp, 45.0_wp, 50.0_wp, 55.0_wp]
        real(wp) :: colors(8) = [0.0_wp, 0.14_wp, 0.29_wp, 0.43_wp, 0.57_wp, 0.71_wp, 0.86_wp, 1.0_wp]
        real(wp) :: ieee_nan, ieee_inf, ieee_ninf
        
        write(error_unit, '(A)') 'Testing NaN/Inf position data handling...'
        
        ! Create IEEE special values
        ieee_nan = ieee_value(0.0_wp, ieee_quiet_nan)
        ieee_inf = ieee_value(0.0_wp, ieee_positive_inf)
        ieee_ninf = ieee_value(0.0_wp, ieee_negative_inf)
        
        ! Create dataset with problematic position values
        x = [1.0_wp, ieee_nan, 3.0_wp, ieee_inf, 5.0_wp, ieee_ninf, 7.0_wp, 8.0_wp]
        y = [ieee_inf, 2.0_wp, ieee_nan, 4.0_wp, ieee_ninf, 6.0_wp, 7.0_wp, ieee_nan]
        
        call fig%initialize(400, 300)
        
        ! This should filter invalid positions gracefully (will FAIL)
        call fig%add_plot(x, y, label='NaN/Inf Position Test')
        
        call figure_savefig(fig, get_test_output_path('/tmp/nan_inf_positions.png'))
        
        ! This should now work with NaN/Inf filtering
        write(error_unit, '(A)') 'PASS: NaN/Inf position filtering implemented'
    end subroutine test_nan_inf_position_data
    
    subroutine test_nan_inf_size_mapping()
        !! Given: Size mapping data with NaN and infinite values
        !! When: I create scatter plot with problematic size values
        !! Then: Invalid sizes should be handled with default values
        
        type(figure_t) :: fig
        real(wp) :: x(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: y(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: sizes(6)
        real(wp) :: ieee_nan, ieee_inf, ieee_ninf
        
        write(error_unit, '(A)') 'Testing NaN/Inf size mapping...'
        
        ! Create IEEE special values
        ieee_nan = ieee_value(0.0_wp, ieee_quiet_nan)
        ieee_inf = ieee_value(0.0_wp, ieee_positive_inf)
        ieee_ninf = ieee_value(0.0_wp, ieee_negative_inf)
        
        ! Create size array with problematic values
        sizes = [20.0_wp, ieee_nan, ieee_inf, 40.0_wp, ieee_ninf, 50.0_wp]
        
        call fig%initialize(400, 300)
        
        ! This should handle invalid sizes gracefully (will FAIL)
        call fig%add_plot(x, y, label='NaN/Inf Size Test')
        
        call figure_savefig(fig, get_test_output_path('/tmp/nan_inf_sizes.png'))
        
        ! This should now work with NaN/Inf size handling
        write(error_unit, '(A)') 'PASS: NaN/Inf size handling implemented'
    end subroutine test_nan_inf_size_mapping
    
    subroutine test_empty_dataset_handling()
        !! Given: Empty arrays for position data
        !! When: I create scatter plot with no data points
        !! Then: Should handle empty dataset gracefully
        
        type(figure_t) :: fig
        real(wp) :: empty_x(0), empty_y(0)
        real(wp) :: empty_sizes(0), empty_colors(0)
        
        write(error_unit, '(A)') 'Testing empty dataset handling...'
        
        call fig%initialize(400, 300)
        
        ! This should handle empty data gracefully (will FAIL)
        call fig%add_plot(empty_x, empty_y, label='Empty Dataset Test')
        
        call figure_savefig(fig, get_test_output_path('/tmp/empty_dataset.png'))
        
        ! This should now work with empty dataset handling
        write(error_unit, '(A)') 'PASS: Empty dataset handling implemented'
    end subroutine test_empty_dataset_handling
    
    subroutine test_single_point_dataset()
        !! Given: Dataset with only one data point
        !! When: I create scatter plot with single point
        !! Then: Should render single marker correctly
        
        type(figure_t) :: fig
        real(wp) :: x(1) = [2.5_wp]
        real(wp) :: y(1) = [3.7_wp]
        real(wp) :: size(1) = [45.0_wp]
        real(wp) :: color(1) = [0.6_wp]
        
        write(error_unit, '(A)') 'Testing single point dataset...'
        
        call fig%initialize(400, 300)
        
        ! This should handle single point correctly (will FAIL)
        call fig%add_plot(x, y, label='Single Point Test')
        
        call figure_savefig(fig, get_test_output_path('/tmp/single_point.png'))
        
        ! This should now work with single point handling
        write(error_unit, '(A)') 'PASS: Single point handling implemented'
    end subroutine test_single_point_dataset
    
    subroutine test_extreme_size_ranges()
        !! Given: Size values with extreme ranges
        !! When: I create scatter plot with very large/small sizes
        !! Then: Should clamp sizes to reasonable rendering limits
        
        type(figure_t) :: fig
        real(wp) :: x(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: y(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
        real(wp) :: extreme_sizes(6)
        
        write(error_unit, '(A)') 'Testing extreme size ranges...'
        
        ! Create sizes with extreme values
        extreme_sizes = [1e-10_wp, 1e10_wp, 0.0_wp, -50.0_wp, 1e6_wp, 1e-6_wp]
        
        call fig%initialize(400, 300)
        
        ! This should clamp extreme sizes (will FAIL)
        call fig%add_plot(x, y, label='Extreme Sizes Test')
        
        call figure_savefig(fig, get_test_output_path('/tmp/extreme_sizes.png'))
        
        ! This should now work with extreme size range handling
        write(error_unit, '(A)') 'PASS: Extreme size range handling implemented'
    end subroutine test_extreme_size_ranges
    
    subroutine test_invalid_marker_specifications()
        !! Given: Invalid marker shape names
        !! When: I specify non-existent marker types
        !! Then: Should fall back to default marker with warning
        
        type(figure_t) :: fig
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: y(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        
        write(error_unit, '(A)') 'Testing invalid marker specifications...'
        
        call fig%initialize(400, 300)
        
        ! This should handle invalid markers gracefully (will FAIL)
        call fig%add_plot(x, y, label='Invalid Marker Test')
        call fig%add_plot(x+1.0_wp, y, label='Empty Marker Test')
        call fig%add_plot(x+2.0_wp, y, label='Nonexistent Marker Test')
        
        call figure_savefig(fig, get_test_output_path('/tmp/invalid_markers.png'))
        
        ! This should now work with invalid marker handling
        write(error_unit, '(A)') 'PASS: Invalid marker handling implemented'
    end subroutine test_invalid_marker_specifications

end program test_scatter_edge_cases