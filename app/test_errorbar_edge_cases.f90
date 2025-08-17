program test_errorbar_edge_cases
    !! Test error bar edge cases and error handling from user perspective
    !! Tests invalid inputs, boundary conditions, and error reporting
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(5), y(5), yerr(5), xerr(5)
    real(wp) :: bad_size_err(3), negative_err(5)
    real(wp) :: huge_values(5), tiny_values(5), inf_values(5), nan_values(5)
    real(wp) :: x_empty(0), y_empty(0)
    integer :: i
    
    write(*,*) '=== Error Bar Edge Cases and Error Handling Tests ==='
    
    ! Setup basic data
    do i = 1, 5
        x(i) = real(i, wp)
        y(i) = real(i, wp) * 2.0_wp
        yerr(i) = 0.1_wp
        xerr(i) = 0.1_wp
    end do
    
    ! Test 1: Mismatched array sizes - expect error message
    write(*,*) 'Test 1: Testing mismatched array sizes (should show error)'
    bad_size_err = [0.1_wp, 0.2_wp, 0.3_wp]
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=bad_size_err)  ! Should fail gracefully
    write(*,*) '✓ Test 1 completed: Mismatched array size handling'
    
    ! Test 2: Empty arrays
    write(*,*) 'Test 2: Testing empty arrays (should handle gracefully)'
    call fig%initialize(640, 480)
    call fig%errorbar(x_empty, y_empty)  ! Should handle empty case
    write(*,*) '✓ Test 2 completed: Empty array handling'
    
    ! Test 3: Negative error values (mathematically valid, should work)
    write(*,*) 'Test 3: Testing negative error values'
    negative_err = [-0.1_wp, -0.2_wp, 0.1_wp, 0.2_wp, -0.3_wp]
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=negative_err, label='Negative errors')
    call fig%set_title('Test 3: Negative Error Values')
    call fig%legend()
    call fig%savefig('test_plots/edge_test3_negative_errors.png')
    write(*,*) '✓ Test 3 completed: Negative error values'
    
    ! Test 4: Very large error values
    write(*,*) 'Test 4: Testing very large error values'
    huge_values = [1e6_wp, 1e8_wp, 1e10_wp, 1e12_wp, 1e15_wp]
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=huge_values, label='Huge errors')
    call fig%set_title('Test 4: Very Large Error Values')
    call fig%legend()
    call fig%savefig('test_plots/edge_test4_huge_errors.png')
    write(*,*) '✓ Test 4 completed: Very large error values'
    
    ! Test 5: Very small error values (near machine precision)
    write(*,*) 'Test 5: Testing very small error values'
    tiny_values = [1e-15_wp, 1e-14_wp, 1e-13_wp, 1e-12_wp, 1e-11_wp]
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=tiny_values, label='Tiny errors')
    call fig%set_title('Test 5: Very Small Error Values')
    call fig%legend()
    call fig%savefig('test_plots/edge_test5_tiny_errors.png')
    write(*,*) '✓ Test 5 completed: Very small error values'
    
    ! Test 6: Conflicting parameters - symmetric and asymmetric
    write(*,*) 'Test 6: Testing conflicting symmetric and asymmetric parameters'
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=yerr, yerr_lower=yerr, yerr_upper=yerr)
    ! Should show warning about conflicting parameters
    call fig%set_title('Test 6: Conflicting Parameters')
    call fig%savefig('test_plots/edge_test6_conflicting.png')
    write(*,*) '✓ Test 6 completed: Conflicting parameter handling'
    
    ! Test 7: Single data point
    write(*,*) 'Test 7: Testing single data point'
    call fig%initialize(640, 480)
    call fig%errorbar([1.0_wp], [2.0_wp], yerr=[0.5_wp], label='Single point')
    call fig%set_title('Test 7: Single Data Point')
    call fig%legend()
    call fig%savefig('test_plots/edge_test7_single_point.png')
    write(*,*) '✓ Test 7 completed: Single data point'
    
    ! Test 8: Only one asymmetric parameter provided
    write(*,*) 'Test 8: Testing incomplete asymmetric parameters'
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr_lower=yerr)  ! Missing yerr_upper
    ! Should handle gracefully (probably ignore)
    call fig%set_title('Test 8: Incomplete Asymmetric Parameters')
    call fig%savefig('test_plots/edge_test8_incomplete_asym.png')
    write(*,*) '✓ Test 8 completed: Incomplete asymmetric parameters'
    
    ! Test 9: No error parameters provided
    write(*,*) 'Test 9: Testing no error parameters'
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, label='No errors')  ! Just points, no error bars
    call fig%set_title('Test 9: No Error Parameters')
    call fig%legend()
    call fig%savefig('test_plots/edge_test9_no_errors.png')
    write(*,*) '✓ Test 9 completed: No error parameters'
    
    ! Test 10: Extreme customization values
    write(*,*) 'Test 10: Testing extreme customization values'
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=yerr, &
                     capsize=100.0_wp, elinewidth=50.0_wp, &  ! Extreme values
                     label='Extreme styling')
    call fig%set_title('Test 10: Extreme Customization Values')
    call fig%legend()
    call fig%savefig('test_plots/edge_test10_extreme_custom.png')
    write(*,*) '✓ Test 10 completed: Extreme customization values'
    
    ! Test 11: Invalid color values
    write(*,*) 'Test 11: Testing invalid color values'
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=yerr, &
                     color=[-1.0_wp, 2.0_wp, 0.5_wp], &  ! Invalid RGB values
                     label='Invalid colors')
    call fig%set_title('Test 11: Invalid Color Values')
    call fig%legend()
    call fig%savefig('test_plots/edge_test11_invalid_colors.png')
    write(*,*) '✓ Test 11 completed: Invalid color values'
    
    ! Test 12: Very long labels and strings
    write(*,*) 'Test 12: Testing very long labels'
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=yerr, &
                     label='This is an extremely long label that might cause issues with layout and rendering in the legend area')
    call fig%set_title('Test 12: Very Long Labels and Strings That Might Cause Layout Issues')
    call fig%legend()
    call fig%savefig('test_plots/edge_test12_long_labels.png')
    write(*,*) '✓ Test 12 completed: Very long labels'
    
    ! Test 13: Invalid linestyle and marker values
    write(*,*) 'Test 13: Testing invalid linestyle and marker values'
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=yerr, &
                     linestyle='invalid_style', marker='invalid_marker', &
                     label='Invalid styles')
    call fig%set_title('Test 13: Invalid Linestyle and Marker Values')
    call fig%legend()
    call fig%savefig('test_plots/edge_test13_invalid_styles.png')
    write(*,*) '✓ Test 13 completed: Invalid linestyle and marker values'
    
    write(*,*) ''
    write(*,*) '=== All Edge Case Tests Completed! ==='
    write(*,*) 'Check test_plots/ directory for visual verification.'
    write(*,*) 'Review console output for error handling behavior.'
    
end program test_errorbar_edge_cases