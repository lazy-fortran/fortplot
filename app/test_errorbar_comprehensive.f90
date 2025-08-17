program test_errorbar_comprehensive
    !! Comprehensive user acceptance testing for error bar functionality
    !! Tests all documented API patterns and edge cases
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(10), y(10), yerr(10), xerr(10)
    real(wp) :: xerr_lower(5), xerr_upper(5), yerr_lower(5), yerr_upper(5)
    real(wp) :: small_err(3), zero_err(3), negative_err(3)
    real(wp) :: large_values(3), tiny_values(3)
    integer :: i
    logical :: test_passed
    
    write(*,*) '=== Comprehensive Error Bar User Acceptance Testing ==='
    
    test_passed = .true.
    
    ! Test 1: Basic symmetric Y error bars
    write(*,*) 'Test 1: Basic symmetric Y error bars'
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = sin(real(i, wp) * 0.5_wp) * 3.0_wp
        yerr(i) = 0.3_wp
    end do
    
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, yerr=yerr, label='Symmetric Y errors')
    call fig%set_title('Test 1: Symmetric Y Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test1_symmetric_y.png')
    write(*,*) '✓ Test 1 completed: Symmetric Y error bars'
    
    ! Test 2: Basic symmetric X error bars  
    write(*,*) 'Test 2: Basic symmetric X error bars'
    do i = 1, 10
        xerr(i) = 0.2_wp
    end do
    
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, xerr=xerr, label='Symmetric X errors')
    call fig%set_title('Test 2: Symmetric X Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test2_symmetric_x.png')
    write(*,*) '✓ Test 2 completed: Symmetric X error bars'
    
    ! Test 3: Combined X and Y error bars
    write(*,*) 'Test 3: Combined X and Y error bars'
    call fig%initialize(640, 480)
    call fig%errorbar(x, y, xerr=xerr, yerr=yerr, label='Combined X+Y errors')
    call fig%set_title('Test 3: Combined X and Y Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test3_combined_xy.png')
    write(*,*) '✓ Test 3 completed: Combined X and Y error bars'
    
    ! Test 4: Asymmetric Y error bars
    write(*,*) 'Test 4: Asymmetric Y error bars'
    do i = 1, 5
        yerr_lower(i) = 0.1_wp * real(i, wp)
        yerr_upper(i) = 0.3_wp * real(i, wp)
    end do
    
    call fig%initialize(640, 480)
    call fig%errorbar(x(1:5), y(1:5), yerr_lower=yerr_lower, yerr_upper=yerr_upper, &
                     label='Asymmetric Y errors')
    call fig%set_title('Test 4: Asymmetric Y Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test4_asymmetric_y.png')
    write(*,*) '✓ Test 4 completed: Asymmetric Y error bars'
    
    ! Test 5: Asymmetric X error bars
    write(*,*) 'Test 5: Asymmetric X error bars'
    do i = 1, 5
        xerr_lower(i) = 0.05_wp * real(i, wp)
        xerr_upper(i) = 0.15_wp * real(i, wp)
    end do
    
    call fig%initialize(640, 480)
    call fig%errorbar(x(1:5), y(1:5), xerr_lower=xerr_lower, xerr_upper=xerr_upper, &
                     label='Asymmetric X errors')
    call fig%set_title('Test 5: Asymmetric X Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test5_asymmetric_x.png')
    write(*,*) '✓ Test 5 completed: Asymmetric X error bars'
    
    ! Test 6: Combined asymmetric X and Y error bars
    write(*,*) 'Test 6: Combined asymmetric X and Y error bars'
    call fig%initialize(640, 480)
    call fig%errorbar(x(1:5), y(1:5), &
                     xerr_lower=xerr_lower, xerr_upper=xerr_upper, &
                     yerr_lower=yerr_lower, yerr_upper=yerr_upper, &
                     label='Asymmetric X+Y errors')
    call fig%set_title('Test 6: Combined Asymmetric X and Y Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test6_combined_asymmetric.png')
    write(*,*) '✓ Test 6 completed: Combined asymmetric X and Y error bars'
    
    ! Test 7: Customization options
    write(*,*) 'Test 7: Customization options'
    call fig%initialize(640, 480)
    call fig%errorbar(x(1:5), y(1:5), yerr=yerr(1:5), &
                     capsize=10.0_wp, elinewidth=3.0_wp, &
                     marker='o', linestyle='--', &
                     color=[1.0_wp, 0.0_wp, 0.0_wp], &
                     label='Customized errors')
    call fig%set_title('Test 7: Customized Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test7_customized.png')
    write(*,*) '✓ Test 7 completed: Customization options'
    
    ! Test 8: Edge case - very small error bars
    write(*,*) 'Test 8: Edge case - very small error bars'
    small_err = [1e-6_wp, 1e-8_wp, 1e-10_wp]
    call fig%initialize(640, 480)
    call fig%errorbar([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 2.0_wp, 3.0_wp], &
                     yerr=small_err, label='Tiny errors')
    call fig%set_title('Test 8: Very Small Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test8_tiny_errors.png')
    write(*,*) '✓ Test 8 completed: Very small error bars'
    
    ! Test 9: Edge case - zero error bars
    write(*,*) 'Test 9: Edge case - zero error bars'
    zero_err = [0.0_wp, 0.0_wp, 0.0_wp]
    call fig%initialize(640, 480)
    call fig%errorbar([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 2.0_wp, 3.0_wp], &
                     yerr=zero_err, label='Zero errors')
    call fig%set_title('Test 9: Zero Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test9_zero_errors.png')
    write(*,*) '✓ Test 9 completed: Zero error bars'
    
    ! Test 10: Large error bars relative to data
    write(*,*) 'Test 10: Large error bars relative to data'
    large_values = [10.0_wp, 15.0_wp, 20.0_wp]
    call fig%initialize(640, 480)
    call fig%errorbar([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 2.0_wp, 3.0_wp], &
                     yerr=large_values, label='Large errors')
    call fig%set_title('Test 10: Large Error Bars')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test10_large_errors.png')
    write(*,*) '✓ Test 10 completed: Large error bars'
    
    ! Test 11: Error bars with different backends (PNG, ASCII)
    write(*,*) 'Test 11: Backend compatibility testing'
    call fig%initialize(640, 480)
    call fig%errorbar(x(1:5), y(1:5), yerr=yerr(1:5), label='Backend test')
    call fig%set_title('Test 11: Backend Compatibility')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    
    ! PNG backend
    call fig%savefig('test_plots/test11_backend_png.png')
    write(*,*) '✓ PNG backend test completed'
    
    ! ASCII backend
    call fig%savefig('test_plots/test11_backend_ascii.txt')
    write(*,*) '✓ ASCII backend test completed'
    
    ! Test 12: Multiple error bar plots on same figure
    write(*,*) 'Test 12: Multiple error bar plots'
    call fig%initialize(800, 600)
    call fig%errorbar(x(1:5), y(1:5), yerr=yerr(1:5), &
                     color=[1.0_wp, 0.0_wp, 0.0_wp], label='Dataset 1')
    call fig%errorbar(x(1:5) + 0.2_wp, y(1:5) + 0.5_wp, yerr=yerr(1:5) * 1.5_wp, &
                     color=[0.0_wp, 1.0_wp, 0.0_wp], label='Dataset 2')
    call fig%errorbar(x(1:5) + 0.4_wp, y(1:5) + 1.0_wp, yerr=yerr(1:5) * 0.5_wp, &
                     color=[0.0_wp, 0.0_wp, 1.0_wp], label='Dataset 3')
    call fig%set_title('Test 12: Multiple Error Bar Plots')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test12_multiple_plots.png')
    write(*,*) '✓ Test 12 completed: Multiple error bar plots'
    
    ! Test 13: Error bars with mixed plot types
    write(*,*) 'Test 13: Error bars with other plot types'
    call fig%initialize(800, 600)
    ! Regular line plot
    call fig%add_plot(x, y + 2.0_wp, label='Line plot')
    ! Error bar plot
    call fig%errorbar(x, y, yerr=yerr, marker='o', label='Error bars')
    call fig%set_title('Test 13: Mixed Plot Types')
    call fig%set_xlabel('X values')
    call fig%set_ylabel('Y values')
    call fig%legend()
    call fig%savefig('test_plots/test13_mixed_types.png')
    write(*,*) '✓ Test 13 completed: Error bars with other plot types'
    
    write(*,*) ''
    write(*,*) '=== All User Acceptance Tests Completed Successfully! ==='
    write(*,*) 'Check test_plots/ directory for visual verification.'
    
end program test_errorbar_comprehensive