program ylabel_comparison
    !! Visual comparison example showing ylabel positioning improvement
    !! This demonstrates the fix for issue #1136 where ylabel was cut off
    use fortplot
    implicit none
    
    real(wp) :: x(100), y1(100), y2(100), y3(100)
    integer :: i
    
    ! Generate test data with different ranges
    do i = 1, 100
        x(i) = real(i-1, wp) * 0.1_wp
        y1(i) = sin(x(i)) * 1000000.0_wp   ! Large values for wide tick labels
        y2(i) = cos(x(i)) * 0.000001_wp    ! Small values
        y3(i) = exp(x(i) * 0.1_wp) * 1000.0_wp  ! Exponential growth
    end do
    
    ! Test 1: Large values with wide tick labels
    call figure()
    call plot(x, y1, label='Large amplitude')
    call xlabel('Time (seconds)')
    call ylabel('Amplitude (microvolts)')  ! Relatively long ylabel
    call title('Test 1: Wide tick labels with ylabel')
    call legend()
    call savefig('ylabel_test1_wide_ticks.png')
    
    ! Test 2: Small values with scientific notation
    call figure()
    call plot(x, y2, label='Small amplitude')
    call xlabel('Distance (meters)')
    call ylabel('Field strength (Tesla)')
    call title('Test 2: Scientific notation tick labels')
    call legend()
    call savefig('ylabel_test2_scientific.png')
    
    ! Test 3: Very long ylabel text
    call figure()
    call plot(x, y3, label='Exponential')
    call xlabel('Iteration number')
    call ylabel('Normalized intensity (arbitrary units)')  ! Very long ylabel
    call title('Test 3: Very long ylabel text')
    call legend()
    call savefig('ylabel_test3_long_label.png')
    
    ! Test 4: Multiple plots with varying ranges
    call figure()
    call plot(x, y1/1000.0_wp, label='Dataset 1')
    call plot(x, y2*1e9_wp, label='Dataset 2')
    call plot(x, y3/100.0_wp, label='Dataset 3')
    call xlabel('Sample index')
    call ylabel('Measurement value')
    call title('Test 4: Multiple datasets')
    call legend()
    call savefig('ylabel_test4_multiple.png')
    
    print *, '=== Y-label positioning comparison tests generated ==='
    print *, ''
    print *, 'Generated files:'
    print *, '  ylabel_test1_wide_ticks.png - Tests wide tick label handling'
    print *, '  ylabel_test2_scientific.png - Tests scientific notation'
    print *, '  ylabel_test3_long_label.png - Tests very long ylabel text'
    print *, '  ylabel_test4_multiple.png - Tests multiple datasets'
    print *, ''
    print *, 'With the fix:'
    print *, '  - Y-labels should be fully visible (not cut off at left edge)'
    print *, '  - Proper spacing maintained from tick labels'
    print *, '  - Minimum 15px margin from canvas edge enforced'
    
end program ylabel_comparison