program test_axes_labels_comprehensive
    !! Comprehensive test for Issue #335: Missing axis labels and tick marks
    !! Tests that all axis text elements are properly rendered in both PNG and ASCII
    
    use fortplot
    implicit none
    
    real(wp), dimension(6) :: x_data, y_linear, y_log
    integer :: i
    
    print *, "=== Comprehensive Axes Labels Test ==="
    
    ! Generate test data
    x_data = [(real(i, wp), i = 1, 6)]
    y_linear = x_data * 2.0_wp + 1.0_wp              ! Linear: 3, 5, 7, 9, 11, 13
    y_log = exp(x_data * 0.5_wp)                     ! Exponential for log scale
    
    ! Test 1: Linear scale with all axis elements
    print *, "Testing linear scale with comprehensive axis labeling..."
    call figure()
    call plot(x_data, y_linear)
    call title('Linear Scale Test - All Elements')
    call xlabel('Input Values (x)')
    call ylabel('Linear Response (2x+1)')
    call savefig("test/output/test_linear_axes_comprehensive.png")
    call savefig("test/output/test_linear_axes_comprehensive.txt")
    
    ! Test 2: Log scale with proper formatting
    print *, "Testing log scale with scientific notation..."
    call figure()
    call plot(x_data, y_log)
    call set_yscale('log')
    call title('Log Scale Test - Scientific Labels')
    call xlabel('Input Index')
    call ylabel('Exponential Growth')
    call savefig("test/output/test_log_axes_comprehensive.png")
    call savefig("test/output/test_log_axes_comprehensive.txt")
    
    print *, ""
    print *, "VERIFICATION CHECKLIST:"
    print *, "========================"
    print *, "For each output file, verify:"
    print *, "1. Title text is visible at top"
    print *, "2. X-axis label appears below plot"
    print *, "3. Y-axis label appears left of plot"
    print *, "4. Tick marks are visible on axes"
    print *, "5. Tick labels show numerical values"
    print *, "6. Log scale uses appropriate notation"
    print *, ""
    print *, "Files created:"
    print *, "- test_linear_axes_comprehensive.png/txt"
    print *, "- test_log_axes_comprehensive.png/txt"
    
end program test_axes_labels_comprehensive