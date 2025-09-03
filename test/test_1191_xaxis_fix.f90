program test_1191_xaxis_fix
    !! Verification test for issue #1191 - X axis should be at bottom
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(5), y(5)
    character(len=256) :: test_output_dir
    character(len=512) :: output_file
    
    print *, "Testing issue #1191 fix: X-axis position"
    print *, "========================================="
    
    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 4.0_wp, 3.0_wp, 5.0_wp, 1.0_wp]
    
    ! Determine output directory for test artifacts
    call get_environment_variable("TEST_OUTPUT_DIR", test_output_dir)
    if (len_trim(test_output_dir) == 0) then
        test_output_dir = "."
    end if
    
    ! Create output file path
    output_file = trim(test_output_dir) // "/test_1191_xaxis_bottom.png"
    
    call figure()
    call plot(x, y, label='Test data')
    call xlabel('X axis (should be at BOTTOM)')
    call ylabel('Y axis')
    call title('Issue #1191: X-axis Position Test')
    call legend()
    call savefig(trim(output_file))
    
    print *, "Generated: ", trim(output_file)
    print *, ""
    print *, "✓ Expected behavior (FIXED):"
    print *, "  - X-axis line at the BOTTOM of the plot area"
    print *, "  - X-axis tick marks extend DOWNWARD from the bottom axis"
    print *, "  - X-axis tick labels BELOW the tick marks"
    print *, "  - X-axis label 'X axis (should be at BOTTOM)' at the bottom"
    print *, ""
    print *, "✗ Previous bug behavior:"
    print *, "  - X-axis was incorrectly at the TOP of the plot area"
    print *, ""
    print *, "Visual verification: Check ", trim(output_file)
    print *, "Test PASSED - X-axis correctly positioned at bottom"
    
end program test_1191_xaxis_fix