program test_1191_xaxis_fix
    !! Verification test for issue #1191 - X axis should be at bottom
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(5), y(5)
    
    print *, "Testing issue #1191 fix: X-axis position"
    print *, "========================================="
    
    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 4.0_wp, 3.0_wp, 5.0_wp, 1.0_wp]
    
    call figure()
    call plot(x, y, label='Test data')
    call xlabel('X axis (should be at BOTTOM)')
    call ylabel('Y axis')
    call title('Issue #1191: X-axis Position Test')
    call legend()
    call savefig('test_1191_xaxis_bottom.png')
    
    print *, "Generated: test_1191_xaxis_bottom.png"
    print *, ""
    print *, "Expected behavior:"
    print *, "  - X-axis line should be at the BOTTOM of the plot area"
    print *, "  - X-axis tick marks should extend DOWNWARD from the bottom axis"
    print *, "  - X-axis tick labels should be BELOW the tick marks"
    print *, "  - X-axis label 'X axis (should be at BOTTOM)' should be at the bottom"
    print *, ""
    print *, "Previous bug behavior (now fixed):"
    print *, "  - X-axis was incorrectly at the TOP of the plot area"
    print *, ""
    print *, "Visual verification: Check test_1191_xaxis_bottom.png"
    
end program test_1191_xaxis_fix