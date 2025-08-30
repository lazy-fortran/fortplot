program test_numeric_limits_integration
    !! Integration test for Issue #433: Numeric limits in full plotting pipeline
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(3), y(3)
    real(wp) :: huge_val, tiny_val
    type(figure_t) :: fig
    logical :: test_passed = .true.
    
    print *, "=== Integration Test: Numeric Limits (Issue #433) ==="
    
    ! Set up extreme numeric values
    huge_val = huge(1.0_wp)
    tiny_val = tiny(1.0_wp)
    
    print *, "Testing with extreme values:"
    print *, "  huge_val = ", huge_val
    print *, "  tiny_val = ", tiny_val
    
    ! Test data with extreme range
    x = [tiny_val, 1.0_wp, huge_val]
    y = [1.0_wp, 2.0_wp, 3.0_wp]
    
    !---------------------------------------------------------------------
    ! Test 1: Log scale with extreme range (should now work with clamping)
    !---------------------------------------------------------------------
    print *, ""
    print *, "Test 1: Log scale with extreme range (should show clamping info)"
    
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x, y, label="extreme values")
    call fig%set_title("Numeric Limits Fixed - Log Scale")
    call fig%set_xlabel("X axis (log scale)")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")  ! This should trigger clamping
    call fig%savefig("test/output/test_limits_fixed_log.png")
    
    !---------------------------------------------------------------------
    ! Test 2: ASCII output for verification
    !---------------------------------------------------------------------
    print *, ""
    print *, "Test 2: ASCII output with extreme range"
    
    call fig%initialize(80, 24, 'ascii')
    call fig%add_plot(x, y, label="extreme values")
    call fig%set_title("Numeric Limits Fixed - ASCII")
    call fig%set_xlabel("X axis (log scale)")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")  ! This should trigger clamping
    call fig%savefig("test/output/test_limits_fixed_ascii.txt")
    
    !---------------------------------------------------------------------
    ! Test 3: More reasonable extreme values
    !---------------------------------------------------------------------
    print *, ""
    print *, "Test 3: Very large but reasonable extreme values"
    
    x = [1.0e-20_wp, 1.0_wp, 1.0e20_wp]  ! Still extreme but within bounds
    y = [1.0_wp, 2.0_wp, 3.0_wp]
    
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x, y, label="large range")
    call fig%set_title("Large Range - Should Not Clamp")
    call fig%set_xlabel("X axis (log scale)")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")
    call fig%savefig("test/output/test_limits_no_clamp.png")
    
    !---------------------------------------------------------------------
    ! Test 4: Y-axis extreme range
    !---------------------------------------------------------------------
    print *, ""
    print *, "Test 4: Y-axis with extreme range"
    
    x = [1.0_wp, 2.0_wp, 3.0_wp]
    y = [tiny_val, 1.0_wp, huge_val]
    
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x, y, label="y extreme")
    call fig%set_title("Y-Axis Extreme Range")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis (log scale)")
    call fig%set_yscale("log")  ! This should trigger Y clamping
    call fig%savefig("test/output/test_limits_y_extreme.png")
    
    !---------------------------------------------------------------------
    ! Summary
    !---------------------------------------------------------------------
    print *, ""
    print *, "============================================="
    print *, "Integration test files generated:"
    print *, "- test_limits_fixed_log.png (should show proper data with clamping info)"
    print *, "- test_limits_fixed_ascii.txt (should show reasonable plot)"
    print *, "- test_limits_no_clamp.png (should work without clamping)"
    print *, "- test_limits_y_extreme.png (should show Y-axis clamping)"
    print *, ""
    print *, "✓ Integration test complete - check console output for clamping messages"
    print *, "============================================="
    
end program test_numeric_limits_integration