program test_numeric_limits_433
    !! Test for Issue #433: Critical numeric limits handling
    !! Reproduces blank PNG output with huge() and tiny() values
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: x(3), y(3)
    real(wp) :: huge_val, tiny_val
    type(figure_t) :: fig
    logical :: test_passed = .true.
    
    print *, "=== Testing Numeric Limits Handling (Issue #433) ==="
    
    ! Set up extreme numeric values
    huge_val = huge(1.0_wp)
    tiny_val = tiny(1.0_wp)
    
    print *, "huge_val = ", huge_val
    print *, "tiny_val = ", tiny_val
    print *, "range = ", log10(huge_val) - log10(tiny_val), " orders of magnitude"
    
    ! Test data with extreme range
    x = [tiny_val, 1.0_wp, huge_val]
    y = [1.0_wp, 2.0_wp, 3.0_wp]
    
    !---------------------------------------------------------------------
    ! Test 1: Linear scale with extreme range
    !---------------------------------------------------------------------
    print *, "Test 1: Linear scale with extreme range"
    
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x, y, label="limits linear")
    call fig%set_title("Numeric Limits Test - Linear Scale")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis") 
    call fig%savefig('test_limits_linear.png')
    
    !---------------------------------------------------------------------
    ! Test 2: Log scale with extreme range (should work)
    !---------------------------------------------------------------------
    print *, "Test 2: Log scale with extreme range"
    
    call fig%initialize(400, 300, 'png')
    call fig%add_plot(x, y, label="limits log")
    call fig%set_title("Numeric Limits Test - Log Scale")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")
    call fig%savefig('test_limits_log.png')
    
    !---------------------------------------------------------------------
    ! Test 3: ASCII backend for easier debugging
    !---------------------------------------------------------------------
    print *, "Test 3: ASCII backend with extreme values"
    
    call fig%initialize(80, 24, 'ascii')
    call fig%add_plot(x, y, label="limits ascii")
    call fig%set_title("Numeric Limits Test - ASCII")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")
    call fig%savefig('test_limits_ascii.txt')
    
    !---------------------------------------------------------------------
    ! Test 4: Controlled extreme values
    !---------------------------------------------------------------------
    print *, "Test 4: Controlled extreme values"
    
    x = [1.0e-300_wp, 1.0_wp, 1.0e+300_wp]
    y = [1.0_wp, 2.0_wp, 3.0_wp]
    
    call fig%initialize(400, 300, 'png') 
    call fig%add_plot(x, y, label="controlled extreme")
    call fig%set_title("Controlled Extreme Values")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis")
    call fig%set_xscale("log")
    call fig%savefig('test_controlled_extreme.png')
    
    !---------------------------------------------------------------------
    ! Summary
    !---------------------------------------------------------------------
    print *, "============================================="
    print *, "Test files generated:"
    print *, "- test_limits_linear.png (likely blank/corrupted)"
    print *, "- test_limits_log.png (should show data but may be blank)"
    print *, "- test_limits_ascii.txt (text output for debugging)"
    print *, "- test_controlled_extreme.png (less extreme test)"
    print *, "============================================="
    
end program test_numeric_limits_433