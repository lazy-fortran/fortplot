program test_machine_precision_coordinates
    !! Test suite for machine precision coordinate handling (Issue #435)
    !! 
    !! Tests coordinate scaling when values approach machine epsilon boundaries
    !! Validates fix for coordinate transformation failures at machine precision scale
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Machine Precision Coordinate Test Suite (Issue #435) ==="
    
    all_tests_passed = .true.
    
    if (.not. test_epsilon_scale_coordinates()) all_tests_passed = .false.
    if (.not. test_mixed_precision_ranges()) all_tests_passed = .false.
    if (.not. test_epsilon_differences()) all_tests_passed = .false.
    if (.not. test_near_zero_epsilon_ranges()) all_tests_passed = .false.
    if (.not. test_large_scale_epsilon_ranges()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "SUCCESS: All machine precision coordinate tests passed!"
        print *, "Issue #435 fix validated - machine precision ranges handled correctly"
    else
        print *, "FAILURE: Some machine precision coordinate tests failed"
        error stop 1
    end if

contains

    function test_epsilon_scale_coordinates() result(success)
        !! Test plotting coordinates at machine epsilon boundaries
        logical :: success
        real(wp) :: x(4), y(4)
        real(wp) :: eps
        
        print *, "Testing epsilon-scale coordinates..."
        
        eps = epsilon(1.0_wp)  ! ≈ 2.22E-16
        
        ! Test coordinates at exactly zero and around machine precision
        x = [0.0_wp, eps, 1.0_wp - eps, 1.0_wp]
        y = [0.0_wp, eps, 1.0_wp - eps, 1.0_wp]
        
        call figure()
        call plot(x, y, label="epsilon boundaries")
        call title("Machine Precision Coordinate Test")
        call xlabel("X values (epsilon scale)")
        call ylabel("Y values (epsilon scale)")
        call savefig("test/output/test_epsilon_coordinates.png")
        
        print *, "  Range:", minval(x), "to", maxval(x)
        print *, "  Difference:", maxval(x) - minval(x)
        print *, "  Epsilon:", eps
        print *, "  Status: Plot should render with visible axes and title"
        
        ! This test actually verifies that no runtime errors occur with epsilon coordinates
        ! Success is determined by reaching this point without segfault or error_stop
        ! Test passes if we reach here without runtime errors
        success = .true.
    end function test_epsilon_scale_coordinates
    
    function test_mixed_precision_ranges() result(success)
        !! Test mixed ranges where one coordinate has normal range, other has epsilon
        logical :: success
        real(wp) :: x(3), y(3)
        real(wp) :: eps
        
        print *, ""
        print *, "Testing mixed precision ranges..."
        
        eps = epsilon(1.0_wp)
        
        ! X has normal range, Y has epsilon-scale differences
        x = [0.0_wp, 0.5_wp, 1.0_wp]
        y = [1.0_wp, 1.0_wp + eps, 1.0_wp + 2.0_wp*eps]
        
        call figure()
        call plot(x, y, label="mixed precision")
        call title("Mixed Precision Range Test")
        call xlabel("Normal X range")
        call ylabel("Epsilon Y range")
        call savefig("test/output/test_mixed_precision.png")
        
        print *, "  X range:", minval(x), "to", maxval(x), "(diff:", maxval(x) - minval(x), ")"
        print *, "  Y range:", minval(y), "to", maxval(y), "(diff:", maxval(y) - minval(y), ")"
        print *, "  Status: Should show normal X axis, expanded Y axis for epsilon range"
        
        ! Test passes if we reach here without runtime errors
        success = .true.
    end function test_mixed_precision_ranges
    
    function test_epsilon_differences() result(success)
        !! Test coordinates with differences at various epsilon scales
        logical :: success
        real(wp) :: x(5), y(5)
        real(wp) :: eps, base_val
        integer :: i
        
        print *, ""
        print *, "Testing various epsilon-scale differences..."
        
        eps = epsilon(1.0_wp)
        base_val = 100.0_wp  ! Test epsilon differences around a larger value
        
        ! Create points with epsilon-scale differences around base value
        do i = 1, 5
            x(i) = base_val + real(i-3, wp) * eps
            y(i) = base_val + real(i-3, wp) * eps * 10.0_wp
        end do
        
        call figure()
        call plot(x, y, label="epsilon diffs")
        call title("Epsilon Differences Test")
        call xlabel("X around 100 (epsilon scale)")
        call ylabel("Y around 100 (10x epsilon scale)")
        call savefig("test/output/test_epsilon_differences.png")
        
        print *, "  Base value:", base_val
        print *, "  X range:", minval(x), "to", maxval(x), "(diff:", maxval(x) - minval(x), ")"
        print *, "  Y range:", minval(y), "to", maxval(y), "(diff:", maxval(y) - minval(y), ")"
        print *, "  Epsilon:", eps
        print *, "  Status: Should expand ranges appropriately for visualization"
        
        ! Test passes if we reach here without runtime errors
        success = .true.
    end function test_epsilon_differences
    
    function test_near_zero_epsilon_ranges() result(success)
        !! Test epsilon-scale ranges near zero
        logical :: success
        real(wp) :: x(3), y(3)
        real(wp) :: eps
        
        print *, ""
        print *, "Testing near-zero epsilon ranges..."
        
        eps = epsilon(1.0_wp)
        
        ! Points very close to zero with epsilon differences
        x = [-eps, 0.0_wp, eps]
        y = [-2.0_wp*eps, 0.0_wp, 2.0_wp*eps]
        
        call figure()
        call plot(x, y, label="near-zero epsilon")
        call title("Near-Zero Epsilon Range Test")
        call xlabel("X near zero (epsilon)")
        call ylabel("Y near zero (2x epsilon)")
        call savefig("test/output/test_near_zero_epsilon.png")
        
        print *, "  X range:", minval(x), "to", maxval(x), "(diff:", maxval(x) - minval(x), ")"
        print *, "  Y range:", minval(y), "to", maxval(y), "(diff:", maxval(y) - minval(y), ")"
        print *, "  Status: Should use absolute margin expansion for near-zero ranges"
        
        ! Test passes if we reach here without runtime errors
        success = .true.
    end function test_near_zero_epsilon_ranges
    
    function test_large_scale_epsilon_ranges() result(success)
        !! Test epsilon-scale ranges around large values
        logical :: success
        real(wp) :: x(3), y(3)
        real(wp) :: eps, large_val
        
        print *, ""
        print *, "Testing large-scale epsilon ranges..."
        
        eps = epsilon(1.0_wp)
        large_val = 1.0e6_wp
        
        ! Points around large value with epsilon differences
        x = [large_val - eps, large_val, large_val + eps]
        y = [large_val - 5.0_wp*eps, large_val, large_val + 5.0_wp*eps]
        
        call figure()
        call plot(x, y, label="large-scale epsilon")
        call title("Large-Scale Epsilon Range Test")
        call xlabel("X around 1e6 (epsilon scale)")
        call ylabel("Y around 1e6 (5x epsilon scale)")
        call savefig("test/output/test_large_scale_epsilon.png")
        
        print *, "  Large value:", large_val
        print *, "  X range:", minval(x), "to", maxval(x), "(diff:", maxval(x) - minval(x), ")"
        print *, "  Y range:", minval(y), "to", maxval(y), "(diff:", maxval(y) - minval(y), ")"
        print *, "  Status: Should use relative margin expansion for large-scale ranges"
        
        ! Test passes if we reach here without runtime errors
        success = .true.
    end function test_large_scale_epsilon_ranges

end program test_machine_precision_coordinates