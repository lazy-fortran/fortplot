program test_debug_numeric_limits
    !! Diagnostic test to understand numeric limits issue
    !! Focus on understanding the exact values passed through the pipeline
    
    use fortplot_scales, only: apply_scale_transform
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: huge_val, tiny_val, normal_val
    real(wp) :: log_huge, log_tiny, log_normal
    real(wp) :: range_linear, range_log
    
    print *, "=== Diagnostic: Numeric Limits Issue ==="
    
    ! Test values
    huge_val = huge(1.0_wp)
    tiny_val = tiny(1.0_wp)
    normal_val = 1.0_wp
    
    print *, "Original values:"
    print *, "  huge_val = ", huge_val
    print *, "  tiny_val = ", tiny_val
    print *, "  normal   = ", normal_val
    
    ! Linear scale (pass-through)
    print *, ""
    print *, "Linear scale transformations:"
    print *, "  huge_val (linear) = ", apply_scale_transform(huge_val, "linear", 1.0_wp)
    print *, "  tiny_val (linear) = ", apply_scale_transform(tiny_val, "linear", 1.0_wp)
    
    range_linear = huge_val - tiny_val
    print *, "  Linear range = ", range_linear
    
    ! Log scale
    print *, ""
    print *, "Log scale transformations:"
    log_huge = apply_scale_transform(huge_val, "log", 1.0_wp)
    log_tiny = apply_scale_transform(tiny_val, "log", 1.0_wp)
    log_normal = apply_scale_transform(normal_val, "log", 1.0_wp)
    
    print *, "  huge_val (log10) = ", log_huge
    print *, "  tiny_val (log10) = ", log_tiny
    print *, "  normal   (log10) = ", log_normal
    
    range_log = log_huge - log_tiny
    print *, "  Log range = ", range_log, " orders of magnitude"
    
    ! Test problematic values
    print *, ""
    print *, "Problematic intermediate values:"
    print *, "  log10(huge) ≈ ", log10(huge_val)
    print *, "  log10(tiny) ≈ ", log10(tiny_val)
    print *, "  Difference  ≈ ", log10(huge_val) - log10(tiny_val)
    
    ! Test rendering coordinate transformations
    print *, ""
    print *, "Screen coordinate transformations (400px width):"
    call test_screen_coordinates(log_tiny, log_huge, 400)
    
    print *, ""
    print *, "Analysis:"
    if (range_log > 500.0_wp) then
        print *, "  ✗ PROBLEM: Log range > 500 orders of magnitude"
        print *, "    This will cause precision loss in rendering"
    end if
    
    if (abs(log_tiny) > 300.0_wp .or. abs(log_huge) > 300.0_wp) then
        print *, "  ✗ PROBLEM: Log values exceed ±300"
        print *, "    This can cause overflow in coordinate calculations"
    end if
    
    print *, "=== End Diagnostic ==="
    
contains

    subroutine test_screen_coordinates(log_min, log_max, width)
        real(wp), intent(in) :: log_min, log_max
        integer, intent(in) :: width
        
        real(wp) :: test_vals(3), screen_coords(3)
        integer :: i
        
        ! Test coordinate transformation like in fortplot_scales.f90
        test_vals = [log_min, 0.0_wp, log_max]  ! Min, middle, max
        
        do i = 1, 3
            if (log_max > log_min) then
                screen_coords(i) = (test_vals(i) - log_min) / (log_max - log_min) * real(width, wp)
            else
                screen_coords(i) = 0.0_wp
            end if
            print *, "    log value", test_vals(i), " -> screen", screen_coords(i)
        end do
        
        ! Check for precision loss
        if (abs(screen_coords(2) - screen_coords(1)) < 1.0e-10_wp) then
            print *, "  ✗ PRECISION LOSS: Screen coordinates too close together"
        end if
    end subroutine test_screen_coordinates
    
end program test_debug_numeric_limits