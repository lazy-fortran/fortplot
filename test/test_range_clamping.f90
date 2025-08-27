program test_range_clamping
    !! Test range clamping functionality directly
    
    use fortplot_scales, only: clamp_extreme_log_range, apply_scale_transform
    use iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: huge_val, tiny_val, normal_val
    real(wp) :: clamped_min, clamped_max
    
    print *, "=== Testing Range Clamping Function ==="
    
    ! Test values
    huge_val = huge(1.0_wp)
    tiny_val = tiny(1.0_wp)
    normal_val = 1.0_wp
    
    print *, "Original extreme values:"
    print *, "  huge = ", huge_val
    print *, "  tiny = ", tiny_val
    
    ! Test range clamping
    call clamp_extreme_log_range(tiny_val, huge_val, clamped_min, clamped_max)
    
    print *, ""
    print *, "After clamping:"
    print *, "  clamped_min = ", clamped_min
    print *, "  clamped_max = ", clamped_max
    print *, "  log10(clamped_min) = ", log10(clamped_min)
    print *, "  log10(clamped_max) = ", log10(clamped_max)
    print *, "  Log range = ", log10(clamped_max) - log10(clamped_min)
    
    ! Test log transforms with clamping
    print *, ""
    print *, "Log transforms (should be clamped):"
    print *, "  log10(huge) clamped = ", apply_scale_transform(huge_val, "log", 1.0_wp)
    print *, "  log10(tiny) clamped = ", apply_scale_transform(tiny_val, "log", 1.0_wp)
    print *, "  Range after log transform = ", &
             apply_scale_transform(huge_val, "log", 1.0_wp) - &
             apply_scale_transform(tiny_val, "log", 1.0_wp)
    
    ! Test moderate range (should not be clamped)
    print *, ""
    print *, "Testing moderate range (should not clamp):"
    call clamp_extreme_log_range(1.0e-10_wp, 1.0e10_wp, clamped_min, clamped_max)
    print *, "  Input: 1e-10 to 1e10"
    print *, "  Output: ", clamped_min, " to ", clamped_max
    print *, "  Changed: ", (abs(clamped_min - 1.0e-10_wp) > 1.0e-15_wp .or. &
                             abs(clamped_max - 1.0e10_wp) > 1.0e-5_wp)
    
    print *, "=== Range Clamping Test Complete ==="
    
end program test_range_clamping