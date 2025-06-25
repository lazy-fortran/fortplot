program test_debug_text_width
    !! Debug text width calculations
    use fortplot_text, only: calculate_text_width
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer :: width_x, width_sin
    
    print *, "=== Debug Text Width Calculations ==="
    
    ! Test text width calculations
    width_x = calculate_text_width("x")
    width_sin = calculate_text_width("sin(x)")
    
    print *, "Text width for 'x':", width_x, "pixels"
    print *, "Text width for 'sin(x)':", width_sin, "pixels"
    print *, ""
    
    ! Matplotlib measured values for comparison
    print *, "Matplotlib measurements:"
    print *, "  Y-axis label 'sin(x)': right edge at X=22, left edge at X=8"
    print *, "  This implies text width =", 22-8, "pixels"
    print *, ""
    
    ! Test positioning calculation manually
    print *, "Manual positioning calculation:"
    print *, "  plot_left = 80"
    print *, "  Y_AXIS_SPACING = 58"
    print *, "  right_edge_x = 80 - 58 =", 80-58
    print *, "  label_x = right_edge_x - text_width =", 22, "-", width_sin, "=", 22-width_sin
    print *, ""
    
    if (22 - width_sin < 0) then
        print *, "PROBLEM: Calculated text width", width_sin, "is too large!"
        print *, "  This would place left edge at negative X position"
        print *, "  Fallback estimate: len('sin(x)') * 8 =", len_trim("sin(x)") * 8, "pixels"
    end if
    
end program test_debug_text_width