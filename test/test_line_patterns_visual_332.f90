program test_line_patterns_visual_332
    !! Test for Issue #332: Fix line styles pattern appearance
    !! 
    !! This test validates that dashed and dash-dot lines appear 
    !! with proper pattern clarity and not as sparse dots.
    
    use fortplot
    use fortplot_png
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(png_context) :: ctx
    character(len=*), parameter :: output_file = "test_line_patterns_visual_332.png"
    real(wp), parameter :: x_vals(2) = [1.0_wp, 9.0_wp]
    integer :: i
    real(wp) :: x_long(100), y_base
    
    ! Create PNG canvas for pattern visibility testing
    ctx = create_png_canvas(600, 400)
    
    ! Set coordinate system  
    ctx%x_min = 0.0_wp
    ctx%x_max = 10.0_wp
    ctx%y_min = 0.0_wp
    ctx%y_max = 10.0_wp
    
    print *, "Testing line pattern visual appearance for Issue #332..."
    
    ! Create longer line data for pattern visibility
    do i = 1, 100
        x_long(i) = real(i-1, wp) * 0.1_wp
    end do
    
    ! Test dashed line appearance - should show clear dashes, not dots
    call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  ! Red
    call ctx%set_line_width(3.0_wp)  ! Thick for visibility
    call ctx%set_line_style('--')
    y_base = 8.0_wp
    do i = 1, 99
        call ctx%line(x_long(i), y_base, x_long(i+1), y_base)
    end do
    call ctx%text(0.2_wp, y_base + 0.3_wp, "Dashed (--) - should show clear dashes")
    
    ! Test dash-dot line appearance - should show distinct dash-dot rhythm
    call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)  ! Blue
    call ctx%set_line_width(3.0_wp)
    call ctx%set_line_style('-.')
    y_base = 6.0_wp
    do i = 1, 99
        call ctx%line(x_long(i), y_base, x_long(i+1), y_base)
    end do
    call ctx%text(0.2_wp, y_base + 0.3_wp, "Dash-dot (-.) - should show dash-dot-gap rhythm")
    
    ! Test dotted line for comparison - should be clearly different from dashed
    call ctx%color(0.0_wp, 1.0_wp, 0.0_wp)  ! Green
    call ctx%set_line_width(3.0_wp)
    call ctx%set_line_style(':')
    y_base = 4.0_wp
    do i = 1, 99
        call ctx%line(x_long(i), y_base, x_long(i+1), y_base)
    end do
    call ctx%text(0.2_wp, y_base + 0.3_wp, "Dotted (:) - should be distinctly different from dashed")
    
    ! Add solid line for reference
    call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black
    call ctx%set_line_width(2.0_wp)
    call ctx%set_line_style('-')
    call ctx%line(x_vals(1), 2.0_wp, x_vals(2), 2.0_wp)
    call ctx%text(0.2_wp, 2.3_wp, "Solid (-) - reference")
    
    ! Add title and test info
    call ctx%text(5.0_wp, 9.5_wp, "Issue #332: Line Pattern Visual Test")
    call ctx%text(0.2_wp, 0.5_wp, "Pattern Scale Factor: 5.0")
    
    ! Save PNG file
    call ctx%save(output_file)
    
    print *, "Line pattern visual test completed: ", output_file
    print *, ""
    print *, "VISUAL VERIFICATION REQUIRED:"
    print *, "- Dashed line should show clear, regular dashes (not sparse dots)"
    print *, "- Dash-dot should show distinct dash-dot-gap pattern" 
    print *, "- Dotted should be clearly different from dashed"
    print *, "- All patterns should appear visually coherent, not sparse"
    print *, ""
    print *, "BEFORE fix: dashed appeared as sparse dots"
    print *, "AFTER fix: dashed should show proper dash pattern"
    
end program test_line_patterns_visual_332