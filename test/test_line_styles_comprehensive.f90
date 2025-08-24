program test_line_styles_comprehensive
    !! Comprehensive test for PNG line styles (solid, dashed, dotted, dash-dot)
    
    use fortplot
    use fortplot_png
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(png_context) :: ctx
    character(len=*), parameter :: output_file = "test_line_styles_comprehensive.png"
    real(wp), parameter :: x_vals(2) = [1.0_wp, 9.0_wp]
    
    ! Create PNG canvas
    ctx = create_png_canvas(400, 300)
    
    ! Set coordinate system  
    ctx%x_min = 0.0_wp
    ctx%x_max = 10.0_wp
    ctx%y_min = 0.0_wp
    ctx%y_max = 8.0_wp
    
    print *, "Testing comprehensive line styles..."
    
    ! Test solid line (default)
    call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  ! Red
    call ctx%set_line_width(2.0_wp)
    call ctx%set_line_style('-')
    call ctx%line(x_vals(1), 7.0_wp, x_vals(2), 7.0_wp)
    call ctx%text(0.2_wp, 7.2_wp, "Solid (-)")
    
    ! Test dashed line  
    call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)  ! Blue
    call ctx%set_line_width(2.0_wp)
    call ctx%set_line_style('--')
    call ctx%line(x_vals(1), 5.5_wp, x_vals(2), 5.5_wp)
    call ctx%text(0.2_wp, 5.7_wp, "Dashed (--)")
    
    ! Test dotted line
    call ctx%color(0.0_wp, 1.0_wp, 0.0_wp)  ! Green
    call ctx%set_line_width(2.0_wp)
    call ctx%set_line_style(':')
    call ctx%line(x_vals(1), 4.0_wp, x_vals(2), 4.0_wp)
    call ctx%text(0.2_wp, 4.2_wp, "Dotted (:)")
    
    ! Test dash-dot line
    call ctx%color(1.0_wp, 0.0_wp, 1.0_wp)  ! Magenta
    call ctx%set_line_width(2.0_wp)
    call ctx%set_line_style('-.')
    call ctx%line(x_vals(1), 2.5_wp, x_vals(2), 2.5_wp)
    call ctx%text(0.2_wp, 2.7_wp, "Dash-dot (-.)")
    
    ! Test with different thicknesses
    call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black
    call ctx%set_line_width(4.0_wp)  ! Thick
    call ctx%set_line_style('--')
    call ctx%line(x_vals(1), 1.0_wp, x_vals(2), 1.0_wp)
    call ctx%text(0.2_wp, 1.2_wp, "Thick dashed")
    
    ! Add title
    call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black
    call ctx%set_line_style('-')  ! Reset to solid
    call ctx%text(5.0_wp, 7.5_wp, "Line Styles Test")
    
    ! Save PNG file
    call ctx%save(output_file)
    
    print *, "Line styles comprehensive test completed: ", output_file
    print *, ""
    print *, "Visual verification required:"
    print *, "- Red solid line should be continuous"
    print *, "- Blue dashed line should have regular gaps" 
    print *, "- Green dotted line should have small dots with gaps"
    print *, "- Magenta dash-dot should alternate long and short segments"
    print *, "- Black thick dashed should be visibly thicker"
    
end program test_line_styles_comprehensive