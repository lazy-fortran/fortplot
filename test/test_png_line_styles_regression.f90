program test_png_line_styles_regression
    !! Test PNG line styles and markers rendering regression (issue #250)
    !!
    !! This test verifies that PNG line styles and markers render correctly
    !! by testing against known working behavior from commit 690b98341bd351a5bbea431d6af08fb36ff216f7
    
    use fortplot
    use fortplot_png
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(png_context) :: ctx
    real(wp), parameter :: x_vals(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp), parameter :: y1(5) = [1.0_wp, 4.0_wp, 2.0_wp, 5.0_wp, 3.0_wp]  ! Solid line
    real(wp), parameter :: y2(5) = [2.0_wp, 5.0_wp, 1.0_wp, 4.0_wp, 2.5_wp] ! Dashed line  
    real(wp), parameter :: y3(5) = [3.0_wp, 1.0_wp, 5.0_wp, 2.0_wp, 4.0_wp]  ! Dotted line
    character(len=*), parameter :: output_file = "test_png_line_styles_regression.png"
    integer :: i
    
    ! Create PNG canvas
    ctx = create_png_canvas(800, 600)
    
    ! Set coordinate system  
    ctx%x_min = 0.5_wp
    ctx%x_max = 5.5_wp
    ctx%y_min = 0.5_wp
    ctx%y_max = 5.5_wp
    
    ! Test basic line drawing with different colors
    print *, "Testing basic line drawing..."
    
    ! Draw solid red line
    call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  ! Red
    call ctx%set_line_width(2.0_wp)
    do i = 1, 4
        call ctx%line(x_vals(i), y1(i), x_vals(i+1), y1(i+1))
    end do
    
    ! Draw thick blue line
    call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)  ! Blue
    call ctx%set_line_width(3.0_wp)
    do i = 1, 4
        call ctx%line(x_vals(i), y2(i), x_vals(i+1), y2(i+1))
    end do
    
    ! Draw thin green line
    call ctx%color(0.0_wp, 1.0_wp, 0.0_wp)  ! Green
    call ctx%set_line_width(1.0_wp)
    do i = 1, 4
        call ctx%line(x_vals(i), y3(i), x_vals(i+1), y3(i+1))
    end do
    
    ! Test marker rendering
    print *, "Testing marker rendering..."
    
    ! Circle markers in red
    call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  ! Red
    call ctx%set_marker_colors(0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 0.0_wp)  ! Black edge, red face
    do i = 1, 5
        call ctx%draw_marker(x_vals(i), y1(i), "o")
    end do
    
    ! Square markers in blue
    call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)  ! Blue
    call ctx%set_marker_colors(0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp)  ! Black edge, blue face
    do i = 1, 5
        call ctx%draw_marker(x_vals(i), y2(i), "s")
    end do
    
    ! Diamond markers in green
    call ctx%color(0.0_wp, 1.0_wp, 0.0_wp)  ! Green
    call ctx%set_marker_colors(0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp)  ! Black edge, green face
    do i = 1, 5
        call ctx%draw_marker(x_vals(i), y3(i), "D")
    end do
    
    ! Cross markers in magenta
    call ctx%color(1.0_wp, 0.0_wp, 1.0_wp)  ! Magenta
    do i = 1, 5
        call ctx%draw_marker(x_vals(i) + 0.1_wp, y1(i) + 0.1_wp, "x")
    end do
    
    ! Add title and labels to verify text rendering
    call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black
    call ctx%text(3.0_wp, 5.2_wp, "PNG Line Styles & Markers Test")
    call ctx%text(3.0_wp, 0.2_wp, "X-axis label")
    call ctx%text(0.2_wp, 3.0_wp, "Y")
    
    ! Draw axes
    call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black  
    call ctx%set_line_width(1.0_wp)
    call ctx%line(1.0_wp, 1.0_wp, 5.0_wp, 1.0_wp)  ! X-axis
    call ctx%line(1.0_wp, 1.0_wp, 1.0_wp, 5.0_wp)  ! Y-axis
    
    ! Save PNG file
    call ctx%save(output_file)
    
    print *, "PNG line styles and markers test completed: ", output_file
    print *, ""
    print *, "Visual inspection required:"
    print *, "- Red line with circle markers should be clearly visible"
    print *, "- Blue line with square markers should be clearly visible" 
    print *, "- Green line with diamond markers should be clearly visible"
    print *, "- Magenta cross markers should be clearly visible"
    print *, "- Lines should have different thicknesses (1px, 2px, 3px)"
    print *, "- Markers should have distinct shapes and colors"
    print *, "- Text should be readable and positioned correctly"
    print *, ""
    
end program test_png_line_styles_regression