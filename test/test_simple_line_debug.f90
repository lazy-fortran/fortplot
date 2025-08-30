program test_simple_line_debug
    !! Debug PNG line rendering with simple test case
    
    use fortplot
    use fortplot_png
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(png_context) :: ctx
    character(len=*), parameter :: output_file = "test/output/test_simple_line_debug.png"
    
    ! Create small PNG for debugging
    ctx = create_png_canvas(200, 200)
    
    ! Set coordinate system  
    ctx%x_min = 0.0_wp
    ctx%x_max = 10.0_wp
    ctx%y_min = 0.0_wp
    ctx%y_max = 10.0_wp
    
    print *, "Drawing test lines with different widths..."
    
    ! Draw thick red line
    call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  ! Red
    call ctx%set_line_width(5.0_wp)  ! Thick line
    call ctx%line(1.0_wp, 1.0_wp, 9.0_wp, 1.0_wp)  ! Horizontal line
    
    ! Draw medium blue line
    call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)  ! Blue
    call ctx%set_line_width(3.0_wp)
    call ctx%line(1.0_wp, 3.0_wp, 9.0_wp, 3.0_wp)  ! Horizontal line
    
    ! Draw standard plot line  
    call ctx%color(0.0_wp, 1.0_wp, 0.0_wp)  ! Green
    call ctx%set_line_width(2.0_wp)  ! This should use 0.5px according to current code
    call ctx%line(1.0_wp, 5.0_wp, 9.0_wp, 5.0_wp)  ! Horizontal line
    
    ! Draw thin axis line
    call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)  ! Black
    call ctx%set_line_width(1.0_wp)  ! This should use 0.1px according to current code  
    call ctx%line(1.0_wp, 7.0_wp, 9.0_wp, 7.0_wp)  ! Horizontal line
    
    ! Draw diagonal line to test antialiasing
    call ctx%color(1.0_wp, 0.0_wp, 1.0_wp)  ! Magenta
    call ctx%set_line_width(2.0_wp)
    call ctx%line(2.0_wp, 8.0_wp, 8.0_wp, 9.0_wp)  ! Diagonal line
    
    ! Save PNG file
    call ctx%save(output_file)
    
    print *, "Simple line debug test completed: ", output_file
    print *, ""
    print *, "Debug output shows:"
    print *, "- Red line (width=5.0): should be very thick but maps to 0.1px"
    print *, "- Blue line (width=3.0): should be thick but maps to 0.1px" 
    print *, "- Green line (width=2.0): should be medium and maps to 0.5px"
    print *, "- Black line (width=1.0): should be thin and maps to 0.1px"
    print *, "- Magenta diagonal: should show antialiasing quality"
    
end program test_simple_line_debug