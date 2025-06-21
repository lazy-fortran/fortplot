program line_style_examples
    !! Comprehensive demonstration of line styles in fortplotlib
    !! 
    !! This example shows all available line styles:
    !! - Solid lines (-)
    !! - Dashed lines (--)  
    !! - Dotted lines (:)
    !! - Dash-dot lines (-.)
    !! - No line (None)
    use fortplot
    implicit none
    
    real(wp), dimension(50) :: x, y1, y2, y3, y4, y5
    integer :: i
    
    print *, "=== Line Style Examples ==="
    
    ! Generate test data with clear separation for visibility
    do i = 1, 50
        x(i) = real(i-1, wp) * 0.2_wp
        y1(i) = sin(x(i)) + 2.0_wp
        y2(i) = cos(x(i)) + 1.0_wp  
        y3(i) = sin(x(i) * 2.0_wp)
        y4(i) = cos(x(i) * 3.0_wp) - 1.0_wp
        y5(i) = sin(x(i) * 0.5_wp) - 2.0_wp
    end do
    
    ! Comprehensive line style demonstration
    call figure(800, 600)
    call plot(x, y1, label='Solid (-)', linestyle=LINESTYLE_SOLID)
    call plot(x, y2, label='Dashed (--)', linestyle=LINESTYLE_DASHED)
    call plot(x, y3, label='Dotted (:)', linestyle=LINESTYLE_DOTTED)
    call plot(x, y4, label='Dash-dot (-.)', linestyle=LINESTYLE_DASHDOT)
    call plot(x, y5, label='None (invisible)', linestyle=LINESTYLE_NONE)
    call title('Complete Line Style Reference')
    call xlabel('X values')
    call ylabel('Y values')
    call savefig('line_style_reference.png')
    call savefig('line_style_reference.pdf')
    
    ! Terminal display to show line styles in ASCII
    call figure()
    call plot(x, y1, label='Solid', linestyle='-')
    call plot(x, y2, label='Dashed', linestyle='--')
    call plot(x, y3, label='Dotted', linestyle=':')
    call plot(x, y4, label='Dash-dot', linestyle='-.')
    call title('Line Styles in Terminal')
    call show()
    
    print *, "Line style examples completed!"
    print *, "Files created: line_style_reference.png, line_style_reference.pdf"
    print *, "Note: 'None' linestyle creates invisible lines (no output)"
    print *
    
end program line_style_examples