program test_line_styles_simple
    !! Simple test of line styles
    use fortplot
    implicit none
    
    real(wp) :: x(10), y(10)
    integer :: i
    
    ! Simple straight lines at different y positions
    do i = 1, 10
        x(i) = real(i-1, wp)
        y(i) = 0.0_wp
    end do
    
    call figure()
    
    ! Draw 4 horizontal lines with different styles
    call plot(x, y + 3.0_wp, linestyle='-', label='Solid')
    call plot(x, y + 2.0_wp, linestyle='--', label='Dashed')  
    call plot(x, y + 1.0_wp, linestyle=':', label='Dotted')
    call plot(x, y + 0.0_wp, linestyle='-.', label='DashDot')
    
    call xlabel('X')
    call ylabel('Y')
    call title('Line Style Test')
    call legend()
    call xlim(0.0_wp, 9.0_wp)
    call ylim(-0.5_wp, 3.5_wp)
    
    call savefig('test_line_styles_simple.png')
    print *, 'Saved test_line_styles_simple.png'
    
end program test_line_styles_simple