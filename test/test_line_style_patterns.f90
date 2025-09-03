program test_line_style_patterns
    !! Test line style patterns match between PDF and PNG
    use fortplot
    implicit none
    
    real(wp), dimension(100) :: x, y
    integer :: i
    
    print *, "Testing line style pattern consistency..."
    
    ! Generate horizontal lines for easy pattern comparison
    do i = 1, 100
        x(i) = real(i-1, wp) * 0.1_wp
    end do
    
    call figure(figsize=[10.0_wp, 6.0_wp])
    
    ! Test each style with horizontal lines for clear pattern visibility
    y = 4.0_wp
    call plot(x, y, label='Solid', linestyle='-')
    
    y = 3.0_wp
    call plot(x, y, label='Dashed', linestyle='--')
    
    y = 2.0_wp  
    call plot(x, y, label='Dotted', linestyle=':')
    
    y = 1.0_wp
    call plot(x, y, label='DashDot', linestyle='-.')
    
    call xlim(0.0_wp, 10.0_wp)
    call ylim(0.0_wp, 5.0_wp)
    call title('Line Style Pattern Test')
    call xlabel('X')
    call ylabel('Y')
    call legend()
    call grid(.true.)
    
    ! Save both formats
    call savefig('test_line_patterns.png')
    call savefig('test_line_patterns.pdf')
    
    print *, "Test outputs saved: test_line_patterns.png and test_line_patterns.pdf"
    print *, "Visual check: patterns should be consistent between PNG and PDF"
    
end program test_line_style_patterns