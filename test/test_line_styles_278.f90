program test_line_styles_278
    !! Test to verify line styles work correctly in PNG backend (issue #278)
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real :: x(100), y(100)
    integer :: i
    
    ! Generate test data
    do i = 1, 100
        x(i) = real(i-1) / 99.0 * 10.0
        y(i) = sin(x(i))
    end do
    
    ! Create figure
    call figure(fig)
    
    ! Test different line styles
    call fig%plot(x, y, linestyle='-', label='solid')
    call fig%plot(x, y + 1.0, linestyle='--', label='dashed')
    call fig%plot(x, y + 2.0, linestyle=':', label='dotted')
    call fig%plot(x, y + 3.0, linestyle='-.', label='dashdot')
    
    call fig%xlabel('X')
    call fig%ylabel('Y')
    call fig%title('Line Styles Test - Issue #278')
    call fig%legend()
    
    ! Save to PNG to verify line styles render correctly
    call fig%save('test_line_styles_278.png')
    
    print *, "Test complete - check test_line_styles_278.png for line styles"
    
end program test_line_styles_278