program test_line_styles_278
    !! Test to verify line styles work correctly in PNG backend (issue #278)
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(100), y(100)
    integer :: i
    
    ! Generate test data
    do i = 1, 100
        x(i) = real(i-1, wp) / 99.0_wp * 10.0_wp
        y(i) = sin(x(i))
    end do
    
    ! Create figure
    call fig%initialize(640, 480)
    
    ! Test different line styles
    call fig%add_plot(x, y, linestyle='-', label='solid')
    call fig%add_plot(x, y + 1.0_wp, linestyle='--', label='dashed')
    call fig%add_plot(x, y + 2.0_wp, linestyle=':', label='dotted')
    call fig%add_plot(x, y + 3.0_wp, linestyle='-.', label='dashdot')
    
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%set_title('Line Styles Test - Issue #278')
    call fig%legend()
    
    ! Save to PNG to verify line styles render correctly
    call fig%savefig('test_line_styles_278.png')
    
    print *, "Test complete - check test_line_styles_278.png for line styles"
    
end program test_line_styles_278