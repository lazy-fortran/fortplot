program test_backend_scatter
    !! Test scatter plots across different backends (PNG, PDF, ASCII)
    use fortplot
    use fortplot_security, only: get_test_output_path
    use iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(10), y(10), sizes(10), colors(10)
    integer :: i
    
    write(error_unit, '(A)') 'Testing scatter plots across backends...'
    
    ! Generate test data
    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = sin(real(i, wp)) + 2.0_wp
        sizes(i) = 15.0_wp + 10.0_wp * sin(real(i, wp))
        colors(i) = real(i, wp) / 10.0_wp
    end do
    
    ! Test PNG backend
    write(error_unit, '(A)') 'Testing PNG backend...'
    call fig%initialize(600, 400)
    call fig%add_scatter_2d(x, y, s=sizes, c=colors, colormap='viridis', &
                           marker='o', label='PNG Test')
    call fig%set_title('Scatter Plot - PNG Backend')
    call fig%set_xlabel('X Values')
    call fig%set_ylabel('Y Values')
    call figure_legend(fig, )
    call figure_savefig(fig, get_test_output_path('/tmp/scatter_test.png'))
    write(error_unit, '(A)') '  ✓ PNG backend test completed'
    
    ! Test PDF backend
    write(error_unit, '(A)') 'Testing PDF backend...'
    call fig%initialize(600, 400)
    call fig%add_scatter_2d(x, y, s=sizes, c=colors, colormap='plasma', &
                           marker='s', label='PDF Test')
    call fig%set_title('Scatter Plot - PDF Backend')
    call fig%set_xlabel('X Values')
    call fig%set_ylabel('Y Values')
    call figure_legend(fig, )
    call figure_savefig(fig, get_test_output_path('/tmp/scatter_test.pdf'))
    write(error_unit, '(A)') '  ✓ PDF backend test completed'
    
    ! Test ASCII backend
    write(error_unit, '(A)') 'Testing ASCII backend...'
    call fig%initialize(60, 20)  ! Smaller size for ASCII
    call fig%add_scatter_2d(x, y, marker='*', label='ASCII Test')
    call fig%set_title('Scatter Plot - ASCII Backend')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call figure_savefig(fig, get_test_output_path('/tmp/scatter_test.txt'))
    write(error_unit, '(A)') '  ✓ ASCII backend test completed'
    
    write(error_unit, '(A)') 'All backend tests completed successfully!'
    
end program test_backend_scatter