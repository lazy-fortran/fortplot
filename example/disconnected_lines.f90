program disconnected_lines
    use fortplot
    use fortplot_figure, only: figure_t
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    implicit none
    
    type(figure_t) :: fig
    real(8) :: x(11), y(11), nan
    integer :: i
    
    ! Get NaN value
    nan = ieee_value(nan, ieee_quiet_nan)
    
    call fig%initialize(800, 600)
    
    ! Create data with three disconnected segments using NaN as separator
    ! First segment: sine wave from 0 to pi
    x(1:4) = [0.0_8, 1.047_8, 2.094_8, 3.142_8]  ! 0, pi/3, 2pi/3, pi
    do i = 1, 4
        y(i) = sin(x(i))
    end do
    
    ! NaN separator
    x(5) = nan
    y(5) = nan
    
    ! Second segment: cosine wave from pi to 2pi
    x(6:9) = [3.142_8, 4.189_8, 5.236_8, 6.283_8]  ! pi, 4pi/3, 5pi/3, 2pi
    do i = 6, 9
        y(i) = cos(x(i))
    end do
    
    ! NaN separator
    x(10) = nan
    y(10) = nan
    
    ! Third segment: horizontal line at y=0.5
    x(11) = 7.0_8
    y(11) = 0.5_8
    
    ! Plot disconnected segments with markers and lines
    call fig%add_plot(x, y, label='Disconnected segments', linestyle='o-')
    
    ! Add single point (will be disconnected from the line)
    call fig%add_plot([8.0_8], [-0.5_8], linestyle='rs', label='Single point')
    
    ! Configure plot
    call fig%set_title('Disconnected Line Segments Example')
    call fig%set_xlabel('x')
    call fig%set_ylabel('y')
    call fig%legend()
    
    ! Save in multiple formats
    call fig%savefig('build/example/disconnected_lines.png')
    call fig%savefig('build/example/disconnected_lines.pdf')
    call fig%savefig('build/example/disconnected_lines.txt')
    
    print *, "Disconnected lines example saved to:"
    print *, "  - build/example/disconnected_lines.png"
    print *, "  - build/example/disconnected_lines.pdf"
    print *, "  - build/example/disconnected_lines.txt"
    
end program disconnected_lines