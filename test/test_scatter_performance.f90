program test_scatter_performance
    !! Test scatter plot performance with larger datasets
    use fortplot
    use iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    type(figure_t) :: fig
    integer, parameter :: N_POINTS = 1000
    real(wp) :: x(N_POINTS), y(N_POINTS), sizes(N_POINTS), colors(N_POINTS)
    integer :: i
    real(wp) :: start_time, end_time
    
    write(error_unit, '(A)') 'Testing scatter plot performance with large datasets...'
    write(error_unit, '(A,I0,A)') 'Data points: ', N_POINTS, ' points'
    
    ! Generate large test dataset
    do i = 1, N_POINTS
        x(i) = real(i, wp) / 100.0_wp
        y(i) = sin(x(i)) + 0.1_wp * cos(10.0_wp * x(i))
        sizes(i) = 5.0_wp + 15.0_wp * abs(sin(x(i) * 2.0_wp))
        colors(i) = real(i, wp) / real(N_POINTS, wp)
    end do
    
    call cpu_time(start_time)
    
    ! Test large scatter plot
    call fig%initialize(800, 600)
    call fig%add_scatter_2d(x, y, s=sizes, c=colors, colormap='viridis', &
                           marker='o', label='Large Dataset')
    call fig%set_title('Performance Test - 1000 Points')
    call fig%set_xlabel('X Values')
    call fig%set_ylabel('Y Values') 
    call fig%legend()
    call fig%savefig('/tmp/scatter_performance_test.png')
    
    call cpu_time(end_time)
    
    write(error_unit, '(A,F8.3,A)') '  ✓ Large dataset test completed in ', &
                                    end_time - start_time, ' seconds'
    write(error_unit, '(A)') 'Performance test successful!'
    
end program test_scatter_performance