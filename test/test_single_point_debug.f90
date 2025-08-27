program test_single_point_debug
    !! Debug test for single point plotting fix
    
    use fortplot
    use fortplot_logging, only: set_log_level, LOG_LEVEL_DEBUG
    use fortplot_test_helpers, only: test_initialize_figure, test_savefig, test_cleanup_all
    use iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x_data(1), y_data(1)
    
    ! Enable debug logging to see validation messages
    call set_log_level(LOG_LEVEL_DEBUG)
    
    print *, "DEBUG: Testing single point plotting with debug logging"
    
    ! Test data
    x_data = [5.0_wp]
    y_data = [3.0_wp]
    
    print *, "DEBUG: Creating figure..."
    call test_initialize_figure(fig, 200, 150, 'png')
    
    print *, "DEBUG: Adding single point plot..."
    print *, "DEBUG: x_data = ", x_data
    print *, "DEBUG: y_data = ", y_data
    
    call fig%add_plot(x_data, y_data, label="debug single point")
    
    print *, "DEBUG: Saving figure..."
    call test_savefig(fig, 'debug_single_point.png')
    
    print *, "DEBUG: Test completed!"
    call test_cleanup_all()
    
end program test_single_point_debug