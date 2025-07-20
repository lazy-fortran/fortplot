program test_disconnected_lines
    use fortplot
    use fortplot_figure, only: figure_t
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    implicit none
    
    call test_multiple_plots_should_not_connect()
    call test_nan_values_should_break_lines()
    call test_nan_with_patterned_lines()
    call test_nan_with_markers_only()
    
    print *, "All disconnected lines tests passed!"
    
contains
    
    subroutine test_multiple_plots_should_not_connect()
        type(figure_t) :: fig
        real(8) :: x1(2), y1(2), x2(2), y2(2)
        
        call fig%initialize(400, 300)
        
        ! First line segment: (0,0) to (1,0)
        x1 = [0.0_8, 1.0_8]
        y1 = [0.0_8, 0.0_8]
        call fig%add_plot(x1, y1)
        
        ! Second line segment: (2,1) to (3,1)
        x2 = [2.0_8, 3.0_8]
        y2 = [1.0_8, 1.0_8]
        call fig%add_plot(x2, y2)
        
        ! This test will fail initially, showing the issue
        call fig%savefig("test_disconnected_multiple_plots.png")
        
        print *, "Test multiple plots: Generated test_disconnected_multiple_plots.png"
    end subroutine test_multiple_plots_should_not_connect
    
    subroutine test_nan_values_should_break_lines()
        type(figure_t) :: fig
        real(8) :: x(5), y(5), nan
        
        ! Get NaN value
        nan = ieee_value(nan, ieee_quiet_nan)
        
        call fig%initialize(400, 300)
        
        ! Two segments separated by NaN: (0,0)-(1,0) and (2,1)-(3,1)
        x = [0.0_8, 1.0_8, nan, 2.0_8, 3.0_8]
        y = [0.0_8, 0.0_8, nan, 1.0_8, 1.0_8]
        
        call fig%add_plot(x, y)
        
        call fig%savefig("test_disconnected_nan_break.png")
        
        print *, "Test NaN line breaks: Generated test_disconnected_nan_break.png"
    end subroutine test_nan_values_should_break_lines
    
    subroutine test_nan_with_patterned_lines()
        type(figure_t) :: fig
        real(8) :: x(7), y(7), nan
        
        ! Get NaN value
        nan = ieee_value(nan, ieee_quiet_nan)
        
        call fig%initialize(400, 300)
        
        ! Test dashed line with NaN breaks
        x = [0.0_8, 1.0_8, 2.0_8, nan, 3.0_8, 4.0_8, 5.0_8]
        y = [0.0_8, 1.0_8, 0.0_8, nan, 1.0_8, 0.0_8, 1.0_8]
        
        call fig%add_plot(x, y, linestyle='--')
        
        call fig%savefig("test_disconnected_dashed.png")
        
        print *, "Test dashed NaN breaks: Generated test_disconnected_dashed.png"
    end subroutine test_nan_with_patterned_lines
    
    subroutine test_nan_with_markers_only()
        type(figure_t) :: fig
        real(8) :: x(5), y(5), nan
        
        ! Get NaN value
        nan = ieee_value(nan, ieee_quiet_nan)
        
        call fig%initialize(400, 300)
        
        ! Test markers only with NaN values
        x = [0.0_8, 1.0_8, nan, 3.0_8, 4.0_8]
        y = [0.0_8, 1.0_8, nan, 1.0_8, 0.0_8]
        
        call fig%add_plot(x, y, linestyle='o')
        
        call fig%savefig("test_disconnected_markers.png")
        
        print *, "Test markers with NaN: Generated test_disconnected_markers.png"
    end subroutine test_nan_with_markers_only
    
end program test_disconnected_lines