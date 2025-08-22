program test_disconnected_lines
    use fortplot
    use fortplot_figure, only: figure_t
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    implicit none
    
    call test_multiple_plots_should_not_connect()
    call test_nan_values_should_break_lines()
    call test_nan_with_patterned_lines()
    call test_nan_with_markers_only()
    call test_all_linestyle_patterns()
    call test_edge_cases()
    call test_empty_and_small_arrays()
    call test_all_nan_array_edge_case()
    
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
        call figure_savefig(fig, "test_disconnected_multiple_plots.png")
        
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
        
        call figure_savefig(fig, "test_disconnected_nan_break.png")
        
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
        
        call figure_savefig(fig, "test_disconnected_dashed.png")
        
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
        
        call figure_savefig(fig, "test_disconnected_markers.png")
        
        print *, "Test markers with NaN: Generated test_disconnected_markers.png"
    end subroutine test_nan_with_markers_only
    
    subroutine test_all_linestyle_patterns()
        type(figure_t) :: fig
        real(8) :: x(7), y(7), nan
        
        ! Get NaN value
        nan = ieee_value(nan, ieee_quiet_nan)
        
        call fig%initialize(600, 400)
        
        ! Test all pattern types with NaN breaks
        x = [0.0_8, 1.0_8, 2.0_8, nan, 3.0_8, 4.0_8, 5.0_8]
        y = [0.0_8, 1.0_8, 0.0_8, nan, 1.0_8, 0.0_8, 1.0_8]
        
        ! Test dotted line
        call fig%add_plot(x, y + 0.0_8, linestyle=':')
        
        ! Test dash-dot line  
        call fig%add_plot(x, y + 1.5_8, linestyle='-.')
        
        ! Test dashed line
        call fig%add_plot(x, y + 3.0_8, linestyle='--')
        
        ! Test unknown pattern (should fall back to solid)
        call fig%add_plot(x, y + 4.5_8, linestyle='unknown')
        
        call figure_savefig(fig, "test_all_patterns_nan.png")
        
        print *, "Test all patterns: Generated test_all_patterns_nan.png"
    end subroutine test_all_linestyle_patterns
    
    subroutine test_edge_cases()
        type(figure_t) :: fig
        real(8) :: x(10), y(10), nan
        integer :: i
        
        ! Get NaN value
        nan = ieee_value(nan, ieee_quiet_nan)
        
        call fig%initialize(400, 300)
        
        ! Test all NaN values
        x = [(nan, i=1,10)]
        y = [(nan, i=1,10)]
        call fig%add_plot(x, y, linestyle='-')
        
        ! Test starting with NaN
        x(1:5) = [nan, nan, 1.0_8, 2.0_8, 3.0_8]
        y(1:5) = [nan, nan, 0.0_8, 1.0_8, 0.0_8]
        call fig%add_plot(x(1:5), y(1:5), linestyle='--')
        
        ! Test ending with NaN
        x(1:5) = [1.0_8, 2.0_8, 3.0_8, nan, nan]
        y(1:5) = [0.0_8, 1.0_8, 0.0_8, nan, nan]
        call fig%add_plot(x(1:5), y(1:5) + 1.0_8, linestyle=':')
        
        ! Test consecutive NaN values in middle
        x(1:7) = [0.0_8, 1.0_8, nan, nan, nan, 4.0_8, 5.0_8]
        y(1:7) = [0.0_8, 1.0_8, nan, nan, nan, 1.0_8, 0.0_8]
        call fig%add_plot(x(1:7), y(1:7) + 2.0_8, linestyle='-.')
        
        call figure_savefig(fig, "test_edge_cases_nan.png")
        
        print *, "Test edge cases: Generated test_edge_cases_nan.png"
    end subroutine test_edge_cases
    
    subroutine test_empty_and_small_arrays()
        type(figure_t) :: fig
        real(8) :: x(1), y(1), x2(2), y2(2), nan
        
        ! Get NaN value
        nan = ieee_value(nan, ieee_quiet_nan)
        
        call fig%initialize(400, 300)
        
        ! Test single point (should not draw lines)
        x(1) = 1.0_8
        y(1) = 1.0_8
        call fig%add_plot(x, y, linestyle='-')
        
        ! Test single NaN point
        x(1) = nan
        y(1) = nan
        call fig%add_plot(x, y, linestyle='o')
        
        ! Test two points with one NaN
        x2 = [1.0_8, nan]
        y2 = [0.0_8, nan]
        call fig%add_plot(x2, y2, linestyle='--')
        
        ! Test two NaN points
        x2 = [nan, nan]
        y2 = [nan, nan]
        call fig%add_plot(x2, y2, linestyle=':')
        
        ! Test very short segment
        x2 = [1.0_8, 1.000001_8]
        y2 = [1.0_8, 1.000001_8]
        call fig%add_plot(x2, y2 + 1.0_8, linestyle='-.')
        
        call figure_savefig(fig, "test_small_arrays_nan.png")
        
        print *, "Test small arrays: Generated test_small_arrays_nan.png"
    end subroutine test_empty_and_small_arrays
    
    subroutine test_all_nan_array_edge_case()
        type(figure_t) :: fig
        real(8) :: x(5), y(5), nan
        integer :: i
        
        ! Get NaN value
        nan = ieee_value(nan, ieee_quiet_nan)
        
        call fig%initialize(400, 300)
        
        ! Create array with all NaN values to test edge case
        ! where valid_count = 0 and maxval/minval might fail
        x = [(nan, i=1,5)]
        y = [(nan, i=1,5)]
        
        ! Test with different line styles to ensure no crashes
        call fig%add_plot(x, y, linestyle='-')   ! solid
        call fig%add_plot(x, y, linestyle='--')  ! dashed
        call fig%add_plot(x, y, linestyle=':')   ! dotted
        call fig%add_plot(x, y, linestyle='-.')  ! dash-dot
        call fig%add_plot(x, y, linestyle='o')   ! markers only
        
        call figure_savefig(fig, "test_all_nan_edge.png")
        
        print *, "Test all NaN edge case: Generated test_all_nan_edge.png"
    end subroutine test_all_nan_array_edge_case
    
end program test_disconnected_lines