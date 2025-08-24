program test_legend_minimal
    !! Minimal legend test to verify basic functionality without backend
    use fortplot_figure_base, only: figure_t
    use fortplot_rendering, only: figure_legend
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(5) :: x, y1, y2
    integer :: i
    
    ! Generate simple test data
    x = [(real(i, wp), i=1, 5)]
    y1 = x
    y2 = x**2
    
    call fig%initialize(640, 480)
    call fig%add_plot(x, y1, label="Linear")
    call fig%add_plot(x, y2, label="Quadratic")
    
    ! Test legend API
    call figure_legend(fig)
    
    ! Verify legend state
    if (fig%legend_added) then
        print *, "SUCCESS: Legend was added to figure"
    else
        print *, "FAILURE: Legend was not added"
    end if
    
    ! Check legend data structure  
    if (fig%legend_data%num_entries == 2) then
        print *, "SUCCESS: Legend has 2 entries"
    else
        print *, "FAILURE: Expected 2 legend entries, got", fig%legend_data%num_entries
    end if
    
    print *, "Minimal legend test completed"
    
end program test_legend_minimal