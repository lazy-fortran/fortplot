program test_missing_markers_986
    !! Test to demonstrate missing markers issue #986
    !! This test creates scatter plots with markers that should appear in both PNG and PDF
    !! but markers may be missing in PDF output
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    implicit none
    
    type(figure_t) :: fig
    real(wp), parameter :: x_data(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp]
    real(wp), parameter :: y_data(6) = [2.0_wp, 4.0_wp, 3.0_wp, 6.0_wp, 5.0_wp, 7.0_wp]
    
    ! Create figure
    call fig%initialize(800, 600)
    
    ! Add scatter plot with different marker styles  
    call fig%scatter(x_data(1:2), y_data(1:2), marker='o', label='Circle markers')
    call fig%scatter(x_data(3:4), y_data(3:4), marker='s', label='Square markers')  
    call fig%scatter(x_data(5:6), y_data(5:6), marker='^', label='Triangle markers')
    
    call fig%set_title("Missing Markers Test - Issue #986")
    call fig%set_xlabel("X values")
    call fig%set_ylabel("Y values")
    call fig%legend()
    
    ! Save both formats for comparison
    call fig%save("test_missing_markers_986.pdf")  ! May have missing markers
    call fig%save("test_missing_markers_986.png")  ! Should show markers correctly
    
    print *, "Generated marker test files:"
    print *, "  test_missing_markers_986.pdf (check for missing markers)"
    print *, "  test_missing_markers_986.png (reference with markers)"
    print *, "Compare to identify if markers are missing in PDF output"
    
end program test_missing_markers_986