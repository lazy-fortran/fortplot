program test_pdf_scale_regression
    !! Test to demonstrate PDF scale regression issue #985
    !! This test generates a simple plot that should fill the available plot area
    !! but currently generates smaller plots with centering due to aspect ratio preservation
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    implicit none
    
    type(figure_t) :: fig
    real(wp), parameter :: x_data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp), parameter :: y_data(5) = [1.0_wp, 4.0_wp, 2.0_wp, 8.0_wp, 5.0_wp]
    
    ! Create figure
    call fig%initialize(800, 600)
    call fig%plot(x_data, y_data)
    call fig%set_title("PDF Scale Regression Test - Issue #985")
    call fig%set_xlabel("X values")
    call fig%set_ylabel("Y values")
    
    ! Save PDF - this should fill the plot area but currently creates smaller plot
    call fig%save("test_pdf_scale_regression.pdf")
    call fig%save("test_pdf_scale_regression.png")  ! For comparison
    
    print *, "Generated test files:"
    print *, "  test_pdf_scale_regression.pdf (current - smaller scale)"
    print *, "  test_pdf_scale_regression.png (reference - correct scale)"
    print *, "Compare the scaling between PDF and PNG outputs"
    
end program test_pdf_scale_regression