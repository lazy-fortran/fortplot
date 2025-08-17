program test_pdf_ylabel_fix
    !! Test program to visually verify PDF Y-axis label overlap fix
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp), parameter :: y_data(10) = [-0.05_wp, -0.04_wp, -0.03_wp, -0.02_wp, -0.01_wp, &
                                         0.0_wp, 0.01_wp, 0.02_wp, 0.03_wp, 0.04_wp]
    real(wp), parameter :: x_data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                         6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]
    
    print *, "Creating test plot to verify PDF Y-axis label overlap fix..."
    
    ! Create plot with very tight Y-axis range to force close tick spacing
    call fig%initialize(640, 480)
    call fig%add_plot(x_data, y_data)
    call fig%set_ylim(-0.06_wp, 0.05_wp)  ! Very tight range around origin
    call fig%set_xlabel("X values")
    call fig%set_ylabel("Y values near origin")
    call fig%set_title("PDF Y-axis Overlap Fix Test")
    
    ! Save to PDF - labels should now be properly spaced without overlap
    call fig%savefig("test_ylabel_fix.pdf")
    
    print *, "Test PDF created: test_ylabel_fix.pdf"
    print *, "Check the PDF to verify Y-axis labels are properly spaced without overlap"
    
end program test_pdf_ylabel_fix