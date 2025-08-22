program test_pdf_parentheses
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(2), y(2)
    
    x = [0.0_wp, 1.0_wp]
    y = [0.0_wp, 1.0_wp]
    
    call fig%initialize(600, 400)
    call fig%add_plot(x, y, label="sin(omega)")
    call figure_savefig(fig, "test_parentheses.pdf")
    
    print *, "Test completed"
    
end program test_pdf_parentheses