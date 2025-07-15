program debug_pdf_parentheses
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(5) :: x, y
    integer :: i
    
    print *, "=== Debug PDF Greek Letters in Parentheses ==="
    
    ! Create simple test data
    x = [(real(i, wp), i = 1, 5)]
    y = [1.0_wp, 2.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]
    
    ! Initialize figure
    call fig%initialize(width=800, height=600)
    
    ! Add plot with parentheses test
    call fig%add_plot(x, y, label="Test: sin(\omega t) and f(\alpha, \beta)")
    
    ! Set title with various parentheses cases
    call fig%set_title("Greek in parentheses: (\omega) sin(\omega) f(\alpha,\beta)")
    call fig%set_xlabel("x (\mu m)")
    call fig%set_ylabel("y (\Psi)")
    
    ! Add legend
    call fig%legend("upper right")
    
    ! Save PDF version
    call fig%savefig("debug_pdf_parentheses.pdf")
    
    print *, "Saved debug_pdf_parentheses.pdf"
    print *, "Check if omega, alpha, beta appear correctly inside parentheses"
    
end program debug_pdf_parentheses