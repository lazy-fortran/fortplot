program debug_pdf_minimal
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(2) :: x, y
    
    print *, "=== Minimal PDF Greek Letters Test ==="
    
    ! Create minimal data
    x = [0.0_wp, 1.0_wp]
    y = [0.0_wp, 1.0_wp]
    
    ! Initialize figure
    call fig%initialize(width=600, height=400)
    
    ! Test various cases
    call fig%add_plot(x, y, label="Test 1: omega=ω")
    call fig%add_plot(x, y+0.1_wp, label="Test 2: (ω)")
    call fig%add_plot(x, y+0.2_wp, label="Test 3: sin(ω)")
    call fig%add_plot(x, y+0.3_wp, label="Test 4: f(α,β)")
    
    ! Simple title
    call fig%set_title("PDF Test: sin(ω) and (α)")
    
    ! Add legend
    call fig%legend("upper left")
    
    ! Save PDF only
    call fig%savefig("debug_pdf_minimal.pdf")
    
    print *, "Saved debug_pdf_minimal.pdf"
    print *, "Check: Do you see omega/alpha/beta inside parentheses?"
    
end program debug_pdf_minimal