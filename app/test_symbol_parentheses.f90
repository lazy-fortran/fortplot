program test_symbol_parentheses
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(10) :: x, y
    integer :: i
    
    ! Create test data
    x = [(real(i, wp) * 0.1_wp, i = 1, 10)]
    y = sin(x * 6.28_wp)
    
    ! Initialize figure
    call fig%initialize(width=800, height=600)
    
    ! Add plot with test label
    call fig%add_plot(x, y, label="Test: (ω) and sin(ω)")
    
    ! Set title
    call fig%set_title("Symbol Font Test: (ω) vs sin(ω)")
    
    ! Add legend
    call fig%legend("upper right")
    
    ! Save PDF
    call fig%savefig("test_symbol_parentheses.pdf")
    
    print *, "Check test_symbol_parentheses.pdf to see if parentheses work correctly"
    
end program test_symbol_parentheses