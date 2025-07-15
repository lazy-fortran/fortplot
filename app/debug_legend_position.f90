program debug_legend_position
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(5) :: x, y1
    integer :: i
    
    print *, "=== Debug Legend Position ==="
    
    ! Create simple test data
    x = [(real(i, wp), i = 1, 5)]
    y1 = [1.0_wp, 2.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]
    
    ! Initialize figure with explicit ASCII backend
    call fig%initialize(width=80, height=24)
    
    ! Add plot with specific legend label
    call fig%add_plot(x, y1, label="α damped: sin(ω t)e^{-λτ}")
    
    ! Set title and labels
    call fig%set_title("Test Legend Position")
    call fig%set_xlabel("X")
    call fig%set_ylabel("Y")
    
    ! Add legend
    call fig%legend("upper right")
    
    ! Save ASCII version
    call fig%savefig("debug_legend_position.txt")
    
    print *, "Saved debug_legend_position.txt"
    print *, "Expected legend width for 'α damped: sin(ω t)e^{-λτ}' + '-- ' = ", len_trim("-- α damped: sin(ω t)e^{-λτ}"), " characters"
    
end program debug_legend_position