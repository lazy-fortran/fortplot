program debug_ascii_legend
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(5) :: x, y1, y2, y3
    integer :: i
    
    print *, "=== Debug ASCII Legend Truncation ==="
    
    ! Create simple test data
    x = [(real(i, wp), i = 1, 5)]
    y1 = [1.0_wp, 2.0_wp, 3.0_wp, 2.0_wp, 1.0_wp]
    y2 = [2.0_wp, 3.0_wp, 4.0_wp, 3.0_wp, 2.0_wp]
    y3 = [3.0_wp, 4.0_wp, 5.0_wp, 4.0_wp, 3.0_wp]
    
    ! Initialize figure  
    call fig%initialize(width=800, height=600)
    
    ! Add plots with long legend labels
    call fig%add_plot(x, y1, label="This is a very long legend label for line 1")
    call fig%add_plot(x, y2, label="Another extremely long legend label for line 2")
    call fig%add_plot(x, y3, label="Yet another super long legend label for line 3")
    
    ! Set title and labels
    call fig%set_title("Test Long Legend Labels")
    call fig%set_xlabel("X axis")
    call fig%set_ylabel("Y axis")
    
    ! Add legend
    call fig%legend("upper right")
    
    ! Save ASCII version
    call fig%savefig("debug_ascii_legend.txt")
    
    print *, "Saved debug_ascii_legend.txt"
    
end program debug_ascii_legend