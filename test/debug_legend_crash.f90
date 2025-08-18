program debug_legend_crash
    !! Debug exact crash scenario from legend test
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(20) :: x, y1, y2, y3, y4
    integer :: i
    
    print *, "=== DEBUG: Exact Legend Crash Scenario ==="
    
    ! Same data as crashing test
    x = [(real(i, wp), i=1, 20)]
    y1 = x
    y2 = x**2
    y3 = sqrt(x)
    y4 = log(x)
    
    call fig%initialize(640, 480)
    call fig%add_plot(x, y1, label="Linear")
    call fig%add_plot(x, y2, label="Quadratic")
    call fig%add_plot(x, y3, label="Square Root")
    call fig%add_plot(x, y4, label="Logarithmic")
    
    print *, "Legend upper left..."
    call fig%legend(location="upper left")
    print *, "Savefig 1..."
    call fig%savefig('/tmp/debug_legend_ul.png')
    
    print *, "Legend upper right..."
    call fig%legend(location="upper right")
    print *, "Savefig 2..."
    call fig%savefig('/tmp/debug_legend_ur.png')
    
    print *, "Legend lower left..."
    call fig%legend(location="lower left")
    print *, "Savefig 3..."
    call fig%savefig('/tmp/debug_legend_ll.png')
    
    print *, "Legend lower right..."
    call fig%legend(location="lower right")
    print *, "Savefig 4..."
    call fig%savefig('/tmp/debug_legend_lr.png')
    
    print *, "Repeat save (this might crash)..."
    call fig%savefig('/tmp/debug_legend_lr2.png')
    
    print *, "=== All operations completed successfully ==="
    
end program debug_legend_crash