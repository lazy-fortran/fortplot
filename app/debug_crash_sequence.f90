program debug_crash_sequence
    !! Debug the exact crash sequence from failing test
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(20) :: x, y1, y2, y3, y4
    integer :: i
    
    print *, "=== DEBUG: Exact Crash Sequence ==="
    
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
    
    print *, "Step 1: Legend upper left + save to output/"
    call fig%legend(location="upper left")
    call fig%savefig('output/test/test_legend_box_improvements/test_legend_margins_ul.png')
    
    print *, "Step 2: Legend upper right + save to output/"
    call fig%legend(location="upper right")
    call fig%savefig('output/test/test_legend_box_improvements/test_legend_margins_ur.png')
    
    print *, "Step 3: Legend lower left + save to output/"
    call fig%legend(location="lower left")
    call fig%savefig('output/test/test_legend_box_improvements/test_legend_margins_ll.png')
    
    print *, "Step 4: Legend lower right + save to output/"
    call fig%legend(location="lower right")
    call fig%savefig('output/test/test_legend_box_improvements/test_legend_margins_lr.png')
    
    print *, "Step 5: Save to /tmp/ (this should crash)"
    call fig%savefig('/tmp/test/test_legend_margins_ul.png')
    
    print *, "=== If we reach here, no crash occurred ==="
    
end program debug_crash_sequence