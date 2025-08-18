program debug_2d_scatter
    !! Debug 2D scatter rendering for comparison
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3)
    
    ! Simple 2D points
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp]
    
    call fig%initialize(800, 600)
    call fig%add_scatter_2d(x, y, label="2D points", marker='o')
    call fig%set_title('2D Scatter Test')
    call fig%legend()
    call fig%savefig('output/debug_2d_scatter.png')
    
    print *, "2D scatter test saved"
end program debug_2d_scatter