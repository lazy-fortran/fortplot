program debug_scatter_3d
    !! Debug 3D scatter rendering
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3), z(3)
    
    ! Simple test data with different ranges
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp]
    z = [0.0_wp, 1.0_wp, 2.0_wp]
    
    call fig%initialize(800, 600)
    call fig%add_scatter_3d(x, y, z, label="Test points", marker='o')
    call fig%set_title('Debug 3D Scatter')
    call fig%legend()
    call fig%savefig('output/debug_scatter_3d.png')
    
    print *, "Debug plot saved"
end program debug_scatter_3d