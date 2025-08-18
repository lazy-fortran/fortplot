program debug_3d_marker_visible
    !! Debug 3D marker visibility with forced large markers
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3), z(3)
    
    ! Simple test data
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 2.0_wp]
    z = [0.0_wp, 1.0_wp, 2.0_wp]
    
    call fig%initialize(800, 600)
    
    ! Add with large marker size
    call fig%add_scatter_3d(x, y, z, label="Test points", marker='o', markersize=20.0_wp)
    
    call fig%set_title('Debug 3D Scatter - Large Markers')
    call fig%legend()
    call fig%savefig('output/debug_3d_marker_visible.png')
    
    print *, "Debug plot with large markers saved"
end program debug_3d_marker_visible