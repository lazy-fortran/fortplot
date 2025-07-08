program debug_vertex_encoding
    !! Debug program to verify vertex encoding
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), dimension(4) :: x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
    real(wp), dimension(4) :: y = [0.0_wp, 1.0_wp, 0.0_wp, -1.0_wp]
    real(wp), dimension(4) :: z = [0.0_wp, 0.5_wp, 1.0_wp, 0.5_wp]
    
    print *, "Creating GLTF with vertex data..."
    
    call fig%initialize(640, 480)
    call fig%add_3d_plot(x, y, z, label="Test line")
    call fig%savefig("debug_vertex.gltf")
    
    print *, "GLTF file created!"
    
end program debug_vertex_encoding