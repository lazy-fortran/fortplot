program test_gltf_output
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(8) :: x(5), y(5), z(5)
    
    ! Create 3D data
    x = [0.0d0, 1.0d0, 2.0d0, 3.0d0, 4.0d0]
    y = [0.0d0, 1.0d0, 0.0d0, -1.0d0, 0.0d0]
    z = [0.0d0, 0.5d0, 1.0d0, 0.5d0, 0.0d0]
    
    call fig%initialize(640, 480)
    call fig%add_3d_plot(x, y, z, label="3D line")
    call fig%savefig("output_3d.gltf")
    
    print *, "Created output_3d.gltf"
    
end program test_gltf_output