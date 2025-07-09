program debug_3d_coords
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(8) :: x(3), y(3), z(3)
    
    ! Simple triangle in 3D
    x = [0.0_8, 1.0_8, 0.5_8]
    y = [0.0_8, 0.0_8, 1.0_8]
    z = [0.0_8, 0.5_8, 1.0_8]
    
    print *, "3D data points:"
    print *, "Point 1:", x(1), y(1), z(1)
    print *, "Point 2:", x(2), y(2), z(2)  
    print *, "Point 3:", x(3), y(3), z(3)
    
    call fig%initialize(width=800, height=600)
    call fig%add_3d_plot(x, y, z, label="Triangle", linestyle='-')
    call fig%savefig('output/test/debug_3d_triangle.png')
    
end program debug_3d_coords