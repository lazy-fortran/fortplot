program debug_3d_data_bounds
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(8) :: t(100), x(100), y(100), z(100)
    integer :: i
    
    ! Generate helix data
    do i = 1, 100
        t(i) = real(i-1, 8) * 4.0_8 * 3.14159265359_8 / 99.0_8
        x(i) = sin(t(i))
        y(i) = cos(t(i))  
        z(i) = t(i) / (4.0_8 * 3.14159265359_8)
    end do
    
    print *, "Data bounds:"
    print *, "X: min=", minval(x), " max=", maxval(x)
    print *, "Y: min=", minval(y), " max=", maxval(y)
    print *, "Z: min=", minval(z), " max=", maxval(z)
    
    call fig%initialize(width=800, height=600)
    call fig%add_scatter(x, y, z, label="3D Scatter", marker='o')
    call fig%savefig('output/test/debug_3d_bounds.png')
    
end program debug_3d_data_bounds