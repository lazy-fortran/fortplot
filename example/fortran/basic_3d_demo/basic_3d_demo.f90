program basic_3d_demo
    !! Basic 3D plotting demonstration
    !! Shows natural 3D API - no special initialization needed
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: x(:), y(:), z(:)
    real(wp), allocatable :: x_grid(:), y_grid(:), z_grid(:,:)
    integer :: i, j
    
    ! Example 1: 3D line plot - helix
    print *, "Creating 3D helix plot..."
    allocate(x(100), y(100), z(100))
    
    do i = 1, 100
        x(i) = cos(real(i-1, wp) * 0.1_wp)
        y(i) = sin(real(i-1, wp) * 0.1_wp)
        z(i) = real(i-1, wp) * 0.05_wp
    end do
    
    call fig%initialize(800, 600)
    call fig%add_3d_plot(x, y, z, label="3D Helix")
    call fig%set_title("3D Line Plot Example")
    
    ! Note: For now, save as PNG (GLTF export coming in Phase 3)
    call fig%savefig('3d_helix.png')
    
    ! Example 2: 3D surface plot - paraboloid
    print *, "Creating 3D surface plot..."
    call fig%initialize(800, 600)
    
    ! Create grid
    allocate(x_grid(21), y_grid(21), z_grid(21, 21))
    
    do i = 1, 21
        x_grid(i) = -2.0_wp + (i-1) * 0.2_wp
        y_grid(i) = -2.0_wp + (i-1) * 0.2_wp
    end do
    
    ! Calculate z values - paraboloid
    do i = 1, 21
        do j = 1, 21
            z_grid(i,j) = x_grid(i)**2 + y_grid(j)**2
        end do
    end do
    
    call fig%add_surface(x_grid, y_grid, z_grid, label="Paraboloid")
    call fig%set_title("3D Surface Plot Example")
    
    ! Note: For now, save as PNG (GLTF export coming in Phase 3)
    call fig%savefig('3d_surface.png')
    
    ! Example 3: Mixed 2D and 3D plots
    print *, "Creating mixed 2D/3D plot..."
    call fig%initialize(800, 600)
    
    ! Add 2D plot (z not allocated)
    deallocate(x, y)
    allocate(x(50), y(50))
    do i = 1, 50
        x(i) = real(i-1, wp) * 0.1_wp
        y(i) = sin(x(i))
    end do
    call fig%add_plot(x, y, label="2D sine wave")
    
    ! Add 3D plot
    deallocate(x, y, z)
    allocate(x(50), y(50), z(50))
    do i = 1, 50
        x(i) = real(i-1, wp) * 0.1_wp
        y(i) = cos(x(i))
        z(i) = x(i) * 0.2_wp
    end do
    call fig%add_3d_plot(x, y, z, label="3D cosine")
    
    if (fig%has_3d_plots()) then
        print *, "Figure contains 3D plots - will use GLTF export when available"
    end if
    
    call fig%set_title("Mixed 2D and 3D Plots")
    call fig%savefig('mixed_plots.png')
    
    print *, "3D demo complete!"
    print *, "Note: GLTF export will be added in Phase 3"
    
end program basic_3d_demo