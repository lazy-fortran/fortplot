program gltf_glb_demo
    !! Demonstrate GLTF and GLB export for 3D plots
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: t(:), x(:), y(:), z(:)
    real(wp), allocatable :: x_grid(:), y_grid(:), z_grid(:,:)
    integer :: i, j, n
    real(wp) :: pi
    
    pi = 4.0_wp * atan(1.0_wp)
    
    ! Create helix data
    n = 100
    allocate(t(n), x(n), y(n), z(n))
    
    do i = 1, n
        t(i) = real(i-1, wp) / real(n-1, wp) * 4.0_wp * pi
        x(i) = cos(t(i))
        y(i) = sin(t(i))
        z(i) = t(i) / (4.0_wp * pi)
    end do
    
    ! Save helix as GLTF (text format)
    print *, "Creating helix GLTF file..."
    call fig%initialize(800, 600)
    call fig%add_3d_plot(x, y, z, label="Helix")
    call fig%savefig('output/example/fortran/gltf_glb_demo/helix_demo.gltf')
    
    ! Save helix as GLB (binary format)
    print *, "Creating helix GLB file..."
    call fig%initialize(800, 600)
    call fig%add_3d_plot(x, y, z, label="Helix")
    call fig%savefig('output/example/fortran/gltf_glb_demo/helix_demo.glb')
    
    ! Create surface data
    n = 20
    allocate(x_grid(n), y_grid(n), z_grid(n,n))
    
    do i = 1, n
        x_grid(i) = -2.0_wp + 4.0_wp * real(i-1, wp) / real(n-1, wp)
        y_grid(i) = -2.0_wp + 4.0_wp * real(i-1, wp) / real(n-1, wp)
    end do
    
    do i = 1, n
        do j = 1, n
            z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
        end do
    end do
    
    ! Save surface as GLTF
    print *, "Creating surface GLTF file..."
    call fig%initialize(800, 600)
    call fig%add_surface(x_grid, y_grid, z_grid)
    call fig%savefig('output/example/fortran/gltf_glb_demo/surface_demo.gltf')
    
    ! Save surface as GLB
    print *, "Creating surface GLB file..."
    call fig%initialize(800, 600)
    call fig%add_surface(x_grid, y_grid, z_grid)
    call fig%savefig('output/example/fortran/gltf_glb_demo/surface_demo.glb')
    
    print *, "GLTF/GLB demo complete!"
    print *, "Generated files:"
    print *, "  - helix_demo.gltf   (text format, can be inspected)"
    print *, "  - helix_demo.glb    (binary format, more compact)"
    print *, "  - surface_demo.gltf (text format)"
    print *, "  - surface_demo.glb  (binary format)"
    print *, ""
    print *, "These files can be viewed in:"
    print *, "  - https://gltf-viewer.donmccurdy.com/"
    print *, "  - Windows 3D Viewer"
    print *, "  - Blender"
    print *, "  - Any GLTF-compatible viewer"
    
end program gltf_glb_demo