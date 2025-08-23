program comprehensive_3d_demo
    !! Comprehensive 3D plotting demonstration consolidating multiple 3D features
    !! Shows line plots, scatter plots, surfaces, and GLTF/GLB export
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    print *, "=== Comprehensive 3D Plotting Demo ==="
    
    call demo_3d_line_plots()
    call demo_3d_scatter_plots()
    call demo_3d_surface_plots()
    call demo_gltf_export()
    call demo_mixed_plots()
    
    print *, "All 3D plotting demonstrations completed!"
    
contains

    subroutine demo_3d_line_plots()
        !! Demonstrate 3D line plots with helices and parametric curves
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), z(:)
        real(wp) :: theta, pi
        integer :: i, n
        
        print *, "=== 3D Line Plots Demo ==="
        pi = 4.0_wp * atan(1.0_wp)
        
        ! Basic 3D helix
        n = 100
        allocate(x(n), y(n), z(n))
        
        do i = 1, n
            x(i) = cos(real(i-1, wp) * 0.1_wp)
            y(i) = sin(real(i-1, wp) * 0.1_wp)
            z(i) = real(i-1, wp) * 0.05_wp
        end do
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_3d_plot(x, y, z, label="3D Helix")
        call title("3D Line Plot - Helix")
        call savefig('output/example/fortran/3d_plotting/3d_helix.png')
        
        ! Parametric spiral curve
        deallocate(x, y, z)
        n = 200
        allocate(x(n), y(n), z(n))
        
        do i = 1, n
            theta = 6.0_wp * pi * real(i-1, wp) / real(n-1, wp)
            x(i) = cos(theta) * exp(-theta * 0.1_wp)
            y(i) = sin(theta) * exp(-theta * 0.1_wp)
            z(i) = theta * 0.1_wp
        end do
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_3d_plot(x, y, z, label="Parametric spiral", linestyle='-')
        call title("3D Parametric Curve")
        call savefig('output/example/fortran/3d_plotting/parametric_curve.png')
        
        print *, "Created: 3d_helix.png, parametric_curve.png"
        deallocate(x, y, z)
    end subroutine demo_3d_line_plots

    subroutine demo_3d_scatter_plots()
        !! Demonstrate various 3D scatter plot patterns and combinations
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), z(:)
        real(wp) :: theta, phi, pi
        integer :: i, n
        
        print *, "=== 3D Scatter Plots Demo ==="
        pi = 4.0_wp * atan(1.0_wp)
        
        ! Distorted sphere scatter
        n = 100
        allocate(x(n), y(n), z(n))
        
        do i = 1, n
            theta = 2.0_wp * pi * real(i-1, wp) / real(n-1, wp) 
            phi = pi * (real(i-1, wp) / real(n-1, wp) - 0.5_wp)
            
            x(i) = cos(theta) * cos(phi) * (1.0_wp + 0.3_wp * sin(5.0_wp * theta))
            y(i) = sin(theta) * cos(phi) * (1.0_wp + 0.3_wp * cos(7.0_wp * theta))
            z(i) = sin(phi) * (1.0_wp + 0.2_wp * sin(3.0_wp * phi))
        end do
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_scatter(x, y, z, label="Distorted sphere")
        call title("3D Scatter Plot - Sphere Pattern")
        call savefig('output/example/fortran/3d_plotting/scatter_sphere.png')
        
        ! Multiple scatter patterns
        call figure(figsize=[8.0_wp, 6.0_wp])
        
        ! First pattern - spiral
        deallocate(x, y, z)
        n = 50
        allocate(x(n), y(n), z(n))
        
        do i = 1, n
            theta = 4.0_wp * pi * real(i-1, wp) / real(n-1, wp)
            x(i) = cos(theta) * real(i-1, wp) / real(n-1, wp) 
            y(i) = sin(theta) * real(i-1, wp) / real(n-1, wp)
            z(i) = real(i-1, wp) / real(n-1, wp)
        end do
        call add_scatter(x, y, z, label="Spiral", marker='o')
        
        ! Second pattern - cluster
        do i = 1, n
            x(i) = 0.5_wp + 0.3_wp * cos(real(i, wp) * 0.7_wp)
            y(i) = 0.5_wp + 0.3_wp * sin(real(i, wp) * 0.7_wp) 
            z(i) = 0.5_wp + 0.2_wp * sin(real(i, wp) * 0.3_wp)
        end do
        call add_scatter(x, y, z, label="Cluster", marker='s')
        
        ! Third pattern - grid
        do i = 1, n
            x(i) = mod(real(i-1, wp), 7.0_wp) / 7.0_wp - 0.5_wp
            y(i) = real((i-1)/7, wp) / 7.0_wp - 0.5_wp
            z(i) = 0.1_wp * sin(x(i) * 10.0_wp) * cos(y(i) * 10.0_wp)
        end do
        call add_scatter(x, y, z, label="Grid", marker='D')
        
        call title("Multiple 3D Scatter Patterns")
        call legend()
        call savefig('output/example/fortran/3d_plotting/scatter_multiple.png')
        
        print *, "Created: scatter_sphere.png, scatter_multiple.png"
        deallocate(x, y, z)
    end subroutine demo_3d_scatter_plots

    subroutine demo_3d_surface_plots()
        !! Demonstrate 3D surface plots with different mathematical functions
        type(figure_t) :: fig
        real(wp), allocatable :: x_grid(:), y_grid(:), z_grid(:,:)
        integer :: i, j, n
        
        print *, "=== 3D Surface Plots Demo ==="
        
        ! Paraboloid surface
        n = 21
        allocate(x_grid(n), y_grid(n), z_grid(n, n))
        
        do i = 1, n
            x_grid(i) = -2.0_wp + (i-1) * 4.0_wp / real(n-1, wp)
            y_grid(i) = -2.0_wp + (i-1) * 4.0_wp / real(n-1, wp)
        end do
        
        do i = 1, n
            do j = 1, n
                z_grid(i,j) = x_grid(i)**2 + y_grid(j)**2
            end do
        end do
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_surface(x_grid, y_grid, z_grid, label="Paraboloid")
        call title("3D Surface Plot - Paraboloid")
        call savefig('output/example/fortran/3d_plotting/surface_paraboloid.png')
        
        ! Gaussian surface
        do i = 1, n
            do j = 1, n
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_surface(x_grid, y_grid, z_grid, label="Gaussian")
        call title("3D Surface Plot - Gaussian")
        call savefig('output/example/fortran/3d_plotting/surface_gaussian.png')
        
        print *, "Created: surface_paraboloid.png, surface_gaussian.png"
        deallocate(x_grid, y_grid, z_grid)
    end subroutine demo_3d_surface_plots

    subroutine demo_gltf_export()
        !! Demonstrate GLTF and GLB export formats for 3D plots
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), z(:), x_grid(:), y_grid(:), z_grid(:,:)
        real(wp) :: t, pi
        integer :: i, j, n
        
        print *, "=== GLTF/GLB Export Demo ==="
        pi = 4.0_wp * atan(1.0_wp)
        
        ! Create helix for GLTF/GLB export
        n = 100
        allocate(x(n), y(n), z(n))
        
        do i = 1, n
            t = real(i-1, wp) / real(n-1, wp) * 4.0_wp * pi
            x(i) = cos(t)
            y(i) = sin(t)
            z(i) = t / (4.0_wp * pi)
        end do
        
        ! Export helix as GLTF (text format)
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_3d_plot(x, y, z, label="Helix")
        call savefig('output/example/fortran/3d_plotting/helix_demo.gltf')
        
        ! Export helix as GLB (binary format)
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_3d_plot(x, y, z, label="Helix")
        call savefig('output/example/fortran/3d_plotting/helix_demo.glb')
        
        deallocate(x, y, z)
        
        ! Create surface for GLTF/GLB export
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
        
        ! Export surface as GLTF and GLB
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_surface(x_grid, y_grid, z_grid, label="Surface")
        call savefig('output/example/fortran/3d_plotting/surface_demo.gltf')
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call add_surface(x_grid, y_grid, z_grid, label="Surface")
        call savefig('output/example/fortran/3d_plotting/surface_demo.glb')
        
        print *, "Created: helix_demo.gltf/.glb, surface_demo.gltf/.glb"
        print *, "View with: https://gltf-viewer.donmccurdy.com/"
        deallocate(x_grid, y_grid, z_grid)
    end subroutine demo_gltf_export

    subroutine demo_mixed_plots()
        !! Demonstrate mixing 2D and 3D plots, and combining different 3D types
        type(figure_t) :: fig
        real(wp), allocatable :: x(:), y(:), z(:)
        integer :: i, n
        
        print *, "=== Mixed 2D/3D Plots Demo ==="
        
        n = 50
        allocate(x(n), y(n), z(n))
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        
        ! Add 2D plot
        do i = 1, n
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = sin(x(i))
        end do
        call add_plot(x, y, label="2D sine wave")
        
        ! Add 3D plot
        do i = 1, n
            x(i) = real(i-1, wp) * 0.1_wp
            y(i) = cos(x(i))
            z(i) = x(i) * 0.2_wp
        end do
        call add_3d_plot(x, y, z, label="3D cosine")
        
        call title("Mixed 2D and 3D Plots")
        call legend()
        call savefig('output/example/fortran/3d_plotting/mixed_plots.png')
        
        ! Combination of 3D scatter and line plots
        call figure(figsize=[8.0_wp, 6.0_wp])
        
        ! First add scatter points
        n = 20
        deallocate(x, y,z)
        allocate(x(n), y(n), z(n))
        
        do i = 1, n
            x(i) = cos(real(i-1, wp) * 0.3_wp) * exp(-real(i-1, wp) * 0.05_wp)
            y(i) = sin(real(i-1, wp) * 0.3_wp) * exp(-real(i-1, wp) * 0.05_wp)
            z(i) = real(i-1, wp) * 0.05_wp
        end do
        call add_scatter(x, y, z, label="Sample points", marker='o')
        
        ! Then add continuous curve
        n = 200
        deallocate(x, y, z)
        allocate(x(n), y(n), z(n))
        
        do i = 1, n
            x(i) = cos(real(i-1, wp) * 0.03_wp) * exp(-real(i-1, wp) * 0.005_wp)
            y(i) = sin(real(i-1, wp) * 0.03_wp) * exp(-real(i-1, wp) * 0.005_wp)
            z(i) = real(i-1, wp) * 0.005_wp
        end do
        call add_3d_plot(x, y, z, label="Continuous curve", linestyle='-')
        
        call title("3D Scatter + Line Combination")
        call legend()
        call savefig('output/example/fortran/3d_plotting/scatter_line_combo.png')
        
        print *, "Created: mixed_plots.png, scatter_line_combo.png"
        deallocate(x, y, z)
    end subroutine demo_mixed_plots

end program comprehensive_3d_demo