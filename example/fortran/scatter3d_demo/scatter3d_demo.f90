program scatter3d_demo
    !! Demonstrates 3D scatter plots with fortplotlib
    !! Replicates matplotlib's scatter3d example with cleaner API
    !!
    !! Key differences from matplotlib:
    !! - No need for separate mplot3d import
    !! - No ax object, just use fig directly  
    !! - Cleaner argument order: label before linestyle
    !! - Automatic 3D detection based on data
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: x(:), y(:), z(:), c(:)
    real(wp) :: theta, phi
    integer :: i, n
    real(wp), parameter :: PI = 3.14159265358979323846_wp
    
    ! Generate random 3D data points
    n = 100
    allocate(x(n), y(n), z(n), c(n))
    
    ! Create random points in 3D space
    ! Using a deterministic pattern for reproducibility
    do i = 1, n
        ! Generate points on a distorted sphere
        theta = 2.0_wp * PI * real(i-1, wp) / real(n-1, wp) 
        phi = PI * (real(i-1, wp) / real(n-1, wp) - 0.5_wp)
        
        ! Add some randomness using a simple hash function
        x(i) = cos(theta) * cos(phi) * (1.0_wp + 0.3_wp * sin(5.0_wp * theta))
        y(i) = sin(theta) * cos(phi) * (1.0_wp + 0.3_wp * cos(7.0_wp * theta))
        z(i) = sin(phi) * (1.0_wp + 0.2_wp * sin(3.0_wp * phi))
        
        ! Color based on z-coordinate
        c(i) = z(i)
    end do
    
    ! Create figure - no special 3D initialization needed
    call fig%initialize(800, 600)
    
    ! Add 3D scatter plot - fortplotlib automatically detects 3D data
    ! Clean API: label before style options, dedicated scatter method
    call fig%add_scatter(x, y, z, label="3D Scatter")
    
    ! Set labels and title
    call fig%set_xlabel('X Label')
    call fig%set_ylabel('Y Label')
    ! Note: set_zlabel will be available when full 3D axes are implemented
    call fig%set_title('3D Scatter Plot Demo')
    
    ! Save the figure
    call fig%savefig('output/example/fortran/scatter3d_demo/scatter3d_demo.png')
    
    ! Also demonstrate multiple 3D scatter plots with different markers
    call fig%initialize(800, 600)
    
    ! Generate three different point clouds
    deallocate(x, y, z)
    n = 50
    allocate(x(n), y(n), z(n))
    
    ! First cloud - spiral
    do i = 1, n
        theta = 4.0_wp * PI * real(i-1, wp) / real(n-1, wp)
        x(i) = cos(theta) * real(i-1, wp) / real(n-1, wp) 
        y(i) = sin(theta) * real(i-1, wp) / real(n-1, wp)
        z(i) = real(i-1, wp) / real(n-1, wp)
    end do
    call fig%add_scatter(x, y, z, label="Spiral", marker='o')
    
    ! Second cloud - random cluster
    do i = 1, n
        x(i) = 0.5_wp + 0.3_wp * cos(real(i, wp) * 0.7_wp)
        y(i) = 0.5_wp + 0.3_wp * sin(real(i, wp) * 0.7_wp) 
        z(i) = 0.5_wp + 0.2_wp * sin(real(i, wp) * 0.3_wp)
    end do
    call fig%add_scatter(x, y, z, label="Cluster", marker='s')
    
    ! Third cloud - grid pattern
    do i = 1, n
        x(i) = mod(real(i-1, wp), 7.0_wp) / 7.0_wp - 0.5_wp
        y(i) = real((i-1)/7, wp) / 7.0_wp - 0.5_wp
        z(i) = 0.1_wp * sin(x(i) * 10.0_wp) * cos(y(i) * 10.0_wp)
    end do
    call fig%add_scatter(x, y, z, label="Grid", marker='D')
    
    call fig%set_title('Multiple 3D Scatter Plots')
    call fig%legend()
    call fig%savefig('output/example/fortran/scatter3d_demo/scatter3d_multi.png')
    
    ! Demonstrate 3D line plot combined with scatter
    call fig%initialize(800, 600)
    
    ! Generate parametric curve
    n = 200
    deallocate(x, y, z)
    allocate(x(n), y(n), z(n))
    
    do i = 1, n
        theta = 6.0_wp * PI * real(i-1, wp) / real(n-1, wp)
        x(i) = cos(theta) * exp(-theta * 0.1_wp)
        y(i) = sin(theta) * exp(-theta * 0.1_wp)
        z(i) = theta * 0.1_wp
    end do
    
    ! Add as continuous line
    call fig%add_3d_plot(x, y, z, label="Parametric curve", linestyle='-')
    
    ! Add scatter points along the curve
    deallocate(x, y, z)
    n = 20
    allocate(x(n), y(n), z(n))
    
    do i = 1, n
        theta = 6.0_wp * PI * real(i-1, wp) / real(n-1, wp)
        x(i) = cos(theta) * exp(-theta * 0.1_wp)
        y(i) = sin(theta) * exp(-theta * 0.1_wp)
        z(i) = theta * 0.1_wp
    end do
    
    call fig%add_scatter(x, y, z, label="Sample points", marker='o')
    
    call fig%set_title('3D Parametric Curve with Scatter Points')
    call fig%legend()
    call fig%savefig('output/example/fortran/scatter3d_demo/scatter3d_curve.png')
    
    print *, "3D scatter plot demos complete!"
    print *, "Generated files:"
    print *, "  - scatter3d_demo.png   (basic 3D scatter)"
    print *, "  - scatter3d_multi.png  (multiple scatter plots)"
    print *, "  - scatter3d_curve.png  (parametric curve with points)"
    
end program scatter3d_demo