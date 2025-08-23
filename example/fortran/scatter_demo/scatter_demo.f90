program scatter_demo
    !! Demonstrates 2D scatter plots with fortplotlib
    !! Shows the clean scatter API for 2D data
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: x(:), y(:)
    integer :: i, n
    real(wp), parameter :: PI = 3.14159265358979323846_wp
    
    ! Example 1: Basic 2D scatter plot
    n = 50
    allocate(x(n), y(n))
    
    ! Generate random-looking data
    do i = 1, n
        x(i) = real(i-1, wp) / real(n-1, wp) * 10.0_wp
        y(i) = sin(x(i)) + 0.2_wp * cos(5.0_wp * x(i) + real(i, wp))
    end do
    
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_scatter(x, y, label="Data points")
    call xlabel('X')
    call ylabel('Y')
    call title('Basic 2D Scatter Plot')
    call savefig('output/example/fortran/scatter_demo/scatter_basic.png')
    
    ! Example 2: Multiple scatter plots with different markers
    call figure(figsize=[8.0_wp, 6.0_wp])
    
    ! Dataset 1 - sine wave with noise
    do i = 1, n
        x(i) = real(i-1, wp) / real(n-1, wp) * 4.0_wp * PI
        y(i) = sin(x(i)) + 0.1_wp * sin(10.0_wp * x(i))
    end do
    call add_scatter(x, y, label="Sine", marker='o')
    
    ! Dataset 2 - cosine wave with noise
    do i = 1, n
        y(i) = cos(x(i)) + 0.1_wp * cos(15.0_wp * x(i))
    end do
    call add_scatter(x, y, label="Cosine", marker='s')
    
    ! Dataset 3 - exponential decay
    do i = 1, n
        y(i) = exp(-x(i) * 0.3_wp) * (1.0_wp + 0.1_wp * sin(8.0_wp * x(i)))
    end do
    call add_scatter(x, y, label="Exponential", marker='D')
    
    call xlabel('X')
    call ylabel('Y')
    call title('Multiple 2D Scatter Plots')
    call legend()
    call savefig('output/example/fortran/scatter_demo/scatter_multi.png')
    
    ! Example 3: Scatter with custom marker sizes (when implemented)
    call figure(figsize=[8.0_wp, 6.0_wp])
    
    ! Create data with varying density
    n = 100
    deallocate(x, y)
    allocate(x(n), y(n))
    
    do i = 1, n
        ! Gaussian-like distribution
        x(i) = 2.0_wp * (real(i-1, wp) / real(n-1, wp) - 0.5_wp) * 3.0_wp
        y(i) = exp(-x(i)**2) + 0.05_wp * sin(20.0_wp * x(i))
    end do
    
    call add_scatter(x, y, label="Gaussian distribution")
    call xlabel('X')
    call ylabel('Y')
    call title('2D Scatter Plot - Gaussian Distribution')
    call savefig('output/example/fortran/scatter_demo/scatter_gaussian.png')
    
    print *, "2D scatter plot demos complete!"
    print *, "Generated files:"
    print *, "  - scatter_basic.png    (basic 2D scatter)"
    print *, "  - scatter_multi.png    (multiple datasets)"
    print *, "  - scatter_gaussian.png (Gaussian distribution)"
    
end program scatter_demo