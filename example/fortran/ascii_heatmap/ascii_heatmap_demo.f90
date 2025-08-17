program ascii_heatmap_demo
    !! Demonstrate ASCII heatmap visualization with various 2D functions
    use fortplot, only: figure_t, wp
    implicit none
    
    type(figure_t) :: fig
    real(wp), allocatable :: x(:), y(:), z1(:,:), z2(:,:), z3(:,:)
    integer :: i, j, nx, ny
    real(wp) :: r
    
    nx = 40
    ny = 30
    
    allocate(x(nx), y(ny))
    allocate(z1(nx, ny), z2(nx, ny), z3(nx, ny))
    
    ! Create coordinate arrays
    do i = 1, nx
        x(i) = -3.0_wp + 6.0_wp * (i - 1) / real(nx - 1, wp)
    end do
    
    do j = 1, ny
        y(j) = -2.0_wp + 4.0_wp * (j - 1) / real(ny - 1, wp)
    end do
    
    ! Generate different 2D patterns
    do i = 1, nx
        do j = 1, ny
            ! Pattern 1: Circular ripples
            r = sqrt(x(i)**2 + y(j)**2)
            z1(i, j) = sin(2.0_wp * r) / (1.0_wp + 0.1_wp * r)
            
            ! Pattern 2: Saddle point
            z2(i, j) = x(i)**2 - y(j)**2
            
            ! Pattern 3: Four peaks
            z3(i, j) = exp(-((x(i)-1)**2 + (y(j)-1)**2)) + &
                      exp(-((x(i)+1)**2 + (y(j)-1)**2)) + &
                      exp(-((x(i)-1)**2 + (y(j)+1)**2)) + &
                      exp(-((x(i)+1)**2 + (y(j)+1)**2))
        end do
    end do
    
    ! Demo 1: Circular ripples with contour_filled
    call fig%initialize(80, 25)
    call fig%add_contour_filled(x, y, z1, label="Ripples")
    call fig%set_title("ASCII Heatmap: Circular Ripples")
    
    ! Demo 2: Saddle point with pcolormesh
    block
        real(wp), allocatable :: x_edges(:), y_edges(:)
        allocate(x_edges(nx+1), y_edges(ny+1))
        
        ! Create edge coordinates
        do i = 1, nx+1
            x_edges(i) = -3.05_wp + 6.1_wp * (i - 1) / real(nx, wp)
        end do
        
        do j = 1, ny+1
            y_edges(j) = -2.05_wp + 4.1_wp * (j - 1) / real(ny, wp)
        end do
        
        call fig%initialize(80, 25)
        call fig%add_pcolormesh(x_edges, y_edges, transpose(z2))
        call fig%set_title("ASCII Heatmap: Saddle Point")
    end block
    
    ! Demo 3: Four peaks
    call fig%initialize(80, 25)
    call fig%add_contour_filled(x, y, z3, label="Four Peaks")
    call fig%set_title("ASCII Heatmap: Four Gaussian Peaks")
    
    print *, "ASCII heatmap demos saved:"
    
end program ascii_heatmap_demo