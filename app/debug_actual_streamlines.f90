program debug_actual_streamlines
    !! Debug what streamlines are actually being generated
    use fortplot
    use fortplot_streamline
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: nx = 20, ny = 20
    real(real64), dimension(nx) :: x
    real(real64), dimension(ny) :: y
    real(real64), dimension(nx, ny) :: u, v
    integer :: i, j
    type(figure_t) :: fig
    real, allocatable :: seed_x(:), seed_y(:)
    integer :: n_seeds
    
    ! Create same grid as streamplot_demo
    do i = 1, nx
        x(i) = -2.0_real64 + 4.0_real64 * real(i-1, real64) / real(nx-1, real64)
    end do
    
    do j = 1, ny
        y(j) = -2.0_real64 + 4.0_real64 * real(j-1, real64) / real(ny-1, real64)
    end do
    
    ! Create circular flow field
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -y(j)
            v(i,j) = x(i)
        end do
    end do
    
    ! Get just the seed points to see where they are
    call calculate_seed_points(real(x), real(y), 1.0, seed_x, seed_y, n_seeds)
    
    print *, "Total seed points generated:", n_seeds
    print *, "Grid bounds: x=", x(1), "to", x(nx), ", y=", y(1), "to", y(ny)
    
    ! Show first 20 seed points
    print *, "First 20 seed points:"
    do i = 1, min(20, n_seeds)
        print *, i, ":", seed_x(i), seed_y(i)
    end do
    
    ! Create figure with just seed points as dots to see placement
    call fig%initialize(800, 600)
    
    ! Plot all seed points as small dots
    call fig%add_plot(real(seed_x(1:min(100, n_seeds)), real64), &
                      real(seed_y(1:min(100, n_seeds)), real64), &
                      linestyle='o')
    
    call fig%set_title('Seed Point Placement (showing actual positions)')
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%savefig('debug_seed_positions.png')
    
    print *, "Created debug_seed_positions.png"
    print *, "This shows where seeds are actually placed"
    
end program debug_actual_streamlines