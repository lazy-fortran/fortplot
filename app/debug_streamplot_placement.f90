program debug_streamplot_placement
    !! Debug streamplot placement to understand what's happening
    use fortplot
    use fortplot_streamline_placement
    use fortplot_streamline
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: nx = 20, ny = 20
    real(real64), dimension(nx) :: x
    real(real64), dimension(ny) :: y
    real, allocatable :: seed_x_old(:), seed_y_old(:), seed_x_new(:), seed_y_new(:)
    integer :: i, j, n_seeds_old, n_seeds_new
    type(stream_mask_t) :: mask
    
    ! Create grid
    do i = 1, nx
        x(i) = -2.0_real64 + 4.0_real64 * real(i-1, real64) / real(nx-1, real64)
    end do
    
    do j = 1, ny
        y(j) = -2.0_real64 + 4.0_real64 * real(j-1, real64) / real(ny-1, real64)
    end do
    
    ! Test old vs new seed generation
    print *, "=== Comparing Old vs New Seed Generation ==="
    print *, "Grid size:", nx, "x", ny
    print *, "X range:", x(1), "to", x(nx)
    print *, "Y range:", y(1), "to", y(ny)
    
    ! Old method
    call calculate_seed_points(real(x), real(y), 1.0, seed_x_old, seed_y_old, n_seeds_old)
    print *, "Old method: Generated", n_seeds_old, "seeds"
    
    ! New method
    call calculate_seed_points_matplotlib(real(x), real(y), 1.0_real64, seed_x_new, seed_y_new, n_seeds_new, mask)
    print *, "New method: Generated", n_seeds_new, "seeds"
    print *, "Mask size:", mask%nx, "x", mask%ny
    
    ! Show first few seeds from each method
    print *, "=== First 10 seeds comparison ==="
    print *, "Old method seeds:"
    do i = 1, min(10, n_seeds_old)
        print *, "  ", i, ":", seed_x_old(i), seed_y_old(i)
    end do
    
    print *, "New method seeds:"
    do i = 1, min(10, n_seeds_new)
        print *, "  ", i, ":", seed_x_new(i), seed_y_new(i)
    end do
    
    ! Test with smaller density
    print *, "=== Testing with density=0.5 ==="
    call calculate_seed_points_matplotlib(real(x), real(y), 0.5_real64, seed_x_new, seed_y_new, n_seeds_new, mask)
    print *, "Density 0.5: Generated", n_seeds_new, "seeds, mask size:", mask%nx, "x", mask%ny
    
    print *, "Debug completed!"
    
end program debug_streamplot_placement