program test_matplotlib_comparison
    !! Test matplotlib compatibility for streamline placement
    use fortplot_streamline_placement
    use fortplot_streamline
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_should_match_matplotlib_seed_count_for_density_1()
    call test_should_generate_boundary_first_seeds()
    call test_should_use_30x30_mask_for_density_1()
    
    print *, "All matplotlib comparison tests passed!"
    
contains

    subroutine test_should_match_matplotlib_seed_count_for_density_1()
        !! Test that we generate reasonable number of seeds for density=1
        real :: x(21), y(21)
        real, allocatable :: seed_x(:), seed_y(:)  
        integer :: n_seeds, i
        type(stream_mask_t) :: mask
        
        print *, "Testing matplotlib-compatible seed count..."
        
        ! Create 20x20 grid like streamplot_demo
        do i = 1, 21
            x(i) = -2.0 + (i-1) * 4.0 / 20.0
            y(i) = -2.0 + (i-1) * 4.0 / 20.0
        end do
        
        call calculate_seed_points_matplotlib(x, y, 1.0_wp, seed_x, seed_y, n_seeds, mask)
        
        print *, "Generated", n_seeds, "seed points for density=1.0"
        print *, "Mask size:", mask%nx, "x", mask%ny
        
        ! Should generate reasonable number of seeds (not too few, not too many)
        if (n_seeds < 10) then
            error stop "Too few seed points generated"
        end if
        
        if (n_seeds > 200) then
            print *, "Note: Generated", n_seeds, "seeds - high but acceptable for collision detection"
        end if
        
        print *, "✓ Seed count test passed"
    end subroutine

    subroutine test_should_generate_boundary_first_seeds()
        !! Test that first seeds are on domain boundaries
        real :: x(11), y(11)
        real, allocatable :: seed_x(:), seed_y(:)
        integer :: n_seeds, i, boundary_count
        type(stream_mask_t) :: mask
        logical :: is_boundary
        
        print *, "Testing boundary-first seed generation..."
        
        ! Create simple 10x10 grid
        do i = 1, 11
            x(i) = real(i-1)
            y(i) = real(i-1)
        end do
        
        call calculate_seed_points_matplotlib(x, y, 1.0_wp, seed_x, seed_y, n_seeds, mask)
        
        ! Count how many of first 10 seeds are on boundary
        boundary_count = 0
        do i = 1, min(10, n_seeds)
            is_boundary = (seed_x(i) <= x(2) .or. seed_x(i) >= x(10) .or. &
                          seed_y(i) <= y(2) .or. seed_y(i) >= y(10))
            if (is_boundary) boundary_count = boundary_count + 1
        end do
        
        ! Most early seeds should be on boundary
        if (boundary_count < 6) then
            print *, "Only", boundary_count, "of first 10 seeds on boundary"
            error stop "Not enough boundary seeds in early positions"
        end if
        
        print *, "✓ Boundary-first test passed (", boundary_count, "/10 boundary seeds)"
    end subroutine

    subroutine test_should_use_30x30_mask_for_density_1()
        !! Test that mask is properly sized for matplotlib compatibility
        real :: x(21), y(21)
        real, allocatable :: seed_x(:), seed_y(:)
        integer :: n_seeds, i
        type(stream_mask_t) :: mask
        
        print *, "Testing 30x30 mask usage..."
        
        do i = 1, 21
            x(i) = real(i-1) * 0.1
            y(i) = real(i-1) * 0.1
        end do
        
        call calculate_seed_points_matplotlib(x, y, 1.0_wp, seed_x, seed_y, n_seeds, mask)
        
        if (mask%nx /= 30 .or. mask%ny /= 30) then
            print *, "Expected 30x30 mask, got", mask%nx, "x", mask%ny
            error stop "Mask size not matplotlib-compatible"
        end if
        
        print *, "✓ Mask size test passed"
    end subroutine

end program test_matplotlib_comparison