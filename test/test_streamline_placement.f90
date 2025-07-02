program test_streamline_placement
    !! Test streamline placement algorithms for matplotlib compatibility
    !! Following TDD approach - write failing tests first
    use fortplot_streamline_placement
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_should_create_30x30_base_mask_for_density_1()
    call test_should_scale_mask_size_with_density()
    call test_should_generate_spiral_seed_points()
    call test_should_detect_collisions_in_mask()
    call test_should_start_from_boundary_points_first()
    
    print *, "All streamline placement tests passed!"
    
contains

    subroutine test_should_create_30x30_base_mask_for_density_1()
        !! Test that density=1 creates 30x30 mask like matplotlib
        type(stream_mask_t) :: mask
        
        print *, "Testing 30x30 base mask creation..."
        
        call mask%initialize(1.0_wp)
        
        if (mask%nx /= 30) then
            print *, "Expected nx=30, got:", mask%nx
            error stop "Mask width incorrect for density=1"
        end if
        
        if (mask%ny /= 30) then
            print *, "Expected ny=30, got:", mask%ny
            error stop "Mask height incorrect for density=1"
        end if
        
        print *, "✓ 30x30 base mask test passed"
    end subroutine

    subroutine test_should_scale_mask_size_with_density()
        !! Test that mask size scales correctly with density parameter
        type(stream_mask_t) :: mask
        
        print *, "Testing mask size scaling..."
        
        ! Test density=2.0 → 60x60
        call mask%initialize(2.0_wp)
        if (mask%nx /= 60 .or. mask%ny /= 60) then
            print *, "Expected 60x60 for density=2, got:", mask%nx, "x", mask%ny
            error stop "Density=2 scaling failed"
        end if
        
        ! Test density=0.5 → 15x15  
        call mask%initialize(0.5_wp)
        if (mask%nx /= 15 .or. mask%ny /= 15) then
            print *, "Expected 15x15 for density=0.5, got:", mask%nx, "x", mask%ny
            error stop "Density=0.5 scaling failed"
        end if
        
        print *, "✓ Mask scaling test passed"
    end subroutine

    subroutine test_should_generate_spiral_seed_points()
        !! Test spiral seed point generation starting from (1,1)
        integer, allocatable :: seed_points(:,:)
        integer :: n_seeds
        integer :: expected_x(9), expected_y(9)
        integer :: i
        
        print *, "Testing spiral seed generation..."
        
        ! Expected sequence for 3x3: (1,1), (2,1), (3,1), (3,2), (3,3), (2,3), (1,3), (1,2), (2,2)
        expected_x = [1, 2, 3, 3, 3, 2, 1, 1, 2]
        expected_y = [1, 1, 1, 2, 3, 3, 3, 2, 2]
        
        call generate_spiral_seeds([3, 3], seed_points, n_seeds)
        
        if (n_seeds /= 9) then
            print *, "Expected 9 seeds for 3x3, got:", n_seeds
            error stop "Wrong number of spiral seeds"
        end if
        
        ! Check first few points match spiral pattern
        do i = 1, min(9, n_seeds)
            if (seed_points(1, i) /= expected_x(i) .or. seed_points(2, i) /= expected_y(i)) then
                print *, "Seed", i, "expected:", expected_x(i), expected_y(i), &
                         "got:", seed_points(1, i), seed_points(2, i)
                error stop "Spiral pattern incorrect"
            end if
        end do
        
        print *, "✓ Spiral generation test passed"
    end subroutine

    subroutine test_should_detect_collisions_in_mask()
        !! Test collision detection prevents overlapping streamlines
        type(stream_mask_t) :: mask
        
        print *, "Testing collision detection..."
        
        call mask%initialize(1.0_wp)
        
        ! Initially position should be free
        if (.not. mask%is_free(5, 5)) then
            error stop "Fresh mask position should be free"
        end if
        
        ! Start trajectory and mark position
        call mask%start_trajectory(5, 5)
        
        ! Position should now be occupied
        if (mask%is_free(5, 5)) then
            error stop "Marked position should not be free"
        end if
        
        ! Test undo functionality
        call mask%undo_trajectory()
        if (.not. mask%is_free(5, 5)) then
            error stop "Position should be free after undo"
        end if
        
        print *, "✓ Collision detection test passed"
    end subroutine

    subroutine test_should_start_from_boundary_points_first()
        !! Test that boundary points have priority in seed generation
        integer, allocatable :: seed_points(:,:)
        integer :: n_seeds, x, y, i
        logical :: is_boundary
        
        print *, "Testing boundary-first generation..."
        
        call generate_spiral_seeds([5, 5], seed_points, n_seeds)
        
        ! Check that first several points are on boundary
        do i = 1, min(10, n_seeds)
            x = seed_points(1, i)
            y = seed_points(2, i)
            
            ! Point is on boundary if x=1, x=5, y=1, or y=5
            is_boundary = (x == 1 .or. x == 5 .or. y == 1 .or. y == 5)
            
            if (.not. is_boundary .and. i <= 12) then  ! First 12 should be boundary for 5x5
                print *, "Point", i, "at (", x, ",", y, ") should be on boundary"
                error stop "Boundary priority not working"
            end if
        end do
        
        print *, "✓ Boundary priority test passed"
    end subroutine

end program test_streamline_placement