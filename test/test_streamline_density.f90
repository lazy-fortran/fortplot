program test_streamline_density
    use fortplot_streamline, only: calculate_seed_points
    implicit none
    
    call test_uniform_density()
    call test_custom_density()
    call test_boundary_conditions()
    
    print *, "All streamline density tests passed!"
    
contains
    
    subroutine test_uniform_density()
        real, dimension(5) :: x = [0.0, 1.0, 2.0, 3.0, 4.0]
        real, dimension(4) :: y = [0.0, 1.0, 2.0, 3.0]
        real, allocatable :: seed_x(:), seed_y(:)
        integer :: n_seeds
        
        call calculate_seed_points(x, y, 1.0, seed_x, seed_y, n_seeds)
        
        if (n_seeds < 4) error stop "Too few seed points generated"
        if (n_seeds > 20) error stop "Too many seed points generated"
        
        deallocate(seed_x, seed_y)
    end subroutine
    
    subroutine test_custom_density()
        real, dimension(3) :: x = [0.0, 1.0, 2.0]
        real, dimension(3) :: y = [0.0, 1.0, 2.0]
        real, allocatable :: seed_x(:), seed_y(:)
        integer :: n_seeds
        
        call calculate_seed_points(x, y, 2.0, seed_x, seed_y, n_seeds)
        
        if (n_seeds < 8) error stop "Density parameter not working correctly"
        
        deallocate(seed_x, seed_y)
    end subroutine
    
    subroutine test_boundary_conditions()
        real, dimension(2) :: x = [0.0, 1.0]
        real, dimension(2) :: y = [0.0, 1.0]
        real, allocatable :: seed_x(:), seed_y(:)
        integer :: n_seeds, i
        
        call calculate_seed_points(x, y, 1.0, seed_x, seed_y, n_seeds)
        
        do i = 1, n_seeds
            if (seed_x(i) < x(1) .or. seed_x(i) > x(2)) then
                error stop "Seed point outside x bounds"
            end if
            if (seed_y(i) < y(1) .or. seed_y(i) > y(2)) then
                error stop "Seed point outside y bounds"
            end if
        end do
        
        deallocate(seed_x, seed_y)
    end subroutine
    
end program test_streamline_density