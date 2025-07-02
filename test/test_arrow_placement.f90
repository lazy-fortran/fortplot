program test_arrow_placement
    use fortplot_streamline, only: calculate_arrow_positions
    implicit none
    
    call test_uniform_arrow_spacing()
    call test_minimum_arrow_count()
    call test_arrow_density_parameter()
    
    print *, "All arrow placement tests passed!"
    
contains
    
    subroutine test_uniform_arrow_spacing()
        real, dimension(100) :: path_x, path_y
        logical, allocatable :: arrow_mask(:)
        integer :: i, n_arrows
        real :: spacing
        
        do i = 1, 100
            path_x(i) = real(i - 1) * 0.1
            path_y(i) = 0.0
        end do
        
        call calculate_arrow_positions(path_x, path_y, 100, 1.0, arrow_mask)
        
        n_arrows = count(arrow_mask)
        if (n_arrows < 3) error stop "Too few arrows placed"
        if (n_arrows > 15) error stop "Too many arrows placed"
        
        deallocate(arrow_mask)
    end subroutine
    
    subroutine test_minimum_arrow_count()
        real, dimension(5) :: path_x, path_y
        logical, allocatable :: arrow_mask(:)
        integer :: i
        
        do i = 1, 5
            path_x(i) = real(i - 1) * 0.25
            path_y(i) = 0.0
        end do
        
        call calculate_arrow_positions(path_x, path_y, 5, 1.0, arrow_mask)
        
        if (count(arrow_mask) < 1) error stop "At least one arrow should be placed"
        
        deallocate(arrow_mask)
    end subroutine
    
    subroutine test_arrow_density_parameter()
        real, dimension(50) :: path_x, path_y
        logical, allocatable :: arrow_mask1(:), arrow_mask2(:)
        integer :: i
        
        do i = 1, 50
            path_x(i) = real(i - 1) * 0.1
            path_y(i) = 0.0
        end do
        
        call calculate_arrow_positions(path_x, path_y, 50, 0.5, arrow_mask1)
        call calculate_arrow_positions(path_x, path_y, 50, 2.0, arrow_mask2)
        
        if (count(arrow_mask1) >= count(arrow_mask2)) then
            error stop "Higher density should produce more arrows"
        end if
        
        deallocate(arrow_mask1, arrow_mask2)
    end subroutine
    
end program test_arrow_placement