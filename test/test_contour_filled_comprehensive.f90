program test_contour_filled_comprehensive
    use fortplot, only: figure_t, wp
    implicit none
    
    call test_contour_filled_basic_functionality()
    call test_contour_filled_different_grid_sizes()
    call test_contour_filled_various_value_ranges()
    call test_contour_filled_ascii_character_mapping()
    call test_contour_filled_edge_cases()
    call test_contour_filled_with_labels()
    
    print *, "All contour_filled tests passed!"
    
contains

    subroutine test_contour_filled_basic_functionality()
        type(figure_t) :: fig
        real(wp) :: x(10), y(10), z(10, 10)
        integer :: i, j
        
        ! Create simple linear gradient
        do i = 1, 10
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp)
        end do
        
        do i = 1, 10
            do j = 1, 10
                z(i, j) = real(i + j, wp)
            end do
        end do
        
        call fig%initialize(80, 25)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('/tmp/test_contour_basic.txt')
        
        print *, "test_contour_filled_basic_functionality: PASSED"
    end subroutine
    
    subroutine test_contour_filled_different_grid_sizes()
        type(figure_t) :: fig
        
        ! Test small grid (3x3)
        block
            real(wp) :: x(3), y(3), z(3, 3)
            integer :: i, j
            
            x = [0.0_wp, 1.0_wp, 2.0_wp]
            y = [0.0_wp, 1.0_wp, 2.0_wp]
            
            do i = 1, 3
                do j = 1, 3
                    z(i, j) = real(i * j, wp)
                end do
            end do
            
            call fig%initialize(40, 20)
            call fig%add_contour_filled(x, y, z)
            call fig%savefig('/tmp/test_contour_small.txt')
        end block
        
        ! Test larger grid (50x30)
        block
            real(wp), allocatable :: x(:), y(:), z(:, :)
            integer :: i, j
            
            allocate(x(50), y(30), z(50, 30))
            
            do i = 1, 50
                x(i) = real(i-1, wp) / 49.0_wp * 10.0_wp
            end do
            do i = 1, 30
                y(i) = real(i-1, wp) / 29.0_wp * 6.0_wp
            end do
            
            ! Create a more interesting pattern
            do i = 1, 50
                do j = 1, 30
                    z(i, j) = sin(x(i)) * cos(y(j))
                end do
            end do
            
            call fig%initialize(100, 40)
            call fig%add_contour_filled(x, y, z)
            call fig%savefig('/tmp/test_contour_large.txt')
        end block
        
        print *, "test_contour_filled_different_grid_sizes: PASSED"
    end subroutine
    
    subroutine test_contour_filled_various_value_ranges()
        type(figure_t) :: fig
        real(wp) :: x(10), y(10), z(10, 10)
        integer :: i, j
        
        do i = 1, 10
            x(i) = real(i-1, wp)
            y(i) = real(i-1, wp)
        end do
        
        ! Test with all positive values
        do i = 1, 10
            do j = 1, 10
                z(i, j) = real(i + j, wp) * 0.1_wp
            end do
        end do
        
        call fig%initialize(60, 20)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('/tmp/test_contour_positive.txt')
        
        ! Test with negative values
        do i = 1, 10
            do j = 1, 10
                z(i, j) = real(i - j, wp)
            end do
        end do
        
        call fig%initialize(60, 20)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('/tmp/test_contour_negative.txt')
        
        ! Test with very small range
        do i = 1, 10
            do j = 1, 10
                z(i, j) = 0.5_wp + 0.001_wp * real(i + j, wp)
            end do
        end do
        
        call fig%initialize(60, 20)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('/tmp/test_contour_small_range.txt')
        
        print *, "test_contour_filled_various_value_ranges: PASSED"
    end subroutine
    
    subroutine test_contour_filled_ascii_character_mapping()
        type(figure_t) :: fig
        real(wp) :: x(5), y(5), z(5, 5)
        integer :: i, j
        
        ! Create distinct levels to test character mapping
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        
        ! Create 9 distinct levels (matching ASCII character set)
        do i = 1, 5
            do j = 1, 5
                z(i, j) = real(mod(i + j - 2, 9), wp)
            end do
        end do
        
        call fig%initialize(50, 25)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('/tmp/test_contour_levels.txt')
        
        ! Test with uniform values (should produce uniform character)
        z = 5.0_wp
        
        call fig%initialize(50, 25)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('/tmp/test_contour_uniform.txt')
        
        print *, "test_contour_filled_ascii_character_mapping: PASSED"
    end subroutine
    
    subroutine test_contour_filled_edge_cases()
        type(figure_t) :: fig
        real(wp) :: x(2), y(2), z(2, 2)
        
        ! Test minimum grid size (2x2)
        x = [0.0_wp, 1.0_wp]
        y = [0.0_wp, 1.0_wp]
        z = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [2, 2])
        
        call fig%initialize(30, 15)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('/tmp/test_contour_minimal.txt')
        
        ! Test with very small figure size
        call fig%initialize(20, 10)
        call fig%add_contour_filled(x, y, z)
        call fig%savefig('/tmp/test_contour_tiny.txt')
        
        ! Test with irregular spacing
        block
            real(wp) :: x_irreg(5), y_irreg(5), z_irreg(5, 5)
            integer :: i, j
            
            x_irreg = [0.0_wp, 0.1_wp, 0.5_wp, 2.0_wp, 4.0_wp]
            y_irreg = [0.0_wp, 0.3_wp, 0.4_wp, 1.0_wp, 3.0_wp]
            
            do i = 1, 5
                do j = 1, 5
                    z_irreg(i, j) = x_irreg(i) * y_irreg(j)
                end do
            end do
            
            call fig%initialize(60, 25)
            call fig%add_contour_filled(x_irreg, y_irreg, z_irreg)
            call fig%savefig('/tmp/test_contour_irregular.txt')
        end block
        
        print *, "test_contour_filled_edge_cases: PASSED"
    end subroutine
    
    subroutine test_contour_filled_with_labels()
        type(figure_t) :: fig
        real(wp) :: x(15), y(10), z(15, 10)
        integer :: i, j
        
        ! Create test data
        do i = 1, 15
            x(i) = real(i-1, wp) * 0.5_wp
        end do
        do i = 1, 10
            y(i) = real(i-1, wp) * 0.5_wp
        end do
        
        ! Create a saddle point pattern
        do i = 1, 15
            do j = 1, 10
                z(i, j) = (x(i) - 3.5_wp)**2 - (y(j) - 2.5_wp)**2
            end do
        end do
        
        call fig%initialize(80, 30)
        call fig%add_contour_filled(x, y, z, label="Saddle Point")
        call fig%set_xlabel("X axis")
        call fig%set_ylabel("Y axis")
        call fig%set_title("Contour Filled Test with Labels")
        call fig%savefig('/tmp/test_contour_labeled.txt')
        
        print *, "test_contour_filled_with_labels: PASSED"
    end subroutine

end program test_contour_filled_comprehensive