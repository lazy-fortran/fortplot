program test_pcolormesh
    !! Test program for pcolormesh functionality
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    implicit none
    
    call test_pcolormesh_basic()
    call test_pcolormesh_colormap()
    call test_pcolormesh_regular_grid()
    
contains

    subroutine test_pcolormesh_basic()
        !! Test basic pcolormesh functionality with simple grid
        type(figure_t) :: fig
        real(wp) :: x(4), y(3)
        real(wp) :: c(2, 3)
        integer :: i, j
        
        ! Arrange - create simple grid
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]  ! 4 vertices
        y = [0.0_wp, 1.0_wp, 2.0_wp]          ! 3 vertices
        
        ! Create test data for 2x3 grid (3 rows, 2 cols)
        do i = 1, 2
            do j = 1, 3
                c(i, j) = real(i + j, wp)
            end do
        end do
        
        call fig%initialize(400, 300)
        
        ! Act & Assert - should not crash and should add pcolormesh to figure
        call fig%add_pcolormesh(x, y, c)
        
        print *, "test_pcolormesh_basic: PASSED"
    end subroutine test_pcolormesh_basic
    
    subroutine test_pcolormesh_colormap()
        !! Test pcolormesh with specific colormap
        type(figure_t) :: fig
        real(wp) :: x(3), y(3)
        real(wp) :: c(2, 2)
        
        ! Arrange
        x = [0.0_wp, 1.0_wp, 2.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp]
        c = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp], [2, 2])
        
        call fig%initialize(400, 300)
        
        ! Act & Assert - should accept colormap parameter
        call fig%add_pcolormesh(x, y, c, colormap='viridis')
        
        print *, "test_pcolormesh_colormap: PASSED"
    end subroutine test_pcolormesh_colormap
    
    subroutine test_pcolormesh_regular_grid()
        !! Test pcolormesh with regular grid (most common case)
        type(figure_t) :: fig
        real(wp) :: x(5), y(4)
        real(wp) :: c(3, 4)
        integer :: i, j
        
        ! Arrange - regular grid
        do i = 1, 5
            x(i) = real(i-1, wp) * 0.5_wp
        end do
        do i = 1, 4
            y(i) = real(i-1, wp) * 0.3_wp
        end do
        
        ! Create test data
        do i = 1, 3
            do j = 1, 4
                c(i, j) = sin(x(i)) * cos(y(j))
            end do
        end do
        
        call fig%initialize(500, 400)
        
        ! Act & Assert
        call fig%add_pcolormesh(x, y, c, colormap='plasma')
        
        print *, "test_pcolormesh_regular_grid: PASSED"
    end subroutine test_pcolormesh_regular_grid

end program test_pcolormesh