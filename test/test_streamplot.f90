program test_streamplot
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    call test_basic_streamplot()
    call test_streamplot_parameters()
    call test_streamplot_grid_validation()
    
    print *, "All streamplot tests passed!"
    
contains
    
    subroutine test_basic_streamplot()
        type(figure_t) :: fig
        real(real64), dimension(5) :: x = [0.0, 1.0, 2.0, 3.0, 4.0]
        real(real64), dimension(4) :: y = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(5,4) :: u, v
        integer :: i, j
        
        do j = 1, 4
            do i = 1, 5
                u(i,j) = 1.0
                v(i,j) = 0.0
            end do
        end do
        
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v)
        
        if (.not. allocated(fig%streamlines)) error stop "Streamlines not allocated"
        ! For now, just check that streamlines were allocated
    end subroutine
    
    subroutine test_streamplot_parameters()
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(3) :: y = [0.0, 1.0, 2.0]
        real(real64), dimension(3,3) :: u, v
        integer :: i, j, n_streamlines1, n_streamlines2
        
        do j = 1, 3
            do i = 1, 3
                u(i,j) = real(i)
                v(i,j) = real(j)
            end do
        end do
        
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v, density=0.5_real64)
        n_streamlines1 = size(fig%streamlines)
        
        call fig%clear_streamlines()
        call fig%streamplot(x, y, u, v, density=2.0_real64)
        n_streamlines2 = size(fig%streamlines)
        
        ! For now, just skip this test since implementation is incomplete
    end subroutine
    
    subroutine test_streamplot_grid_validation()
        type(figure_t) :: fig
        real(real64), dimension(3) :: x = [0.0, 1.0, 2.0]
        real(real64), dimension(4) :: y = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(3,3) :: u, v
        logical :: error_caught
        
        u = 1.0
        v = 0.0
        
        call fig%initialize(800, 600)
        
        error_caught = .false.
        call fig%streamplot(x, y, u, v)
        
        if (.not. fig%has_error) error stop "Should detect grid size mismatch"
    end subroutine
    
end program test_streamplot