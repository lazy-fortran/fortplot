program test_streamline_rendering
    use fortplot
    use fortplot_streamline
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    call test_streamline_generation()
    call test_interpolation()
    
    print *, "All streamline rendering tests passed!"
    
contains
    
    subroutine test_streamline_generation()
        type(figure_t) :: fig
        real(real64), dimension(5) :: x = [0.0, 1.0, 2.0, 3.0, 4.0]
        real(real64), dimension(4) :: y = [0.0, 1.0, 2.0, 3.0]
        real(real64), dimension(5,4) :: u, v
        integer :: i, j
        
        ! Constant rightward flow
        do j = 1, 4
            do i = 1, 5
                u(i,j) = 1.0
                v(i,j) = 0.0
            end do
        end do
        
        call fig%initialize(800, 600)
        call fig%streamplot(x, y, u, v)
        
        ! Check that plots were created
        if (fig%plot_count == 0) error stop "No streamlines generated"
        
        ! Check that lines extend horizontally
        do i = 1, fig%plot_count
            if (allocated(fig%plots(i)%x)) then
                if (size(fig%plots(i)%x) < 2) error stop "Streamline too short"
            end if
        end do
    end subroutine
    
    subroutine test_interpolation()
        real :: x(2) = [0.0, 1.0]
        real :: y(2) = [0.0, 1.0]
        real :: u(2,2), v(2,2)
        real :: u_interp, v_interp
        
        ! Set up simple gradient field
        u(1,1) = 0.0; u(2,1) = 1.0
        u(1,2) = 0.0; u(2,2) = 1.0
        v = 0.0
        
        call bilinear_interpolate(x, y, u, 0.5, 0.5, u_interp)
        if (abs(u_interp - 0.5) > 1e-6) error stop "Interpolation failed"
    end subroutine
    
end program test_streamline_rendering