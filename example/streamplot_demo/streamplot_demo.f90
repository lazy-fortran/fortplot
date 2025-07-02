program streamplot_demo
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: nx = 20, ny = 20
    real(real64), dimension(nx) :: x
    real(real64), dimension(ny) :: y
    real(real64), dimension(nx, ny) :: u, v
    integer :: i, j
    type(figure_t) :: fig
    
    ! Create grid
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
    
    ! Create figure and add streamplot
    call fig%initialize(800, 600)
    call fig%streamplot(x, y, u, v, density=1.0_real64)
    call fig%set_xlabel('X')
    call fig%set_ylabel('Y')
    call fig%set_title('Streamline Plot Demo - Circular Flow')
    
    ! Save figure
    call fig%savefig('example/streamplot_demo/streamplot_demo.png')
    
    print *, 'Streamplot demo completed!'
    
end program streamplot_demo