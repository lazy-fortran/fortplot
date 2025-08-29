program debug_streamplot_circular
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: nx = 10, ny = 10
    real(real64), dimension(nx) :: x
    real(real64), dimension(ny) :: y
    real(real64), dimension(nx, ny) :: u, v
    integer :: i, j
    type(figure_t) :: fig
    
    ! Create grid from -2 to 2
    do i = 1, nx
        x(i) = -2.0_real64 + 4.0_real64 * real(i-1, real64) / real(nx-1, real64)
    end do
    
    do j = 1, ny
        y(j) = -2.0_real64 + 4.0_real64 * real(j-1, real64) / real(ny-1, real64)
    end do
    
    ! Create circular flow field: u = -y, v = x (same as original example)
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -y(j)  ! u = -y creates clockwise rotation
            v(i,j) = x(i)   ! v = x creates clockwise rotation
        end do
    end do
    
    ! Print vector field at a few points
    print *, 'Circular flow field (sample points):'
    print *, 'At (x,y)=(0,0): u=', u(nx/2,ny/2), ', v=', v(nx/2,ny/2)
    print *, 'At (x,y)=(2,-2): u=', u(nx,1), ', v=', v(nx,1)
    print *, 'At (x,y)=(-2,2): u=', u(1,ny), ', v=', v(1,ny)
    
    call fig%initialize(600, 600)
    print *, 'Created figure, calling streamplot...'
    
    call fig%streamplot(x, y, u, v, density=1.0_real64)
    
    print *, 'Streamplot completed, plot_count:', fig%plot_count
    
    if (fig%plot_count > 0) then
        print *, 'First plot has', size(fig%plots(1)%x), 'points'
        if (size(fig%plots(1)%x) >= 5) then
            print *, 'First 5 X coords:', fig%plots(1)%x(1:5)
            print *, 'First 5 Y coords:', fig%plots(1)%y(1:5)
        end if
        if (fig%plot_count >= 2) then
            print *, 'Second plot has', size(fig%plots(2)%x), 'points'
            if (size(fig%plots(2)%x) >= 5) then
                print *, 'Second plot first 5 X coords:', fig%plots(2)%x(1:5)
                print *, 'Second plot first 5 Y coords:', fig%plots(2)%y(1:5)
            end if
        end if
    else
        print *, 'ERROR: No plots generated!'
        stop 1
    end if
    
    ! Save debug output
    call fig%savefig('debug_streamplot_circular.png')
    call fig%savefig('debug_streamplot_circular.txt')
    
    print *, 'Debug files saved: debug_streamplot_circular.png/txt'
    
end program debug_streamplot_circular