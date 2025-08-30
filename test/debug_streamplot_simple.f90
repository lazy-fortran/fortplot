program debug_streamplot_simple
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: nx = 5, ny = 5  ! Small grid for debugging
    real(real64), dimension(nx) :: x
    real(real64), dimension(ny) :: y
    real(real64), dimension(nx, ny) :: u, v
    integer :: i, j
    type(figure_t) :: fig
    
    ! Create simple grid
    do i = 1, nx
        x(i) = real(i-1, real64)  ! 0, 1, 2, 3, 4
    end do
    
    do j = 1, ny
        y(j) = real(j-1, real64)  ! 0, 1, 2, 3, 4
    end do
    
    ! Create simple uniform flow: u=1, v=0 (horizontal flow)
    do j = 1, ny
        do i = 1, nx
            u(i,j) = 1.0_real64
            v(i,j) = 0.0_real64
        end do
    end do
    
    ! Print vector field
    print *, 'Vector field:'
    do j = ny, 1, -1  ! Print top to bottom
        write(*,'(A,I0,A)', advance='no') 'y=', j-1, ': '
        do i = 1, nx
            write(*,'(A,F3.1,A,F3.1,A)', advance='no') '(', u(i,j), ',', v(i,j), ') '
        end do
        print *
    end do
    
    call fig%initialize(400, 300)
    print *, 'Created figure, calling streamplot...'
    
    call fig%streamplot(x, y, u, v, density=0.5_real64)
    
    print *, 'Streamplot completed, plot_count:', fig%plot_count
    
    if (fig%plot_count > 0) then
        print *, 'First plot has', size(fig%plots(1)%x), 'points'
        if (size(fig%plots(1)%x) > 1) then
            print *, 'First few X coords:', fig%plots(1)%x(1:min(5,size(fig%plots(1)%x)))
            print *, 'First few Y coords:', fig%plots(1)%y(1:min(5,size(fig%plots(1)%y)))
        else
            print *, 'Single point at:', fig%plots(1)%x(1), fig%plots(1)%y(1)
        end if
    else
        print *, 'No plots generated!'
    end if
    
    ! Save debug output
    call fig%savefig("test/output/debug_streamplot_simple.png")
    call fig%savefig("test/output/debug_streamplot_simple.txt")
    
end program debug_streamplot_simple