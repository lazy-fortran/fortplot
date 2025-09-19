program test_streamplot_arrows
    !! Verify streamplot arrow rendering via matplotlib-style wrapper
    use fortplot, only: figure, streamplot, title, savefig, wp
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(wp), allocatable :: x(:), y(:)
    real(wp), allocatable :: u(:,:), v(:,:)
    character(len=:), allocatable :: outdir
    integer :: i, j, nx, ny

    call ensure_test_output_dir('streamplot', outdir)

    nx = 20; ny = 15
    allocate(x(nx), y(ny), u(nx, ny), v(nx, ny))

    do i = 1, nx
        x(i) = real(i-1, wp) / real(nx-1, wp)
    end do
    do j = 1, ny
        y(j) = real(j-1, wp) / real(ny-1, wp)
    end do

    ! Simple rotational field
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -(y(j) - 0.5_wp)
            v(i,j) =  (x(i) - 0.5_wp)
        end do
    end do

    call figure()
    call streamplot(x, y, u, v, density=1.0_wp, arrowsize=1.0_wp, arrowstyle='->')
    call title('streamplot arrows smoke test')
    call savefig(trim(outdir)//'streamplot_arrows.png')

    deallocate(x, y, u, v)
end program test_streamplot_arrows
