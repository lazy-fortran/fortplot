program test_streamplot_arrows
    !! Verify streamplot arrow rendering via matplotlib-style wrapper
    use fortplot
    use, intrinsic :: iso_fortran_env, only: real64
    use test_output_helpers, only: ensure_test_output_dir
    implicit none

    real(real64), allocatable :: x(:), y(:)
    real(real64), allocatable :: u(:,:), v(:,:)
    character(len=:), allocatable :: outdir
    integer :: i, j, nx, ny

    call ensure_test_output_dir('streamplot', outdir)

    nx = 20; ny = 15
    allocate(x(nx), y(ny), u(nx, ny), v(nx, ny))

    do i = 1, nx
        x(i) = real(i-1, real64) / real(nx-1, real64)
    end do
    do j = 1, ny
        y(j) = real(j-1, real64) / real(ny-1, real64)
    end do

    ! Simple rotational field
    do j = 1, ny
        do i = 1, nx
            u(i,j) = -(y(j) - 0.5_real64)
            v(i,j) =  (x(i) - 0.5_real64)
        end do
    end do

    call figure()
    call streamplot(x, y, u, v, density=1.0_real64, arrowsize=1.0_real64, arrowstyle='->')
    call title('streamplot arrows smoke test')
    call savefig(trim(outdir)//'streamplot_arrows.png')

    deallocate(x, y, u, v)
end program test_streamplot_arrows
