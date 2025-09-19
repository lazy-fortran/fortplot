program test_contour_is_not_3d
    !! Ensure contour/pcolormesh style data does not mark figure as 3D
    use iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    implicit none

    type(plot_data_t) :: plot

    call setup_contour_like_plot(plot)

    if (plot%is_3d()) then
        print *, 'DEBUG: allocated(z)=', allocated(plot%z)
        print *, 'DEBUG: allocated(z_grid)=', allocated(plot%z_grid)
        error stop 'Contour-style plot must not be detected as 3D'
    end if

    print *, 'PASS: contour/pcolormesh data is treated as 2D'

contains

    subroutine setup_contour_like_plot(p)
        type(plot_data_t), intent(inout) :: p
        real(wp), allocatable :: x(:), y(:)
        real(wp), allocatable :: z(:,:)

        integer :: nx, ny, i, j

        nx = 10; ny = 8
        allocate(x(nx), y(ny), z(ny, nx))

        x = [(real(i-1, wp), i=1,nx)]
        y = [(real(j-1, wp), j=1,ny)]
        z = 0.0_wp

        p%x_grid = x
        p%y_grid = y
        p%z_grid = z
    end subroutine setup_contour_like_plot

end program test_contour_is_not_3d
