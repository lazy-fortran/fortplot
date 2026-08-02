program test_contour_rectangular_orientation
    !! Rectangular contour inputs may be supplied as z(nx,ny) or z(ny,nx).
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t, PLOT_TYPE_CONTOUR
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(5), y(3), z(5, 3)
    integer :: i, j

    x = [(real(i - 1, wp), i=1, size(x))]
    y = [(real(2*j - 1, wp), j=1, size(y))]
    do j = 1, size(y)
        do i = 1, size(x)
            z(i, j) = 10.0_wp*real(j, wp) + real(i, wp)
        end do
    end do

    call fig%initialize()
    call fig%add_contourf(x, y, z)
    if (fig%plot_count /= 1) error stop "contour plot was not registered"
    if (fig%plots(1)%plot_type /= PLOT_TYPE_CONTOUR) &
        error stop "contour plot type was not retained"
    if (size(fig%plots(1)%z_grid, 1) /= size(y) .or. &
        size(fig%plots(1)%z_grid, 2) /= size(x)) &
        error stop "rectangular contour grid was not normalized to (ny,nx)"
    do j = 1, size(y)
        do i = 1, size(x)
            if (abs(fig%plots(1)%z_grid(j, i) - z(i, j)) > 1.0e-12_wp) &
                error stop "rectangular contour values were transposed incorrectly"
        end do
    end do
    print *, "PASS: rectangular contour orientation normalized"
end program test_contour_rectangular_orientation
