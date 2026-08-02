program test_pcolormesh_rectangular_orientation
    !! Rectangular pcolormesh inputs may be supplied as c(nx,ny).
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t, PLOT_TYPE_PCOLORMESH
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(5), y(3), c(5, 3)
    integer :: i, j

    x = [(real(i - 1, wp), i=1, size(x))]
    y = [(real(2*j - 1, wp), j=1, size(y))]
    do j = 1, size(y)
        do i = 1, size(x)
            c(i, j) = 10.0_wp*real(j, wp) + real(i, wp)
        end do
    end do

    call fig%initialize()
    call fig%add_pcolormesh(x, y, c)
    if (fig%plot_count /= 1) error stop "pcolormesh was not registered"
    if (fig%plots(1)%plot_type /= PLOT_TYPE_PCOLORMESH) &
        error stop "pcolormesh plot type was not retained"
    if (size(fig%plots(1)%pcolormesh_data%c_values, 1) /= size(y) .or. &
        size(fig%plots(1)%pcolormesh_data%c_values, 2) /= size(x)) &
        error stop "rectangular pcolormesh grid was not normalized"
    do j = 1, size(y)
        do i = 1, size(x)
            if (abs(fig%plots(1)%pcolormesh_data%c_values(j, i) - c(i, j)) > &
                1.0e-12_wp) error stop "pcolormesh values were transposed incorrectly"
        end do
    end do
    print *, "PASS: rectangular pcolormesh orientation normalized"
end program test_pcolormesh_rectangular_orientation
