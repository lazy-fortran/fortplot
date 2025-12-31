program test_streamplot_interface_color
    use fortplot, only: wp
    use fortplot_matplotlib_advanced, only: streamplot, figure, get_global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: plot_data_t
    implicit none

    real(wp), dimension(5) :: x
    real(wp), dimension(4) :: y
    real(wp), dimension(5,4) :: u, v
    class(figure_t), pointer :: fig
    type(plot_data_t), pointer :: plots(:)
    integer :: i, j, n
    real(wp), dimension(3) :: ref_color
    logical :: all_same

    ! Setup a vector field matching test_streamplot.f90 parameters
    do i = 1, 5
        x(i) = real(i-1, wp)
    end do
    do i = 1, 4
        y(i) = real(i-1, wp)
    end do
    do j = 1, 4
        do i = 1, 5
            u(i,j) = 1.0_wp
            v(i,j) = 0.0_wp
        end do
    end do

    ! Use explicit figure dimensions like test_streamplot.f90
    call figure(figsize=[8.0_wp, 6.0_wp], dpi=100)

    call streamplot(x, y, u, v)

    fig => get_global_figure()
    n = fig%get_plot_count()

    if (n <= 0) then
        print *, "ERROR: streamplot produced no plots"
        stop 1
    end if

    plots => fig%get_plots()
    ref_color = plots(1)%color
    all_same = .true.
    do i = 2, n
        if (any(abs(plots(i)%color - ref_color) > 1.0e-12_wp)) then
            all_same = .false.
            exit
        end if
    end do

    if (.not. all_same) then
        print *, "ERROR: Streamplot interface assigned varying colors across streamlines"
        stop 1
    end if

    ! Optional: check equals default blue
    if (any(abs(ref_color - [0.0_wp, 0.447_wp, 0.698_wp]) > 1.0e-12_wp)) then
        print *, "ERROR: Streamplot default color not blue as expected"
        stop 1
    end if

    print *, "Streamplot interface color test passed"
end program test_streamplot_interface_color
