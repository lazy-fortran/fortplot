program test_streamplot_interface_color
    use, intrinsic :: iso_fortran_env, only: real64
    use fortplot_matplotlib_advanced, only: streamplot, get_global_figure, ensure_global_figure_initialized
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: plot_data_t
    implicit none

    real(real64), dimension(5) :: x
    real(real64), dimension(5) :: y
    real(real64), dimension(5,5) :: u, v
    class(figure_t), pointer :: fig
    type(plot_data_t), pointer :: plots(:)
    integer :: i, j, n
    real(real64), dimension(3) :: ref_color
    logical :: all_same

    ! Setup a simple vector field
    do i = 1, 5
        x(i) = real(i-1, real64)
        y(i) = real(i-1, real64)
    end do
    do j = 1, 5
        do i = 1, 5
            u(i,j) = 1.0_real64
            v(i,j) = 0.0_real64
        end do
    end do

    call ensure_global_figure_initialized()
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
        if (any(abs(plots(i)%color - ref_color) > 1.0e-12_real64)) then
            all_same = .false.
            exit
        end if
    end do

    if (.not. all_same) then
        print *, "ERROR: Streamplot interface assigned varying colors across streamlines"
        stop 1
    end if

    ! Optional: check equals default blue
    if (any(abs(ref_color - [0.0_real64, 0.447_real64, 0.698_real64]) > 1.0e-12_real64)) then
        print *, "ERROR: Streamplot default color not blue as expected"
        stop 1
    end if

    print *, "Streamplot interface color test passed"
end program test_streamplot_interface_color

