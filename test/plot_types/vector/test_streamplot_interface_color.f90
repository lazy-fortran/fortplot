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

    call test_default_color()
    call test_custom_color()
    call test_cmap_stored_on_plot()
    call test_label_stored_on_plot()
    call test_linewidth_via_wrapper()
    call test_default_generates_arrows()

    print *, "Streamplot interface color tests passed"

contains

    subroutine test_default_color()
        !! Default streamplot color is blue when arrowsize=0 (line mode).
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

        call figure(figsize=[8.0_wp, 6.0_wp], dpi=100)

        call streamplot(x, y, u, v, arrowsize=0.0_wp)

        fig => get_global_figure()
        n = fig%get_plot_count()

        if (n <= 0) then
            print *, "ERROR: streamplot (no arrows) produced no plots"
            stop 1
        end if

        plots => fig%get_plots()
        do i = 1, n
            if (any(abs(plots(i)%color - [0.0_wp, 0.447_wp, 0.698_wp]) > 1.0e-12_wp)) then
                print *, "ERROR: Streamplot default color not blue as expected"
                stop 1
            end if
        end do

        print *, "test_default_color passed"
    end subroutine test_default_color

    subroutine test_custom_color()
        real(wp), dimension(3) :: custom_rgb

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

        call figure(figsize=[8.0_wp, 6.0_wp], dpi=100)
        custom_rgb = [1.0_wp, 0.0_wp, 0.0_wp]

        call streamplot(x, y, u, v, color=custom_rgb, arrowsize=0.0_wp)

        fig => get_global_figure()
        n = fig%get_plot_count()

        if (n <= 0) then
            print *, "ERROR: streamplot with custom color produced no plots"
            stop 1
        end if

        plots => fig%get_plots()
        if (any(abs(plots(1)%color - custom_rgb) > 1.0e-12_wp)) then
            print *, "ERROR: Streamplot custom color not applied. Got ", &
                     plots(1)%color, " expected ", custom_rgb
            stop 1
        end if

        print *, "test_custom_color passed"
    end subroutine test_custom_color

    subroutine test_cmap_stored_on_plot()
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

        call figure(figsize=[8.0_wp, 6.0_wp], dpi=100)

        call streamplot(x, y, u, v, cmap='plasma', arrowsize=0.0_wp)

        fig => get_global_figure()
        n = fig%get_plot_count()

        if (n <= 0) then
            print *, "ERROR: streamplot with cmap produced no plots"
            stop 1
        end if

        plots => fig%get_plots()
        if (trim(plots(n)%colormap) /= 'plasma') then
            print *, "DEBUG: n=", n, " colormap on last plot = '", &
                     trim(plots(n)%colormap), "'"
            print *, "ERROR: Streamplot cmap not stored on last plot. Got '", &
                     trim(plots(n)%colormap), "' expected 'plasma'"
            stop 1
        end if

        print *, "test_cmap_stored_on_plot passed"
    end subroutine test_cmap_stored_on_plot

    subroutine test_label_stored_on_plot()
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

        call figure(figsize=[8.0_wp, 6.0_wp], dpi=100)

        call streamplot(x, y, u, v, label='flow_field', arrowsize=0.0_wp)

        fig => get_global_figure()
        n = fig%get_plot_count()

        if (n <= 0) then
            print *, "ERROR: streamplot with label produced no plots"
            stop 1
        end if

        plots => fig%get_plots()
        if (trim(plots(n)%label) /= 'flow_field') then
            print *, "ERROR: Streamplot label not stored on last plot. Got '", &
                     trim(plots(n)%label), "' expected 'flow_field'"
            stop 1
        end if

        print *, "test_label_stored_on_plot passed"
    end subroutine test_label_stored_on_plot

    subroutine test_linewidth_via_wrapper()
        !! Verify linewidth parameter flows through the stateful wrapper
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

        call figure(figsize=[8.0_wp, 6.0_wp], dpi=100)

        call streamplot(x, y, u, v, linewidth=2.5_wp, arrowsize=0.0_wp)

        fig => get_global_figure()
        n = fig%get_plot_count()

        if (n <= 0) then
            print *, "ERROR: streamplot with linewidth produced no plots"
            stop 1
        end if

        plots => fig%get_plots()
        if (plots(n)%line_width /= 2.5_wp) then
            print *, "ERROR: Streamplot linewidth not applied via wrapper. Got ", &
                     plots(n)%line_width, " expected 2.5"
            stop 1
        end if

        print *, "test_linewidth_via_wrapper passed"
    end subroutine test_linewidth_via_wrapper

    subroutine test_default_generates_arrows()
        !! Default streamplot (no arrowsize specified) generates arrows.
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

        call figure(figsize=[8.0_wp, 6.0_wp], dpi=100)

        call streamplot(x, y, u, v)

        fig => get_global_figure()

        ! Default streamplot should have arrows, not trajectory plots
        if (fig%get_plot_count() > 0) then
            print *, "ERROR: Default streamplot should not generate trajectory plots"
            stop 1
        end if

        ! But should have stream arrows
        if (.not. allocated(fig%state%stream_arrows)) then
            print *, "ERROR: Default streamplot should generate stream arrows"
            stop 1
        end if

        if (size(fig%state%stream_arrows) == 0) then
            print *, "ERROR: Default streamplot arrows array is empty"
            stop 1
        end if

        print *, "test_default_generates_arrows passed"
    end subroutine test_default_generates_arrows

end program test_streamplot_interface_color
