module fortplot_bar_rendering
    !! Bar plot rendering for memory/vector backends
    !!
    !! Renders vertical and horizontal bars using backend quadrilateral fills
    !! with outline reinforcement for ASCII and vector outputs.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_scales, only: apply_scale_transform
    implicit none

    private
    public :: render_bar_plot

contains

    subroutine render_bar_plot(backend, plot_data, xscale, yscale, symlog_threshold)
        !! Render bar plots (vertical or horizontal) for the active backend
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: i, j, n
        real(wp) :: half_width
        real(wp) :: x_data(4), y_data(4)
        real(wp) :: x_screen(4), y_screen(4)
        real(wp) :: effective_width

        if (.not. allocated(plot_data%bar_x)) return
        if (.not. allocated(plot_data%bar_heights)) return

        n = min(size(plot_data%bar_x), size(plot_data%bar_heights))
        if (n <= 0) return

        effective_width = plot_data%bar_width
        if (effective_width <= 0.0_wp) effective_width = 0.8_wp
        half_width = 0.5_wp * abs(effective_width)

        call backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))

        do i = 1, n
            if (plot_data%bar_horizontal) then
                x_data = [min(0.0_wp, plot_data%bar_heights(i)), &
                          max(0.0_wp, plot_data%bar_heights(i)), &
                          max(0.0_wp, plot_data%bar_heights(i)), &
                          min(0.0_wp, plot_data%bar_heights(i))]
                y_data = [plot_data%bar_x(i) - half_width, &
                          plot_data%bar_x(i) - half_width, &
                          plot_data%bar_x(i) + half_width, &
                          plot_data%bar_x(i) + half_width]
            else
                x_data = [plot_data%bar_x(i) - half_width, &
                          plot_data%bar_x(i) + half_width, &
                          plot_data%bar_x(i) + half_width, &
                          plot_data%bar_x(i) - half_width]
                y_data = [min(0.0_wp, plot_data%bar_heights(i)), &
                          min(0.0_wp, plot_data%bar_heights(i)), &
                          max(0.0_wp, plot_data%bar_heights(i)), &
                          max(0.0_wp, plot_data%bar_heights(i))]
            end if

            do j = 1, 4
                x_screen(j) = apply_scale_transform(x_data(j), xscale, symlog_threshold)
                y_screen(j) = apply_scale_transform(y_data(j), yscale, symlog_threshold)
            end do

            call backend%fill_quad(x_screen, y_screen)
            call backend%line(x_screen(1), y_screen(1), x_screen(2), y_screen(2))
            call backend%line(x_screen(2), y_screen(2), x_screen(3), y_screen(3))
            call backend%line(x_screen(3), y_screen(3), x_screen(4), y_screen(4))
            call backend%line(x_screen(4), y_screen(4), x_screen(1), y_screen(1))
        end do
    end subroutine render_bar_plot

end module fortplot_bar_rendering
