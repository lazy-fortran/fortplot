module fortplot_figure_plot_dispatch
    !! Plot rendering dispatch module
    !!
    !! Single Responsibility: Orchestrate rendering of all plots in a figure.
    !! Individual plot type renderers are in fortplot_figure_plot_renderers.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, PLOT_TYPE_LINE, &
                                  PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                  PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
                                  PLOT_TYPE_BOXPLOT, PLOT_TYPE_ERRORBAR, &
                                  PLOT_TYPE_SURFACE, PLOT_TYPE_PIE, &
                                  PLOT_TYPE_BAR, PLOT_TYPE_REFLINE, &
                                  PLOT_TYPE_QUIVER, PLOT_TYPE_POLAR, &
                                  AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_rendering, only: render_line_plot, render_contour_plot, &
                                  render_pcolormesh_plot, render_fill_between_plot, &
                                  render_markers, render_boxplot_plot, &
                                  render_errorbar_plot, &
                                  render_pie_plot, render_bar_plot
    use fortplot_surface_rendering, only: render_surface_plot
    use fortplot_figure_plot_renderers, only: render_refline_plot, &
                                              render_quiver_plot, &
                                              render_streamplot_arrows, &
                                              render_polar_axes, &
                                              render_polar_plot_internal
    implicit none

    private
    public :: render_all_plots

contains

    subroutine render_all_plots(backend, plots, plot_count, &
                                x_min_transformed, x_max_transformed, &
                                y_min_transformed, y_max_transformed, &
                                xscale, yscale, symlog_threshold, &
                                width, height, margin_left, margin_right, &
                                margin_bottom, margin_top, state)
        !! Render all plots in the figure
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        real(wp), intent(in) :: x_min_transformed, x_max_transformed
        real(wp), intent(in) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        type(figure_state_t), intent(in), optional :: state

        integer :: i
        real(wp) :: x_min_curr, x_max_curr, y_min_curr, y_max_curr
        character(len=10) :: xscale_curr, yscale_curr
        real(wp) :: primary_x_min, primary_x_max, primary_y_min, primary_y_max
        real(wp) :: default_line_width
        logical :: restore_needed

        call resolve_primary_coordinates(state, primary_x_min, primary_x_max, &
                                         primary_y_min, primary_y_max, default_line_width)

        do i = 1, plot_count
            call resolve_plot_coordinates(plots(i), state, primary_x_min, primary_x_max, &
                                          primary_y_min, primary_y_max, &
                                          x_min_curr, x_max_curr, y_min_curr, y_max_curr, &
                                          xscale_curr, yscale_curr, restore_needed)

            if (restore_needed) then
                call backend%set_coordinates(x_min_curr, x_max_curr, y_min_curr, &
                                             y_max_curr)
            end if

            if (plots(i)%line_width > 0.0_wp) then
                call backend%set_line_width(plots(i)%line_width)
            else
                call backend%set_line_width(default_line_width)
            end if

            call dispatch_plot_render(backend, plots(i), &
                                      x_min_curr, x_max_curr, y_min_curr, y_max_curr, &
                                      xscale_curr, yscale_curr, symlog_threshold, &
                                      width, height, margin_left, margin_right, &
                                      margin_bottom, margin_top, default_line_width, state)

            if (present(state) .and. restore_needed) then
                call backend%set_coordinates(primary_x_min, primary_x_max, &
                                             primary_y_min, primary_y_max)
            end if
        end do

        if (present(state)) then
            call backend%set_coordinates(primary_x_min, primary_x_max, primary_y_min, &
                                         primary_y_max)
        end if
    end subroutine render_all_plots

    subroutine resolve_primary_coordinates(state, px_min, px_max, py_min, py_max, default_lw)
        !! Resolve primary coordinate ranges and default line width from figure state
        type(figure_state_t), intent(in), optional :: state
        real(wp), intent(out) :: px_min, px_max, py_min, py_max
        real(wp), intent(out) :: default_lw

        if (present(state)) then
            px_min = state%x_min_transformed
            px_max = state%x_max_transformed
            py_min = state%y_min_transformed
            py_max = state%y_max_transformed
            default_lw = state%current_line_width
        else
            px_min = 0.0_wp; px_max = 0.0_wp
            py_min = 0.0_wp; py_max = 0.0_wp
            default_lw = 1.5_wp
        end if
    end subroutine resolve_primary_coordinates

    subroutine resolve_plot_coordinates(plot, state, primary_x_min, primary_x_max, &
                                        primary_y_min, primary_y_max, &
                                        x_min, x_max, y_min, y_max, &
                                        xscale, yscale, restore_needed)
        !! Resolve coordinate ranges for a single plot, handling twin axes
        type(plot_data_t), intent(in) :: plot
        type(figure_state_t), intent(in), optional :: state
        real(wp), intent(in) :: primary_x_min, primary_x_max, primary_y_min, primary_y_max
        real(wp), intent(out) :: x_min, x_max, y_min, y_max
        character(len=10), intent(out) :: xscale, yscale
        logical, intent(out) :: restore_needed

        if (present(state)) then
            select case (plot%axis)
            case (AXIS_TWINX)
                x_min = state%x_min_transformed
                x_max = state%x_max_transformed
                y_min = state%twinx_y_min_transformed
                y_max = state%twinx_y_max_transformed
                xscale = state%xscale
                yscale = state%twinx_yscale
                restore_needed = .true.
            case (AXIS_TWINY)
                x_min = state%twiny_x_min_transformed
                x_max = state%twiny_x_max_transformed
                y_min = state%y_min_transformed
                y_max = state%y_max_transformed
                xscale = state%twiny_xscale
                yscale = state%yscale
                restore_needed = .true.
            case default
                x_min = primary_x_min
                x_max = primary_x_max
                y_min = primary_y_min
                y_max = primary_y_max
                xscale = state%xscale
                yscale = state%yscale
                restore_needed = .false.
            end select
        else
            x_min = 0.0_wp; x_max = 0.0_wp
            y_min = 0.0_wp; y_max = 0.0_wp
            xscale = ''; yscale = ''
            restore_needed = .false.
        end if
    end subroutine resolve_plot_coordinates

    subroutine dispatch_plot_render(backend, plot, x_min, x_max, y_min, y_max, &
                                    xscale, yscale, symlog_threshold, &
                                    width, height, margin_left, margin_right, &
                                    margin_bottom, margin_top, default_line_width, state)
        !! Dispatch rendering for a single plot type
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        real(wp), intent(in) :: default_line_width
        type(figure_state_t), intent(in), optional :: state

        select case (plot%plot_type)
        case (PLOT_TYPE_LINE)
            call render_line_plot(backend, plot, xscale, yscale, symlog_threshold)
            if (allocated(plot%marker)) then
                call render_markers(backend, plot, x_min, x_max, y_min, y_max, &
                                    xscale, yscale, symlog_threshold)
            end if

        case (PLOT_TYPE_SCATTER)
            if (allocated(plot%marker)) then
                call render_markers(backend, plot, x_min, x_max, y_min, y_max, &
                                    xscale, yscale, symlog_threshold)
            end if

        case (PLOT_TYPE_CONTOUR)
            call render_contour_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                     xscale, yscale, symlog_threshold, &
                                     width, height, margin_left, margin_right, &
                                     margin_bottom, margin_top)

        case (PLOT_TYPE_PCOLORMESH)
            call render_pcolormesh_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                        xscale, yscale, symlog_threshold, &
                                        width, height, margin_right)

        case (PLOT_TYPE_SURFACE)
            call render_surface_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                     xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_FILL)
            call render_fill_between_plot(backend, plot, xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_BAR)
            call render_bar_plot(backend, plot, xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_PIE)
            call render_pie_plot(backend, plot, xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_BOXPLOT)
            call render_boxplot_plot(backend, plot, xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_ERRORBAR)
            call render_errorbar_plot(backend, plot, xscale, yscale, symlog_threshold, &
                                      default_line_width)
            call render_markers(backend, plot, x_min, x_max, y_min, y_max, &
                                xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_REFLINE)
            call render_refline_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                     xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_QUIVER)
            call render_quiver_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                    xscale, yscale, symlog_threshold)

        case (PLOT_TYPE_POLAR)
            call render_polar_plot_internal(backend, plot, x_min, x_max, y_min, y_max, state)

        end select
end subroutine dispatch_plot_render

end module fortplot_figure_plot_dispatch
