submodule(fortplot_figure_core) fortplot_figure_core_wrappers

    implicit none

contains

    module subroutine initialize(self, width, height, backend, dpi)
        !! Initialize figure with optional DPI support
        !! Added DPI parameter for consistency with matplotlib interface
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        real(wp), intent(in), optional :: dpi

        call core_initialize(self%state, self%plots, self%streamlines, &
                             self%subplots_array, self%subplot_rows, &
                             self%subplot_cols, self%current_subplot, &
                             self%title, self%xlabel, self%ylabel, &
                             self%plot_count, width, height, backend, dpi)
    end subroutine initialize

    module subroutine add_plot_real(self, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)

        call core_add_plot(self%plots, self%state, x, y, label, linestyle, color, &
                           self%plot_count)
    end subroutine add_plot_real

    module subroutine colorbar(self, plot_index, label, location, fraction, pad, &
                               shrink, ticks, ticklabels, label_fontsize)
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: plot_index
        character(len=*), intent(in), optional :: label, location
        real(wp), intent(in), optional :: fraction, pad, shrink
        real(wp), intent(in), optional :: ticks(:)
        character(len=*), intent(in), optional :: ticklabels(:)
        real(wp), intent(in), optional :: label_fontsize

        if (self%subplot_rows > 0 .and. self%subplot_cols > 0) then
            call log_error("colorbar: Subplot grids are not supported yet")
            return
        end if

        call core_colorbar(self%state, self%plots, self%plot_count, &
                           plot_index=plot_index, &
                           label=label, location=location, fraction=fraction, pad=pad, &
                           shrink=shrink, ticks=ticks, ticklabels=ticklabels, &
                           label_fontsize=label_fontsize)
    end subroutine colorbar

    module subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label

        call core_add_contour(self%plots, self%state, x_grid, y_grid, z_grid, &
                              levels, label, self%plot_count)
    end subroutine add_contour

    module subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, &
                                         colormap, show_colorbar, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar

        call core_add_contour_filled(self%plots, self%state, x_grid, y_grid, &
                                     z_grid, levels, colormap, show_colorbar, &
                                     label, self%plot_count)
    end subroutine add_contour_filled

    module subroutine add_surface(self, x_grid, y_grid, z_grid, label, colormap, &
                                  show_colorbar, alpha, edgecolor, linewidth, filled)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        character(len=*), intent(in), optional :: label, colormap
        logical, intent(in), optional :: show_colorbar, filled
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        call core_add_surface(self%plots, self%state, x_grid, y_grid, z_grid, &
                              label, colormap, show_colorbar, alpha, edgecolor, &
                              linewidth, filled, self%plot_count)
    end subroutine add_surface

    module subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, edgecolors, &
                                     linewidths)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths

        call core_add_pcolormesh(self%plots, self%state, x, y, c, colormap, &
                                 vmin, vmax, edgecolors, linewidths, &
                                 self%plot_count)
    end subroutine add_pcolormesh

    module subroutine streamplot(self, x, y, u, v, density, color, &
                                 linewidth, rtol, &
                                 atol, max_time)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth
        real(wp), intent(in), optional :: rtol, atol, max_time

        call core_streamplot(self%plots, self%state, self%plot_count, &
                             x, y, u, v, &
                             density, color, linewidth, rtol, atol, max_time)
    end subroutine streamplot

    module subroutine quiver(self, x, y, u, v, scale, color, width, headwidth, &
                             headlength, units)
        !! Add quiver plot (discrete vector arrows) to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units

        call core_quiver(self%plots, self%state, self%plot_count, x, y, u, v, &
                         scale, color, width, headwidth, headlength, units)
    end subroutine quiver

    module subroutine add_hist(self, data, bins, density, label, color)
        !! Create a histogram plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)

        call core_hist(self%plots, self%state, self%plot_count, data, bins, &
                       density, label, color)
    end subroutine add_hist

    module subroutine boxplot(self, data, position, width, label, show_outliers, &
                              horizontal, color)
        !! Create a box plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        character(len=*), intent(in), optional :: color

        call core_boxplot(self%plots, self%state, self%plot_count, data, &
                          position, width, &
                          label, show_outliers, horizontal, color, &
                          self%state%max_plots)
    end subroutine boxplot

    module subroutine scatter(self, x, y, s, c, marker, markersize, color, &
                              colormap, alpha, edgecolor, facecolor, linewidth, &
                              vmin, vmax, label, show_colorbar)
        !! Add an efficient scatter plot using a single plot object
        !! Properly handles thousands of points without O(n) overhead
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, alpha, linewidth, vmin, vmax
        real(wp), intent(in), optional :: color(3), edgecolor(3), facecolor(3)
        logical, intent(in), optional :: show_colorbar

        real(wp) :: default_color(3)

        ! Get default color from state using shared cycling logic
        default_color = next_plot_color(self%state)

        call core_scatter(self%plots, self%state, self%plot_count, x, y, s, c, &
                          marker, markersize, color, colormap, alpha, edgecolor, &
                          facecolor, linewidth, vmin, vmax, label, show_colorbar, &
                          default_color)
    end subroutine scatter

end submodule fortplot_figure_core_wrappers
