submodule(fortplot_figure_core) fortplot_figure_core_impl_plots

    !! Plot operations implementations
    !!
    !! Single Responsibility: Handle all plot creation and data addition
    !! operations including line plots, contours, surfaces, pcolormesh,
    !! scatter, histograms, boxplots, and specialized plots.
    !! Extracted from fortplot_figure_core_impl to maintain file size compliance.

    implicit none

contains

    !! ── Initialization ────────────────────────────────────────────────

    module subroutine initialize(self, width, height, backend, dpi)
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

    !! ── Basic plot operations ─────────────────────────────────────────

    module subroutine add_plot_real(self, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x(:), y(:)
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
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label

        call core_add_contour(self%plots, self%state, x_grid, y_grid, z_grid, &
                               levels, label, self%plot_count)
    end subroutine add_contour

    module subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, &
                                            cmap, show_colorbar, label, colormap)
        !! Add a filled contour plot to the figure
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        call core_add_contour_filled(self%plots, self%state, x_grid, y_grid, &
                                        z_grid, levels=levels, cmap=cmap, &
                                        show_colorbar=show_colorbar, label=label, &
                                        colormap=colormap, plot_count=self%plot_count)
    end subroutine add_contour_filled

    module subroutine add_contourf(self, x_grid, y_grid, z_grid, levels, &
                                    cmap, show_colorbar, label, colormap)
        !! Add a filled contour plot to the figure
        !!
        !! Matplotlib-canonical alias for add_contour_filled.
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        call self%add_contour_filled(x_grid, y_grid, z_grid, levels=levels, &
                                      cmap=cmap, show_colorbar=show_colorbar, &
                                      label=label, colormap=colormap)
    end subroutine add_contourf

    module subroutine add_surface(self, x_grid, y_grid, z_grid, label, cmap, &
                                   show_colorbar, alpha, edgecolor, linewidth, filled, &
                                   colormap)
        !! Add a 3D surface plot to the figure
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        character(len=*), intent(in), optional :: label, cmap, colormap
        logical, intent(in), optional :: show_colorbar, filled
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        call core_add_surface(self%plots, self%state, x_grid, y_grid, z_grid, &
                              label=label, cmap=cmap, show_colorbar=show_colorbar, &
                              alpha=alpha, edgecolor=edgecolor, linewidth=linewidth, &
                              filled=filled, plot_count=self%plot_count, &
                              colormap=colormap)
    end subroutine add_surface

    module subroutine add_pcolormesh(self, x, y, c, cmap, vmin, vmax, edgecolors, &
                                      linewidths, colormap)
        !! Add a pcolormesh plot to the figure
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths

        call core_add_pcolormesh(self%plots, self%state, x, y, c, cmap=cmap, &
                                  vmin=vmin, vmax=vmax, edgecolors=edgecolors, &
                                  linewidths=linewidths, plot_count=self%plot_count, &
                                  colormap=colormap)
    end subroutine add_pcolormesh

    !! ── Specialized plot operations ───────────────────────────────────

    module subroutine streamplot(self, x, y, u, v, density, color, &
                                  linewidth, rtol, &
                                  atol, max_time, arrowsize, arrowstyle)
        use fortplot_plotting_advanced, only: streamplot_impl
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth
        real(wp), intent(in), optional :: rtol, atol, max_time
        real(wp), intent(in), optional :: arrowsize
        character(len=*), intent(in), optional :: arrowstyle

        call streamplot_impl(self, x, y, u, v, density=density, color=color, &
                              linewidth=linewidth, rtol=rtol, atol=atol, &
                              max_time=max_time, arrowsize=arrowsize, &
                              arrowstyle=arrowstyle)
    end subroutine streamplot

    module subroutine quiver(self, x, y, u, v, scale, color, width, headwidth, &
                              headlength, units, pivot, scale_units, angles, colormap, &
                              alpha)
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units, pivot, scale_units, angles
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: alpha

        call core_quiver(self%plots, self%state, self%plot_count, x, y, u, v, &
                          scale, color, width, headwidth, headlength, units, &
                          pivot, scale_units, angles=angles, colormap=colormap, &
                          alpha=alpha)
    end subroutine quiver

    module subroutine add_hist(self, data, bins, density, label, color, &
                            range, weights, cumulative, orientation, alpha)
        !! Add a histogram plot (matplotlib-compatible).
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: range(2)
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: orientation
        real(wp), intent(in), optional :: alpha

        real(wp) :: color_rgb(3)
        logical :: has_color

        if (present(color) .and. len_trim(color) > 0) then
            call resolve_color_string_or_rgb(color_str=color, context='hist', &
                                             rgb_out=color_rgb, has_color=has_color)
            if (has_color) then
                call core_hist(self%plots, self%state, self%plot_count, data, bins, &
                               density, label, color_rgb, range=range, weights=weights, &
                               cumulative=cumulative, orientation=orientation, &
                               alpha=alpha)
            else
                call core_hist(self%plots, self%state, self%plot_count, data, bins, &
                               density, label, range=range, weights=weights, &
                               cumulative=cumulative, orientation=orientation, &
                               alpha=alpha)
            end if
        else
            call core_hist(self%plots, self%state, self%plot_count, data, bins, &
                           density, label, range=range, weights=weights, &
                           cumulative=cumulative, orientation=orientation, &
                           alpha=alpha)
        end if
    end subroutine add_hist

    module subroutine boxplot(self, data, position, width, label, show_outliers, &
                               horizontal, color)
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        real(wp), intent(in), optional :: color(3)

        call core_boxplot(self%plots, self%state, self%plot_count, data, &
                           position, width, &
                           label, show_outliers, horizontal, color, &
                           self%state%max_plots)
    end subroutine boxplot

    module subroutine scatter(self, x, y, s, c, marker, markersize, color, &
                               colormap, alpha, edgecolor, facecolor, linewidth, &
                               vmin, vmax, label, show_colorbar)
        !! Scatter with deferred-shape `s(..)` to accept scalar or array.
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(..), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, alpha, linewidth, vmin, vmax
        real(wp), intent(in), optional :: color(3), edgecolor(3), facecolor(3)
        logical, intent(in), optional :: show_colorbar

        real(wp) :: default_color(3)

        ! Scatter uses matplotlib's patch colour cycle, independent of the line
        ! cycle: it neither consumes nor is offset by line plots. Storage must
        ! exist before counting patch artists for the cycle.
        call ensure_figure_storage(self%plots, self%state)
        default_color = next_patch_color(self%state, self%plots)

        call core_scatter(self%plots, self%state, self%plot_count, x, y, s, c, &
                           marker, markersize, color, colormap, alpha, edgecolor, &
                           facecolor, linewidth, vmin, vmax, label, show_colorbar, &
                           default_color)
    end subroutine scatter

end submodule fortplot_figure_core_impl_plots
