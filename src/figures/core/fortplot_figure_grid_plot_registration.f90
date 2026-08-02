module fortplot_figure_grid_plot_registration
    !! Grid-based plot data registration (contour, surface, pcolormesh)
    !! Extracted from fortplot_figure_plot_management for size compliance (refs #1694)

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_CONTOUR, PLOT_TYPE_SURFACE, &
                                  PLOT_TYPE_PCOLORMESH
    use fortplot_logging, only: log_warning
    use fortplot_errors, only: fortplot_error_t
    use fortplot_pcolormesh, only: coordinates_from_centers
    use fortplot_contour_level_calculation, only: compute_default_contour_levels
    implicit none

    private
    public :: add_contour_plot_data, add_colored_contour_plot_data
    public :: add_surface_plot_data, add_parametric_surface_plot_data
    public :: register_pcolormesh_plot_data
    public :: generate_default_contour_levels

contains

    subroutine normalize_grid_shape(x_grid, y_grid, values, normalized)
        !! Normalize rectangular fields to the renderer's (ny,nx) layout.
        !!
        !! Public examples historically used both z(nx,ny) and z(ny,nx).
        !! Store one unambiguous layout at registration time so every backend
        !! sees the complete rectangular domain.
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:)
        real(wp), contiguous, intent(in) :: values(:, :)
        real(wp), allocatable, intent(out) :: normalized(:, :)

        if (size(values, 1) == size(y_grid) .and. &
            size(values, 2) == size(x_grid)) then
            allocate(normalized, source=values)
        else if (size(values, 1) == size(x_grid) .and. &
                 size(values, 2) == size(y_grid)) then
            allocate(normalized(size(y_grid), size(x_grid)))
            normalized = transpose(values)
        else if (size(values, 1) == size(y_grid) - 1 .and. &
                 size(values, 2) == size(x_grid) - 1) then
            allocate(normalized, source=values)
        else if (size(values, 1) == size(x_grid) - 1 .and. &
                 size(values, 2) == size(y_grid) - 1) then
            allocate(normalized(size(y_grid) - 1, size(x_grid) - 1))
            normalized = transpose(values)
        else
            ! Preserve the legacy behavior for malformed inputs; downstream
            ! bounds checks can issue the existing diagnostics.
            allocate(normalized, source=values)
        end if
    end subroutine normalize_grid_shape

    subroutine generate_default_contour_levels(plot_data)
        type(plot_data_t), intent(inout) :: plot_data

        real(wp) :: z_min, z_max

        z_min = minval(plot_data%z_grid)
        z_max = maxval(plot_data%z_grid)
        call compute_default_contour_levels(z_min, z_max, plot_data%contour_levels)
    end subroutine generate_default_contour_levels

    subroutine add_contour_plot_data(plots, plot_count, max_plots, colors, &
                                     x_grid, y_grid, z_grid, levels, label)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(in) :: max_plots
        real(wp), contiguous, intent(in) :: colors(:, :)
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label

        real(wp), allocatable :: normalized_z(:, :)

        if (plot_count >= max_plots) then
            call log_warning("Maximum number of plots reached")
            return
        end if

        plot_count = plot_count + 1
        plots(plot_count)%plot_type = PLOT_TYPE_CONTOUR
        call normalize_grid_shape(x_grid, y_grid, z_grid, normalized_z)
        allocate (plots(plot_count)%x_grid(size(x_grid)))
        allocate (plots(plot_count)%y_grid(size(y_grid)))
        allocate (plots(plot_count)%z_grid(size(normalized_z, 1), &
                                           size(normalized_z, 2)))
        plots(plot_count)%x_grid = x_grid
        plots(plot_count)%y_grid = y_grid
        plots(plot_count)%z_grid = normalized_z

        if (present(levels)) then
            if (size(levels) > 0) then
                allocate (plots(plot_count)%contour_levels(size(levels)))
                plots(plot_count)%contour_levels = levels
            else
                call generate_default_contour_levels(plots(plot_count))
            end if
        else
            call generate_default_contour_levels(plots(plot_count))
        end if

        if (present(label)) then
            plots(plot_count)%label = label
        end if

        plots(plot_count)%color = colors(:, mod(plot_count - 1, size(colors, 2)) + 1)
        ! Match matplotlib: a plain contour() colours each level with the default
        ! colormap (viridis), not a single flat line colour.
        plots(plot_count)%use_color_levels = .true.
        plots(plot_count)%colormap = 'viridis'
    end subroutine add_contour_plot_data

    subroutine add_colored_contour_plot_data(plots, plot_count, max_plots, &
                                             x_grid, y_grid, z_grid, levels, &
                                             cmap, show_colorbar, label, colormap)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(in) :: max_plots
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        real(wp), allocatable :: normalized_z(:, :)

        if (plot_count >= max_plots) then
            call log_warning("Maximum number of plots reached")
            return
        end if

        plot_count = plot_count + 1
        plots(plot_count)%plot_type = PLOT_TYPE_CONTOUR
        call normalize_grid_shape(x_grid, y_grid, z_grid, normalized_z)
        allocate (plots(plot_count)%x_grid(size(x_grid)))
        allocate (plots(plot_count)%y_grid(size(y_grid)))
        allocate (plots(plot_count)%z_grid(size(normalized_z, 1), &
                                           size(normalized_z, 2)))
        plots(plot_count)%x_grid = x_grid
        plots(plot_count)%y_grid = y_grid
        plots(plot_count)%z_grid = normalized_z

        if (present(levels)) then
            if (size(levels) > 0) then
                allocate (plots(plot_count)%contour_levels(size(levels)))
                plots(plot_count)%contour_levels = levels
            else
                call generate_default_contour_levels(plots(plot_count))
            end if
        else
            call generate_default_contour_levels(plots(plot_count))
        end if

        if (present(cmap)) then
            plots(plot_count)%colormap = cmap
        else if (present(colormap)) then
            call log_warning( &
                "add_colored_contour_plot_data: 'colormap' is deprecated; use 'cmap'")
            plots(plot_count)%colormap = colormap
        else
            plots(plot_count)%colormap = 'crest'
        end if

        ! matplotlib contourf does not auto-add a colorbar; one appears only
        ! when explicitly requested (fig.colorbar / show_colorbar=.true.).
        if (present(show_colorbar)) then
            plots(plot_count)%show_colorbar = show_colorbar
        else
            plots(plot_count)%show_colorbar = .false.
        end if

        if (present(label)) then
            plots(plot_count)%label = label
        end if

        plots(plot_count)%use_color_levels = .true.
        plots(plot_count)%fill_contours = .true.
    end subroutine add_colored_contour_plot_data

    subroutine add_surface_plot_data(plots, plot_count, max_plots, colors, &
                                     x_grid, y_grid, z_grid, label, cmap, &
                                     show_colorbar, alpha, edgecolor, linewidth, &
                                     filled, colormap)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(in) :: max_plots
        real(wp), contiguous, intent(in) :: colors(:, :)
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        character(len=*), intent(in), optional :: label, cmap, colormap
        logical, intent(in), optional :: show_colorbar, filled
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        real(wp) :: default_color(3)

        if (plot_count >= max_plots) then
            call log_warning("Maximum number of plots reached")
            return
        end if

        plot_count = plot_count + 1
        plots(plot_count)%plot_type = PLOT_TYPE_SURFACE
        plots(plot_count)%x_grid = x_grid
        plots(plot_count)%y_grid = y_grid
        plots(plot_count)%z_grid = z_grid

        if (present(label)) then
            plots(plot_count)%label = label
        end if

        default_color = colors(:, mod(plot_count - 1, size(colors, 2)) + 1)
        plots(plot_count)%color = default_color

        if (present(edgecolor)) then
            plots(plot_count)%color = edgecolor
            plots(plot_count)%surface_edgecolor = edgecolor
        else
            plots(plot_count)%surface_edgecolor = default_color
        end if

        plots(plot_count)%surface_alpha = 1.0_wp
        if (present(alpha)) plots(plot_count)%surface_alpha = max(0.0_wp, &
                                                                  min(1.0_wp, alpha))

        plots(plot_count)%surface_filled = .false.
        if (present(filled)) plots(plot_count)%surface_filled = filled

        plots(plot_count)%surface_linewidth = 1.0_wp
        if (plots(plot_count)%surface_filled) then
            plots(plot_count)%surface_linewidth = 0.0_wp
        end if
        if (present(linewidth)) plots(plot_count)%surface_linewidth = &
            max(0.0_wp, linewidth)

        plots(plot_count)%surface_use_colormap = .false.
        if (allocated(plots(plot_count)%surface_colormap)) deallocate &
            (plots(plot_count)%surface_colormap)
        if (present(cmap)) then
            if (len_trim(cmap) > 0) then
                allocate (character(len=len_trim(cmap)) :: &
                          plots(plot_count)%surface_colormap)
                plots(plot_count)%surface_colormap = trim(cmap)
                plots(plot_count)%surface_use_colormap = .true.
            end if
        else if (present(colormap)) then
            call log_warning( &
                "add_surface_plot_data: 'colormap' is deprecated; use 'cmap'")
            if (len_trim(colormap) > 0) then
                allocate (character(len=len_trim(colormap)) :: &
                          plots(plot_count)%surface_colormap)
                plots(plot_count)%surface_colormap = trim(colormap)
                plots(plot_count)%surface_use_colormap = .true.
            end if
        end if

        plots(plot_count)%surface_show_colorbar = .false.
        if (present(show_colorbar)) plots(plot_count)%surface_show_colorbar = &
            show_colorbar

    end subroutine add_surface_plot_data

    subroutine add_parametric_surface_plot_data(plots, plot_count, max_plots, &
                                                colors, x_grid, y_grid, z_grid, &
                                                label, cmap, show_colorbar, alpha, &
                                                edgecolor, linewidth, filled, &
                                                row_stride, column_stride)
        !! Register a filled or wireframe tensor-product surface in physical
        !! three-dimensional coordinates.
        !!
        !! ``add_surface`` stores a height field ``z(y,x)``.  A torus, sphere,
        !! or trimmed spline patch instead needs independent x and y coordinates
        !! at every grid point.  Keep the ordinary grid fields as the compact
        !! extent metadata used by the 3-D axes, and retain the full coordinates
        !! in the parametric fields for the renderer.
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(in) :: max_plots
        real(wp), contiguous, intent(in) :: colors(:, :)
        real(wp), contiguous, intent(in) :: x_grid(:, :), y_grid(:, :), z_grid(:, :)
        character(len=*), intent(in), optional :: label, cmap
        logical, intent(in), optional :: show_colorbar, filled
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)
        integer, intent(in), optional :: row_stride, column_stride

        real(wp) :: x_extent(2), y_extent(2)
        integer :: plot_index

        if (size(x_grid, 1) < 2 .or. size(x_grid, 2) < 2 .or. &
            any(shape(y_grid) /= shape(x_grid)) .or. &
            any(shape(z_grid) /= shape(x_grid))) then
            call log_warning("add_parametric_surface: coordinate grids must " // &
                             "have identical shapes of at least 2 by 2")
            return
        end if
        if (plot_count >= max_plots) then
            call log_warning("Maximum number of plots reached")
            return
        end if

        x_extent = [minval(x_grid), maxval(x_grid)]
        y_extent = [minval(y_grid), maxval(y_grid)]
        call add_surface_plot_data(plots, plot_count, max_plots, colors, &
                                   x_extent, y_extent, z_grid, label=label, &
                                   cmap=cmap, show_colorbar=show_colorbar, &
                                   alpha=alpha, edgecolor=edgecolor, &
                                   linewidth=linewidth, filled=filled)
        plot_index = plot_count
        allocate (plots(plot_index)%parametric_x_grid(size(x_grid, 1), size(x_grid, 2)))
        allocate (plots(plot_index)%parametric_y_grid(size(y_grid, 1), size(y_grid, 2)))
        allocate (plots(plot_index)%parametric_z_grid(size(z_grid, 1), size(z_grid, 2)))
        plots(plot_index)%parametric_x_grid = x_grid
        plots(plot_index)%parametric_y_grid = y_grid
        plots(plot_index)%parametric_z_grid = z_grid
        if (present(row_stride)) plots(plot_index)%surface_row_stride = max(1, row_stride)
        if (present(column_stride)) then
            plots(plot_index)%surface_column_stride = max(1, column_stride)
        end if
    end subroutine add_parametric_surface_plot_data

    subroutine register_pcolormesh_plot_data(plots, plot_count, max_plots, &
                                             x, y, c, cmap, vmin, vmax, &
                                             edgecolors, linewidths, colormap, &
                                             shading)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(in) :: max_plots
        real(wp), contiguous, intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: shading, cmap, colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths

        if (plot_count >= max_plots) then
            call log_warning("Maximum number of plots reached")
            return
        end if

        plot_count = plot_count + 1
        plots(plot_count)%plot_type = PLOT_TYPE_PCOLORMESH
        plots(plot_count)%pcolormesh_data%shading = 'flat'
        if (present(shading)) then
            plots(plot_count)%pcolormesh_data%shading = trim(shading)
        end if
        ! matplotlib pcolormesh draws no colorbar unless one is requested.
        plots(plot_count)%show_colorbar = .false.

        block
            type(fortplot_error_t) :: init_error
            integer :: data_nx, data_ny
            real(wp), allocatable :: x_edges(:), y_edges(:)
            real(wp), allocatable :: normalized_c(:, :)
            character(len=:), allocatable :: resolved_cmap

            if (present(cmap)) then
                resolved_cmap = cmap
            else if (present(colormap)) then
                call log_warning( &
                    "register_pcolormesh_plot_data: 'colormap' is deprecated; use 'cmap'")
                resolved_cmap = colormap
            end if

            call normalize_grid_shape(x, y, c, normalized_c)
            data_ny = size(normalized_c, 1)
            data_nx = size(normalized_c, 2)

            if (size(x) == data_nx .and. size(y) == data_ny) then
                allocate (x_edges(data_nx + 1))
                allocate (y_edges(data_ny + 1))
                call coordinates_from_centers(x, x_edges)
                call coordinates_from_centers(y, y_edges)
                call plots(plot_count)%pcolormesh_data%initialize_regular_grid( &
                    x_edges, y_edges, normalized_c, resolved_cmap, init_error)
            elseif (size(x) == data_ny .and. size(y) == data_nx) then
                allocate (x_edges(data_ny + 1))
                allocate (y_edges(data_nx + 1))
                call coordinates_from_centers(x, x_edges)
                call coordinates_from_centers(y, y_edges)
                call plots(plot_count)%pcolormesh_data%initialize_regular_grid( &
                    x_edges, y_edges, normalized_c, resolved_cmap, init_error)
            else
                call plots(plot_count)%pcolormesh_data%initialize_regular_grid( &
                    x, y, normalized_c, resolved_cmap, init_error)
            end if
        end block

        if (present(vmin)) then
            plots(plot_count)%pcolormesh_data%vmin = vmin
            plots(plot_count)%pcolormesh_data%vmin_set = .true.
        end if

        if (present(vmax)) then
            plots(plot_count)%pcolormesh_data%vmax = vmax
            plots(plot_count)%pcolormesh_data%vmax_set = .true.
        end if

        if (present(edgecolors)) then
            plots(plot_count)%pcolormesh_data%show_edges = .true.
            plots(plot_count)%pcolormesh_data%edge_color = edgecolors
        end if

        if (present(linewidths)) then
            plots(plot_count)%pcolormesh_data%edge_width = linewidths
        end if
    end subroutine register_pcolormesh_plot_data

end module fortplot_figure_grid_plot_registration
