module fortplot_figure_plot_management
    !! Figure plot data management module
    !!
    !! Single Responsibility: Manage plot data storage and operations
    !! Extracted from fortplot_figure_core to improve modularity

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, &
                                  PLOT_TYPE_PCOLORMESH, PLOT_TYPE_FILL, &
                                  PLOT_TYPE_SURFACE, PLOT_TYPE_PIE
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_logging, only: log_warning, log_info
    use fortplot_legend, only: legend_t
    use fortplot_errors, only: fortplot_error_t
    use fortplot_pcolormesh, only: coordinates_from_centers
    use fortplot_contour_level_calculation, only: compute_default_contour_levels
    use fortplot_figure_legend_setup, only: setup_figure_legend
    implicit none

    private
    public :: register_line_plot_data, add_contour_plot_data, add_colored_contour_plot_data
    public :: add_surface_plot_data, register_pcolormesh_plot_data, &
              add_fill_between_plot_data
    public :: generate_default_contour_levels
    public :: setup_figure_legend, update_plot_ydata, validate_plot_data
    public :: next_plot_color

contains

    subroutine validate_plot_data(x, y, label)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label
        character(len=100) :: label_str
        logical :: has_label

        if (present(label)) then
            label_str = "'"//trim(label)//"'"
            has_label = len_trim(label) > 0
        else
            label_str = "(unlabeled plot)"
            has_label = .false.
        end if

        if (size(x) == 0 .or. size(y) == 0) then
            call log_warning("Plot data "//trim(label_str)// &
                             " contains zero-size arrays. The plot will show axes "// &
                             "and labels but no data points.")
            return
        end if

        if (size(x) /= size(y)) then
            call log_warning("Plot data "//trim(label_str)// &
                             " has mismatched array sizes: "// &
                             trim(adjustl(transfer(size(x), '          ')))//" vs "// &
                             trim(adjustl(transfer(size(y), '          ')))// &
                             ". Only the common size will be plotted.")
        end if

        if (size(x) == 1 .and. size(y) == 1) then
            call log_info("Plot data "//trim(label_str)// &
                          " contains a single point. Automatic scaling will add "// &
                          "margins for visibility.")
        end if

        if (has_label) then
            if (size(x) > 1 .and. abs(maxval(x) - minval(x)) < 1.0e-10_wp) then
                call log_warning("All x values in plot "//trim(label_str)// &
                                 " are identical. This may result in a vertical "// &
                                 "line or poor visualization.")
            end if

            if (size(y) > 1 .and. abs(maxval(y) - minval(y)) < 1.0e-10_wp) then
                call log_warning("All y values in plot "//trim(label_str)// &
                                 " are identical. This may result in a horizontal "// &
                                 "line or poor visualization.")
            end if
        end if
    end subroutine validate_plot_data

    pure function next_plot_color(state) result(color)
        type(figure_state_t), intent(in) :: state
        real(wp) :: color(3)
        integer :: palette_size

        palette_size = size(state%colors, 2)
        if (palette_size <= 0) then
            color = [0.0_wp, 0.0_wp, 0.0_wp]
        else
            color = state%colors(:, mod(state%plot_count, palette_size) + 1)
        end if
    end function next_plot_color

    subroutine register_line_plot_data(plots, plot_count, max_plots, &
                                      x, y, label, linestyle, color, marker)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(in) :: max_plots
        real(wp), contiguous, intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in) :: color(3)

        if (plot_count >= max_plots) then
            call log_warning("Maximum number of plots reached")
            return
        end if

        call validate_plot_data(x, y, label)

        plot_count = plot_count + 1
        plots(plot_count)%plot_type = PLOT_TYPE_LINE
        plots(plot_count)%x = x
        plots(plot_count)%y = y
        plots(plot_count)%color = color

        if (present(label)) then
            plots(plot_count)%label = label
        end if

        if (present(linestyle)) then
            plots(plot_count)%linestyle = linestyle
        else
            plots(plot_count)%linestyle = '-'
        end if

        if (present(marker)) then
            if (len_trim(marker) > 0) then
                plots(plot_count)%marker = marker
            end if
        end if
    end subroutine register_line_plot_data

    subroutine add_fill_between_plot_data(plots, plot_count, max_plots, x, upper, &
                                          lower, mask, &
                                          color, alpha)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(in) :: max_plots
        real(wp), contiguous, intent(in) :: x(:)
        real(wp), contiguous, intent(in) :: upper(:)
        real(wp), contiguous, intent(in) :: lower(:)
        logical, intent(in), optional :: mask(:)
        real(wp), intent(in) :: color(3)
        real(wp), intent(in), optional :: alpha

        integer :: n
        logical :: has_mask

        if (plot_count >= max_plots) then
            call log_warning('fill_between: maximum number of plots reached')
            return
        end if

        n = size(x)
        if (n < 2) then
            call log_warning('fill_between: at least two points required for area fill')
            return
        end if

        if (size(upper) /= n .or. size(lower) /= n) then
            call log_warning('fill_between: array size mismatch')
            return
        end if

        if (present(mask)) then
            if (size(mask) /= n) then
                call log_warning('fill_between: mask size mismatch; '// &
                                 'ignoring fill segment')
                return
            end if
            if (.not. any(mask)) then
                call log_warning('fill_between: mask excludes all points')
                return
            end if
            has_mask = .true.
        else
            has_mask = .false.
        end if

        plot_count = plot_count + 1
        call reset_plot_storage(plots(plot_count))

        plots(plot_count)%plot_type = PLOT_TYPE_FILL
        plots(plot_count)%color = color
        if (present(alpha)) then
            plots(plot_count)%fill_alpha = max(0.0_wp, min(1.0_wp, alpha))
        else
            plots(plot_count)%fill_alpha = 1.0_wp
        end if

        call assign_vector(plots(plot_count)%fill_between_data%x, x)
        call assign_vector(plots(plot_count)%fill_between_data%upper, upper)
        call assign_vector(plots(plot_count)%fill_between_data%lower, lower)

        plots(plot_count)%fill_between_data%has_mask = has_mask
        if (has_mask) then
            call assign_logical_vector(plots(plot_count)%fill_between_data%mask, mask)
        else
            if (allocated(plots(plot_count)%fill_between_data%mask)) then
                deallocate (plots(plot_count)%fill_between_data%mask)
            end if
        end if
    end subroutine add_fill_between_plot_data

    subroutine assign_vector(target, source)
        real(wp), allocatable, intent(inout) :: target(:)
        real(wp), contiguous, intent(in) :: source(:)

        target = source
    end subroutine assign_vector

    subroutine assign_logical_vector(target, source)
        logical, allocatable, intent(inout) :: target(:)
        logical, intent(in) :: source(:)

        target = source
    end subroutine assign_logical_vector

    subroutine reset_plot_storage(plot)
        type(plot_data_t), intent(inout) :: plot

        if (allocated(plot%fill_between_data%x)) deallocate (plot%fill_between_data%x)
        if (allocated(plot%fill_between_data%upper)) deallocate &
            (plot%fill_between_data%upper)
        if (allocated(plot%fill_between_data%lower)) deallocate &
            (plot%fill_between_data%lower)
        if (allocated(plot%fill_between_data%mask)) deallocate &
            (plot%fill_between_data%mask)
        plot%fill_between_data%has_mask = .false.
        plot%fill_alpha = 1.0_wp
        plot%surface_show_colorbar = .false.
        plot%surface_alpha = 1.0_wp
        plot%surface_linewidth = 1.0_wp
        plot%surface_use_colormap = .false.
        plot%surface_edgecolor = [0.0_wp, 0.447_wp, 0.698_wp]
        if (allocated(plot%surface_colormap)) deallocate (plot%surface_colormap)
        plot%pie_slice_count = 0
        if (allocated(plot%pie_start)) deallocate (plot%pie_start)
        if (allocated(plot%pie_end)) deallocate (plot%pie_end)
        if (allocated(plot%pie_offsets)) deallocate (plot%pie_offsets)
        if (allocated(plot%pie_colors)) deallocate (plot%pie_colors)
        if (allocated(plot%pie_label_pos)) deallocate (plot%pie_label_pos)
        if (allocated(plot%pie_values)) deallocate (plot%pie_values)
        if (allocated(plot%pie_source_index)) deallocate (plot%pie_source_index)
        if (allocated(plot%pie_labels)) deallocate (plot%pie_labels)
        if (allocated(plot%pie_autopct)) deallocate (plot%pie_autopct)
        plot%pie_radius = 1.0_wp
        plot%pie_center = [0.0_wp, 0.0_wp]
    end subroutine reset_plot_storage

    subroutine add_contour_plot_data(plots, plot_count, max_plots, colors, &
                                     x_grid, y_grid, z_grid, levels, label)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(in) :: max_plots
        real(wp), contiguous, intent(in) :: colors(:, :)
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label

        if (plot_count >= max_plots) then
            call log_warning("Maximum number of plots reached")
            return
        end if

        plot_count = plot_count + 1
        plots(plot_count)%plot_type = PLOT_TYPE_CONTOUR
        allocate (plots(plot_count)%x_grid(size(x_grid)))
        allocate (plots(plot_count)%y_grid(size(y_grid)))
        allocate (plots(plot_count)%z_grid(size(z_grid, 1), size(z_grid, 2)))
        plots(plot_count)%x_grid = x_grid
        plots(plot_count)%y_grid = y_grid
        plots(plot_count)%z_grid = z_grid

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
        plots(plot_count)%use_color_levels = .false.
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

        if (plot_count >= max_plots) then
            call log_warning("Maximum number of plots reached")
            return
        end if

        plot_count = plot_count + 1
        plots(plot_count)%plot_type = PLOT_TYPE_CONTOUR
        allocate (plots(plot_count)%x_grid(size(x_grid)))
        allocate (plots(plot_count)%y_grid(size(y_grid)))
        allocate (plots(plot_count)%z_grid(size(z_grid, 1), size(z_grid, 2)))
        plots(plot_count)%x_grid = x_grid
        plots(plot_count)%y_grid = y_grid
        plots(plot_count)%z_grid = z_grid

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

        if (present(show_colorbar)) then
            plots(plot_count)%show_colorbar = show_colorbar
        else
            plots(plot_count)%show_colorbar = .true.
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

        plots(plot_count)%surface_linewidth = 1.0_wp
        if (present(linewidth)) plots(plot_count)%surface_linewidth = &
            max(1.0e-6_wp, linewidth)

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

        plots(plot_count)%surface_filled = .false.
        if (present(filled)) plots(plot_count)%surface_filled = filled
    end subroutine add_surface_plot_data

    subroutine register_pcolormesh_plot_data(plots, plot_count, max_plots, &
                                           x, y, c, cmap, vmin, vmax, &
                                           edgecolors, linewidths, colormap)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(in) :: max_plots
        real(wp), contiguous, intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths

        if (plot_count >= max_plots) then
            call log_warning("Maximum number of plots reached")
            return
        end if

        plot_count = plot_count + 1
        plots(plot_count)%plot_type = PLOT_TYPE_PCOLORMESH

        block
            type(fortplot_error_t) :: init_error
            integer :: data_nx, data_ny
            real(wp), allocatable :: x_edges(:), y_edges(:)
            character(len=:), allocatable :: resolved_cmap

            if (present(cmap)) then
                resolved_cmap = cmap
            else if (present(colormap)) then
                call log_warning( &
                    "register_pcolormesh_plot_data: 'colormap' is deprecated; use 'cmap'")
                resolved_cmap = colormap
            end if

            data_ny = size(c, 1)
            data_nx = size(c, 2)

            if (size(x) == data_nx .and. size(y) == data_ny) then
                allocate (x_edges(data_nx + 1))
                allocate (y_edges(data_ny + 1))
                call coordinates_from_centers(x, x_edges)
                call coordinates_from_centers(y, y_edges)
                call plots(plot_count)%pcolormesh_data%initialize_regular_grid( &
                    x_edges, y_edges, c, resolved_cmap, init_error)
            elseif (size(x) == data_ny .and. size(y) == data_nx) then
                allocate (x_edges(data_ny + 1))
                allocate (y_edges(data_nx + 1))
                call coordinates_from_centers(x, x_edges)
                call coordinates_from_centers(y, y_edges)
                call plots(plot_count)%pcolormesh_data%initialize_regular_grid( &
                    x_edges, y_edges, c, resolved_cmap, init_error)
            else
                call plots(plot_count)%pcolormesh_data%initialize_regular_grid( &
                    x, y, c, resolved_cmap, init_error)
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

    subroutine generate_default_contour_levels(plot_data)
        type(plot_data_t), intent(inout) :: plot_data

        real(wp) :: z_min, z_max

        z_min = minval(plot_data%z_grid)
        z_max = maxval(plot_data%z_grid)
        call compute_default_contour_levels(z_min, z_max, plot_data%contour_levels)
    end subroutine generate_default_contour_levels

    subroutine update_plot_ydata(plots, plot_count, plot_index, y_new)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        integer, intent(in) :: plot_index
        real(wp), contiguous, intent(in) :: y_new(:)
        character(len=32) :: idx_str, new_sz, old_sz

        if (plot_index < 1 .or. plot_index > plot_count) then
            write (idx_str, '(I0)') plot_index
            call log_warning("Invalid plot index: "//trim(adjustl(idx_str)))
            return
        end if

        if (.not. allocated(plots(plot_index)%y)) then
            write (idx_str, '(I0)') plot_index
            call log_warning("Plot "//trim(adjustl(idx_str))// &
                             " has no y data to update")
            return
        end if

        if (size(y_new) /= size(plots(plot_index)%y)) then
            write (new_sz, '(I0)') size(y_new)
            write (old_sz, '(I0)') size(plots(plot_index)%y)
            call log_warning("New y data size "//trim(adjustl(new_sz))// &
                             " does not match existing size "//trim(adjustl(old_sz)))
            return
        end if

        plots(plot_index)%y = y_new
    end subroutine update_plot_ydata

end module fortplot_figure_plot_management
