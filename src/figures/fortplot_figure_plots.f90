module fortplot_figure_plots
    !! Plot creation methods for figure_t
    !! 
    !! This module contains the core plot creation functionality extracted
    !! from fortplot_figure_core to achieve QADS compliance (<500 lines).
    !!
    !! Single Responsibility: Handle creation of different plot types
    !! (line plots, contours, filled contours, pcolormesh)

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_PIE
    use fortplot_figure_plot_management
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_format_parser, only: parse_format_string
    use fortplot_colors, only: parse_color
    use fortplot_logging, only: log_warning, log_error
    implicit none

    private
    public :: figure_add_plot, figure_add_contour, figure_add_contour_filled
    public :: figure_add_surface, figure_add_pcolormesh, figure_add_fill_between
    public :: figure_add_pie

contains

    subroutine figure_add_plot(plots, state, x, y, label, linestyle, color)
        !! Add a line plot to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        real(wp) :: plot_color(3)
        character(len=:), allocatable :: ls
        character(len=20) :: parsed_marker, parsed_linestyle
        
        ! Determine color
        if (present(color)) then
            plot_color = color
        else
            plot_color = next_plot_color(state)
        end if
        
        ! Parse linestyle to extract marker and actual linestyle
        if (present(linestyle)) then
            call parse_format_string(linestyle, parsed_marker, parsed_linestyle)
            if (len_trim(parsed_linestyle) > 0) then
                ls = trim(parsed_linestyle)
            else
                ! If only a marker was specified, do not draw connecting lines
                ls = 'none'
            end if
        else
            ls = '-'
            parsed_marker = ''
        end if
        
        ! Add the plot data using focused module
        call add_line_plot_data(plots, state%plot_count, state%max_plots, &
                               x, y, label, ls, plot_color, &
                               marker=trim(parsed_marker))
    end subroutine figure_add_plot

    subroutine figure_add_contour(plots, state, x_grid, y_grid, z_grid, levels, label)
        !! Add a contour plot to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call add_contour_plot_data(plots, state%plot_count, state%max_plots, &
                                  state%colors, x_grid, y_grid, z_grid, levels, label)
    end subroutine figure_add_contour

    subroutine figure_add_contour_filled(plots, state, x_grid, y_grid, z_grid, levels, &
                                        colormap, show_colorbar, label)
        !! Add a filled contour plot with color mapping
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar

        call add_colored_contour_plot_data(plots, state%plot_count, state%max_plots, &
                                          x_grid, y_grid, z_grid, levels, colormap, &
                                          show_colorbar, label)
    end subroutine figure_add_contour_filled

    subroutine figure_add_surface(plots, state, x_grid, y_grid, z_grid, label, colormap, &
                                  show_colorbar, alpha, edgecolor, linewidth)
        !! Add a 3D surface plot to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        character(len=*), intent(in), optional :: label, colormap
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        call add_surface_plot_data(plots, state%plot_count, state%max_plots, state%colors, &
                                   x_grid, y_grid, z_grid, label, colormap, show_colorbar, &
                                   alpha, edgecolor, linewidth)
    end subroutine figure_add_surface

    subroutine figure_add_pcolormesh(plots, state, x, y, c, colormap, vmin, vmax, &
                                    edgecolors, linewidths)
        !! Add a pcolormesh plot
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        
        call add_pcolormesh_plot_data(plots, state%plot_count, state%max_plots, &
                                     x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
    end subroutine figure_add_pcolormesh

    subroutine figure_add_fill_between(plots, state, x, upper, lower, mask, color_string, alpha)
        !! Add an area fill between two curves
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: upper(:)
        real(wp), intent(in) :: lower(:)
        logical, intent(in), optional :: mask(:)
        character(len=*), intent(in), optional :: color_string
        real(wp), intent(in), optional :: alpha

        real(wp) :: fill_color(3)
        logical :: success
        real(wp) :: fill_alpha

        fill_color = next_plot_color(state)
        if (present(color_string)) then
            call parse_color(color_string, fill_color, success)
            if (.not. success) then
                call log_warning('fill_between: unsupported color string; using default palette color')
                fill_color = next_plot_color(state)
            end if
        end if

        fill_alpha = 1.0_wp
        if (present(alpha)) fill_alpha = max(0.0_wp, min(1.0_wp, alpha))

        call add_fill_between_plot_data(plots, state%plot_count, state%max_plots, x, upper, lower, &
                                        mask, fill_color, fill_alpha)
    end subroutine figure_add_fill_between

    subroutine figure_add_pie(plots, state, values, labels, startangle, color_strings, explode, autopct)
        !! Store pie chart slices using polar wedges with optional explode & colors
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        real(wp), intent(in), optional :: startangle
        character(len=*), intent(in), optional :: color_strings(:)
        real(wp), intent(in), optional :: explode(:)
        character(len=*), intent(in), optional :: autopct

        integer :: n_total, slice_count, plot_idx, i, palette_size, max_len
        integer, allocatable :: idx_map(:)
        real(wp), allocatable :: explode_values(:)
        real(wp) :: total, current_angle, angle_span, mid_angle
        real(wp) :: start_angle_rad, radius, label_radius
        real(wp) :: offset_value, center_x, center_y
        real(wp) :: color_rgb(3)
        logical :: success
        character(len=40) :: idx_str
        real(wp), parameter :: PI = acos(-1.0_wp)

        n_total = size(values)
        if (n_total == 0) then
            call log_error('pie: values array must contain data')
            return
        end if

        allocate(idx_map(n_total))
        slice_count = 0
        total = 0.0_wp
        do i = 1, n_total
            if (values(i) > 0.0_wp) then
                slice_count = slice_count + 1
                idx_map(slice_count) = i
                total = total + values(i)
            else
                write(idx_str, '(" ", I0)') i
                call log_warning('pie: ignoring non-positive value at index' // trim(idx_str))
            end if
        end do

        if (slice_count == 0 .or. total <= 0.0_wp) then
            call log_error('pie: sum of values must be positive')
            deallocate(idx_map)
            return
        end if

        if (state%plot_count >= state%max_plots) then
            call log_warning('pie: maximum number of plots reached')
            deallocate(idx_map)
            return
        end if

        state%plot_count = state%plot_count + 1
        plot_idx = state%plot_count
        if (plot_idx > size(plots)) then
            call log_error('pie: internal plot storage exceeded capacity')
            state%plot_count = state%plot_count - 1
            deallocate(idx_map)
            return
        end if

        if (allocated(plots(plot_idx)%pie_start)) deallocate(plots(plot_idx)%pie_start)
        if (allocated(plots(plot_idx)%pie_end)) deallocate(plots(plot_idx)%pie_end)
        if (allocated(plots(plot_idx)%pie_offsets)) deallocate(plots(plot_idx)%pie_offsets)
        if (allocated(plots(plot_idx)%pie_colors)) deallocate(plots(plot_idx)%pie_colors)
        if (allocated(plots(plot_idx)%pie_label_pos)) deallocate(plots(plot_idx)%pie_label_pos)
        if (allocated(plots(plot_idx)%pie_values)) deallocate(plots(plot_idx)%pie_values)
        if (allocated(plots(plot_idx)%pie_source_index)) deallocate(plots(plot_idx)%pie_source_index)
        if (allocated(plots(plot_idx)%pie_labels)) deallocate(plots(plot_idx)%pie_labels)
        if (allocated(plots(plot_idx)%pie_autopct)) deallocate(plots(plot_idx)%pie_autopct)

        plots(plot_idx)%plot_type = PLOT_TYPE_PIE
        plots(plot_idx)%pie_slice_count = slice_count
        plots(plot_idx)%pie_radius = 1.0_wp
        plots(plot_idx)%pie_center = [0.0_wp, 0.0_wp]

        allocate(plots(plot_idx)%pie_start(slice_count))
        allocate(plots(plot_idx)%pie_end(slice_count))
        allocate(plots(plot_idx)%pie_offsets(slice_count))
        allocate(plots(plot_idx)%pie_colors(3, slice_count))
        allocate(plots(plot_idx)%pie_label_pos(2, slice_count))
        allocate(plots(plot_idx)%pie_values(slice_count))
        allocate(plots(plot_idx)%pie_source_index(slice_count))

        if (present(labels)) then
            max_len = 1
            do i = 1, slice_count
                max_len = max(max_len, len_trim(labels(idx_map(i))))
            end do
            allocate(character(len=max_len) :: plots(plot_idx)%pie_labels(slice_count))
            do i = 1, slice_count
                plots(plot_idx)%pie_labels(i) = adjustl(labels(idx_map(i)))
            end do
        end if

        if (present(autopct)) then
            plots(plot_idx)%pie_autopct = trim(autopct)
        end if

        allocate(explode_values(slice_count))
        explode_values = 0.0_wp
        if (present(explode)) then
            do i = 1, slice_count
                if (idx_map(i) <= size(explode)) then
                    explode_values(i) = max(0.0_wp, explode(idx_map(i)))
                end if
            end do
        end if

        palette_size = size(state%colors, 2)
        if (palette_size <= 0) palette_size = 1

        start_angle_rad = PI / 2.0_wp
        if (present(startangle)) start_angle_rad = startangle * PI / 180.0_wp
        radius = plots(plot_idx)%pie_radius
        label_radius = radius * 0.65_wp
        current_angle = start_angle_rad

        do i = 1, slice_count
            plots(plot_idx)%pie_values(i) = values(idx_map(i))
            plots(plot_idx)%pie_source_index(i) = idx_map(i)
            angle_span = 2.0_wp * PI * plots(plot_idx)%pie_values(i) / total
            plots(plot_idx)%pie_start(i) = current_angle
            plots(plot_idx)%pie_end(i) = current_angle + angle_span
            mid_angle = current_angle + 0.5_wp * angle_span

            offset_value = explode_values(i) * radius
            plots(plot_idx)%pie_offsets(i) = offset_value
            center_x = plots(plot_idx)%pie_center(1) + offset_value * cos(mid_angle)
            center_y = plots(plot_idx)%pie_center(2) + offset_value * sin(mid_angle)
            plots(plot_idx)%pie_label_pos(1, i) = center_x + label_radius * cos(mid_angle)
            plots(plot_idx)%pie_label_pos(2, i) = center_y + label_radius * sin(mid_angle)

            color_rgb = state%colors(:, mod(i - 1, palette_size) + 1)
            if (present(color_strings)) then
                if (idx_map(i) <= size(color_strings)) then
                    call parse_color(color_strings(idx_map(i)), color_rgb, success)
                    if (.not. success) then
                        call log_warning('pie: unsupported color string; using default palette entry')
                        color_rgb = state%colors(:, mod(i - 1, palette_size) + 1)
                    end if
                end if
            end if
            plots(plot_idx)%pie_colors(:, i) = color_rgb

            current_angle = current_angle + angle_span
        end do

        plots(plot_idx)%color = plots(plot_idx)%pie_colors(:, 1)

        if (present(labels)) then
            plots(plot_idx)%label = trim(labels(idx_map(1)))
        else
            plots(plot_idx)%label = 'pie'
        end if

        deallocate(idx_map)
        deallocate(explode_values)
    end subroutine figure_add_pie

end module fortplot_figure_plots
