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

    real(wp), parameter :: PI = acos(-1.0_wp)

    type :: pie_prepared_t
        integer :: slice_count = 0
        real(wp) :: total = 0.0_wp
        integer, allocatable :: indices(:)
        real(wp), allocatable :: explode(:)
        logical :: valid = .false.
    end type pie_prepared_t

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
                                  show_colorbar, alpha, edgecolor, linewidth, filled)
        !! Add a 3D surface plot to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        character(len=*), intent(in), optional :: label, colormap
        logical, intent(in), optional :: show_colorbar, filled
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        call add_surface_plot_data(plots, state%plot_count, state%max_plots, &
                                   state%colors, x_grid, y_grid, z_grid, label, &
                                   colormap, show_colorbar, alpha, edgecolor, &
                                   linewidth, filled)
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
                                      x, y, c, colormap, vmin, vmax, edgecolors, &
                                      linewidths)
    end subroutine figure_add_pcolormesh

    subroutine figure_add_fill_between(plots, state, x, upper, lower, mask, &
                                       color_string, alpha)
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
                call log_warning('fill_between: unsupported color string; using ' // &
                                 'default palette color')
                fill_color = next_plot_color(state)
            end if
        end if

        fill_alpha = 1.0_wp
        if (present(alpha)) fill_alpha = max(0.0_wp, min(1.0_wp, alpha))

        call add_fill_between_plot_data(plots, state%plot_count, state%max_plots, x, &
                                        upper, lower, mask, fill_color, fill_alpha)
    end subroutine figure_add_fill_between

    subroutine figure_add_pie(plots, state, values, labels, startangle, &
                              color_strings, explode, autopct)
        !! Store pie chart slices using polar wedges with optional explode & colors
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        real(wp), intent(in), optional :: startangle
        character(len=*), intent(in), optional :: color_strings(:)
        real(wp), intent(in), optional :: explode(:)
        character(len=*), intent(in), optional :: autopct

        type(pie_prepared_t) :: prep
        integer :: plot_idx
        integer :: label_len
        logical :: reserved

        call prepare_pie_input(values, explode, prep)
        if (.not. prep%valid) return

        reserved = reserve_pie_plot_slot(state, plots, plot_idx)
        if (.not. reserved) then
            call release_pie_prepared(prep)
            return
        end if

        label_len = compute_label_length(labels, prep)
        call allocate_pie_arrays(plots(plot_idx), prep%slice_count, label_len)
        call assign_pie_labels(plots(plot_idx), labels, prep)
        call populate_pie_plot(plots(plot_idx), state, values, color_strings, &
                              autopct, startangle, prep)

        if (present(labels)) then
            if (prep%indices(1) <= size(labels)) then
                plots(plot_idx)%label = trim(labels(prep%indices(1)))
            else
                plots(plot_idx)%label = 'pie'
            end if
        else
            plots(plot_idx)%label = 'pie'
        end if

        call release_pie_prepared(prep)
    end subroutine figure_add_pie

    subroutine prepare_pie_input(values, explode, prep)
        !! Filter values and prepare index/explode buffers for pie slices
        real(wp), intent(in) :: values(:)
        real(wp), intent(in), optional :: explode(:)
        type(pie_prepared_t), intent(inout) :: prep

        integer :: n, i
        integer, allocatable :: tmp_idx(:)
        character(len=40) :: idx_str

        call release_pie_prepared(prep)

        n = size(values)
        if (n == 0) then
            call log_error('pie: values array must contain data')
            return
        end if

        allocate(prep%indices(n))
        prep%slice_count = 0
        prep%total = 0.0_wp
        do i = 1, n
            if (values(i) > 0.0_wp) then
                prep%slice_count = prep%slice_count + 1
                prep%indices(prep%slice_count) = i
                prep%total = prep%total + values(i)
            else
                write(idx_str, '(" ", I0)') i
                call log_warning('pie: ignoring non-positive value at index' // &
                                 trim(idx_str))
            end if
        end do

        if (prep%slice_count == 0 .or. prep%total <= 0.0_wp) then
            call log_error('pie: sum of values must be positive')
            call release_pie_prepared(prep)
            return
        end if

        allocate(tmp_idx(prep%slice_count))
        tmp_idx = prep%indices(1:prep%slice_count)
        call move_alloc(tmp_idx, prep%indices)

        allocate(prep%explode(prep%slice_count))
        prep%explode = 0.0_wp
        if (present(explode)) then
            do i = 1, prep%slice_count
                if (prep%indices(i) <= size(explode)) then
                    prep%explode(i) = max(0.0_wp, explode(prep%indices(i)))
                end if
            end do
        end if

        prep%valid = .true.
    end subroutine prepare_pie_input

    logical function reserve_pie_plot_slot(state, plots, plot_idx) result(ok)
        !! Reserve space for a new pie plot and reset its storage
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(out) :: plot_idx

        if (state%plot_count >= state%max_plots) then
            call log_warning('pie: maximum number of plots reached')
            ok = .false.
            return
        end if

        state%plot_count = state%plot_count + 1
        plot_idx = state%plot_count
        if (plot_idx > size(plots)) then
            call log_error('pie: internal plot storage exceeded capacity')
            state%plot_count = state%plot_count - 1
            ok = .false.
            return
        end if

        ok = .true.
    end function reserve_pie_plot_slot

    integer function compute_label_length(labels, prep) result(max_len)
        !! Determine the maximum trimmed label length among included slices
        character(len=*), intent(in), optional :: labels(:)
        type(pie_prepared_t), intent(in) :: prep
        integer :: i

        max_len = 0
        if (.not. present(labels)) return
        if (prep%slice_count <= 0) return

        do i = 1, prep%slice_count
            if (prep%indices(i) <= size(labels)) then
                max_len = max(max_len, len_trim(labels(prep%indices(i))))
            end if
        end do
    end function compute_label_length

    subroutine allocate_pie_arrays(plot, slice_count, label_len)
        !! Allocate plot storage for pie slices and optional labels
        type(plot_data_t), intent(inout) :: plot
        integer, intent(in) :: slice_count
        integer, intent(in) :: label_len

        call release_real_vector(plot%pie_start)
        call release_real_vector(plot%pie_end)
        call release_real_vector(plot%pie_offsets)
        call release_real_matrix(plot%pie_colors)
        call release_real_matrix(plot%pie_label_pos)
        call release_real_vector(plot%pie_values)
        call release_int_vector(plot%pie_source_index)
        call release_char_vector(plot%pie_labels)
        call release_char_scalar(plot%pie_autopct)

        plot%plot_type = PLOT_TYPE_PIE
        plot%pie_slice_count = slice_count
        plot%pie_radius = 1.0_wp
        plot%pie_center = [0.0_wp, 0.0_wp]

        allocate(plot%pie_start(slice_count))
        allocate(plot%pie_end(slice_count))
        allocate(plot%pie_offsets(slice_count))
        allocate(plot%pie_colors(3, slice_count))
        allocate(plot%pie_label_pos(2, slice_count))
        allocate(plot%pie_values(slice_count))
        allocate(plot%pie_source_index(slice_count))

        if (label_len > 0) then
            allocate(character(len=label_len) :: plot%pie_labels(slice_count))
        end if
    end subroutine allocate_pie_arrays

    subroutine assign_pie_labels(plot, labels, prep)
        !! Copy user-provided labels into the plot storage
        type(plot_data_t), intent(inout) :: plot
        character(len=*), intent(in), optional :: labels(:)
        type(pie_prepared_t), intent(in) :: prep
        integer :: i

        if (.not. present(labels)) return
        if (.not. allocated(plot%pie_labels)) return

        do i = 1, prep%slice_count
            if (prep%indices(i) <= size(labels)) then
                plot%pie_labels(i) = adjustl(labels(prep%indices(i)))
            else
                plot%pie_labels(i) = ''
            end if
        end do
    end subroutine assign_pie_labels

    subroutine populate_pie_plot(plot, state, values, color_strings, autopct, &
                                 startangle, prep)
        !! Populate pie plot fields including geometry and colors
        type(plot_data_t), intent(inout) :: plot
        type(figure_state_t), intent(in) :: state
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: color_strings(:)
        character(len=*), intent(in), optional :: autopct
        real(wp), intent(in), optional :: startangle
        type(pie_prepared_t), intent(in) :: prep

        integer :: i, palette_size, color_index
        real(wp) :: current_angle, angle_span, mid_angle
        real(wp) :: start_angle_rad, radius, label_radius
        real(wp) :: offset_value, center_x, center_y
        real(wp) :: color_rgb(3)
        logical :: success

        if (present(autopct)) plot%pie_autopct = trim(autopct)

        start_angle_rad = PI / 2.0_wp
        if (present(startangle)) start_angle_rad = startangle * PI / 180.0_wp

        radius = plot%pie_radius
        label_radius = radius * 0.65_wp
        palette_size = max(1, size(state%colors, 2))
        current_angle = start_angle_rad

        do i = 1, prep%slice_count
            plot%pie_values(i) = values(prep%indices(i))
            plot%pie_source_index(i) = prep%indices(i)
            angle_span = 2.0_wp * PI * plot%pie_values(i) / prep%total
            plot%pie_start(i) = current_angle
            plot%pie_end(i) = current_angle + angle_span
            mid_angle = current_angle + 0.5_wp * angle_span

            offset_value = prep%explode(i) * radius
            plot%pie_offsets(i) = offset_value
            center_x = plot%pie_center(1) + offset_value * cos(mid_angle)
            center_y = plot%pie_center(2) + offset_value * sin(mid_angle)
            plot%pie_label_pos(1, i) = center_x + label_radius * &
                                       cos(mid_angle)
            plot%pie_label_pos(2, i) = center_y + label_radius * &
                                       sin(mid_angle)

            color_index = mod(i - 1, palette_size) + 1
            color_rgb = state%colors(:, color_index)
            if (present(color_strings)) then
                if (prep%indices(i) <= size(color_strings)) then
                    call parse_color(color_strings(prep%indices(i)), color_rgb, &
                                     success)
                    if (.not. success) then
                        call log_warning('pie: unsupported color string; ' // &
                                         'using default palette entry')
                        color_rgb = state%colors(:, color_index)
                    end if
                end if
            end if
            plot%pie_colors(:, i) = color_rgb

            current_angle = current_angle + angle_span
        end do

        plot%color = plot%pie_colors(:, 1)
    end subroutine populate_pie_plot

    subroutine release_pie_prepared(prep)
        !! Release allocatables used during pie preparation
        type(pie_prepared_t), intent(inout) :: prep
        integer, allocatable :: tmp_idx(:)
        real(wp), allocatable :: tmp_real(:)

        prep%slice_count = 0
        prep%total = 0.0_wp
        prep%valid = .false.
        if (allocated(prep%indices)) then
            call move_alloc(prep%indices, tmp_idx)
        end if
        if (allocated(prep%explode)) then
            call move_alloc(prep%explode, tmp_real)
        end if
    end subroutine release_pie_prepared

    subroutine release_real_vector(array)
        !! Release a real vector allocatable without explicit deallocate
        real(wp), allocatable, intent(inout) :: array(:)
        real(wp), allocatable :: tmp(:)

        if (allocated(array)) then
            call move_alloc(array, tmp)
        end if
    end subroutine release_real_vector

    subroutine release_real_matrix(array)
        !! Release a real matrix allocatable without explicit deallocate
        real(wp), allocatable, intent(inout) :: array(:,:)
        real(wp), allocatable :: tmp(:,:)

        if (allocated(array)) then
            call move_alloc(array, tmp)
        end if
    end subroutine release_real_matrix

    subroutine release_int_vector(array)
        !! Release an integer vector allocatable without explicit deallocate
        integer, allocatable, intent(inout) :: array(:)
        integer, allocatable :: tmp(:)

        if (allocated(array)) then
            call move_alloc(array, tmp)
        end if
    end subroutine release_int_vector

    subroutine release_char_vector(array)
        !! Release a character vector allocatable without explicit deallocate
        character(len=:), allocatable, intent(inout) :: array(:)
        character(len=:), allocatable :: tmp(:)

        if (allocated(array)) then
            call move_alloc(array, tmp)
        end if
    end subroutine release_char_vector

    subroutine release_char_scalar(value)
        !! Release a scalar deferred-length character allocatable
        character(len=:), allocatable, intent(inout) :: value
        character(len=:), allocatable :: tmp

        if (allocated(value)) then
            call move_alloc(value, tmp)
        end if
    end subroutine release_char_scalar

end module fortplot_figure_plots
