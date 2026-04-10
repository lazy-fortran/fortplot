module fortplot_spec_rendering
    !! Native render path for spec_t values.
    !!
    !! This module translates spec_t into the low-level render state used by the
    !! existing backends without routing through figure_t.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_colors, only: parse_color
    use fortplot_figure_core_advanced, only: core_scatter
    use fortplot_figure_core_config, only: core_grid, core_set_line_width, &
                                           core_set_title, core_set_xlabel, &
                                           core_set_ylabel, core_set_xscale, &
                                           core_set_yscale, core_set_xlim, &
                                           core_set_ylim
    use fortplot_figure_core_operations, only: core_add_plot, core_add_contour, &
                                               core_add_contour_filled, &
                                               core_add_pcolormesh, &
                                               core_add_fill_between, &
                                               core_streamplot
    use fortplot_figure_initialization, only: figure_state_t, initialize_figure_state
    use fortplot_figure_core_utils, only: core_figure_legend
    use fortplot_figure_management, only: figure_savefig_with_status, figure_show
    use fortplot_plot_bars, only: bar_plot_state
    use fortplot_plot_data, only: plot_data_t, subplot_data_t
    use fortplot_spec_types, only: spec_t, data_t, encoding_t, field_plot_t, &
                                   layer_t, mark_t
    use fortplot_annotations, only: text_annotation_t
    use fortplot_spec_config_apply, only: apply_config_to_state, &
                                          apply_padding_to_margins

    implicit none
    private

    public :: apply_spec_to_render_state
    public :: render_spec_to_file
    public :: show_spec
    public :: spec_target_is_json

contains

    logical function spec_target_is_json(filename) result(is_json)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: trimmed

        trimmed = trim(filename)
        is_json = ends_with(trimmed, '.vl.json') .or. ends_with(trimmed, '.json')
    end function spec_target_is_json

    subroutine render_spec_to_file(spec, filename, status, rendered_state)
        type(spec_t), intent(in) :: spec
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        type(figure_state_t), intent(out), optional :: rendered_state

        type(figure_state_t) :: state
        type(plot_data_t), allocatable :: plots(:)
        type(text_annotation_t), allocatable :: annotations(:)
        type(subplot_data_t), allocatable :: subplots_array(:, :)
        integer :: plot_count
        integer :: annotation_count
        integer :: subplot_rows
        integer :: subplot_cols

        call build_render_inputs(spec, state, plots, plot_count, annotations, &
                                 annotation_count, subplots_array, subplot_rows, &
                                 subplot_cols, status)
        if (status /= 0) return

        if (allocated(subplots_array) .and. subplot_rows > 0 .and. subplot_cols > 0) then
            call figure_savefig_with_status(state, plots, plot_count, filename, status, &
                                            annotations=annotations, &
                                            annotation_count=annotation_count, &
                                            subplots_array=subplots_array, &
                                            subplot_rows=subplot_rows, &
                                            subplot_cols=subplot_cols)
        else
            call figure_savefig_with_status(state, plots, plot_count, filename, status, &
                                            annotations=annotations, &
                                            annotation_count=annotation_count)
        end if
        if (present(rendered_state)) rendered_state = state
    end subroutine render_spec_to_file

    subroutine show_spec(spec, backend_name, blocking, rendered_state)
        type(spec_t), intent(in) :: spec
        character(len=*), intent(in), optional :: backend_name
        logical, intent(in), optional :: blocking
        type(figure_state_t), intent(out), optional :: rendered_state

        type(figure_state_t) :: state
        type(plot_data_t), allocatable :: plots(:)
        type(text_annotation_t), allocatable :: annotations(:)
        type(subplot_data_t), allocatable :: subplots_array(:, :)
        integer :: status
        integer :: plot_count
        integer :: annotation_count
        integer :: subplot_rows
        integer :: subplot_cols

        call build_render_inputs(spec, state, plots, plot_count, annotations, &
                                 annotation_count, subplots_array, subplot_rows, &
                                 subplot_cols, status, backend_name)
        if (status /= 0) return

        if (allocated(subplots_array) .and. subplot_rows > 0 .and. subplot_cols > 0) then
            call figure_show(state, plots, plot_count, blocking, &
                             annotations=annotations, &
                             annotation_count=annotation_count, &
                             subplots_array=subplots_array, &
                             subplot_rows=subplot_rows, &
                             subplot_cols=subplot_cols)
        else
            call figure_show(state, plots, plot_count, blocking, &
                             annotations=annotations, &
                             annotation_count=annotation_count)
        end if
        if (present(rendered_state)) rendered_state = state
    end subroutine show_spec

    subroutine build_render_inputs(spec, state, plots, plot_count, annotations, &
                                   annotation_count, subplots_array, subplot_rows, &
                                   subplot_cols, status, backend_name)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(out) :: state
        type(plot_data_t), allocatable, intent(out) :: plots(:)
        type(text_annotation_t), allocatable, intent(out) :: annotations(:)
        type(subplot_data_t), allocatable, intent(out) :: subplots_array(:, :)
        integer, intent(out) :: plot_count
        integer, intent(out) :: annotation_count
        integer, intent(out) :: subplot_rows
        integer, intent(out) :: subplot_cols
        integer, intent(out) :: status
        character(len=*), intent(in), optional :: backend_name

        character(len=10) :: active_backend

        status = 0
        annotation_count = 0
        subplot_rows = 0
        subplot_cols = 0
        active_backend = 'png'
        if (present(backend_name)) active_backend = trim(backend_name)

        call initialize_figure_state(state, width=spec%width, height=spec%height, &
                                     backend=active_backend)

        if (spec%config%defined) then
            call apply_config_to_state(spec%config, state)
        end if
        if (spec%padding%defined) then
            if (allocated(spec%autosize_type)) then
                call apply_padding_to_margins(spec%padding, state, &
                    spec%autosize_type)
            else
                call apply_padding_to_margins(spec%padding, state)
            end if
        end if

        allocate (plots(state%max_plots))
        plot_count = 0

        call apply_spec_to_render_state(spec, state, plots, plot_count, status)
    end subroutine build_render_inputs


    subroutine apply_spec_to_render_state(spec, state, plots, plot_count, status)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(out) :: status

        integer :: i

        status = 0
        plot_count = 0
        state%plot_count = 0

        if (spec%is_layered .and. spec%layer_count > 0) then
            do i = 1, spec%layer_count
                call add_layer_to_render_state(spec%layers(i), spec%data, state, plots, &
                                               plot_count, status)
                if (status /= 0) return
            end do
        else
            call add_single_view_to_render_state(spec, state, plots, plot_count, status)
            if (status /= 0) return
        end if

        call apply_spec_metadata(spec, state)
        call build_spec_legend_if_needed(state, plots, plot_count)
    end subroutine apply_spec_to_render_state

    subroutine add_single_view_to_render_state(spec, state, plots, plot_count, status)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(out) :: status

        real(wp), allocatable :: x(:), y(:)

        status = 0

        if (spec%field%defined) then
            call render_field_plot_to_state(spec%mark, spec%field, spec%encoding, state, &
                                            plots, plot_count, status)
            return
        end if

        call extract_xy(spec%data, spec%encoding, x, y)
        if (.not. allocated(x) .or. .not. allocated(y)) return

        call add_mark_to_state(spec%mark, x, y, spec%encoding, spec%data, state, plots, &
                               plot_count, status)
    end subroutine add_single_view_to_render_state

    subroutine add_layer_to_render_state(layer, shared_data, state, plots, plot_count, &
                                         status)
        type(layer_t), intent(in) :: layer
        type(data_t), intent(in) :: shared_data
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(out) :: status

        real(wp), allocatable :: x(:), y(:)

        status = 0

        if (layer%field%defined) then
            call render_field_plot_to_state(layer%mark, layer%field, layer%encoding, &
                                            state, plots, plot_count, status)
            return
        end if

        if (layer%has_data) then
            call extract_xy(layer%data, layer%encoding, x, y)
        else
            call extract_xy(shared_data, layer%encoding, x, y)
        end if
        if (.not. allocated(x) .or. .not. allocated(y)) return

        if (layer%has_data) then
            call add_mark_to_state(layer%mark, x, y, layer%encoding, layer%data, state, &
                                   plots, plot_count, status)
        else
            call add_mark_to_state(layer%mark, x, y, layer%encoding, shared_data, state, &
                                   plots, plot_count, status)
        end if
    end subroutine add_layer_to_render_state

    subroutine add_mark_to_state(mark, x, y, enc, data, state, plots, plot_count, status)
        type(mark_t), intent(in) :: mark
        real(wp), intent(in) :: x(:), y(:)
        type(encoding_t), intent(in) :: enc
        type(data_t), intent(in) :: data
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(out) :: status

        character(len=:), allocatable :: label
        character(len=:), allocatable :: linestyle
        character(len=:), allocatable :: fill_color
        real(wp), allocatable :: zeros(:)
        real(wp) :: rgb(3)
        real(wp) :: default_color(3)
        real(wp) :: previous_line_width
        logical :: has_stroke, has_fill

        status = 0
        if (.not. allocated(mark%type)) then
            status = 1
            return
        end if

        label = get_label_from_encoding(enc, data)
        if (allocated(mark%stroke_dash)) linestyle = line_style_from_mark(mark)
        previous_line_width = state%current_line_width
        call parse_mark_color(mark%stroke, rgb, has_stroke)
        call parse_mark_color(mark%fill, default_color, has_fill)

        select case (trim(mark%type))
        case ('line')
            if (mark%stroke_width >= 0.0_wp) then
                call core_set_line_width(state, mark%stroke_width)
            end if

            if (allocated(label) .and. has_stroke .and. allocated(linestyle)) then
                call core_add_plot(plots, state, x, y, label=label, color=rgb, &
                                   linestyle=linestyle, plot_count=plot_count)
            else if (allocated(label) .and. allocated(linestyle)) then
                call core_add_plot(plots, state, x, y, label=label, &
                                   linestyle=linestyle, plot_count=plot_count)
            else if (has_stroke .and. allocated(linestyle)) then
                call core_add_plot(plots, state, x, y, color=rgb, &
                                   linestyle=linestyle, plot_count=plot_count)
            else if (allocated(label) .and. has_stroke) then
                call core_add_plot(plots, state, x, y, label=label, color=rgb, &
                                   plot_count=plot_count)
            else if (allocated(label)) then
                call core_add_plot(plots, state, x, y, label=label, &
                                   plot_count=plot_count)
            else if (has_stroke) then
                call core_add_plot(plots, state, x, y, color=rgb, plot_count=plot_count)
            else
                call core_add_plot(plots, state, x, y, plot_count=plot_count)
            end if

            if (plot_count > 0) then
                if (mark%stroke_width >= 0.0_wp) plots(plot_count)%line_width = mark%stroke_width
                if (allocated(mark%point)) then
                    if (trim(mark%point) == 'true') plots(plot_count)%marker = 'o'
                end if
            end if
            call core_set_line_width(state, previous_line_width)

        case ('point')
            if (.not. has_fill .and. has_stroke) default_color = rgb
            if (.not. has_fill .and. .not. has_stroke) then
                default_color = state%colors(:, mod(plot_count, size(state%colors, 2)) + 1)
            end if

            if (allocated(label)) then
                call core_scatter(plots, state, plot_count, x, y, label=label, &
                                  default_color=default_color)
            else
                call core_scatter(plots, state, plot_count, x, y, &
                                  default_color=default_color)
            end if

            if (plot_count > 0) then
                if (mark%size > 0.0_wp) plots(plot_count)%scatter_size_default = mark%size
                if (has_fill) then
                    plots(plot_count)%marker_facecolor = default_color
                    plots(plot_count)%marker_facecolor_set = .true.
                end if
                if (has_stroke) then
                    plots(plot_count)%marker_edgecolor = rgb
                    plots(plot_count)%marker_edgecolor_set = .true.
                end if
                if (mark%stroke_width >= 0.0_wp) then
                    plots(plot_count)%marker_linewidth = mark%stroke_width
                end if
                if (mark%opacity < 1.0_wp) then
                    plots(plot_count)%marker_face_alpha = mark%opacity
                    plots(plot_count)%marker_edge_alpha = mark%opacity
                end if
            end if

        case ('bar')
            if (allocated(label) .and. has_stroke .and. has_fill) then
                call bar_plot_state(plots, state, plot_count, x, y, label=label, &
                                    color=default_color, edgecolor=rgb)
            else if (allocated(label) .and. has_fill) then
                call bar_plot_state(plots, state, plot_count, x, y, label=label, &
                                    color=default_color)
            else if (allocated(label)) then
                call bar_plot_state(plots, state, plot_count, x, y, label=label)
            else if (has_stroke .and. has_fill) then
                call bar_plot_state(plots, state, plot_count, x, y, color=default_color, &
                                    edgecolor=rgb)
            else if (has_fill) then
                call bar_plot_state(plots, state, plot_count, x, y, color=default_color)
            else
                call bar_plot_state(plots, state, plot_count, x, y)
            end if

        case ('area')
            if (size(x) < 2) return
            allocate (zeros(size(y)))
            zeros = 0.0_wp

            if (allocated(mark%fill)) then
                fill_color = trim(mark%fill)
            else if (allocated(mark%stroke)) then
                fill_color = trim(mark%stroke)
            end if

            if (allocated(fill_color)) then
                call core_add_fill_between(plots, state, x, y, zeros, &
                                           color_string=fill_color, alpha=mark%opacity, &
                                           plot_count=plot_count)
            else
                call core_add_fill_between(plots, state, x, y, zeros, &
                                           alpha=mark%opacity, plot_count=plot_count)
            end if

        case default
            status = 2
        end select
    end subroutine add_mark_to_state

    subroutine render_field_plot_to_state(mark, field, enc, state, plots, plot_count, &
                                          status)
        type(mark_t), intent(in) :: mark
        type(field_plot_t), intent(in) :: field
        type(encoding_t), intent(in) :: enc
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(out) :: status

        real(wp), allocatable :: zmat(:, :), umat(:, :), vmat(:, :)

        status = 0
        if (.not. allocated(mark%type)) then
            status = 3
            return
        end if

        select case (trim(mark%type))
        case ('contour', 'contour_filled', 'pcolormesh')
            if (.not. allocated(field%z)) return
            call reshape_field_matrix(field%z, field%nrows, field%ncols, zmat)
            if (.not. allocated(zmat)) then
                status = 4
                return
            end if
            select case (trim(mark%type))
            case ('contour')
                call render_contour_to_state(field, enc, zmat, state, plots, plot_count)
            case ('contour_filled')
                call render_contour_filled_to_state(field, enc, zmat, state, plots, &
                                                    plot_count)
            case ('pcolormesh')
                call render_pcolormesh_to_state(field, zmat, state, plots, plot_count)
            end select
        case ('streamplot')
            if (.not. allocated(field%u) .or. .not. allocated(field%v)) return
            call reshape_field_matrix(field%u, field%nrows, field%ncols, umat)
            call reshape_field_matrix(field%v, field%nrows, field%ncols, vmat)
            if (.not. allocated(umat) .or. .not. allocated(vmat)) then
                status = 5
                return
            end if
            if (field%density >= 0.0_wp) then
                call core_streamplot(plots, state, plot_count, field%x, field%y, umat, &
                                     vmat, density=field%density)
            else
                call core_streamplot(plots, state, plot_count, field%x, field%y, umat, &
                                     vmat)
            end if
        case default
            status = 6
        end select
    end subroutine render_field_plot_to_state

    subroutine render_contour_to_state(field, enc, zmat, state, plots, plot_count)
        type(field_plot_t), intent(in) :: field
        type(encoding_t), intent(in) :: enc
        real(wp), intent(in) :: zmat(:, :)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count

        character(len=:), allocatable :: label

        label = get_label_from_encoding(enc)

        if (allocated(field%levels) .and. allocated(label)) then
            call core_add_contour(plots, state, field%x, field%y, zmat, levels=field%levels, &
                                  label=label, plot_count=plot_count)
        else if (allocated(field%levels)) then
            call core_add_contour(plots, state, field%x, field%y, zmat, levels=field%levels, &
                                  plot_count=plot_count)
        else if (allocated(label)) then
            call core_add_contour(plots, state, field%x, field%y, zmat, label=label, &
                                  plot_count=plot_count)
        else
            call core_add_contour(plots, state, field%x, field%y, zmat, &
                                  plot_count=plot_count)
        end if
    end subroutine render_contour_to_state

    subroutine render_contour_filled_to_state(field, enc, zmat, state, plots, plot_count)
        type(field_plot_t), intent(in) :: field
        type(encoding_t), intent(in) :: enc
        real(wp), intent(in) :: zmat(:, :)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count

        character(len=:), allocatable :: label

        label = get_label_from_encoding(enc)

        if (allocated(field%levels) .and. allocated(field%colormap) .and. &
            field%show_colorbar_set .and. allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, colormap=field%colormap, &
                                         show_colorbar=field%show_colorbar, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. allocated(field%colormap) .and. &
                 field%show_colorbar_set) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, colormap=field%colormap, &
                                         show_colorbar=field%show_colorbar, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. allocated(field%colormap) .and. &
                 allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, colormap=field%colormap, &
                                         label=label, plot_count=plot_count)
        else if (allocated(field%levels) .and. allocated(field%colormap)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, colormap=field%colormap, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. field%show_colorbar_set .and. &
                 allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, &
                                         show_colorbar=field%show_colorbar, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. field%show_colorbar_set) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, &
                                         show_colorbar=field%show_colorbar, &
                                         plot_count=plot_count)
        else if (allocated(field%levels) .and. allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%levels)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         levels=field%levels, plot_count=plot_count)
        else if (allocated(field%colormap) .and. field%show_colorbar_set .and. &
                 allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         colormap=field%colormap, &
                                         show_colorbar=field%show_colorbar, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%colormap) .and. field%show_colorbar_set) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         colormap=field%colormap, &
                                         show_colorbar=field%show_colorbar, &
                                         plot_count=plot_count)
        else if (allocated(field%colormap) .and. allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         colormap=field%colormap, label=label, &
                                         plot_count=plot_count)
        else if (allocated(field%colormap)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         colormap=field%colormap, plot_count=plot_count)
        else if (field%show_colorbar_set .and. allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         show_colorbar=field%show_colorbar, label=label, &
                                         plot_count=plot_count)
        else if (field%show_colorbar_set) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         show_colorbar=field%show_colorbar, &
                                         plot_count=plot_count)
        else if (allocated(label)) then
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         label=label, plot_count=plot_count)
        else
            call core_add_contour_filled(plots, state, field%x, field%y, zmat, &
                                         plot_count=plot_count)
        end if
    end subroutine render_contour_filled_to_state

    subroutine render_pcolormesh_to_state(field, zmat, state, plots, plot_count)
        type(field_plot_t), intent(in) :: field
        real(wp), intent(in) :: zmat(:, :)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count

        if (allocated(field%colormap) .and. field%vmin_set .and. field%vmax_set .and. &
            field%linewidths >= 0.0_wp) then
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     colormap=field%colormap, vmin=field%vmin, &
                                     vmax=field%vmax, linewidths=field%linewidths, &
                                     plot_count=plot_count)
        else if (allocated(field%colormap) .and. field%vmin_set .and. field%vmax_set) then
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     colormap=field%colormap, vmin=field%vmin, &
                                     vmax=field%vmax, plot_count=plot_count)
        else if (allocated(field%colormap) .and. field%linewidths >= 0.0_wp) then
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     colormap=field%colormap, &
                                     linewidths=field%linewidths, &
                                     plot_count=plot_count)
        else if (allocated(field%colormap)) then
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     colormap=field%colormap, plot_count=plot_count)
        else
            call core_add_pcolormesh(plots, state, field%x, field%y, zmat, &
                                     plot_count=plot_count)
        end if
    end subroutine render_pcolormesh_to_state

    subroutine apply_spec_metadata(spec, state)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(inout) :: state

        character(len=:), allocatable :: title_compat
        character(len=:), allocatable :: xlabel_compat
        character(len=:), allocatable :: ylabel_compat
        character(len=1) :: grid_axis
        logical :: x_grid, y_grid

        if (allocated(spec%title)) then
            call core_set_title(state, title_compat, spec%title)
        end if

        if (spec%encoding%x%defined .and. spec%encoding%x%axis%title_set) then
            call core_set_xlabel(state, xlabel_compat, spec%encoding%x%axis%title)
        end if
        if (spec%encoding%y%defined .and. spec%encoding%y%axis%title_set) then
            call core_set_ylabel(state, ylabel_compat, spec%encoding%y%axis%title)
        end if

        if (allocated(spec%encoding%x%scale%type)) then
            select case (trim(spec%encoding%x%scale%type))
            case ('linear', 'log', 'pow', 'sqrt', 'symlog', 'date')
                call core_set_xscale(state, spec%encoding%x%scale%type)
            end select
        end if
        if (allocated(spec%encoding%y%scale%type)) then
            select case (trim(spec%encoding%y%scale%type))
            case ('linear', 'log', 'pow', 'sqrt', 'symlog', 'date')
                call core_set_yscale(state, spec%encoding%y%scale%type)
            end select
        end if

        if (spec%encoding%x%scale%domain_set) then
            call core_set_xlim(state, spec%encoding%x%scale%domain_min, &
                               spec%encoding%x%scale%domain_max)
        end if
        if (spec%encoding%y%scale%domain_set) then
            call core_set_ylim(state, spec%encoding%y%scale%domain_min, &
                               spec%encoding%y%scale%domain_max)
        end if

        x_grid = spec%encoding%x%defined .and. spec%encoding%x%axis%grid
        y_grid = spec%encoding%y%defined .and. spec%encoding%y%axis%grid
        if (x_grid .or. y_grid) then
            grid_axis = 'b'
            if (x_grid .and. .not. y_grid) grid_axis = 'x'
            if (y_grid .and. .not. x_grid) grid_axis = 'y'
            call core_grid(state, enabled=.true., axis=grid_axis)
            ! Apply encoding-level grid opacity (e.g., alpha=0.3)
            if (spec%encoding%x%axis%grid_opacity >= 0.0_wp) then
                state%grid_alpha = spec%encoding%x%axis%grid_opacity
            end if
            if (spec%encoding%y%axis%grid_opacity >= 0.0_wp) then
                state%grid_alpha = spec%encoding%y%axis%grid_opacity
            end if
        end if

        ! Apply explicit tick values from the encoding, filtered to
        ! the visible domain range to avoid out-of-range tick labels.
        if (spec%encoding%x%defined .and. &
            allocated(spec%encoding%x%axis%tick_values)) then
            call apply_custom_ticks_filtered( &
                spec%encoding%x%axis%tick_values, &
                spec%encoding%x%axis%format, &
                state%x_min, state%x_max, &
                state%custom_xtick_positions, &
                state%custom_xtick_labels, &
                state%custom_xticks_set)
        end if
        if (spec%encoding%y%defined .and. &
            allocated(spec%encoding%y%axis%tick_values)) then
            call apply_custom_ticks_filtered( &
                spec%encoding%y%axis%tick_values, &
                spec%encoding%y%axis%format, &
                state%y_min, state%y_max, &
                state%custom_ytick_positions, &
                state%custom_ytick_labels, &
                state%custom_yticks_set)
        end if
    end subroutine apply_spec_metadata

    subroutine apply_custom_ticks_filtered(values, fmt, dmin, dmax, &
                                          positions, labels, is_set)
        !! Convert tick values to positions + string labels.
        !! Filters out tick values outside [dmin, dmax] domain.
        !! Determines decimal places from the tick spacing.
        real(wp), intent(in) :: values(:)
        character(len=:), allocatable, intent(in) :: fmt
        real(wp), intent(in) :: dmin, dmax
        real(wp), allocatable, intent(out) :: positions(:)
        character(len=50), allocatable, intent(out) :: labels(:)
        logical, intent(out) :: is_set

        integer :: i, n, count, decimals
        character(len=50) :: buf
        character(len=10) :: fmtstr
        real(wp) :: step_size
        real(wp), allocatable :: filtered(:)

        n = size(values)
        if (n == 0) then
            is_set = .false.
            return
        end if

        ! Filter to domain range
        allocate (filtered(n))
        count = 0
        do i = 1, n
            if (values(i) >= dmin .and. values(i) <= dmax) then
                count = count + 1
                filtered(count) = values(i)
            end if
        end do

        if (count == 0) then
            is_set = .false.
            return
        end if

        allocate (positions(count), labels(count))
        positions = filtered(1:count)

        if (allocated(fmt)) then
            if (trim(fmt) == 'd') then
                do i = 1, count
                    write (buf, '(i0)') nint(positions(i))
                    labels(i) = adjustl(buf)
                end do
                is_set = .true.
                return
            end if
        end if

        ! Determine decimal places from tick spacing
        decimals = 1
        if (count >= 2) then
            step_size = abs(positions(2) - positions(1))
            if (step_size > 0.0_wp) then
                if (step_size >= 1.0_wp) then
                    decimals = 1
                    if (abs(step_size - nint(step_size)) < 1.0d-9) &
                        decimals = 0
                else if (step_size >= 0.1_wp) then
                    decimals = 1
                else if (step_size >= 0.01_wp) then
                    decimals = 2
                else
                    decimals = 3
                end if
            end if
        end if

        write (fmtstr, '(a,i1,a)') '(f20.', decimals, ')'
        do i = 1, count
            write (buf, fmtstr) positions(i)
            labels(i) = adjustl(buf)
        end do

        is_set = .true.
    end subroutine apply_custom_ticks_filtered

    subroutine build_spec_legend_if_needed(state, plots, plot_count)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count

        integer :: i

        do i = 1, plot_count
            if (.not. allocated(plots(i)%label)) cycle
            if (len_trim(plots(i)%label) == 0) cycle
            call core_figure_legend(state, plots, plot_count)
            return
        end do
    end subroutine build_spec_legend_if_needed

    subroutine extract_xy(data, enc, x, y)
        type(data_t), intent(in) :: data
        type(encoding_t), intent(in) :: enc
        real(wp), allocatable, intent(out) :: x(:), y(:)

        integer :: j
        character(len=:), allocatable :: xfield
        character(len=:), allocatable :: yfield

        if (.not. allocated(data%columns) .or. data%nrows == 0) return

        xfield = 'x'
        yfield = 'y'
        if (enc%x%defined .and. allocated(enc%x%field)) xfield = enc%x%field
        if (enc%y%defined .and. allocated(enc%y%field)) yfield = enc%y%field

        do j = 1, size(data%columns)
            if (data%columns(j)%field == xfield) x = data%columns(j)%values
            if (data%columns(j)%field == yfield) y = data%columns(j)%values
        end do
    end subroutine extract_xy

    subroutine reshape_field_matrix(values, nrows, ncols, matrix)
        real(wp), intent(in) :: values(:)
        integer, intent(in) :: nrows
        integer, intent(in) :: ncols
        real(wp), allocatable, intent(out) :: matrix(:, :)

        if (nrows <= 0 .or. ncols <= 0) return
        if (size(values) /= nrows*ncols) return

        allocate (matrix(nrows, ncols))
        matrix = reshape(values, [nrows, ncols])
    end subroutine reshape_field_matrix

    function get_label_from_encoding(enc, data) result(label)
        type(encoding_t), intent(in) :: enc
        type(data_t), intent(in), optional :: data
        character(len=:), allocatable :: label

        integer :: value_length, i, j
        character(len=:), allocatable :: field_name

        if (.not. enc%color%defined) return

        if (allocated(enc%color%value)) then
            value_length = len(enc%color%value)
            if (value_length >= 2 .and. enc%color%value(1:1) == '"' .and. &
                enc%color%value(value_length:value_length) == '"') then
                label = enc%color%value(2:value_length - 1)
            else
                label = enc%color%value
            end if
            return
        end if

        if (.not. present(data)) return
        if (.not. allocated(enc%color%field)) return
        if (.not. allocated(data%columns)) return

        field_name = enc%color%field
        do i = 1, size(data%columns)
            if (data%columns(i)%field /= field_name) cycle
            if (.not. data%columns(i)%is_string) return
            if (.not. allocated(data%columns(i)%string_values)) return
            if (size(data%columns(i)%string_values) == 0) return

            label = trim(data%columns(i)%string_values(1))
            if (len_trim(label) == 0) then
                deallocate (label)
                return
            end if
            if (size(data%columns(i)%string_values) > 1) then
                do j = 2, size(data%columns(i)%string_values)
                    if (trim(data%columns(i)%string_values(j)) /= label) then
                        deallocate (label)
                        exit
                    end if
                end do
            end if
            return
        end do
    end function get_label_from_encoding

    function line_style_from_mark(mark) result(linestyle)
        type(mark_t), intent(in) :: mark
        character(len=:), allocatable :: linestyle

        linestyle = '-'

        if (size(mark%stroke_dash) == 2) then
            if (approx_equal(mark%stroke_dash(1), 6.0_wp) .and. &
                approx_equal(mark%stroke_dash(2), 3.0_wp)) then
                linestyle = '--'
                return
            end if
            if (approx_equal(mark%stroke_dash(1), 2.0_wp) .and. &
                approx_equal(mark%stroke_dash(2), 3.0_wp)) then
                linestyle = ':'
                return
            end if
        else if (size(mark%stroke_dash) == 4) then
            if (approx_equal(mark%stroke_dash(1), 6.0_wp) .and. &
                approx_equal(mark%stroke_dash(2), 3.0_wp) .and. &
                approx_equal(mark%stroke_dash(3), 2.0_wp) .and. &
                approx_equal(mark%stroke_dash(4), 3.0_wp)) then
                linestyle = '-.'
                return
            end if
        end if
    end function line_style_from_mark

    logical function approx_equal(a, b) result(equal)
        real(wp), intent(in) :: a, b

        equal = abs(a - b) < 1.0e-6_wp
    end function approx_equal

    subroutine parse_mark_color(color_spec, rgb, success)
        character(len=:), allocatable, intent(in) :: color_spec
        real(wp), intent(out) :: rgb(3)
        logical, intent(out) :: success

        success = .false.
        rgb = 0.0_wp
        if (.not. allocated(color_spec)) return

        call parse_color(trim(color_spec), rgb, success)
    end subroutine parse_mark_color

    logical function ends_with(text, suffix) result(matches)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: suffix

        integer :: text_length
        integer :: suffix_length

        text_length = len_trim(text)
        suffix_length = len_trim(suffix)
        if (suffix_length == 0 .or. suffix_length > text_length) then
            matches = .false.
            return
        end if

        matches = text(text_length - suffix_length + 1:text_length) == &
                  suffix(1:suffix_length)
    end function ends_with

end module fortplot_spec_rendering
