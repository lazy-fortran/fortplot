module fortplot_spec_rendering
    !! Native render path for spec_t values.
    !!
    !! This module translates spec_t into the low-level render state used by the
    !! existing backends without routing through figure_t.
    !!
    !! Delegates mark handling to fortplot_spec_mark_handlers and field rendering
    !! to fortplot_spec_field_rendering. Shared utilities live in
    !! fortplot_spec_rendering_utils.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: APPROX_EQUAL_TOLERANCE
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
    use fortplot_figure_core_utils, only: core_figure_legend
    use fortplot_figure_initialization, only: figure_state_t, initialize_figure_state
    use fortplot_figure_management, only: figure_savefig_with_status, figure_show
    use fortplot_plot_data, only: plot_data_t, subplot_data_t
    use fortplot_spec_types, only: spec_t, data_t, encoding_t, field_plot_t, &
                                   layer_t, mark_t
    use fortplot_annotations, only: text_annotation_t
    use fortplot_spec_config_apply, only: apply_config_to_state, &
                                          apply_padding_to_margins, &
                                          set_legend_position_from_orient
    use fortplot_spec_mark_handlers, only: add_mark_to_state
    use fortplot_spec_field_rendering, only: render_field_plot_to_state
    use fortplot_spec_rendering_utils, only: approx_equal, ends_with, get_label_from_encoding

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

        ! Apply legend position from config AFTER legend is built
        if (spec%config%defined .and. spec%config%legend%defined &
            .and. spec%config%legend%orient_set) then
            call set_legend_position_from_orient( &
                spec%config%legend%orient, &
                state%legend_data%position)
        end if
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

    subroutine apply_spec_metadata(spec, state)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(inout) :: state

        call apply_spec_labels(spec, state)
        call apply_spec_scales(spec, state)
        call apply_spec_limits(spec, state)
        call apply_spec_grid(spec, state)
        call apply_spec_ticks(spec, state)
    end subroutine apply_spec_metadata

    subroutine apply_spec_labels(spec, state)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable :: compat

        if (allocated(spec%title)) then
            call core_set_title(state, compat, spec%title)
        end if
        if (spec%encoding%x%defined .and. spec%encoding%x%axis%title_set) then
            call core_set_xlabel(state, compat, spec%encoding%x%axis%title)
        end if
        if (spec%encoding%y%defined .and. spec%encoding%y%axis%title_set) then
            call core_set_ylabel(state, compat, spec%encoding%y%axis%title)
        end if
    end subroutine apply_spec_labels

    subroutine apply_spec_scales(spec, state)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(inout) :: state

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
    end subroutine apply_spec_scales

    subroutine apply_spec_limits(spec, state)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(inout) :: state

        if (spec%encoding%x%scale%domain_set) then
            call core_set_xlim(state, spec%encoding%x%scale%domain_min, &
                               spec%encoding%x%scale%domain_max)
        end if
        if (spec%encoding%y%scale%domain_set) then
            call core_set_ylim(state, spec%encoding%y%scale%domain_min, &
                               spec%encoding%y%scale%domain_max)
        end if
    end subroutine apply_spec_limits

    subroutine apply_spec_grid(spec, state)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(inout) :: state
        character(len=1) :: grid_axis
        logical :: x_grid, y_grid

        x_grid = spec%encoding%x%defined .and. spec%encoding%x%axis%grid
        y_grid = spec%encoding%y%defined .and. spec%encoding%y%axis%grid
        if (.not. (x_grid .or. y_grid)) return

        grid_axis = 'b'
        if (x_grid .and. .not. y_grid) grid_axis = 'x'
        if (y_grid .and. .not. x_grid) grid_axis = 'y'
        call core_grid(state, enabled=.true., axis=grid_axis)

        if (spec%encoding%x%axis%grid_opacity >= 0.0_wp) then
            state%grid_alpha = spec%encoding%x%axis%grid_opacity
        end if
        if (spec%encoding%y%axis%grid_opacity >= 0.0_wp) then
            state%grid_alpha = spec%encoding%y%axis%grid_opacity
        end if
    end subroutine apply_spec_grid

    subroutine apply_spec_ticks(spec, state)
        type(spec_t), intent(in) :: spec
        type(figure_state_t), intent(inout) :: state

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
    end subroutine apply_spec_ticks

    subroutine apply_custom_ticks_filtered(values, fmt, dmin, dmax, &
                                          positions, labels, is_set)
        !! Convert tick values to positions + string labels.
        !! Filters out tick values outside [dmin, dmax] domain.
        !! Determines decimal places from the tick spacing.
        real(wp), contiguous, intent(in) :: values(:)
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

end module fortplot_spec_rendering
