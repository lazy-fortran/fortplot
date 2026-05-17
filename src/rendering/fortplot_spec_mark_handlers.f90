module fortplot_spec_mark_handlers
    !! Mark-type handlers for spec_t rendering.
    !!
    !! Each subroutine handles one mark type (line, point, bar, area),
    !! translating mark properties into plot_data_t entries via the
    !! core figure operations.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: DASH_LONG, DASH_GAP, DASH_SHORT
    use fortplot_colors, only: parse_color
    use fortplot_figure_core_advanced, only: core_scatter
    use fortplot_figure_core_operations, only: core_add_plot, core_add_fill_between
    use fortplot_figure_core_config, only: core_set_line_width
    use fortplot_plot_bars, only: bar_plot_state
    use fortplot_spec_types, only: mark_t, encoding_t, data_t
    use fortplot_spec_rendering_utils, only: approx_equal, get_label_from_encoding
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t

    implicit none
    private

    public :: add_mark_to_state

contains

    subroutine add_mark_to_state(mark, x, y, enc, data, state, plots, plot_count, status)
        type(mark_t), intent(in) :: mark
        real(wp), contiguous, intent(in) :: x(:), y(:)
        type(encoding_t), intent(in) :: enc
        type(data_t), intent(in) :: data
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        integer, intent(out) :: status

        character(len=:), allocatable :: label
        character(len=:), allocatable :: linestyle
        real(wp) :: rgb(3), default_color(3)
        logical :: has_stroke, has_fill

        status = 0
        if (.not. allocated(mark%type)) then
            status = 1
            return
        end if

        label = get_label_from_encoding(enc, data)
        if (allocated(mark%stroke_dash)) linestyle = line_style_from_mark(mark)
        call parse_mark_color(mark%stroke, rgb, has_stroke)
        call parse_mark_color(mark%fill, default_color, has_fill)

        select case (trim(mark%type))
        case ('line')
            call add_line_mark(mark, x, y, state, plots, plot_count, &
                               label, linestyle, rgb, has_stroke)
        case ('point')
            call add_point_mark(mark, x, y, state, plots, plot_count, &
                                label, rgb, default_color, has_stroke, has_fill)
        case ('bar')
            call add_bar_mark(x, y, plots, state, plot_count, &
                              label, rgb, default_color, has_stroke, has_fill)
        case ('area')
            call add_area_mark(mark, x, y, state, plots, plot_count)
        case default
            status = 2
        end select
    end subroutine add_mark_to_state

    subroutine add_line_mark(mark, x, y, state, plots, plot_count, &
                             label, linestyle, rgb, has_stroke)
        type(mark_t), intent(in) :: mark
        real(wp), contiguous, intent(in) :: x(:), y(:)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        character(len=:), allocatable, intent(in) :: label, linestyle
        real(wp), intent(in) :: rgb(3)
        logical, intent(in) :: has_stroke
        real(wp) :: previous_line_width

        previous_line_width = state%current_line_width
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
    end subroutine add_line_mark

    subroutine add_point_mark(mark, x, y, state, plots, plot_count, &
                              label, rgb, default_color, has_stroke, has_fill)
        type(mark_t), intent(in) :: mark
        real(wp), contiguous, intent(in) :: x(:), y(:)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        character(len=:), allocatable, intent(in) :: label
        real(wp), intent(in) :: rgb(3)
        real(wp), intent(in) :: default_color(3)
        logical, intent(in) :: has_stroke, has_fill
        real(wp) :: point_color(3)

        point_color = default_color
        if (.not. has_fill .and. has_stroke) point_color = rgb
        if (.not. has_fill .and. .not. has_stroke) then
            point_color = state%colors(:, mod(plot_count, size(state%colors, 2)) + 1)
        end if

        if (allocated(label)) then
            call core_scatter(plots, state, plot_count, x, y, label=label, &
                              default_color=point_color)
        else
            call core_scatter(plots, state, plot_count, x, y, &
                              default_color=point_color)
        end if

        if (plot_count > 0) then
            if (mark%size > 0.0_wp) plots(plot_count)%scatter_size_default = mark%size
            if (has_fill) then
                plots(plot_count)%marker_facecolor = point_color
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
    end subroutine add_point_mark

    subroutine add_bar_mark(x, y, plots, state, plot_count, &
                            label, rgb, default_color, has_stroke, has_fill)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        character(len=:), allocatable, intent(in) :: label
        real(wp), intent(in) :: rgb(3), default_color(3)
        logical, intent(in) :: has_stroke, has_fill

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
    end subroutine add_bar_mark

    subroutine add_area_mark(mark, x, y, state, plots, plot_count)
        type(mark_t), intent(in) :: mark
        real(wp), contiguous, intent(in) :: x(:), y(:)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), allocatable :: zeros(:)
        character(len=:), allocatable :: fill_color

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
    end subroutine add_area_mark

    subroutine parse_mark_color(color_spec, rgb, success)
        character(len=:), allocatable, intent(in) :: color_spec
        real(wp), intent(out) :: rgb(3)
        logical, intent(out) :: success
        success = .false.
        rgb = 0.0_wp
        if (.not. allocated(color_spec)) return

        call parse_color(trim(color_spec), rgb, success)
    end subroutine parse_mark_color

    function line_style_from_mark(mark) result(linestyle)
        type(mark_t), intent(in) :: mark
        character(len=:), allocatable :: linestyle
        linestyle = '-'
        if (size(mark%stroke_dash) == 2) then
            if (approx_equal(mark%stroke_dash(1), DASH_LONG) .and. &
                approx_equal(mark%stroke_dash(2), DASH_GAP)) then
                linestyle = '--'
                return
            end if
            if (approx_equal(mark%stroke_dash(1), DASH_SHORT) .and. &
                approx_equal(mark%stroke_dash(2), DASH_GAP)) then
                linestyle = ':'
                return
            end if
        else if (size(mark%stroke_dash) == 4) then
            if (approx_equal(mark%stroke_dash(1), DASH_LONG) .and. &
                approx_equal(mark%stroke_dash(2), DASH_GAP) .and. &
                approx_equal(mark%stroke_dash(3), DASH_SHORT) .and. &
                approx_equal(mark%stroke_dash(4), DASH_GAP)) then
                linestyle = '-.'
                return
            end if
        end if
    end function line_style_from_mark

end module fortplot_spec_mark_handlers
