module fortplot_subplot_colorbar
    !! Per-axes colorbars inside subplot grids.
    !!
    !! A panel colorbar is the single-axes colorbar applied to the panel's own
    !! axes box and mappables: the same config type, mappable resolution,
    !! area split and renderer. This module only adds the panel plumbing and
    !! the tight-layout estimate of the space its ticks and label need.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_plot_data, only: subplot_data_t, colorbar_config_t, &
                                  PLOT_TYPE_CONTOUR
    use fortplot_margins, only: plot_area_t
    use fortplot_figure_colorbar, only: prepare_colorbar_layout, &
                                        resolve_colorbar_mappable, &
                                        colorbar_auto_tick_labels
    use fortplot_figure_contour_colorbar, only: get_contour_colorbar_levels
    use fortplot_figure_render_steps, only: render_colorbar_with_config, &
                                            resolve_plot_colorbar_request
    use fortplot_ticks, only: format_tick_value_smart
    use fortplot_string_utils, only: to_lowercase
    implicit none

    private
    public :: panel_colorbar_t
    public :: begin_panel_colorbar, render_panel_colorbar
    public :: panel_colorbar_tick_labels

    type :: panel_colorbar_t
        !! Resolved colorbar of one panel for one render pass
        logical :: active = .false.
        type(colorbar_config_t) :: cfg
        integer :: mappable = 0
        real(wp) :: vmin = 0.0_wp, vmax = 1.0_wp
        character(len=20) :: cmap = 'viridis'
        type(plot_area_t) :: saved_area, bar_area
    end type panel_colorbar_t

    ! Number of automatic ticks assumed when estimating label widths
    integer, parameter :: ESTIMATE_TICK_TARGET = 6

contains

    subroutine resolve_panel_colorbar(sp, cb)
        !! Explicit colorbar() request, else a plot that asked for one
        !! (e.g. contourf show_colorbar), and the mappable it describes.
        type(subplot_data_t), intent(in) :: sp
        type(panel_colorbar_t), intent(out) :: cb
        logical :: mapped

        cb%cfg = sp%colorbar
        if (.not. allocated(sp%plots)) return
        if (.not. cb%cfg%enabled) then
            call resolve_plot_colorbar_request(sp%plots, sp%plot_count, &
                                               cb%cfg%enabled, cb%cfg%plot_index)
        end if
        if (.not. cb%cfg%enabled) return
        call resolve_colorbar_mappable(sp%plots, sp%plot_count, cb%cfg%plot_index, &
                                       cb%mappable, cb%vmin, cb%vmax, cb%cmap, mapped)
        cb%active = mapped
    end subroutine resolve_panel_colorbar

    subroutine begin_panel_colorbar(backend, sp, cb)
        !! Split the panel's current axes box into axes and colorbar areas;
        !! the backend keeps the reduced axes box for the panel's plots.
        class(plot_context), intent(inout) :: backend
        type(subplot_data_t), intent(in) :: sp
        type(panel_colorbar_t), intent(out) :: cb
        type(plot_area_t) :: main_area
        logical :: supported

        call resolve_panel_colorbar(sp, cb)
        if (.not. cb%active) return
        call prepare_colorbar_layout(backend, cb%cfg%location, cb%cfg%fraction, &
                                     cb%cfg%pad, cb%cfg%shrink, cb%saved_area, &
                                     main_area, cb%bar_area, supported)
        cb%active = supported
    end subroutine begin_panel_colorbar

    subroutine render_panel_colorbar(backend, sp, cb, default_line_width)
        class(plot_context), intent(inout) :: backend
        type(subplot_data_t), intent(in) :: sp
        type(panel_colorbar_t), intent(in) :: cb
        real(wp), intent(in) :: default_line_width
        real(wp), allocatable :: line_levels(:)
        real(wp) :: line_width

        if (.not. cb%active) return
        associate (mappable => sp%plots(cb%mappable))
            if (mappable%plot_type == PLOT_TYPE_CONTOUR .and. &
                .not. mappable%fill_contours) then
                line_width = default_line_width
                if (mappable%line_width > 0.0_wp) line_width = mappable%line_width
                call get_contour_colorbar_levels(mappable, line_levels)
                call render_colorbar_with_config(backend, cb%cfg, cb%bar_area, &
                                                 cb%vmin, cb%vmax, cb%cmap, &
                                                 line_levels, line_width)
            else
                call render_colorbar_with_config(backend, cb%cfg, cb%bar_area, &
                                                 cb%vmin, cb%vmax, cb%cmap)
            end if
        end associate
    end subroutine render_panel_colorbar

    subroutine panel_colorbar_tick_labels(sp, active, loc, labels, n, label, &
                                          label_fontsize)
        !! Tick labels (and axis label) the panel colorbar will draw, so a
        !! layout can reserve their extent before rendering.
        type(subplot_data_t), intent(in) :: sp
        logical, intent(out) :: active
        character(len=10), intent(out) :: loc
        character(len=50), intent(out) :: labels(40)
        integer, intent(out) :: n
        character(len=:), allocatable, intent(out) :: label
        real(wp), intent(out) :: label_fontsize
        type(panel_colorbar_t) :: cb
        real(wp) :: ticks(40)
        integer :: i

        n = 0
        label = ''
        call resolve_panel_colorbar(sp, cb)
        active = cb%active
        loc = to_lowercase(trim(cb%cfg%location))
        label_fontsize = cb%cfg%label_fontsize
        if (.not. active) return
        if (cb%cfg%label_set) label = cb%cfg%label

        if (cb%cfg%ticks_set) then
            do i = 1, min(40, size(cb%cfg%ticks))
                if (cb%cfg%ticks(i) < cb%vmin .or. cb%cfg%ticks(i) > cb%vmax) cycle
                n = n + 1
                labels(n) = format_tick_value_smart(cb%cfg%ticks(i), 10)
                if (cb%cfg%ticklabels_set) then
                    if (i <= size(cb%cfg%ticklabels)) labels(n) = cb%cfg%ticklabels(i)
                end if
            end do
        else
            call colorbar_auto_tick_labels(cb%vmin, cb%vmax, ESTIMATE_TICK_TARGET, &
                                           ticks, labels, n)
        end if
    end subroutine panel_colorbar_tick_labels

end module fortplot_subplot_colorbar
