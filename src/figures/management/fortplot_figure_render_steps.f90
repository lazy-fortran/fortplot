module fortplot_figure_render_steps
    !! Rendering step procedures extracted from fortplot_figure_render_engine
    !! for size compliance (refs #1694)

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_PIE, &
                                  PLOT_TYPE_CONTOUR, PLOT_TYPE_SURFACE, &
                                  PLOT_TYPE_SCATTER, PLOT_TYPE_PCOLORMESH
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_rendering_pipeline, only: setup_coordinate_system, &
                                                  render_figure_background, &
                                                  render_figure_axes, &
                                                  render_all_plots, &
                                                  render_streamplot_arrows, &
                                                  render_figure_axes_labels_only, &
                                                  render_title_only, &
                                                  render_polar_axes, &
                                                  render_3d_front_frame
    use fortplot_figure_grid, only: render_grid_lines
    use fortplot_annotation_rendering, only: render_figure_annotations
    use fortplot_figure_aspect, only: contains_pie_plot, enforce_pie_axis_equal, &
                                      only_pie_plots, enforce_aspect_ratio
    use fortplot_margins, only: plot_area_t, plot_margins_t, calculate_plot_area
    use fortplot_figure_colorbar, only: render_colorbar
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context, ASCII_CHAR_ASPECT
    use fortplot_legend, only: legend_render
    use fortplot_legend_best, only: resolve_best_legend_position
    implicit none

    private
    public :: render_background_and_grid
    public :: render_axes_and_plots
    public :: render_labels_overlay
    public :: render_decorations
    public :: regenerate_pie_legend_for_backend
    public :: apply_aspect_ratio_if_needed
    public :: render_colorbar_with_state
    public :: resolve_plot_colorbar_request

contains

    subroutine render_background_and_grid(state, ascii_bk)
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: ascii_bk

        ! Propagate stored 3D view angles so render-time projection (axes,
        ! surfaces) matches the figure's current view.
        state%backend%view_azim = state%view_azim
        state%backend%view_elev = state%view_elev
        state%backend%view_dist = state%view_dist

        call setup_coordinate_system(state%backend, &
                                     state%x_min_transformed, state%x_max_transformed, &
                                     state%y_min_transformed, state%y_max_transformed, &
                                     sticky_x_min=state%sticky_x_min, &
                                     sticky_x_max=state%sticky_x_max, &
                                     sticky_y_min=state%sticky_y_min, &
                                     sticky_y_max=state%sticky_y_max)
        call render_figure_background(state%backend)
        if (state%grid_enabled .and. .not. state%polar_projection .and. .not. ascii_bk) then
            call render_grid_lines(state%backend, state%grid_enabled, &
                                   state%grid_which, state%grid_axis, &
                                   state%grid_alpha, state%width, state%height, &
                                   state%margin_left, state%margin_right, &
                                   state%margin_bottom, state%margin_top, &
                                   state%xscale, state%yscale, &
                                   state%symlog_threshold, state%x_min, state%x_max, &
                                   state%y_min, state%y_max, &
                                   state%x_min_transformed, state%x_max_transformed, &
                                   state%y_min_transformed, state%y_max_transformed, &
                                   state%grid_linestyle, state%grid_color, &
                                   sticky_x_min=state%sticky_x_min, &
                                   sticky_x_max=state%sticky_x_max, &
                                   sticky_y_min=state%sticky_y_min, &
                                   sticky_y_max=state%sticky_y_max)
        end if
        if (state%polar_projection) then
            call render_polar_axes(state%backend, state%x_min_transformed, &
                                   state%x_max_transformed, &
                                   state%y_min_transformed, &
                                   state%y_max_transformed, state)
        end if
    end subroutine render_background_and_grid

    subroutine render_axes_and_plots(state, plots, plot_count, pie_only)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: pie_only

        if (.not. pie_only .and. .not. state%polar_projection) then
            call render_figure_axes(state%backend, state%xscale, state%yscale, &
                                    state%symlog_threshold, state%x_min, state%x_max, &
                                    state%y_min, state%y_max, state%title, &
                                    state%xlabel, state%ylabel, plots, plot_count, &
                                    has_twinx=state%has_twinx, &
                                    twinx_y_min=state%twinx_y_min, &
                                    twinx_y_max=state%twinx_y_max, &
                                    twinx_ylabel=state%twinx_ylabel, &
                                    twinx_yscale=state%twinx_yscale, &
                                    has_twiny=state%has_twiny, &
                                    twiny_x_min=state%twiny_x_min, &
                                    twiny_x_max=state%twiny_x_max, &
                                    twiny_xlabel=state%twiny_xlabel, &
                                    twiny_xscale=state%twiny_xscale, &
                                    state=state)
        else
            call render_title_only(state%backend, state%title, state%x_min, &
                                   state%x_max, state%y_min, state%y_max, &
                                   state%title_font_size)
        end if
        if (plot_count > 0) then
            call render_all_plots(state%backend, plots, plot_count, &
                                  state%x_min_transformed, state%x_max_transformed, &
                                  state%y_min_transformed, state%y_max_transformed, &
                                  state%xscale, state%yscale, state%symlog_threshold, &
                                  state%width, state%height, &
                                  state%margin_left, state%margin_right, &
                                  state%margin_bottom, state%margin_top, state=state)
            ! Front box spines paint over the data so near frame edges occlude
            ! curves and surfaces (global painter ordering, refs #1956).
            if (.not. pie_only .and. .not. state%polar_projection) then
                call render_3d_front_frame(state%backend, plots, plot_count, &
                                           state%x_min, state%x_max, &
                                           state%y_min, state%y_max)
            end if
        end if
        if (allocated(state%stream_arrows)) then
            if (size(state%stream_arrows) > 0) then
                call render_streamplot_arrows(state%backend, state%stream_arrows)
            end if
        end if
    end subroutine render_axes_and_plots

    subroutine render_labels_overlay(state, plots, plot_count, pie_only, &
                                     x_fmt, y_fmt, twinx_fmt, twiny_fmt)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: pie_only
        character(len=64), intent(in) :: x_fmt, y_fmt, twinx_fmt, twiny_fmt

        if (pie_only .or. state%polar_projection) return
        call render_figure_axes_labels_only( &
            state%backend, state%xscale, state%yscale, &
            state%symlog_threshold, &
            state%x_min, state%x_max, state%y_min, state%y_max, &
            state%title, state%xlabel, state%ylabel, plots, plot_count, &
            has_twinx=state%has_twinx, &
            twinx_y_min=state%twinx_y_min, twinx_y_max=state%twinx_y_max, &
            twinx_ylabel=state%twinx_ylabel, twinx_yscale=state%twinx_yscale, &
            has_twiny=state%has_twiny, &
            twiny_x_min=state%twiny_x_min, twiny_x_max=state%twiny_x_max, &
            twiny_xlabel=state%twiny_xlabel, twiny_xscale=state%twiny_xscale, &
            custom_xticks=state%custom_xtick_positions, &
            custom_xtick_labels=state%custom_xtick_labels, &
            custom_yticks=state%custom_ytick_positions, &
            custom_ytick_labels=state%custom_ytick_labels, &
            x_date_format=x_fmt, y_date_format=y_fmt, &
            twinx_y_date_format=twinx_fmt, twiny_x_date_format=twiny_fmt)
    end subroutine render_labels_overlay

    subroutine render_decorations(state, plots, plot_count, have_cbar, &
                                  cbar_pa, vmin, vmax, cmap, saved_pa, &
                                  pa_supported, annotations, ann_count)
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: have_cbar, pa_supported
        type(plot_area_t), intent(in) :: cbar_pa, saved_pa
        real(wp), intent(in) :: vmin, vmax
        character(len=20), intent(in) :: cmap
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: ann_count

        if (have_cbar) then
            call render_colorbar_with_state(state, cbar_pa, vmin, vmax, cmap)
        end if
        if (state%show_legend .and. state%legend_data%num_entries > 0) then
            call regenerate_pie_legend_for_backend(state, plots, plot_count)
            call resolve_best_legend_for_state(state, plots, plot_count)
            call legend_render(state%legend_data, state%backend)
        end if
        if (present(annotations) .and. present(ann_count)) then
            if (ann_count > 0) then
                call render_figure_annotations(state%backend, annotations, &
                                               ann_count, &
                                               state%x_min, state%x_max, &
                                               state%y_min, state%y_max, &
                                               state%width, state%height, state%dpi, &
                                               state%margin_left, state%margin_right, &
                                               state%margin_bottom, state%margin_top)
            end if
        end if
        if (have_cbar .and. pa_supported) then
            select type (bk => state%backend)
            class is (png_context)
                bk%plot_area = saved_pa
            class is (pdf_context)
                bk%plot_area = saved_pa
            class default
            end select
        end if
    end subroutine render_decorations

    subroutine regenerate_pie_legend_for_backend(state, plots, plot_count)
        use fortplot_figure_plot_management, only: setup_figure_legend
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count

        integer :: i
        logical :: has_pie_charts

        has_pie_charts = .false.
        do i = 1, plot_count
            if (plots(i)%plot_type == PLOT_TYPE_PIE) then
                has_pie_charts = .true.
                exit
            end if
        end do

        if (has_pie_charts) then
            ! Place the pie legend inside the axes (matplotlib default). The
            ! 'east' location anchored the box at x_max + margin, outside the
            ! frame, so it was clipped. 'best' resolves to an inside corner
            ! using real free pixels, independent of the pie axis-equal range.
            call state%legend_data%clear()
            call setup_figure_legend(state%legend_data, state%show_legend, &
                                     plots, plot_count, 'best', state%backend_name)
        end if
    end subroutine regenerate_pie_legend_for_backend

    subroutine resolve_best_legend_for_state(state, plots, plot_count)
        !! Resolve a 'best' legend to a concrete corner using the current
        !! backend data window and plot area. No-op for explicit placements.
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count

        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        integer :: px_w, px_h

        call calculate_plot_area(state%backend%width, state%backend%height, &
                                 margins, plot_area)
        px_w = max(1, plot_area%width)
        px_h = max(1, plot_area%height)

        call resolve_best_legend_position(state%legend_data, plots, plot_count, &
            state%backend%x_min, state%backend%x_max, &
            state%backend%y_min, state%backend%y_max, px_w, px_h)
    end subroutine resolve_best_legend_for_state

    subroutine apply_aspect_ratio_if_needed(state, has_pie_plots)
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: has_pie_plots

        real(wp) :: plot_width_px, plot_height_px

        if (has_pie_plots) return
        if (state%aspect_mode == 'auto') return

        select type (bk => state%backend)
        class is (png_context)
            plot_width_px = real(max(1, bk%plot_area%width), wp)
            plot_height_px = real(max(1, bk%plot_area%height), wp)
        class is (pdf_context)
            plot_width_px = real(max(1, bk%plot_area%width), wp)
            plot_height_px = real(max(1, bk%plot_area%height), wp)
        class is (ascii_context)
            plot_width_px = real(max(1, bk%plot_width - 3), wp)
            plot_height_px = real(max(1, bk%plot_height - 3), wp)*ASCII_CHAR_ASPECT
        class default
            return
        end select

        call enforce_aspect_ratio(state, plot_width_px, plot_height_px)
    end subroutine apply_aspect_ratio_if_needed

    subroutine render_colorbar_with_state(state, plot_area, vmin, vmax, colormap)
        type(figure_state_t), intent(inout) :: state
        type(plot_area_t), intent(in) :: plot_area
        real(wp), intent(in) :: vmin, vmax
        character(len=*), intent(in) :: colormap

        if (state%colorbar_ticks_set .and. state%colorbar_ticklabels_set) then
            if (state%colorbar_label_set) then
                call render_colorbar(state%backend, plot_area, vmin, vmax, &
                                     colormap, state%colorbar_location, &
                                     state%colorbar_label, state%colorbar_ticks, &
                                     state%colorbar_ticklabels, &
                                     state%colorbar_label_fontsize)
            else
                call render_colorbar(state%backend, plot_area, vmin, vmax, &
                                     colormap, state%colorbar_location, &
                                     custom_ticks=state%colorbar_ticks, &
                                     custom_ticklabels=state%colorbar_ticklabels)
            end if
        else if (state%colorbar_ticks_set) then
            if (state%colorbar_label_set) then
                call render_colorbar(state%backend, plot_area, vmin, vmax, &
                                     colormap, state%colorbar_location, &
                                     state%colorbar_label, state%colorbar_ticks, &
                                     label_fontsize=state%colorbar_label_fontsize)
            else
                call render_colorbar(state%backend, plot_area, vmin, vmax, &
                                     colormap, state%colorbar_location, &
                                     custom_ticks=state%colorbar_ticks)
            end if
        else if (state%colorbar_label_set) then
            call render_colorbar(state%backend, plot_area, vmin, vmax, &
                                 colormap, state%colorbar_location, &
                                 state%colorbar_label, &
                                 label_fontsize=state%colorbar_label_fontsize)
        else
            call render_colorbar(state%backend, plot_area, vmin, vmax, &
                                 colormap, state%colorbar_location)
        end if
    end subroutine render_colorbar_with_state

    subroutine resolve_plot_colorbar_request(plots, plot_count, enabled, plot_idx)
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(out) :: enabled
        integer, intent(out) :: plot_idx

        integer :: i

        enabled = .false.
        plot_idx = 0

        do i = 1, plot_count
            if (plots(i)%plot_type == PLOT_TYPE_CONTOUR) then
                if (plots(i)%fill_contours .and. plots(i)%show_colorbar) then
                    if (allocated(plots(i)%z_grid) .and. size(plots(i)%z_grid) > 0) then
                        enabled = .true.
                        plot_idx = i
                        return
                    end if
                end if
            end if

            if (plots(i)%plot_type == PLOT_TYPE_SURFACE) then
                if (plots(i)%surface_show_colorbar) then
                    if (allocated(plots(i)%z_grid) .and. size(plots(i)%z_grid) > 0) then
                        enabled = .true.
                        plot_idx = i
                        return
                    end if
                end if
            end if

            if (plots(i)%plot_type == PLOT_TYPE_SCATTER) then
                if (plots(i)%scatter_colorbar) then
                    if (allocated(plots(i)%scatter_colors) .and. &
                        size(plots(i)%scatter_colors) > 0) then
                        enabled = .true.
                        plot_idx = i
                        return
                    end if
                end if
            end if

            if (plots(i)%plot_type == PLOT_TYPE_PCOLORMESH) then
                if (plots(i)%show_colorbar) then
                    if (allocated(plots(i)%pcolormesh_data%c_values) .and. &
                        size(plots(i)%pcolormesh_data%c_values) > 0) then
                        enabled = .true.
                        plot_idx = i
                        return
                    end if
                end if
            end if
        end do
    end subroutine resolve_plot_colorbar_request

end module fortplot_figure_render_steps
