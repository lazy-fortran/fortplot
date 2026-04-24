module fortplot_figure_render_engine
    !! Shared rendering engine for figure outputs.
    !!
    !! Centralises single-axis and subplot rendering so the functional and
    !! object-oriented APIs share identical drawing behaviour.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, subplot_data_t, AXIS_PRIMARY, &
                                  AXIS_TWINX, AXIS_TWINY, PLOT_TYPE_PIE
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges, &
                                                  setup_coordinate_system, &
                                                  render_figure_background, &
                                                  render_figure_axes, &
                                                  render_all_plots, &
                                                  render_streamplot_arrows, &
                                                  render_figure_axes_labels_only, &
                                                  render_title_only, &
                                                  render_polar_axes
    use fortplot_figure_grid, only: render_grid_lines
    use fortplot_annotation_rendering, only: render_figure_annotations
    use fortplot_figure_aspect, only: contains_pie_plot, enforce_pie_axis_equal, &
                                      only_pie_plots, enforce_aspect_ratio
    use fortplot_margins, only: calculate_plot_area, plot_area_t
    use fortplot_pdf_coordinate, only: calculate_pdf_plot_area
    use fortplot_figure_colorbar, only: prepare_colorbar_layout, &
                                        resolve_colorbar_mappable, &
                                        render_colorbar
    use fortplot_subplot_rendering, only: render_subplots
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context, ASCII_CHAR_ASPECT
    implicit none

    private
    public :: figure_render

contains

    subroutine figure_render(state, plots, plot_count, annotations, annotation_count, &
                             subplots_array, subplot_rows, subplot_cols)
        !! Render a figure, handling both single-axis and subplot layouts.
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        type(subplot_data_t), intent(in), optional :: subplots_array(:, :)
        integer, intent(in), optional :: subplot_rows, subplot_cols

        logical :: have_subplots

        have_subplots = .false.
        if (present(subplots_array) .and. present(subplot_rows) .and. &
            present(subplot_cols)) then
            if (size(subplots_array, 1) == subplot_rows .and. &
                size(subplots_array, 2) == subplot_cols) then
                have_subplots = subplot_rows > 0 .and. subplot_cols > 0
            end if
        end if

        if (have_subplots) then
            call render_subplots(state, subplots_array, subplot_rows, subplot_cols)
        else
            call render_single_axis(state, plots, plot_count, annotations, &
                                    annotation_count)
        end if

        state%rendered = .true.
    end subroutine figure_render

    subroutine render_single_axis(state, plots, plot_count, annotations, &
                                  annotation_count)
        !! Render a single-axis figure.
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        logical :: pie_only, ascii_backend, have_colorbar, plot_area_supported
        type(plot_area_t) :: saved_plot_area, colorbar_plot_area
        real(wp) :: cbar_vmin, cbar_vmax
        character(len=20) :: cbar_colormap
        character(len=64) :: x_date_format, y_date_format
        character(len=64) :: twinx_y_date_format, twiny_x_date_format

        call apply_raster_config(state)
        call compute_all_data_ranges(state, plots, plot_count)
        call resolve_date_formats(state, x_date_format, y_date_format, &
                                  twinx_y_date_format, twiny_x_date_format)
        call resolve_pie_and_aspect(state, plots, plot_count, &
                                    pie_only, ascii_backend)
        call resolve_colorbar_layout(state, plots, plot_count, have_colorbar, &
                                     saved_plot_area, colorbar_plot_area, &
                                     cbar_vmin, cbar_vmax, cbar_colormap, &
                                     plot_area_supported)
        call render_background_and_grid(state, ascii_backend)
        call render_axes_and_plots(state, plots, plot_count, pie_only)
        call render_labels_overlay(state, plots, plot_count, pie_only, &
                                   x_date_format, y_date_format, &
                                   twinx_y_date_format, twiny_x_date_format)
        call render_decorations(state, plots, plot_count, have_colorbar, &
                                colorbar_plot_area, cbar_vmin, cbar_vmax, &
                                cbar_colormap, saved_plot_area, &
                                plot_area_supported, annotations, &
                                annotation_count)
    end subroutine render_single_axis

    subroutine apply_raster_config(state)
        use fortplot_raster, only: raster_context
        type(figure_state_t), intent(inout) :: state
        integer :: i, n

        select type (bk => state%backend)
        class is (raster_context)
            bk%raster%config_title_font_size = state%title_font_size
            bk%raster%config_label_font_size = state%label_font_size
            bk%raster%config_tick_font_size = state%tick_font_size
            if (allocated(bk%raster%config_xtick_values)) &
                deallocate (bk%raster%config_xtick_values)
            if (allocated(bk%raster%config_ytick_values)) &
                deallocate (bk%raster%config_ytick_values)
            if (state%custom_xticks_set .and. &
                allocated(state%custom_xtick_positions)) then
                bk%raster%config_xtick_values = state%custom_xtick_positions
            end if
            if (state%custom_yticks_set .and. &
                allocated(state%custom_ytick_positions)) then
                bk%raster%config_ytick_values = state%custom_ytick_positions
            end if
        class is (ascii_context)
            if (allocated(bk%custom_xtick_positions)) &
                deallocate (bk%custom_xtick_positions)
            if (allocated(bk%custom_xtick_labels)) &
                deallocate (bk%custom_xtick_labels)
            if (state%custom_xticks_set .and. &
                allocated(state%custom_xtick_positions) .and. &
                allocated(state%custom_xtick_labels)) then
                n = min(size(state%custom_xtick_positions), &
                        size(state%custom_xtick_labels))
                allocate (bk%custom_xtick_positions(n))
                allocate (bk%custom_xtick_labels(n))
                do i = 1, n
                    bk%custom_xtick_positions(i) = state%custom_xtick_positions(i)
                    bk%custom_xtick_labels(i) = state%custom_xtick_labels(i)
                end do
            end if
        class is (pdf_context)
            if (allocated(bk%custom_xtick_positions)) &
                deallocate (bk%custom_xtick_positions)
            if (allocated(bk%custom_ytick_positions)) &
                deallocate (bk%custom_ytick_positions)
            if (allocated(bk%custom_xtick_labels)) &
                deallocate (bk%custom_xtick_labels)
            if (allocated(bk%custom_ytick_labels)) &
                deallocate (bk%custom_ytick_labels)
            if (state%custom_xticks_set .and. &
                allocated(state%custom_xtick_positions) .and. &
                allocated(state%custom_xtick_labels)) then
                n = min(size(state%custom_xtick_positions), &
                        size(state%custom_xtick_labels))
                allocate (bk%custom_xtick_positions(n))
                allocate (bk%custom_xtick_labels(n))
                do i = 1, n
                    bk%custom_xtick_positions(i) = state%custom_xtick_positions(i)
                    bk%custom_xtick_labels(i) = state%custom_xtick_labels(i)
                end do
            end if
            if (state%custom_yticks_set .and. &
                allocated(state%custom_ytick_positions) .and. &
                allocated(state%custom_ytick_labels)) then
                n = min(size(state%custom_ytick_positions), &
                        size(state%custom_ytick_labels))
                allocate (bk%custom_ytick_positions(n))
                allocate (bk%custom_ytick_labels(n))
                do i = 1, n
                    bk%custom_ytick_positions(i) = state%custom_ytick_positions(i)
                    bk%custom_ytick_labels(i) = state%custom_ytick_labels(i)
                end do
            end if
        class default
        end select
    end subroutine apply_raster_config

    subroutine compute_all_data_ranges(state, plots, plot_count)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        real(wp) :: x_min_dummy, x_max_dummy, y_min_dummy, y_max_dummy
        real(wp) :: twinx_y_min, twinx_y_max
        real(wp) :: twinx_y_min_trans, twinx_y_max_trans
        real(wp) :: twiny_x_min, twiny_x_max
        real(wp) :: twiny_x_min_trans, twiny_x_max_trans

        call calculate_figure_data_ranges(plots, plot_count, &
                                          state%xlim_set, state%ylim_set, &
                                          state%x_min, state%x_max, &
                                          state%y_min, state%y_max, &
                                          state%x_min_transformed, &
                                          state%x_max_transformed, &
                                          state%y_min_transformed, &
                                          state%y_max_transformed, &
                                          state%xscale, state%yscale, &
                                          state%symlog_threshold, &
                                          axis_filter=AXIS_PRIMARY)

        if (state%has_twinx) then
            x_min_dummy = state%x_min
            x_max_dummy = state%x_max
            twinx_y_min = state%twinx_y_min
            twinx_y_max = state%twinx_y_max
            call calculate_figure_data_ranges(plots, plot_count, &
                                              xlim_set=.true., &
                                              ylim_set=state%twinx_ylim_set, &
                                              x_min=x_min_dummy, x_max=x_max_dummy, &
                                              y_min=twinx_y_min, y_max=twinx_y_max, &
                                              x_min_transformed=x_min_dummy, &
                                              x_max_transformed=x_max_dummy, &
                                              y_min_transformed=twinx_y_min_trans, &
                                              y_max_transformed=twinx_y_max_trans, &
                                              xscale=state%xscale, &
                                              yscale=state%twinx_yscale, &
                                              symlog_threshold=state%symlog_threshold, &
                                              axis_filter=AXIS_TWINX)
            state%twinx_y_min = twinx_y_min
            state%twinx_y_max = twinx_y_max
            state%twinx_y_min_transformed = twinx_y_min_trans
            state%twinx_y_max_transformed = twinx_y_max_trans
        end if

        if (state%has_twiny) then
            twiny_x_min = state%twiny_x_min
            twiny_x_max = state%twiny_x_max
            y_min_dummy = state%y_min
            y_max_dummy = state%y_max
            call calculate_figure_data_ranges(plots, plot_count, &
                                              xlim_set=state%twiny_xlim_set, &
                                              ylim_set=.true., &
                                              x_min=twiny_x_min, x_max=twiny_x_max, &
                                              y_min=y_min_dummy, y_max=y_max_dummy, &
                                              x_min_transformed=twiny_x_min_trans, &
                                              x_max_transformed=twiny_x_max_trans, &
                                              y_min_transformed=y_min_dummy, &
                                              y_max_transformed=y_max_dummy, &
                                              xscale=state%twiny_xscale, &
                                              yscale=state%yscale, &
                                              symlog_threshold=state%symlog_threshold, &
                                              axis_filter=AXIS_TWINY)
            state%twiny_x_min = twiny_x_min
            state%twiny_x_max = twiny_x_max
            state%twiny_x_min_transformed = twiny_x_min_trans
            state%twiny_x_max_transformed = twiny_x_max_trans
        end if
    end subroutine compute_all_data_ranges

    subroutine resolve_date_formats(state, x_fmt, y_fmt, twinx_fmt, twiny_fmt)
        type(figure_state_t), intent(in) :: state
        character(len=64), intent(out) :: x_fmt, y_fmt, twinx_fmt, twiny_fmt
        x_fmt = ''; y_fmt = ''; twinx_fmt = ''; twiny_fmt = ''
        if (allocated(state%xaxis_date_format)) x_fmt = state%xaxis_date_format
        if (allocated(state%yaxis_date_format)) y_fmt = state%yaxis_date_format
        if (allocated(state%twinx_yaxis_date_format)) twinx_fmt = state%twinx_yaxis_date_format
        if (allocated(state%twiny_xaxis_date_format)) twiny_fmt = state%twiny_xaxis_date_format
    end subroutine resolve_date_formats

    subroutine resolve_pie_and_aspect(state, plots, plot_count, pie_only, ascii_bk)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(out) :: pie_only, ascii_bk
        logical :: has_pie, have_pie_px
        real(wp) :: pw, ph

        pie_only = .false.; ascii_bk = .false.
        has_pie = contains_pie_plot(plots, plot_count)
        if (has_pie) then
            have_pie_px = .false.
            select type (bk => state%backend)
            class is (png_context)
                pw = real(max(1, bk%plot_area%width), wp)
                ph = real(max(1, bk%plot_area%height), wp)
                have_pie_px = .true.
            class is (pdf_context)
                pw = real(max(1, bk%plot_area%width), wp)
                ph = real(max(1, bk%plot_area%height), wp)
                have_pie_px = .true.
            class is (ascii_context)
                pw = real(max(1, bk%plot_width - 3), wp)
                ph = real(max(1, bk%plot_height - 3), wp) * ASCII_CHAR_ASPECT
                have_pie_px = .true.; ascii_bk = .true.
            class default
            end select
            if (have_pie_px) then
                call enforce_pie_axis_equal(state, pw, ph)
            else
                call enforce_pie_axis_equal(state)
            end if
            pie_only = only_pie_plots(plots, plot_count)
        else
            call apply_aspect_ratio_if_needed(state, has_pie)
        end if
    end subroutine resolve_pie_and_aspect

    subroutine resolve_colorbar_layout(state, plots, plot_count, have_cbar, &
                                       saved_pa, cbar_pa, vmin, vmax, cmap, &
                                       pa_supported)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(out) :: have_cbar, pa_supported
        type(plot_area_t), intent(out) :: saved_pa, cbar_pa
        real(wp), intent(out) :: vmin, vmax
        character(len=20), intent(out) :: cmap
        logical :: have_mappable
        integer :: mappable_index
        type(plot_area_t) :: main_pa

        have_cbar = state%colorbar_enabled
        have_mappable = .false.; pa_supported = .false.
        if (have_cbar) then
            call resolve_colorbar_mappable(plots, plot_count, &
                                           state%colorbar_plot_index, &
                                           mappable_index, vmin, vmax, cmap, &
                                           have_mappable)
            have_cbar = have_mappable
        end if
        if (have_cbar) then
            call prepare_colorbar_layout(state%backend, state%colorbar_location, &
                                         state%colorbar_fraction, state%colorbar_pad, &
                                         state%colorbar_shrink, saved_pa, main_pa, &
                                         cbar_pa, pa_supported)
            if (.not. pa_supported) have_cbar = .false.
        end if
    end subroutine resolve_colorbar_layout

    subroutine render_background_and_grid(state, ascii_bk)
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: ascii_bk

        call setup_coordinate_system(state%backend, &
                                     state%x_min_transformed, state%x_max_transformed, &
                                     state%y_min_transformed, state%y_max_transformed)
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
                                   state%grid_linestyle, state%grid_color)
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
            call state%legend_data%render(state%backend)
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
        !! Regenerate legend data for pie charts with backend-specific markers.
        use fortplot_figure_plot_management, only: setup_figure_legend
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count

        integer :: i
        logical :: has_pie_charts

        ! Check if there are any pie charts
        has_pie_charts = .false.
        do i = 1, plot_count
            if (plots(i)%plot_type == PLOT_TYPE_PIE) then
                has_pie_charts = .true.
                exit
            end if
        end do

        ! Only regenerate if there are pie charts
        if (has_pie_charts) then
            ! Clear and regenerate legend data with current backend
            call state%legend_data%clear()
            call setup_figure_legend(state%legend_data, state%show_legend, &
                                     plots, plot_count, 'east', state%backend_name)
        end if
    end subroutine regenerate_pie_legend_for_backend

    subroutine apply_aspect_ratio_if_needed(state, has_pie_plots)
        !! Apply aspect ratio enforcement if configured and not handled by pie logic.
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
        !! Helper to call render_colorbar with state-based tick/label customization.
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

end module fortplot_figure_render_engine
