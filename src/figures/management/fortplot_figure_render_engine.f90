module fortplot_figure_render_engine
    !! Shared rendering engine for figure outputs.
    !!
    !! Centralises single-axis and subplot rendering so the functional and
    !! object-oriented APIs share identical drawing behaviour.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, subplot_data_t, AXIS_PRIMARY, &
                                   AXIS_TWINX, AXIS_TWINY, PLOT_TYPE_PIE
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges
    use fortplot_figure_data_ranges, only: determine_sticky_edges
    use fortplot_figure_aspect, only: contains_pie_plot, enforce_pie_axis_equal, &
                                      only_pie_plots
    use fortplot_margins, only: calculate_plot_area, plot_area_t
    use fortplot_figure_colorbar, only: prepare_colorbar_layout, &
                                        resolve_colorbar_mappable
    use fortplot_subplot_rendering, only: render_subplots
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    use fortplot_figure_render_steps, only: &
        render_background_and_grid, render_axes_and_plots, &
        render_labels_overlay, render_decorations, &
        apply_aspect_ratio_if_needed, resolve_plot_colorbar_request
    use fortplot_raster, only: raster_context
    use fortplot_scales, only: apply_scale_transform
    implicit none

    private
    public :: figure_render

contains

    subroutine figure_render(state, plots, plot_count, annotations, annotation_count, &
                             subplots_array, subplot_rows, subplot_cols)
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
        type(figure_state_t), intent(inout) :: state
        integer :: i, n

        select type (bk => state%backend)
        class is (raster_context)
            bk%raster%config_title_font_size = state%title_font_size
            bk%raster%config_label_font_size = state%label_font_size
            bk%raster%config_tick_font_size = state%tick_font_size
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
                                          state%symlog_base, state%symlog_linscale, &
                                          axis_filter=AXIS_PRIMARY)
        call fold_stream_arrows_into_ranges(state, plot_count)
        call determine_sticky_edges(plots, plot_count, AXIS_PRIMARY, &
                                        state%sticky_x_min, state%sticky_x_max, &
                                        state%sticky_y_min, state%sticky_y_max)

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
                                              symlog_base=state%symlog_base, &
                                              symlog_linscale=state%symlog_linscale, &
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
                                              symlog_base=state%symlog_base, &
                                              symlog_linscale=state%symlog_linscale, &
                                              axis_filter=AXIS_TWINY)
            state%twiny_x_min = twiny_x_min
            state%twiny_x_max = twiny_x_max
            state%twiny_x_min_transformed = twiny_x_min_trans
            state%twiny_x_max_transformed = twiny_x_max_trans
        end if
    end subroutine compute_all_data_ranges

    subroutine fold_stream_arrows_into_ranges(state, plot_count)
        !! Streamplot's arrow queue lives outside `plots`, so
        !! calculate_figure_data_ranges does not see those points. In
        !! arrow-only mode that leaves the axis at its 0..1 default; in
        !! mixed mode it can clip arrows that fall outside the other
        !! plots' bounds. Expand x/y over the queued arrow positions
        !! whenever they exist.
        type(figure_state_t), intent(inout) :: state
        integer, intent(in) :: plot_count
        real(wp) :: ax_min, ax_max, ay_min, ay_max
        logical :: have_other_plots
        integer :: i

        if (.not. allocated(state%stream_arrows)) return
        if (size(state%stream_arrows) == 0) return

        ax_min = state%stream_arrows(1)%x; ax_max = ax_min
        ay_min = state%stream_arrows(1)%y; ay_max = ay_min
        do i = 2, size(state%stream_arrows)
            ax_min = min(ax_min, state%stream_arrows(i)%x)
            ax_max = max(ax_max, state%stream_arrows(i)%x)
            ay_min = min(ay_min, state%stream_arrows(i)%y)
            ay_max = max(ay_max, state%stream_arrows(i)%y)
        end do

        have_other_plots = plot_count > 0
        if (.not. state%xlim_set) then
            if (have_other_plots) then
                state%x_min = min(state%x_min, ax_min)
                state%x_max = max(state%x_max, ax_max)
            else
                state%x_min = ax_min
                state%x_max = ax_max
            end if
        end if
        if (.not. state%ylim_set) then
            if (have_other_plots) then
                state%y_min = min(state%y_min, ay_min)
                state%y_max = max(state%y_max, ay_max)
            else
                state%y_min = ay_min
                state%y_max = ay_max
            end if
        end if
        state%x_min_transformed = apply_scale_transform(state%x_min, &
            state%xscale, state%symlog_threshold, &
            base=state%symlog_base, linscale=state%symlog_linscale)
        state%x_max_transformed = apply_scale_transform(state%x_max, &
            state%xscale, state%symlog_threshold, &
            base=state%symlog_base, linscale=state%symlog_linscale)
        state%y_min_transformed = apply_scale_transform(state%y_min, &
            state%yscale, state%symlog_threshold, &
            base=state%symlog_base, linscale=state%symlog_linscale)
        state%y_max_transformed = apply_scale_transform(state%y_max, &
            state%yscale, state%symlog_threshold, &
            base=state%symlog_base, linscale=state%symlog_linscale)
    end subroutine fold_stream_arrows_into_ranges

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
                ! Pass the raw cell-row count. The ASCII backend already scales
                ! y by ASCII_CHAR_ASPECT in ascii_set_coord_impl, so applying the
                ! cell-aspect factor here too would double-count it (#1965).
                pw = real(max(1, bk%plot_width - 3), wp)
                ph = real(max(1, bk%plot_height - 3), wp)
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

        if (.not. have_cbar) then
            call resolve_plot_colorbar_request(plots, plot_count, &
                                               state%colorbar_enabled, &
                                               state%colorbar_plot_index)
            have_cbar = state%colorbar_enabled
        end if

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

end module fortplot_figure_render_engine
