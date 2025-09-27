module fortplot_figure_render_engine
    !! Dedicated rendering engine for single-axes figures.
    !!
    !! Provides the shared `figure_render` implementation used by both the
    !! refactored pipeline and the legacy compatibility entry points.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges, &
                                                    setup_coordinate_system, &
                                                    render_figure_background, &
                                                    render_figure_axes, &
                                                    render_all_plots, &
                                                    render_streamplot_arrows, &
                                                    render_figure_axes_labels_only, &
                                                    render_title_only
    use fortplot_figure_grid, only: render_grid_lines
    use fortplot_annotation_rendering, only: render_figure_annotations
    use fortplot_figure_aspect, only: contains_pie_plot, enforce_pie_axis_equal, &
                                      only_pie_plots
    implicit none

    private
    public :: figure_render

contains

    subroutine figure_render(state, plots, plot_count, annotations, annotation_count)
        !! Render a single-axis figure using the modern pipeline.
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count

        real(wp) :: x_min_dummy, x_max_dummy
        real(wp) :: y_min_dummy, y_max_dummy
        real(wp) :: twinx_y_min, twinx_y_max
        real(wp) :: twinx_y_min_trans, twinx_y_max_trans
        real(wp) :: twiny_x_min, twiny_x_max
        real(wp) :: twiny_x_min_trans, twiny_x_max_trans
        logical :: has_pie_plots
        logical :: pie_only

        call calculate_figure_data_ranges(plots, plot_count, &
                                          state%xlim_set, state%ylim_set, &
                                          state%x_min, state%x_max, &
                                          state%y_min, state%y_max, &
                                          state%x_min_transformed, &
                                          state%x_max_transformed, &
                                          state%y_min_transformed, &
                                          state%y_max_transformed, &
                                          state%xscale, state%yscale, &
                                          state%symlog_threshold, axis_filter=AXIS_PRIMARY)

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

        has_pie_plots = contains_pie_plot(plots, plot_count)
        pie_only = .false.
        if (has_pie_plots) then
            call enforce_pie_axis_equal(state)
            pie_only = only_pie_plots(plots, plot_count)
        end if

        call setup_coordinate_system(state%backend, &
                                     state%x_min_transformed, state%x_max_transformed, &
                                     state%y_min_transformed, state%y_max_transformed)

        call render_figure_background(state%backend)

        if (state%grid_enabled) then
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
                                   state%grid_linestyle)
        end if

        if (.not. pie_only) then
            call render_figure_axes(state%backend, state%xscale, state%yscale, &
                                    state%symlog_threshold, state%x_min, state%x_max, &
                                    state%y_min, state%y_max, state%title, &
                                    state%xlabel, state%ylabel, plots, plot_count, &
                                    has_twinx=state%has_twinx, twinx_y_min=state%twinx_y_min, &
                                    twinx_y_max=state%twinx_y_max, twinx_ylabel=state%twinx_ylabel, &
                                    twinx_yscale=state%twinx_yscale, has_twiny=state%has_twiny, &
                                    twiny_x_min=state%twiny_x_min, twiny_x_max=state%twiny_x_max, &
                                    twiny_xlabel=state%twiny_xlabel, twiny_xscale=state%twiny_xscale)
        else
            call render_title_only(state%backend, state%title, state%x_min, state%x_max, &
                                   state%y_min, state%y_max)
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

        if (.not. pie_only) then
            call render_figure_axes_labels_only(state%backend, state%xscale, state%yscale, &
                                                state%symlog_threshold, state%x_min, state%x_max, &
                                                state%y_min, state%y_max, state%title, &
                                                state%xlabel, state%ylabel, plots, plot_count, &
                                                has_twinx=state%has_twinx, twinx_y_min=state%twinx_y_min, &
                                                twinx_y_max=state%twinx_y_max, twinx_ylabel=state%twinx_ylabel, &
                                                twinx_yscale=state%twinx_yscale, has_twiny=state%has_twiny, &
                                                twiny_x_min=state%twiny_x_min, twiny_x_max=state%twiny_x_max, &
                                                twiny_xlabel=state%twiny_xlabel, twiny_xscale=state%twiny_xscale)
        end if

        if (state%show_legend .and. state%legend_data%num_entries > 0) then
            call state%legend_data%render(state%backend)
        end if

        if (present(annotations) .and. present(annotation_count)) then
            if (annotation_count > 0) then
                call render_figure_annotations(state%backend, annotations, annotation_count, &
                                                state%x_min, state%x_max, &
                                                state%y_min, state%y_max, &
                                                state%width, state%height, &
                                                state%margin_left, state%margin_right, &
                                                state%margin_bottom, state%margin_top)
            end if
        end if

        state%rendered = .true.
    end subroutine figure_render

end module fortplot_figure_render_engine
