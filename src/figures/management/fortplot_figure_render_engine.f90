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
                                                    render_title_only
    use fortplot_figure_grid, only: render_grid_lines
    use fortplot_annotation_rendering, only: render_figure_annotations
    use fortplot_figure_aspect, only: contains_pie_plot, enforce_pie_axis_equal, &
                                      only_pie_plots
    use fortplot_margins, only: calculate_plot_area
    use fortplot_pdf_coordinate, only: calculate_pdf_plot_area
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
        type(subplot_data_t), intent(in), optional :: subplots_array(:,:)
        integer, intent(in), optional :: subplot_rows, subplot_cols

        logical :: have_subplots

        have_subplots = .false.
        if (present(subplots_array) .and. present(subplot_rows) .and. present(subplot_cols)) then
            if (size(subplots_array, 1) == subplot_rows .and. &
                size(subplots_array, 2) == subplot_cols) then
                have_subplots = subplot_rows > 0 .and. subplot_cols > 0
            end if
        end if

        if (have_subplots) then
            call render_subplots(state, subplots_array, subplot_rows, subplot_cols)
        else
            call render_single_axis(state, plots, plot_count, annotations, annotation_count)
        end if

        state%rendered = .true.
    end subroutine figure_render

    subroutine render_single_axis(state, plots, plot_count, annotations, annotation_count)
        !! Render a single-axis figure.
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
        real(wp) :: pie_plot_width_px, pie_plot_height_px
        logical :: have_pie_pixels
        logical :: has_pie_plots
        logical :: pie_only
        logical :: ascii_backend

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

        ascii_backend = .false.
        select type (backend_ptr => state%backend)
        class is (ascii_context)
            ascii_backend = .true.
        class default
        end select

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
        ascii_backend = .false.
        if (has_pie_plots) then
            have_pie_pixels = .false.
            select type (bk => state%backend)
            class is (png_context)
                pie_plot_width_px = real(max(1, bk%plot_area%width), wp)
                pie_plot_height_px = real(max(1, bk%plot_area%height), wp)
                have_pie_pixels = .true.
            class is (pdf_context)
                pie_plot_width_px = real(max(1, bk%plot_area%width), wp)
                pie_plot_height_px = real(max(1, bk%plot_area%height), wp)
                have_pie_pixels = .true.
            class is (ascii_context)
                pie_plot_width_px = real(max(1, bk%plot_width - 3), wp)
                pie_plot_height_px = real(max(1, bk%plot_height - 3), wp) * ASCII_CHAR_ASPECT
                have_pie_pixels = .true.
                ascii_backend = .true.
            class default
            end select

            if (have_pie_pixels) then
                call enforce_pie_axis_equal(state, pie_plot_width_px, pie_plot_height_px)
            else
                call enforce_pie_axis_equal(state)
            end if
            pie_only = only_pie_plots(plots, plot_count)
        end if

        call setup_coordinate_system(state%backend, &
                                     state%x_min_transformed, state%x_max_transformed, &
                                     state%y_min_transformed, state%y_max_transformed)

        call render_figure_background(state%backend)

        if (state%grid_enabled) then
            if (.not. ascii_backend) then
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
            ! Regenerate legend for pie charts to ensure correct backend-specific markers
            call regenerate_pie_legend_for_backend(state, plots, plot_count)
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
    end subroutine render_single_axis

    subroutine render_subplots(state, subplots_array, subplot_rows, subplot_cols)
        !! Render a multi-subplot figure layout.
        type(figure_state_t), intent(inout) :: state
        type(subplot_data_t), intent(in) :: subplots_array(:,:)
        integer, intent(in) :: subplot_rows, subplot_cols

        integer :: nr, nc, i, j
        real(wp) :: base_left, base_right, base_bottom, base_top
        real(wp) :: cell_w, cell_h, left_f, right_f, bottom_f, top_f
        real(wp) :: lxmin, lxmax, lymin, lymax
        real(wp) :: lxmin_t, lxmax_t, lymin_t, lymax_t

        base_left   = 0.10_wp
        base_right  = 0.95_wp
        base_bottom = 0.10_wp
        base_top    = 0.90_wp
        nr = subplot_rows
        nc = subplot_cols
        cell_w = (base_right - base_left) / real(nc, wp)
        cell_h = (base_top   - base_bottom) / real(nr, wp)

        do i = 1, nr
            do j = 1, nc
                left_f   = base_left   + real(j - 1, wp) * cell_w
                right_f  = base_left   + real(j,     wp) * cell_w
                bottom_f = base_bottom + real(nr - i, wp) * cell_h
                top_f    = base_bottom + real(nr - i + 1, wp) * cell_h

                select type (bk => state%backend)
                class is (png_context)
                    bk%margins%left = left_f
                    bk%margins%right = right_f
                    bk%margins%bottom = bottom_f
                    bk%margins%top = top_f
                    call calculate_plot_area(bk%width, bk%height, bk%margins, bk%plot_area)
                class is (pdf_context)
                    bk%margins%left = left_f
                    bk%margins%right = right_f
                    bk%margins%bottom = bottom_f
                    bk%margins%top = top_f
                    call calculate_pdf_plot_area(bk%width, bk%height, bk%margins, bk%plot_area)
                class is (ascii_context)
                    ! ASCII backend keeps default layout.
                class default
                    ! Unknown backend; leave margins untouched.
                end select

                call calculate_figure_data_ranges(subplots_array(i, j)%plots, &
                                                subplots_array(i, j)%plot_count, &
                                                subplots_array(i, j)%xlim_set, &
                                                subplots_array(i, j)%ylim_set, &
                                                lxmin, lxmax, lymin, lymax, &
                                                lxmin_t, lxmax_t, lymin_t, lymax_t, &
                                                state%xscale, state%yscale, state%symlog_threshold)

                call setup_coordinate_system(state%backend, lxmin_t, lxmax_t, lymin_t, lymax_t)

                call render_figure_axes(state%backend, state%xscale, state%yscale, &
                                       state%symlog_threshold, lxmin, lxmax, lymin, lymax, &
                                       subplots_array(i, j)%title, subplots_array(i, j)%xlabel, &
                                       subplots_array(i, j)%ylabel, subplots_array(i, j)%plots, &
                                       subplots_array(i, j)%plot_count, has_twinx=.false., &
                                       has_twiny=.false.)

                if (subplots_array(i, j)%plot_count > 0) then
                    call render_all_plots(state%backend, subplots_array(i, j)%plots, &
                                         subplots_array(i, j)%plot_count, lxmin_t, lxmax_t, &
                                         lymin_t, lymax_t, state%xscale, state%yscale, &
                                         state%symlog_threshold, state%width, state%height, &
                                         state%margin_left, state%margin_right, &
                                         state%margin_bottom, state%margin_top)
                end if

                call render_figure_axes_labels_only(state%backend, state%xscale, state%yscale, &
                                                   state%symlog_threshold, lxmin, lxmax, lymin, lymax, &
                                                   subplots_array(i, j)%title, subplots_array(i, j)%xlabel, &
                                                   subplots_array(i, j)%ylabel, subplots_array(i, j)%plots, &
                                                   subplots_array(i, j)%plot_count, has_twinx=.false., &
                                                   has_twiny=.false.)
            end do
        end do
    end subroutine render_subplots

    subroutine regenerate_pie_legend_for_backend(state, plots, plot_count)
        !! Regenerate legend data for pie charts to ensure correct backend-specific markers
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

end module fortplot_figure_render_engine
