module fortplot_figure_core_ranges
    !! Figure data range management module
    !!
    !! This module contains data range calculation methods
    !! extracted from fortplot_figure_core for architectural compliance
    !!
    !! ARCHITECTURAL REFACTORING (Issue #678):
    !! - Focused module for data range management
    !! - Single Responsibility Principle compliance
    !! - Clean separation from main figure functionality

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, &
                                   AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges
    use fortplot_figure_boxplot, only: update_boxplot_ranges
    implicit none

    private
    public :: update_data_ranges_figure, update_data_ranges_pcolormesh_figure
    public :: update_data_ranges_boxplot_figure

contains

    subroutine update_data_ranges_figure(plots, state, plot_count)
        !! Update data ranges based on current plot
        !! Routes to primary, twinx, or twiny ranges depending on the
        !! axis tag of the plot at `plot_count`.
        type(plot_data_t), intent(in) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(in) :: plot_count
        integer :: plot_axis

        if (plot_count <= 0) return

        plot_axis = plots(plot_count)%axis

        select case (plot_axis)
        case (AXIS_TWINX)
            call update_twinx_y_range(plots, plot_count, state)
        case (AXIS_TWINY)
            call update_twiny_x_range(plots, plot_count, state)
        case default
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
                                              state%symlog_base, state%symlog_linscale)
        end select
    end subroutine update_data_ranges_figure

    subroutine update_twinx_y_range(plots, plot_count, state)
        !! Update only the twin-y (right) axis y-range when a twinx plot is added.
        !! Twinx shares the primary x-axis; only y-range needs updating.
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        type(figure_state_t), intent(inout) :: state
        real(wp) :: twinx_y_min, twinx_y_max
        real(wp) :: twinx_y_min_trans, twinx_y_max_trans
        real(wp) :: x_dummy

        x_dummy = state%x_min
        twinx_y_min = state%twinx_y_min
        twinx_y_max = state%twinx_y_max

        call calculate_figure_data_ranges(plots, plot_count, &
                                          xlim_set=.true., &
                                          ylim_set=state%twinx_ylim_set, &
                                          x_min=x_dummy, x_max=x_dummy, &
                                          y_min=twinx_y_min, y_max=twinx_y_max, &
                                          x_min_transformed=x_dummy, &
                                          x_max_transformed=x_dummy, &
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
    end subroutine update_twinx_y_range

    subroutine update_twiny_x_range(plots, plot_count, state)
        !! Update only the twin-x (top) axis x-range when a twiny plot is added.
        !! Twiny shares the primary y-axis; only x-range needs updating.
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        type(figure_state_t), intent(inout) :: state
        real(wp) :: twiny_x_min, twiny_x_max
        real(wp) :: twiny_x_min_trans, twiny_x_max_trans
        real(wp) :: y_dummy

        y_dummy = state%y_min
        twiny_x_min = state%twiny_x_min
        twiny_x_max = state%twiny_x_max

        call calculate_figure_data_ranges(plots, plot_count, &
                                          xlim_set=state%twiny_xlim_set, &
                                          ylim_set=.true., &
                                          x_min=twiny_x_min, x_max=twiny_x_max, &
                                          y_min=y_dummy, y_max=y_dummy, &
                                          x_min_transformed=twiny_x_min_trans, &
                                          x_max_transformed=twiny_x_max_trans, &
                                          y_min_transformed=y_dummy, &
                                          y_max_transformed=y_dummy, &
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
    end subroutine update_twiny_x_range

    subroutine update_data_ranges_pcolormesh_figure(plots, state, plot_count)
        !! Update data ranges after adding pcolormesh plot
        type(plot_data_t), intent(in) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(in) :: plot_count

        real(wp) :: x_min_new, x_max_new, y_min_new, y_max_new

        ! Safety check: ensure pcolormesh arrays are allocated before accessing
        if (.not. allocated(plots(plot_count)%pcolormesh_data%x_vertices) .or. &
            .not. allocated(plots(plot_count)%pcolormesh_data%y_vertices)) then
            ! Arrays not allocated - pcolormesh initialization failed
            ! Skip data range update to prevent segfault
            return
        end if

        ! Additional safety: check arrays have valid size
        if (size(plots(plot_count)%pcolormesh_data%x_vertices) == 0 .or. &
            size(plots(plot_count)%pcolormesh_data%y_vertices) == 0) then
            ! Zero-size arrays - skip data range update
            return
        end if

        x_min_new = minval(plots(plot_count)%pcolormesh_data%x_vertices)
        x_max_new = maxval(plots(plot_count)%pcolormesh_data%x_vertices)
        y_min_new = minval(plots(plot_count)%pcolormesh_data%y_vertices)
        y_max_new = maxval(plots(plot_count)%pcolormesh_data%y_vertices)

        if (.not. state%xlim_set) then
            if (plot_count == 1) then
                state%x_min = x_min_new
                state%x_max = x_max_new
            else
                state%x_min = min(state%x_min, x_min_new)
                state%x_max = max(state%x_max, x_max_new)
            end if
        end if

        if (.not. state%ylim_set) then
            if (plot_count == 1) then
                state%y_min = y_min_new
                state%y_max = y_max_new
            else
                state%y_min = min(state%y_min, y_min_new)
                state%y_max = max(state%y_max, y_max_new)
            end if
        end if
    end subroutine update_data_ranges_pcolormesh_figure

    subroutine update_data_ranges_boxplot_figure(data, position, state)
        !! Update data ranges after adding boxplot
        real(wp), contiguous, intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        type(figure_state_t), intent(inout) :: state

        ! Delegate to module implementation
        call update_boxplot_ranges(data, position, &
                                   state%x_min, state%x_max, &
                                   state%y_min, state%y_max, &
                                   state%xlim_set, state%ylim_set)
    end subroutine update_data_ranges_boxplot_figure

end module fortplot_figure_core_ranges
