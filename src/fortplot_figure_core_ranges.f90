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
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges
    use fortplot_figure_boxplot, only: update_boxplot_ranges
    implicit none

    private
    public :: update_data_ranges_figure, update_data_ranges_pcolormesh_figure
    public :: update_data_ranges_boxplot_figure

contains

    subroutine update_data_ranges_figure(plots, state, plot_count)
        !! Update data ranges based on current plot
        type(plot_data_t), intent(in) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(in) :: plot_count
        
        call calculate_figure_data_ranges(plots, plot_count, &
                                        state%xlim_set, state%ylim_set, &
                                        state%x_min, state%x_max, &
                                        state%y_min, state%y_max, &
                                        state%x_min_transformed, &
                                        state%x_max_transformed, &
                                        state%y_min_transformed, &
                                        state%y_max_transformed, &
                                        state%xscale, state%yscale, &
                                        state%symlog_threshold)
    end subroutine update_data_ranges_figure

    subroutine update_data_ranges_pcolormesh_figure(plots, state, plot_count)
        !! Update data ranges after adding pcolormesh plot
        type(plot_data_t), intent(in) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(in) :: plot_count
        
        real(wp) :: x_min_new, x_max_new, y_min_new, y_max_new
        
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
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        type(figure_state_t), intent(inout) :: state
        
        ! Delegate to module implementation
        call update_boxplot_ranges(data, position, &
                                   state%x_min, state%x_max, &
                                   state%y_min, state%y_max, &
                                   state%xlim_set, state%ylim_set)
    end subroutine update_data_ranges_boxplot_figure

end module fortplot_figure_core_ranges