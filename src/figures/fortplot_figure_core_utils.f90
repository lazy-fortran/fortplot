module fortplot_figure_core_utils
    !! Utility operations extracted from fortplot_figure_core
    !! 
    !! This module contains utility operations like set_ydata and legend
    !! to maintain architectural compliance with size limits.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_operations
    implicit none

    private
    public :: core_set_ydata, core_figure_legend

contains

    subroutine core_set_ydata(plots, plot_count, plot_index, y_new)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count, plot_index
        real(wp), intent(in) :: y_new(:)
        call figure_set_ydata_operation(plots, plot_count, plot_index, y_new)
    end subroutine core_set_ydata

    subroutine core_figure_legend(state, plots, plot_count, location)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in), optional :: location
        call figure_legend_operation(state%legend_data, state%show_legend, &
                                    plots, plot_count, location)
    end subroutine core_figure_legend

end module fortplot_figure_core_utils