module fortplot_figure_ranges
    !! Figure data range management functionality
    !! Extracted from fortplot_figure_core.f90 for size reduction (SRP compliance)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_boxplot, only: update_boxplot_ranges
    implicit none

    private
    public :: update_figure_data_ranges_pcolormesh, update_figure_data_ranges_boxplot

contains

    subroutine update_figure_data_ranges_pcolormesh(plots, plot_count, xlim_set, ylim_set, &
                                                   x_min, x_max, y_min, y_max)
        !! Update data ranges after adding pcolormesh plot
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp) :: x_min_new, x_max_new, y_min_new, y_max_new
        
        x_min_new = minval(plots(plot_count)%pcolormesh_data%x_vertices)
        x_max_new = maxval(plots(plot_count)%pcolormesh_data%x_vertices)
        y_min_new = minval(plots(plot_count)%pcolormesh_data%y_vertices)
        y_max_new = maxval(plots(plot_count)%pcolormesh_data%y_vertices)
        
        if (.not. xlim_set) then
            if (plot_count == 1) then
                x_min = x_min_new
                x_max = x_max_new
            else
                x_min = min(x_min, x_min_new)
                x_max = max(x_max, x_max_new)
            end if
        end if
        
        if (.not. ylim_set) then
            if (plot_count == 1) then
                y_min = y_min_new
                y_max = y_max_new
            else
                y_min = min(y_min, y_min_new)
                y_max = max(y_max, y_max_new)
            end if
        end if
    end subroutine update_figure_data_ranges_pcolormesh

    subroutine update_figure_data_ranges_boxplot(data, position, x_min, x_max, y_min, y_max, &
                                                xlim_set, ylim_set)
        !! Update data ranges after adding boxplot - delegate to specialized module
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        logical, intent(in) :: xlim_set, ylim_set
        logical :: x_range_set, y_range_set
        
        ! Note: logic inversion - xlim_set means user set limits, x_range_set tracks if ranges updated
        x_range_set = .not. xlim_set
        y_range_set = .not. ylim_set
        
        call update_boxplot_ranges(data, position, x_min, x_max, y_min, y_max, x_range_set, y_range_set)
    end subroutine update_figure_data_ranges_boxplot

end module fortplot_figure_ranges