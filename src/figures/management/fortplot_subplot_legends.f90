module fortplot_subplot_legends
    use fortplot_context, only: plot_context
    use fortplot_raster, only: raster_context
    use fortplot_plot_data, only: subplot_data_t
    use fortplot_figure_legend_setup, only: setup_figure_legend
    use fortplot_legend, only: legend_t, legend_render
    use fortplot_legend_best, only: resolve_best_legend_position
    use fortplot_legend_drawing, only: legend_plot_pixel_dimensions
    implicit none
    private
    public :: render_subplot_legend
contains
    subroutine render_subplot_legend(backend, subplot, backend_name)
        class(plot_context), intent(inout) :: backend
        type(subplot_data_t), intent(in) :: subplot
        character(len=*), intent(in) :: backend_name
        type(legend_t) :: legend
        character(len=:), allocatable :: location
        logical :: visible
        integer :: width, height
        if (.not. subplot%show_legend) return
        location = 'best'
        if (allocated(subplot%legend_location)) location = subplot%legend_location
        call setup_figure_legend(legend, visible, subplot%plots, subplot%plot_count, &
                                 location, backend_name)
        if (legend%num_entries <= 0) return
        select type (backend)
        class is (raster_context)
            legend%axes_pixel_width = max(1, backend%plot_area%width)
            legend%axes_pixel_height = max(1, backend%plot_area%height)
        end select
        call legend_plot_pixel_dimensions(backend, width, height, legend)
        call resolve_best_legend_position(legend, subplot%plots, subplot%plot_count, &
                                          backend%x_min, backend%x_max, &
                                          backend%y_min, backend%y_max, width, height)
        call legend_render(legend, backend)
    end subroutine
end module
