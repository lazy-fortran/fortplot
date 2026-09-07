module fortplot_figure_tick_budget
    !! Keep grid lines and minor ticks aligned with each backend's major ticks.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_raster, only: raster_context
    use fortplot_pdf, only: pdf_context
    use fortplot_tick_budget, only: axis_tick_budget, raster_tick_budget
    implicit none
    private
    public :: backend_tick_budget

contains

    integer function backend_tick_budget(backend, horizontal) result(n)
        class(plot_context), intent(in) :: backend
        logical, intent(in) :: horizontal
        integer :: extent

        n = 9
        select type (backend)
        class is (raster_context)
            extent = backend%plot_area%height
            if (horizontal) extent = backend%plot_area%width
            n = raster_tick_budget(extent, backend%raster%dpi, horizontal, &
                backend%raster%config_tick_font_size)
        class is (pdf_context)
            extent = backend%plot_area%height
            if (horizontal) extent = backend%plot_area%width
            n = axis_tick_budget(real(extent, wp), horizontal)
        end select
    end function backend_tick_budget

end module fortplot_figure_tick_budget
