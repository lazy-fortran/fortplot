module fortplot_plotting_advanced
    !! Advanced plotting methods (contour, bar, histogram, boxplot, streamplot)
    !! 
    !! This module provides a unified interface to advanced plotting functionality
    !! by re-exporting from the split plotting modules:
    !! - fortplot_plot_contours: Contour and pcolormesh plots
    !! - fortplot_plot_bars: Bar chart implementations
    !! - fortplot_plot_statistics: Histogram and boxplot functions
    
    use fortplot_plot_contours, only: add_contour_impl, add_contour_filled_impl, &
        add_pcolormesh_impl
    use fortplot_plot_bars, only: bar_impl, barh_impl
    use fortplot_plot_statistics, only: hist_impl, boxplot_impl
    use fortplot_figure_core, only: figure_t
    use fortplot_streamplot_core, only: setup_streamplot_parameters
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: add_contour_impl, add_contour_filled_impl, add_pcolormesh_impl
    public :: bar_impl, barh_impl, hist_impl, boxplot_impl
    public :: streamplot_impl
    
contains

    subroutine streamplot_impl(self, x, y, u, v, density, color, linewidth, rtol, atol, max_time, arrowsize, arrowstyle)
        !! Add streamlines for vector field
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density, color(3), linewidth
        real(wp), intent(in), optional :: rtol, atol, max_time, arrowsize
        character(len=*), intent(in), optional :: arrowstyle
        
        call setup_streamplot_parameters(self, x, y, u, v, density, color, linewidth, &
                                        rtol, atol, max_time, arrowsize, arrowstyle)
    end subroutine streamplot_impl

end module fortplot_plotting_advanced