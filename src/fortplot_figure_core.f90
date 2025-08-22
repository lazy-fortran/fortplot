module fortplot_figure_core
    !! Refactored figure management module (SOLID principles compliance)
    !! 
    !! This module provides a lightweight interface that coordinates between
    !! the separated figure components for better maintainability and testability.
    !! 
    !! Major components have been split into focused modules:
    !! - fortplot_figure_base: Core figure type and initialization
    !! - fortplot_plotting: All add_* methods for different plot types
    !! - fortplot_rendering: All render_* methods and display
    !! - fortplot_coordinates: Coordinate transformations and projections
    !! - fortplot_streamplot_core: Streamplot implementation
    
    use fortplot_figure_base, only: figure_t
    use fortplot_plotting, only: add_plot, add_3d_plot, add_scatter_2d, add_scatter_3d, &
                                 add_surface, add_contour, add_contour_filled, add_pcolormesh, &
                                 bar, barh, hist, boxplot, streamplot, errorbar, &
                                 add_text_annotation, add_arrow_annotation
    use fortplot_rendering, only: render_figure, savefig, show, figure_legend, &
                                  render_annotations, clear_streamlines, gather_subplot_plots
    use fortplot_coordinates, only: transform_annotation_coordinates, transform_quad_to_screen
    use fortplot_streamplot_core, only: setup_streamplot_parameters, generate_streamlines, &
                                       render_streamplot_arrows
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, subplot_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                    PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
                                    PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER
    use fortplot_annotations, only: COORD_DATA, COORD_FIGURE, COORD_AXIS
    use fortplot_utils, only: ensure_directory_exists
    
    implicit none

    private
    public :: figure_t, plot_data_t, subplot_t, arrow_data_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
              PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, PLOT_TYPE_BOXPLOT, &
              PLOT_TYPE_SCATTER
    public :: ensure_directory_exists
    public :: COORD_DATA, COORD_FIGURE, COORD_AXIS
    
    ! All functionality has been moved to specialized modules:
    ! - fortplot_figure_base: Core figure type and initialization  
    ! - fortplot_plotting: Plot addition methods
    ! - fortplot_rendering: Rendering pipeline and display
    ! - fortplot_coordinates: Coordinate transformations
    ! - fortplot_streamplot_core: Streamplot implementation
    !
    ! This module now serves as a convenience wrapper that re-exports
    ! the interfaces from the specialized modules.

end module fortplot_figure_core