module fortplot_plotting
    !! Basic plot addition orchestrator module (SOLID principles compliance)
    !! 
    !! This module orchestrates plotting operations by delegating to
    !! specialized plotting modules for better modularity and maintainability.

    use fortplot_2d_plots
    use fortplot_3d_plots  
    use fortplot_scatter_plots
    use fortplot_errorbar_plots
    use fortplot_plot_annotations

    implicit none

    private
    public :: add_plot, add_3d_plot, add_scatter_2d, add_scatter_3d, add_surface
    public :: errorbar, add_text_annotation, add_arrow_annotation
    public :: add_line_plot_data, add_scatter_plot_data  ! Export for advanced module

end module fortplot_plotting