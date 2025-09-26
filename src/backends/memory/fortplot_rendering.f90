module fortplot_rendering
    !! Figure rendering pipeline orchestrator module
    !! 
    !! This module orchestrates the rendering pipeline by delegating to
    !! specialized rendering modules for different plot types. (no pre-transform exports)

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_line_rendering
    use fortplot_marker_rendering
    use fortplot_contour_rendering
    use fortplot_mesh_rendering
    use fortplot_boxplot_rendering
    use fortplot_bar_rendering, only: render_bar_plot
    use fortplot_errorbar_rendering, only: render_errorbar_plot
    use fortplot_pie_rendering, only: render_pie_plot
    implicit none
    
    private
    public :: render_line_plot
    public :: render_contour_plot
    public :: render_pcolormesh_plot
    public :: render_fill_between_plot
    public :: render_boxplot_plot
    public :: render_markers
    public :: render_errorbar_plot
    public :: render_bar_plot
    public :: render_pie_plot
    public :: render_solid_line
    public :: draw_filled_quad
    public :: draw_quad_edges
    public :: draw_single_point_marker
    
end module fortplot_rendering
