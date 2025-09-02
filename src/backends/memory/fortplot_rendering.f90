module fortplot_rendering
    !! Figure rendering pipeline orchestrator module
    !! 
    !! This module orchestrates the rendering pipeline by delegating to
    !! specialized rendering modules for different plot types.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_line_rendering
    use fortplot_marker_rendering
    use fortplot_contour_rendering
    use fortplot_mesh_rendering
    implicit none
    
    private
    public :: render_line_plot
    public :: render_contour_plot
    public :: render_pcolormesh_plot
    public :: render_markers
    public :: render_solid_line
    public :: draw_filled_quad
    public :: draw_quad_edges
    public :: draw_single_point_marker
    
end module fortplot_rendering
