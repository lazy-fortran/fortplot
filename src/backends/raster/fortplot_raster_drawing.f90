module fortplot_raster_drawing
    !! Main raster drawing module that provides unified interface to drawing subsystems
    use fortplot_raster_primitives, only: draw_line_distance_aa, blend_pixel, distance_point_to_line_segment
    use fortplot_raster_primitives, only: ipart, fpart, rfpart, color_to_byte, draw_filled_quad_raster
    use fortplot_raster_markers, only: draw_circle_antialiased, draw_circle_outline_antialiased
    use fortplot_raster_markers, only: draw_circle_with_edge_face, draw_square_with_edge_face
    use fortplot_raster_markers, only: draw_diamond_with_edge_face, draw_x_marker
    implicit none
    
    private
    
    ! Re-export public interface from sub-modules
    public :: draw_line_distance_aa, blend_pixel, distance_point_to_line_segment
    public :: ipart, fpart, rfpart, color_to_byte
    public :: draw_circle_antialiased, draw_circle_outline_antialiased
    public :: draw_circle_with_edge_face, draw_square_with_edge_face
    public :: draw_diamond_with_edge_face, draw_x_marker
    public :: draw_filled_quad_raster

end module fortplot_raster_drawing