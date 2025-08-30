module fortplot_raster_markers
    !! Raster marker drawing functions
    !!
    !! This module provides specialized drawing functions for various marker types
    !! including circles, squares, diamonds, and X markers. All functions use
    !! antialiasing and support separate edge/face colors for complex markers.
    !!
    !! All marker functions use the same coordinate system and rounding approach
    !! as the primitive drawing functions to ensure perfect alignment with lines.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: EPSILON_GEOMETRY, EPSILON_COMPARE
    use fortplot_raster_primitives, only: blend_pixel, draw_line_distance_aa, draw_filled_quad_raster
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS
    implicit none
    
    private
    public :: draw_circle_antialiased, draw_circle_outline_antialiased
    public :: draw_circle_with_edge_face, draw_square_with_edge_face
    public :: draw_diamond_with_edge_face, draw_x_marker
    
    
contains

    subroutine draw_circle_antialiased(image_data, img_w, img_h, cx, cy, radius, r, g, b)
        !! Draw filled circle with antialiasing
        !!
        !! Renders a filled circle using distance-based antialiasing.
        !! Alpha values computed from exact distance to circle boundary.
        !! Used primarily for circular markers.
        !!
        !! @param image_data Target image buffer
        !! @param img_w, img_h Image dimensions
        !! @param cx, cy Circle center coordinates
        !! @param radius Circle radius in pixels
        !! @param r, g, b Fill color components [0.0, 1.0]
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, radius, r, g, b
        
        integer :: x_min, x_max, y_min, y_max, xi, yi
        real(wp) :: dx, dy, distance_to_center, alpha
        
        x_min = max(1, int(cx - radius - 1.0_wp))
        x_max = min(img_w, int(cx + radius + 1.0_wp))
        y_min = max(1, int(cy - radius - 1.0_wp))
        y_max = min(img_h, int(cy + radius + 1.0_wp))
        
        do yi = y_min, y_max
            do xi = x_min, x_max
                ! Calculate distance from pixel center to circle center
                dx = real(xi, wp) - cx
                dy = real(yi, wp) - cy
                distance_to_center = sqrt(dx**2 + dy**2)
                
                ! Only process pixels near circle boundary (optimization)
                if (distance_to_center <= radius + 1.0_wp) then
                    ! Alpha based on distance from circle edge
                    ! Inside circle: alpha = 1.0, fades to 0.0 beyond radius + 1.0
                    alpha = 1.0_wp - max(0.0_wp, distance_to_center - radius)
                    alpha = max(0.0_wp, min(1.0_wp, alpha))
                    
                    if (alpha > 1e-6_wp) then
                        ! Use integer coordinates for consistent marker positioning
                        call blend_pixel(image_data, img_w, img_h, real(xi, wp), real(yi, wp), alpha, r, g, b)
                    end if
                end if
            end do
        end do
    end subroutine draw_circle_antialiased

    subroutine draw_circle_outline_antialiased(image_data, img_w, img_h, cx, cy, radius, r, g, b)
        !! Draw circle outline with antialiasing
        !!
        !! Renders a circular outline (ring) using distance-based antialiasing.
        !! Alpha computed from distance to the ideal circle boundary.
        !! Used for circle markers with edge-only rendering.
        !!
        !! @param image_data Target image buffer
        !! @param img_w, img_h Image dimensions  
        !! @param cx, cy Circle center coordinates
        !! @param radius Circle radius in pixels
        !! @param r, g, b Outline color components [0.0, 1.0]
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, radius, r, g, b
        
        integer :: x_min, x_max, y_min, y_max, xi, yi
        real(wp) :: dx, dy, distance_to_center, distance_to_edge, alpha
        real(wp), parameter :: edge_width = 1.0_wp
        
        x_min = max(1, int(cx - radius - edge_width - 1.0_wp))
        x_max = min(img_w, int(cx + radius + edge_width + 1.0_wp))
        y_min = max(1, int(cy - radius - edge_width - 1.0_wp))
        y_max = min(img_h, int(cy + radius + edge_width + 1.0_wp))
        
        do yi = y_min, y_max
            do xi = x_min, x_max
                dx = real(xi, wp) - cx
                dy = real(yi, wp) - cy
                distance_to_center = sqrt(dx**2 + dy**2)
                
                ! Distance from pixel to the circle edge (positive = outside, negative = inside)
                distance_to_edge = abs(distance_to_center - radius)
                
                ! Only process pixels near the circle edge
                if (distance_to_edge <= edge_width + 1.0_wp) then
                    ! Alpha based on distance from ideal circle boundary
                    ! Maximum at exact radius, fades with distance
                    alpha = 1.0_wp - max(0.0_wp, distance_to_edge - edge_width * 0.5_wp)
                    alpha = max(0.0_wp, min(1.0_wp, alpha))
                    
                    if (alpha > 1e-6_wp) then
                        call blend_pixel(image_data, img_w, img_h, real(xi, wp), real(yi, wp), alpha, r, g, b)
                    end if
                end if
            end do
        end do
    end subroutine draw_circle_outline_antialiased

    subroutine draw_circle_with_edge_face(image_data, img_w, img_h, cx, cy, radius, &
                                         edge_r, edge_g, edge_b, edge_alpha, &
                                         face_r, face_g, face_b, face_alpha)
        !! Draw circle with separate edge and face colors
        !!
        !! Composite drawing routine for circle markers with both fill and outline.
        !! Draws face (filled circle) first, then edge (outline) on top.
        !! Both components use identical coordinate system for perfect alignment.
        !!
        !! @param image_data Target image buffer
        !! @param img_w, img_h Image dimensions
        !! @param cx, cy Circle center coordinates
        !! @param radius Circle radius in pixels
        !! @param edge_r, edge_g, edge_b, edge_alpha Outline color and opacity
        !! @param face_r, face_g, face_b, face_alpha Fill color and opacity
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, radius
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        
        ! Draw filled circle (face) first
        if (face_alpha > 1e-6_wp) then
            call draw_circle_antialiased(image_data, img_w, img_h, cx, cy, radius, &
                                       face_r, face_g, face_b)
        end if
        
        ! Draw outline (edge) second
        if (edge_alpha > 1e-6_wp) then
            call draw_circle_outline_antialiased(image_data, img_w, img_h, cx, cy, radius, &
                                               edge_r, edge_g, edge_b)
        end if
    end subroutine draw_circle_with_edge_face

    subroutine draw_square_with_edge_face(image_data, img_w, img_h, cx, cy, size, &
                                         edge_r, edge_g, edge_b, edge_alpha, &
                                         face_r, face_g, face_b, face_alpha)
        !! Draw square marker with separate edge and face colors
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, size
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        
        real(wp) :: half_size, x1, y1, x2, y2
        real(wp) :: x_quad(4), y_quad(4)
        integer :: xi, yi, x_min, x_max, y_min, y_max
        
        half_size = size * 0.5_wp
        x1 = cx - half_size
        y1 = cy - half_size
        x2 = cx + half_size
        y2 = cy + half_size
        
        ! Draw filled square (face) if visible
        if (face_alpha > 1e-6_wp) then
            x_quad(1) = x1; y_quad(1) = y1  ! Bottom-left
            x_quad(2) = x2; y_quad(2) = y1  ! Bottom-right
            x_quad(3) = x2; y_quad(3) = y2  ! Top-right
            x_quad(4) = x1; y_quad(4) = y2  ! Top-left
            
            call draw_filled_quad_raster(image_data, img_w, img_h, x_quad, y_quad, face_r, face_g, face_b)
        end if
        
        ! Draw square outline (edge) if visible
        if (edge_alpha > 1e-6_wp) then
            x_min = max(1, int(x1))
            x_max = min(img_w, int(x2))
            y_min = max(1, int(y1))
            y_max = min(img_h, int(y2))
            
            ! Draw outline using line segments
            call draw_line_distance_aa(image_data, img_w, img_h, x1, y1, x2, y1, &
                                     edge_r, edge_g, edge_b, 1.0_wp)  ! Bottom
            call draw_line_distance_aa(image_data, img_w, img_h, x2, y1, x2, y2, &
                                     edge_r, edge_g, edge_b, 1.0_wp)  ! Right
            call draw_line_distance_aa(image_data, img_w, img_h, x2, y2, x1, y2, &
                                     edge_r, edge_g, edge_b, 1.0_wp)  ! Top
            call draw_line_distance_aa(image_data, img_w, img_h, x1, y2, x1, y1, &
                                     edge_r, edge_g, edge_b, 1.0_wp)  ! Left
        end if
    end subroutine draw_square_with_edge_face

    subroutine draw_diamond_with_edge_face(image_data, img_w, img_h, cx, cy, size, &
                                          edge_r, edge_g, edge_b, edge_alpha, &
                                          face_r, face_g, face_b, face_alpha)
        !! Draw diamond marker with separate edge and face colors
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, size
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        
        real(wp) :: half_size, x_quad(4), y_quad(4)
        
        half_size = size * 0.5_wp
        
        ! Diamond vertices
        x_quad(1) = cx;          y_quad(1) = cy - half_size  ! Top
        x_quad(2) = cx + half_size; y_quad(2) = cy           ! Right
        x_quad(3) = cx;          y_quad(3) = cy + half_size  ! Bottom
        x_quad(4) = cx - half_size; y_quad(4) = cy           ! Left
        
        ! Draw filled diamond (face) if visible
        if (face_alpha > 1e-6_wp) then
            call draw_filled_quad_raster(image_data, img_w, img_h, x_quad, y_quad, face_r, face_g, face_b)
        end if
        
        ! Draw diamond outline (edge) if visible
        if (edge_alpha > 1e-6_wp) then
            call draw_line_distance_aa(image_data, img_w, img_h, x_quad(1), y_quad(1), x_quad(2), y_quad(2), &
                                     edge_r, edge_g, edge_b, 1.0_wp)
            call draw_line_distance_aa(image_data, img_w, img_h, x_quad(2), y_quad(2), x_quad(3), y_quad(3), &
                                     edge_r, edge_g, edge_b, 1.0_wp)
            call draw_line_distance_aa(image_data, img_w, img_h, x_quad(3), y_quad(3), x_quad(4), y_quad(4), &
                                     edge_r, edge_g, edge_b, 1.0_wp)
            call draw_line_distance_aa(image_data, img_w, img_h, x_quad(4), y_quad(4), x_quad(1), y_quad(1), &
                                     edge_r, edge_g, edge_b, 1.0_wp)
        end if
    end subroutine draw_diamond_with_edge_face

    subroutine draw_x_marker(image_data, img_w, img_h, cx, cy, size, edge_r, edge_g, edge_b)
        !! Draw X-shaped marker
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: cx, cy, size, edge_r, edge_g, edge_b
        
        real(wp) :: half_size
        
        half_size = size * 0.5_wp
        
        ! Draw diagonal lines to form X
        call draw_line_distance_aa(image_data, img_w, img_h, &
                                 cx - half_size, cy - half_size, &
                                 cx + half_size, cy + half_size, &
                                 edge_r, edge_g, edge_b, 1.0_wp)
        call draw_line_distance_aa(image_data, img_w, img_h, &
                                 cx - half_size, cy + half_size, &
                                 cx + half_size, cy - half_size, &
                                 edge_r, edge_g, edge_b, 1.0_wp)
    end subroutine draw_x_marker

end module fortplot_raster_markers