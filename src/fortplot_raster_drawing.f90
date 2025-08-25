module fortplot_raster_drawing
    !! Raster-specific drawing utility functions
    !!
    !! This module provides low-level drawing primitives for raster graphics
    !! including antialiased lines, markers, shapes, and geometric functions.
    !!
    !! Author: fortplot contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS
    implicit none
    
    private
    public :: draw_line_distance_aa, blend_pixel, distance_point_to_line_segment
    public :: ipart, fpart, rfpart, color_to_byte
    public :: draw_circle_antialiased, draw_circle_outline_antialiased
    public :: draw_circle_with_edge_face, draw_square_with_edge_face
    public :: draw_diamond_with_edge_face, draw_x_marker
    public :: draw_filled_quad_raster

contains

    function color_to_byte(color_val) result(byte_val)
        !! Convert floating-point color value [0,1] to byte [0,255]
        real(wp), intent(in) :: color_val
        integer(1) :: byte_val
        
        if (color_val <= 0.0_wp) then
            byte_val = 0_1
        else if (color_val >= 1.0_wp) then
            byte_val = -1_1  ! 255 in two's complement
        else
            byte_val = int(color_val * 255.0_wp, kind=1)
        end if
    end function color_to_byte

    function distance_point_to_line_segment(px, py, x1, y1, x2, y2) result(distance)
        !! Calculate minimum distance from point to line segment
        real(wp), intent(in) :: px, py, x1, y1, x2, y2
        real(wp) :: distance
        
        real(wp) :: dx, dy, length_sq, t, proj_x, proj_y
        
        dx = x2 - x1
        dy = y2 - y1
        length_sq = dx * dx + dy * dy
        
        if (length_sq < 1e-12_wp) then
            distance = sqrt((px - x1)**2 + (py - y1)**2)
            return
        end if
        
        t = ((px - x1) * dx + (py - y1) * dy) / length_sq
        t = max(0.0_wp, min(1.0_wp, t))
        
        proj_x = x1 + t * dx
        proj_y = y1 + t * dy
        
        distance = sqrt((px - proj_x)**2 + (py - proj_y)**2)
    end function distance_point_to_line_segment

    function ipart(x) result(i)
        !! Integer part of floating-point number
        real(wp), intent(in) :: x
        integer :: i
        i = int(x)
    end function ipart

    function fpart(x) result(f)
        !! Fractional part of floating-point number
        real(wp), intent(in) :: x
        real(wp) :: f
        f = x - int(x)
    end function fpart

    function rfpart(x) result(rf)
        !! Reverse fractional part (1 - fractional part)
        real(wp), intent(in) :: x
        real(wp) :: rf
        rf = 1.0_wp - fpart(x)
    end function rfpart

    subroutine blend_pixel(image_data, img_w, img_h, x, y, alpha, new_r, new_g, new_b)
        !! Alpha blend a pixel with existing pixel data
        !! Uses consistent coordinate rounding to fix marker centering issue #333
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x, y, alpha, new_r, new_g, new_b
        
        integer :: ix, iy, idx
        real(wp) :: old_r, old_g, old_b, blend_r, blend_g, blend_b
        real(wp) :: clamped_alpha
        
        ! Consistent coordinate rounding for line-marker alignment (Issue #333)
        ! Both line and marker drawing should use the same rounding approach
        ix = nint(x)
        iy = nint(y)
        
        if (ix < 1 .or. ix > img_w .or. iy < 1 .or. iy > img_h) return
        
        clamped_alpha = max(0.0_wp, min(1.0_wp, alpha))
        if (clamped_alpha < 1e-6_wp) return
        
        idx = (iy - 1) * img_w * 3 + (ix - 1) * 3 + 1
        
        ! Convert signed bytes to unsigned range [0, 255] then to [0, 1]
        ! Negative values in signed bytes represent values > 127
        old_r = real(iand(int(image_data(idx)), 255), wp) / 255.0_wp
        old_g = real(iand(int(image_data(idx + 1)), 255), wp) / 255.0_wp
        old_b = real(iand(int(image_data(idx + 2)), 255), wp) / 255.0_wp
        
        blend_r = old_r * (1.0_wp - clamped_alpha) + new_r * clamped_alpha
        blend_g = old_g * (1.0_wp - clamped_alpha) + new_g * clamped_alpha
        blend_b = old_b * (1.0_wp - clamped_alpha) + new_b * clamped_alpha
        
        image_data(idx) = color_to_byte(blend_r)
        image_data(idx + 1) = color_to_byte(blend_g)
        image_data(idx + 2) = color_to_byte(blend_b)
    end subroutine blend_pixel

    subroutine draw_line_distance_aa(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b, width)
        !! Draw antialiased line using distance-based approach
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x0, y0, x1, y1, r, g, b, width
        
        integer :: xi, yi
        real(wp) :: distance, alpha, half_width
        integer :: x_min, x_max, y_min, y_max
        
        half_width = width * 0.5_wp
        
        x_min = max(1, int(min(x0, x1) - half_width - 1.0_wp))
        x_max = min(img_w, int(max(x0, x1) + half_width + 1.0_wp))
        y_min = max(1, int(min(y0, y1) - half_width - 1.0_wp))
        y_max = min(img_h, int(max(y0, y1) + half_width + 1.0_wp))
        
        do yi = y_min, y_max
            do xi = x_min, x_max
                distance = distance_point_to_line_segment(real(xi, wp), real(yi, wp), x0, y0, x1, y1)
                
                if (distance <= half_width + 1.0_wp) then
                    alpha = 1.0_wp - max(0.0_wp, distance - half_width)
                    alpha = max(0.0_wp, min(1.0_wp, alpha))
                    
                    if (alpha > 1e-6_wp) then
                        ! Pass sub-pixel coordinates for consistent marker-line alignment (Issue #333)
                        call blend_pixel(image_data, img_w, img_h, real(xi, wp), real(yi, wp), alpha, r, g, b)
                    end if
                end if
            end do
        end do
    end subroutine draw_line_distance_aa

    subroutine draw_circle_antialiased(image_data, img_w, img_h, cx, cy, radius, r, g, b)
        !! Draw filled circle with antialiasing
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
                dx = real(xi, wp) - cx
                dy = real(yi, wp) - cy
                distance_to_center = sqrt(dx**2 + dy**2)
                
                if (distance_to_center <= radius + 1.0_wp) then
                    alpha = 1.0_wp - max(0.0_wp, distance_to_center - radius)
                    alpha = max(0.0_wp, min(1.0_wp, alpha))
                    
                    if (alpha > 1e-6_wp) then
                        ! Use consistent coordinate handling for marker alignment (Issue #333)
                        call blend_pixel(image_data, img_w, img_h, real(xi, wp), real(yi, wp), alpha, r, g, b)
                    end if
                end if
            end do
        end do
    end subroutine draw_circle_antialiased

    subroutine draw_circle_outline_antialiased(image_data, img_w, img_h, cx, cy, radius, r, g, b)
        !! Draw circle outline with antialiasing
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
                distance_to_edge = abs(distance_to_center - radius)
                
                if (distance_to_edge <= edge_width + 1.0_wp) then
                    alpha = 1.0_wp - max(0.0_wp, distance_to_edge - edge_width * 0.5_wp)
                    alpha = max(0.0_wp, min(1.0_wp, alpha))
                    
                    if (alpha > 1e-6_wp) then
                        ! Use consistent coordinate handling for marker alignment (Issue #333)
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

    subroutine draw_filled_quad_raster(image_data, img_w, img_h, x_quad, y_quad, r, g, b)
        !! Draw filled quadrilateral using scanline algorithm
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x_quad(4), y_quad(4), r, g, b
        
        integer :: y, y_min, y_max
        real(wp) :: x_intersect(10)
        integer :: num_intersect, i, j, x_start, x_end, x, idx
        real(wp) :: y_real
        
        y_min = max(1, int(minval(y_quad)))
        y_max = min(img_h, int(maxval(y_quad)) + 1)
        
        do y = y_min, y_max
            y_real = real(y, wp)
            num_intersect = 0
            
            ! Find intersections with quad edges
            do i = 1, 4
                j = mod(i, 4) + 1
                
                if ((y_quad(i) <= y_real .and. y_real < y_quad(j)) .or. &
                    (y_quad(j) <= y_real .and. y_real < y_quad(i))) then
                    
                    if (abs(y_quad(j) - y_quad(i)) > 1e-10_wp) then
                        num_intersect = num_intersect + 1
                        x_intersect(num_intersect) = x_quad(i) + &
                            (y_real - y_quad(i)) * (x_quad(j) - x_quad(i)) / (y_quad(j) - y_quad(i))
                    end if
                end if
            end do
            
            ! Sort intersections and fill spans
            if (num_intersect >= 2) then
                ! Simple bubble sort for small arrays
                do i = 1, num_intersect - 1
                    do j = i + 1, num_intersect
                        if (x_intersect(i) > x_intersect(j)) then
                            ! Swap
                            y_real = x_intersect(i)  ! Reuse y_real as temp
                            x_intersect(i) = x_intersect(j)
                            x_intersect(j) = y_real
                        end if
                    end do
                end do
                
                ! Fill between pairs of intersections
                do i = 1, num_intersect - 1, 2
                    x_start = max(1, int(x_intersect(i)))
                    x_end = min(img_w, int(x_intersect(i + 1)))
                    
                    do x = x_start, x_end
                        idx = (y - 1) * img_w * 3 + (x - 1) * 3 + 1
                        image_data(idx) = color_to_byte(r)
                        image_data(idx + 1) = color_to_byte(g)
                        image_data(idx + 2) = color_to_byte(b)
                    end do
                end do
            end if
        end do
    end subroutine draw_filled_quad_raster

end module fortplot_raster_drawing