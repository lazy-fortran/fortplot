module fortplot_raster_primitives
    !! Basic raster drawing primitives and utilities
    !!
    !! This module provides fundamental drawing operations including antialiased
    !! lines, color conversion, geometric calculations, and pixel blending.
    !!
    !! # Pixel Coordinate System Conventions
    !!
    !! The module uses a 1-based coordinate system consistent with Fortran arrays:
    !! - Pixel (1,1) is at the top-left corner
    !! - x-coordinates increase rightward, y-coordinates increase downward
    !! - All coordinates are internally converted using nint() for consistency
    !!
    !! # Antialiasing Approach
    !!
    !! All drawing functions use distance-based antialiasing:
    !! - Calculate exact distance from pixel center to geometric shape
    !! - Alpha value derived from distance: alpha = 1.0 - max(0, distance - radius/half_width)
    !! - Sub-pixel accuracy maintained through real-valued intermediate calculations
    !! - Final pixel blending uses consistent coordinate rounding (nint)
    !!
    !! # Line-Marker Coordinate Alignment
    !!
    !! Critical design principle: Lines and markers must align precisely at data points.
    !! Both line drawing (draw_line_distance_aa) and marker positioning use identical
    !! coordinate rounding via nint() in blend_pixel to prevent visual misalignment.
    !! This addresses the centering issue where markers appeared offset from line endpoints.
    !!
    !! # Color Representation
    !!
    !! - Input colors: real values in range [0.0, 1.0]
    !! - Storage: signed bytes representing unsigned values [0, 255]
    !! - Conversion: color_to_byte handles proper range mapping and byte encoding
    !! - Alpha blending: standard over-operation with clamped alpha values
    !!
    !! Author: fortplot contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: EPSILON_GEOMETRY, EPSILON_COMPARE
    implicit none
    
    private
    public :: draw_line_distance_aa, blend_pixel, distance_point_to_line_segment
    public :: ipart, fpart, rfpart, color_to_byte, draw_filled_quad_raster
    public :: draw_filled_quad_raster_alpha
    
    
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
        !!
        !! Uses parametric line representation and projection to find closest point.
        !! For degenerate segments (length < EPSILON_GEOMETRY), returns distance to endpoint.
        !!
        !! px, py: Point coordinates
        !! x1, y1: Line segment start point
        !! x2, y2: Line segment end point
        !! Returns distance: Minimum euclidean distance from point to segment
        real(wp), intent(in) :: px, py, x1, y1, x2, y2
        real(wp) :: distance
        
        real(wp) :: dx, dy, length_sq, t, proj_x, proj_y
        
        dx = x2 - x1
        dy = y2 - y1
        length_sq = dx * dx + dy * dy
        
        ! Handle degenerate case: segment is essentially a point
        if (length_sq < EPSILON_GEOMETRY) then
            distance = sqrt((px - x1)**2 + (py - y1)**2)
            return
        end if
        
        ! Project point onto infinite line, then clamp to segment
        ! t = 0 at (x1,y1), t = 1 at (x2,y2)
        t = ((px - x1) * dx + (py - y1) * dy) / length_sq
        t = max(0.0_wp, min(1.0_wp, t))  ! Clamp to segment endpoints
        
        ! Calculate closest point on segment
        proj_x = x1 + t * dx
        proj_y = y1 + t * dy
        
        distance = sqrt((px - proj_x)**2 + (py - proj_y)**2)
    end function distance_point_to_line_segment

    function ipart(x) result(i)
        !! Integer part of floating-point number
        !!
        !! Used for antialiasing calculations.
        !! x: Real number to truncate
        !! Returns i: Integer part (truncated toward zero)
        real(wp), intent(in) :: x
        integer :: i
        i = int(x)
    end function ipart

    function fpart(x) result(f)
        !! Fractional part of floating-point number
        !!
        !! Returns the fractional component for antialiasing alpha calculations.
        !! Always returns positive value in range [0.0, 1.0).
        !! x: Real number to extract fraction from
        !! Returns f: Fractional part (x - floor(x))
        real(wp), intent(in) :: x
        real(wp) :: f
        ! Use floor for correct behavior with negative values
        f = x - floor(x)
    end function fpart

    function rfpart(x) result(rf)
        !! Reverse fractional part (1 - fractional part)
        !!
        !! Complementary fractional part for antialiasing calculations.
        !! Used to compute alpha values for adjacent pixels in line drawing.
        !! x: Real coordinate value
        !! Returns rf: Reverse fraction (1.0 - fpart(x))
        real(wp), intent(in) :: x
        real(wp) :: rf
        rf = 1.0_wp - fpart(x)
    end function rfpart

    subroutine blend_pixel(image_data, img_w, img_h, x, y, alpha, new_r, new_g, new_b)
        !! Alpha blend a pixel with existing pixel data
        !!
        !! Core pixel blending routine used by all drawing functions.
        !! Uses consistent coordinate rounding (nint) to ensure alignment
        !! between line endpoints and marker centers (fixes issue #333).
        !!
        !! @param image_data Packed RGB image array (signed bytes)
        !! @param img_w, img_h Image dimensions in pixels
        !! @param x, y Real-valued pixel coordinates (will be rounded)
        !! @param alpha Blending factor [0.0, 1.0] (clamped internally)
        !! @param new_r, new_g, new_b New color components [0.0, 1.0]
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x, y, alpha, new_r, new_g, new_b
        
        integer :: ix, iy, idx
        real(wp) :: old_r, old_g, old_b, blend_r, blend_g, blend_b
        real(wp) :: clamped_alpha
        
        ! Consistent coordinate rounding for line-marker alignment (Issue #333)
        ! Both line and marker drawing use nint() for identical pixel targeting
        ix = nint(x)
        iy = nint(y)
        
        ! Bounds checking: Fortran uses 1-based indexing
        if (ix < 1 .or. ix > img_w .or. iy < 1 .or. iy > img_h) return
        
        ! Clamp alpha to valid range and skip transparent pixels
        clamped_alpha = max(0.0_wp, min(1.0_wp, alpha))
        if (clamped_alpha < 1e-6_wp) return
        
        ! Calculate 1D array index for packed RGB data
        ! Layout: R1 G1 B1 R2 G2 B2 ... (row-major order)
        idx = (iy - 1) * img_w * 3 + (ix - 1) * 3 + 1
        
        ! Convert signed bytes to unsigned range [0, 255] then normalize to [0, 1]
        ! Use bitwise AND to handle negative signed bytes (which represent 128-255)
        old_r = real(iand(int(image_data(idx)), 255), wp) / 255.0_wp
        old_g = real(iand(int(image_data(idx + 1)), 255), wp) / 255.0_wp
        old_b = real(iand(int(image_data(idx + 2)), 255), wp) / 255.0_wp
        
        ! Standard alpha blending: new_color = old * (1-alpha) + new * alpha
        blend_r = old_r * (1.0_wp - clamped_alpha) + new_r * clamped_alpha
        blend_g = old_g * (1.0_wp - clamped_alpha) + new_g * clamped_alpha
        blend_b = old_b * (1.0_wp - clamped_alpha) + new_b * clamped_alpha
        
        image_data(idx) = color_to_byte(blend_r)
        image_data(idx + 1) = color_to_byte(blend_g)
        image_data(idx + 2) = color_to_byte(blend_b)
    end subroutine blend_pixel

    subroutine draw_line_distance_aa(image_data, img_w, img_h, x0, y0, x1, y1, r, g, b, &
                                     width, opacity)
        !! Draw antialiased line using distance-based approach
        !!
        !! Primary line drawing routine using geometric distance calculation.
        !! Provides high-quality antialiasing for lines of arbitrary width and orientation.
        !! Uses distance_point_to_line_segment for accurate alpha computation.
        !!
        !! @param image_data Target image buffer (packed RGB bytes)
        !! @param img_w, img_h Image dimensions
        !! @param x0, y0, x1, y1 Line endpoints in pixel coordinates
        !! @param r, g, b Line color components [0.0, 1.0]
        !! @param width Line width in pixels (can be fractional)
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x0, y0, x1, y1, r, g, b, width
        real(wp), intent(in), optional :: opacity
        
        integer :: xi, yi
        real(wp) :: distance, alpha, half_width
        real(wp) :: alpha_scale
        integer :: x_min, x_max, y_min, y_max
        
        half_width = width * 0.5_wp
        alpha_scale = 1.0_wp
        if (present(opacity)) alpha_scale = max(0.0_wp, min(1.0_wp, opacity))
        
        ! Calculate bounding box with 1-pixel antialiasing margin
        x_min = max(1, int(min(x0, x1) - half_width - 1.0_wp))
        x_max = min(img_w, int(max(x0, x1) + half_width + 1.0_wp))
        y_min = max(1, int(min(y0, y1) - half_width - 1.0_wp))
        y_max = min(img_h, int(max(y0, y1) + half_width + 1.0_wp))
        
        ! Process each pixel in bounding box
        do yi = y_min, y_max
            do xi = x_min, x_max
                ! Calculate exact distance from pixel center to line segment
                distance = distance_point_to_line_segment(real(xi, wp), real(yi, wp), x0, y0, x1, y1)
                
                ! Skip pixels too far from line (performance optimization)
                if (distance <= half_width + 0.5_wp) then
                    ! Coverage with a 1px-wide antialiasing transition centred on
                    ! the geometric edge: full inside (half_width - 0.5), zero
                    ! outside (half_width + 0.5). The 50% level sits at half_width,
                    ! so the rendered footprint matches the nominal stroke width
                    ! instead of overshooting it by ~1px (matplotlib AGG parity).
                    alpha = alpha_scale*(half_width + 0.5_wp - distance)
                    alpha = max(0.0_wp, min(1.0_wp, alpha))
                    
                    if (alpha > 1e-6_wp) then
                        ! Use integer coordinates - blend_pixel will apply nint() consistently
                        call blend_pixel(image_data, img_w, img_h, real(xi, wp), real(yi, wp), alpha, r, g, b)
                    end if
                end if
            end do
        end do
    end subroutine draw_line_distance_aa

    subroutine draw_filled_quad_raster_alpha(image_data, img_w, img_h, x_quad, y_quad, &
                                             r, g, b, opacity)
        !! Draw filled quadrilateral with anti-aliased, supersampled coverage and
        !! alpha blending. Supersampling in both axes gives the correct filled
        !! area for rotated quads such as diamond markers: a per-scanline nint
        !! span (the previous approach) collapsed the narrow rows near a diamond's
        !! tips, leaving the concave four-petal artifact. Each pixel's coverage is
        !! the fraction of sub-samples inside the polygon, then blended once at
        !! coverage*opacity so overlapping fills do not double-darken.
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x_quad(4), y_quad(4), r, g, b, opacity

        integer, parameter :: SS = 4
        real(wp), parameter :: SUB_W = 1.0_wp/real(SS, wp)
        integer :: y, y_min, y_max, x_lo, x_hi, nx, s, i, x
        real(wp) :: xint(10), y_real, alpha_scale, cov
        real(wp), allocatable :: cover(:)

        alpha_scale = max(0.0_wp, min(1.0_wp, opacity))
        if (alpha_scale < 1e-6_wp) return

        y_min = max(1, floor(minval(y_quad)))
        y_max = min(img_h, ceiling(maxval(y_quad)))
        x_lo = max(1, floor(minval(x_quad)))
        x_hi = min(img_w, ceiling(maxval(x_quad)))
        if (y_max < y_min .or. x_hi < x_lo) return

        nx = x_hi - x_lo + 1
        allocate (cover(nx))

        do y = y_min, y_max
            cover = 0.0_wp

            do s = 1, SS
                y_real = real(y, wp) - 0.5_wp + (real(s, wp) - 0.5_wp)*SUB_W
                call scanline_spans(x_quad, y_quad, y_real, xint, i)
                do x = 1, i - 1, 2
                    call accumulate_span(cover, x_lo, x_hi, xint(x), xint(x + 1), SUB_W)
                end do
            end do

            do x = x_lo, x_hi
                cov = cover(x - x_lo + 1)
                if (cov <= 0.0_wp) cycle
                call blend_pixel(image_data, img_w, img_h, real(x, wp), &
                                 real(y, wp), min(1.0_wp, cov)*alpha_scale, r, g, b)
            end do
        end do
    end subroutine draw_filled_quad_raster_alpha

    subroutine draw_filled_quad_raster(image_data, img_w, img_h, x_quad, y_quad, r, g, b)
        !! Draw filled quadrilateral using scanline algorithm.
        !!
        !! Two passes. The integer-scanline pass reproduces the original solid
        !! fill exactly (so adjacent quads tile and full-coverage shapes are
        !! unchanged). A supersampled recovery pass then anti-aliases the edges
        !! and, crucially, paints thin band polygons that fall between integer
        !! scanlines: without it those strips vanish and leave the broken
        !! one-pixel arcs reported in issue #1961. The recovery pass only
        !! blends pixels the integer pass left untouched, so it never erodes
        !! the solid interior.
        !!
        !! @param image_data Target image buffer
        !! @param img_w, img_h Image dimensions
        !! @param x_quad, y_quad Quadrilateral vertex coordinates [4 vertices]
        !! @param r, g, b Fill color components [0.0, 1.0]
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x_quad(4), y_quad(4), r, g, b

        integer :: y, y_min, y_max
        real(wp) :: x_intersect(10)
        integer :: num_intersect, i, j, x_start, x_end, x, idx
        real(wp) :: y_real

        ! Use rounding to avoid systematic underfill at cell boundaries
        y_min = max(1, nint(minval(y_quad)))
        y_max = min(img_h, nint(maxval(y_quad)) + 1)

        ! Process each scanline from top to bottom
        do y = y_min, y_max
            y_real = real(y, wp)
            num_intersect = 0

            ! Find intersections of current scanline with quadrilateral edges
            do i = 1, 4
                j = mod(i, 4) + 1  ! Next vertex (wrapping to 1 after 4)

                ! Check if scanline crosses this edge (exclusive upper bound prevents double-counting)
                if ((y_quad(i) <= y_real .and. y_real < y_quad(j)) .or. &
                    (y_quad(j) <= y_real .and. y_real < y_quad(i))) then

                    ! Calculate x-coordinate of intersection (avoid division by zero)
                    if (abs(y_quad(j) - y_quad(i)) > EPSILON_COMPARE) then
                        num_intersect = num_intersect + 1
                        ! Linear interpolation along edge
                        x_intersect(num_intersect) = x_quad(i) + &
                            (y_real - y_quad(i)) * (x_quad(j) - x_quad(i)) / (y_quad(j) - y_quad(i))
                    end if
                end if
            end do

            ! Sort intersection x-coordinates and fill spans between pairs
            if (num_intersect >= 2) then
                ! Simple bubble sort (adequate for small arrays)
                do i = 1, num_intersect - 1
                    do j = i + 1, num_intersect
                        if (x_intersect(i) > x_intersect(j)) then
                            y_real = x_intersect(i)  ! Reuse y_real as temporary
                            x_intersect(i) = x_intersect(j)
                            x_intersect(j) = y_real
                        end if
                    end do
                end do

                ! Fill horizontal spans between intersection pairs
                do i = 1, num_intersect - 1, 2
                    x_start = max(1, nint(x_intersect(i)))
                    x_end = min(img_w, nint(x_intersect(i + 1)))

                    ! Draw pixels in current span (no antialiasing for filled shapes)
                    do x = x_start, x_end
                        idx = (y - 1) * img_w * 3 + (x - 1) * 3 + 1
                        image_data(idx) = color_to_byte(r)
                        image_data(idx + 1) = color_to_byte(g)
                        image_data(idx + 2) = color_to_byte(b)
                    end do
                end do
            end if
        end do

        call recover_thin_quad_coverage(image_data, img_w, img_h, x_quad, y_quad, &
                                        r, g, b)
    end subroutine draw_filled_quad_raster

    subroutine recover_thin_quad_coverage(image_data, img_w, img_h, x_quad, y_quad, &
                                          r, g, b)
        !! Supersampled second pass for draw_filled_quad_raster: blend the
        !! polygon's anti-aliased coverage into pixels the integer-scanline pass
        !! left untouched. Reproduces the integer pass's per-pixel mask so it
        !! only adds new (edge and thin-strip) pixels; never overwrites solid
        !! interior pixels.
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: x_quad(4), y_quad(4), r, g, b

        integer, parameter :: SS = 4
        real(wp), parameter :: SUB_W = 1.0_wp/real(SS, wp)
        integer :: y, y_min, y_max, x_lo, x_hi, nx, s, i, j, num, x
        real(wp) :: xint(10), y_real, tmp
        real(wp), allocatable :: cover(:)
        logical, allocatable :: solid(:)

        y_min = max(1, floor(minval(y_quad)))
        y_max = min(img_h, ceiling(maxval(y_quad)))
        x_lo = max(1, floor(minval(x_quad)))
        x_hi = min(img_w, ceiling(maxval(x_quad)))
        if (y_max < y_min .or. x_hi < x_lo) return

        nx = x_hi - x_lo + 1
        allocate (cover(nx), solid(nx))

        do y = y_min, y_max
            cover = 0.0_wp
            solid = .false.

            ! Mark pixels the integer pass already filled at this row.
            call scanline_spans(x_quad, y_quad, real(y, wp), xint, num)
            do i = 1, num - 1, 2
                do x = max(x_lo, nint(xint(i))), min(x_hi, nint(xint(i + 1)))
                    solid(x - x_lo + 1) = .true.
                end do
            end do

            ! Supersample coverage across the pixel row.
            do s = 1, SS
                y_real = real(y, wp) - 0.5_wp + (real(s, wp) - 0.5_wp)*SUB_W
                call scanline_spans(x_quad, y_quad, y_real, xint, num)
                do i = 1, num - 1, 2
                    call accumulate_span(cover, x_lo, x_hi, xint(i), xint(i + 1), &
                                         SUB_W)
                end do
            end do

            do x = x_lo, x_hi
                if (solid(x - x_lo + 1)) cycle
                tmp = cover(x - x_lo + 1)
                if (tmp <= 0.0_wp) cycle
                call blend_pixel(image_data, img_w, img_h, real(x, wp), &
                                 real(y, wp), min(1.0_wp, tmp), r, g, b)
            end do
        end do
    end subroutine recover_thin_quad_coverage

    subroutine scanline_spans(x_quad, y_quad, y_real, xint, num)
        !! Sorted x-intersections of a horizontal scanline with the quad edges.
        real(wp), intent(in) :: x_quad(4), y_quad(4), y_real
        real(wp), intent(out) :: xint(:)
        integer, intent(out) :: num
        integer :: i, j
        real(wp) :: tmp

        num = 0
        do i = 1, 4
            j = mod(i, 4) + 1
            if ((y_quad(i) <= y_real .and. y_real < y_quad(j)) .or. &
                (y_quad(j) <= y_real .and. y_real < y_quad(i))) then
                if (abs(y_quad(j) - y_quad(i)) > EPSILON_COMPARE) then
                    num = num + 1
                    xint(num) = x_quad(i) + (y_real - y_quad(i))* &
                                (x_quad(j) - x_quad(i))/(y_quad(j) - y_quad(i))
                end if
            end if
        end do
        do i = 1, num - 1
            do j = i + 1, num
                if (xint(i) > xint(j)) then
                    tmp = xint(i); xint(i) = xint(j); xint(j) = tmp
                end if
            end do
        end do
    end subroutine scanline_spans

    subroutine accumulate_span(coverage, x_lo, x_hi, xa, xb, weight)
        !! Add horizontal coverage for span [xa, xb] to the per-pixel
        !! accumulator, scaled by the sub-scanline weight. The core pixels
        !! (centres in [xa, xb]) match the integer fill; the pixels just outside
        !! each end take fractional coverage so edges anti-alias.
        real(wp), intent(inout) :: coverage(:)
        integer, intent(in) :: x_lo, x_hi
        real(wp), intent(in) :: xa, xb, weight
        integer :: xs, xe, x
        real(wp) :: frac

        xs = nint(xa)
        xe = nint(xb)
        do x = max(x_lo, xs), min(x_hi, xe)
            coverage(x - x_lo + 1) = coverage(x - x_lo + 1) + weight
        end do

        x = xs - 1
        if (x >= x_lo .and. x <= x_hi) then
            frac = max(0.0_wp, min(1.0_wp, real(xs, wp) - 0.5_wp - xa))
            coverage(x - x_lo + 1) = coverage(x - x_lo + 1) + weight*frac
        end if
        x = xe + 1
        if (x >= x_lo .and. x <= x_hi) then
            frac = max(0.0_wp, min(1.0_wp, xb - (real(xe, wp) + 0.5_wp)))
            coverage(x - x_lo + 1) = coverage(x - x_lo + 1) + weight*frac
        end if
    end subroutine accumulate_span

end module fortplot_raster_primitives
