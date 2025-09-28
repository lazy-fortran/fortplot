module fortplot_ascii_primitives
    !! ASCII terminal plotting backend - Drawing Primitives
    !!
    !! This module contains primitive drawing functions for ASCII plotting,
    !! including line drawing, color management, and shape filling.
    !!
    !! Author: fortplot contributors
    
    use fortplot_ascii_utils, only: get_char_density, get_blend_char, ASCII_CHARS
    use fortplot_latex_parser, only: process_latex_in_text
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: ascii_draw_line_primitive, ascii_fill_quad_primitive
    public :: ascii_draw_text_primitive
    
    ! Color filtering thresholds
    real(wp), parameter :: LIGHT_COLOR_THRESHOLD = 0.8_wp
    real(wp), parameter :: MEDIUM_COLOR_THRESHOLD = 0.7_wp
    
contains

    subroutine ascii_draw_line_primitive(canvas, x1, y1, x2, y2, &
                                        x_min, x_max, y_min, y_max, &
                                        plot_width, plot_height, &
                                        current_r, current_g, current_b)
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        real(wp), intent(in) :: current_r, current_g, current_b
        
        real(wp) :: dx, dy, length, step_x, step_y, x, y
        integer :: steps, i, px, py
        character(len=1) :: line_char
        real(wp) :: luminance
        
        ! Calculate luminance for better character selection
        ! Using standard luminance formula
        luminance = 0.299_wp * current_r + 0.587_wp * current_g + 0.114_wp * current_b
        
        ! Select character based on color dominance and luminance
        ! Don't skip any colors - render everything
        if (luminance > 0.9_wp) then
            ! Very bright colors still get rendered with lighter characters
            line_char = ':'
        else if (current_g > 0.7_wp) then
            line_char = '@'
        else if (current_g > 0.3_wp) then
            line_char = '#'
        else if (current_b > 0.7_wp) then
            line_char = '*'
        else if (current_b > 0.3_wp) then
            line_char = 'o'
        else if (current_r > 0.7_wp) then
            line_char = '%'
        else if (current_r > 0.3_wp) then
            line_char = '+'
        else
            line_char = '.'
        end if
        
        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)
        
        if (length < 1e-6_wp) return
        
        steps = max(int(length * 4), max(abs(int(dx)), abs(int(dy)))) + 1
        step_x = dx / real(steps, wp)
        step_y = dy / real(steps, wp)
        
        x = x1
        y = y1
        
        do i = 0, steps
            ! Map to usable plot area (excluding 1-char border on each side)
            px = int((x - x_min) / (x_max - x_min) * real(plot_width - 3, wp)) + 2
            py = (plot_height - 1) - int((y - y_min) / (y_max - y_min) * real(plot_height - 3, wp))
            
            
            if (px >= 2 .and. px <= plot_width - 1 .and. py >= 2 .and. py <= plot_height - 1) then
                if (canvas(py, px) == ' ') then
                    canvas(py, px) = line_char
                else if (canvas(py, px) /= line_char) then
                    canvas(py, px) = get_blend_char(canvas(py, px), line_char)
                end if
            end if
            
            x = x + step_x
            y = y + step_y
        end do
    end subroutine ascii_draw_line_primitive

    subroutine ascii_fill_quad_primitive(canvas, x_quad, y_quad, &
                                        x_min, x_max, y_min, y_max, &
                                        plot_width, plot_height, &
                                        current_r, current_g, current_b)
        !! Fill quadrilateral using character mapping based on current color
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        real(wp), intent(in) :: current_r, current_g, current_b
        
        integer :: px(4), py(4), i, j, min_x, max_x, min_y, max_y
        logical :: inside_first, inside_second
        real(wp) :: x_center, y_center
        character(len=1) :: fill_char
        real(wp) :: color_intensity
        integer :: char_index
        
        ! Convert coordinates to ASCII canvas coordinates (matching line drawing algorithm)
        do i = 1, 4
            ! Map to usable plot area (excluding 1-char border on each side)
            px(i) = int((x_quad(i) - x_min) / &
                (x_max - x_min) * real(plot_width - 3, wp)) + 2
            py(i) = (plot_height - 1) - int((y_quad(i) - y_min) / &
                (y_max - y_min) * real(plot_height - 3, wp))
        end do
        
        ! Calculate color intensity from RGB values (luminance formula)
        color_intensity = 0.299_wp * current_r + 0.587_wp * current_g + &
            0.114_wp * current_b
        
        ! Map color intensity to ASCII character index with proper low-intensity handling
        if (color_intensity <= 0.001_wp) then
            char_index = 1  ! Space for zero intensity
        else
            ! Map 0.0-1.0 intensity to full character range 1-len(ASCII_CHARS)
            char_index = min(len(ASCII_CHARS), max(1, int(color_intensity * len(ASCII_CHARS)) + 1))
        end if
        
        fill_char = ASCII_CHARS(char_index:char_index)
        
        ! Fill bounding rectangle with bounds checking
        min_x = max(2, min(minval(px), plot_width - 1))
        max_x = max(2, min(maxval(px), plot_width - 1))  
        min_y = max(2, min(minval(py), plot_height - 1))
        max_y = max(2, min(maxval(py), plot_height - 1))
        
        do j = min_y, max_y
            y_center = real(j, wp)
            do i = min_x, max_x
                x_center = real(i, wp)
                inside_first = point_in_triangle(x_center, y_center, &
                                    real(px(1), wp), real(py(1), wp), &
                                    real(px(2), wp), real(py(2), wp), &
                                    real(px(3), wp), real(py(3), wp))
                inside_second = point_in_triangle(x_center, y_center, &
                                     real(px(1), wp), real(py(1), wp), &
                                     real(px(3), wp), real(py(3), wp), &
                                     real(px(4), wp), real(py(4), wp))
                if (.not. (inside_first .or. inside_second)) cycle

                if (canvas(j, i) == ' ') then
                    canvas(j, i) = fill_char
                end if
            end do
        end do
    end subroutine ascii_fill_quad_primitive

    pure logical function point_in_triangle(px, py, ax, ay, bx, by, cx, cy)
        real(wp), intent(in) :: px, py, ax, ay, bx, by, cx, cy
        real(wp) :: v0x, v0y, v1x, v1y, v2x, v2y
        real(wp) :: dot00, dot01, dot02, dot11, dot12, inv_denom

        v0x = cx - ax
        v0y = cy - ay
        v1x = bx - ax
        v1y = by - ay
        v2x = px - ax
        v2y = py - ay

        dot00 = v0x * v0x + v0y * v0y
        dot01 = v0x * v1x + v0y * v1y
        dot02 = v0x * v2x + v0y * v2y
        dot11 = v1x * v1x + v1y * v1y
        dot12 = v1x * v2x + v1y * v2y

        inv_denom = dot00 * dot11 - dot01 * dot01
        if (abs(inv_denom) < 1.0e-12_wp) then
            point_in_triangle = .false.
            return
        end if
        inv_denom = 1.0_wp / inv_denom

        v0x = (dot11 * dot02 - dot01 * dot12) * inv_denom
        v0y = (dot00 * dot12 - dot01 * dot02) * inv_denom

        point_in_triangle = (v0x >= -1.0e-9_wp) .and. (v0y >= -1.0e-9_wp) .and. &
                            (v0x + v0y <= 1.0_wp + 1.0e-9_wp)
    end function point_in_triangle

    subroutine ascii_draw_text_primitive(text_x, text_y, text, &
                                        x, y, text_input, &
                                        x_min, x_max, y_min, y_max, &
                                        plot_width, plot_height, &
                                        current_r, current_g, current_b)
        integer, intent(out) :: text_x, text_y
        character(len=:), allocatable, intent(out) :: text
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text_input
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        real(wp), intent(in) :: current_r, current_g, current_b
        
        character(len=500) :: processed_text
        integer :: processed_len
        
        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text_input, processed_text, processed_len)
        
        ! Convert coordinates - check if already in screen coordinates
        if (x >= 1.0_wp .and. x <= real(plot_width, wp) .and. &
            y >= 1.0_wp .and. y <= real(plot_height, wp)) then
            ! Already in screen coordinates (e.g., from legend)
            text_x = nint(x)
            text_y = nint(y)
        else
            ! Convert from data coordinates to canvas coordinates
            text_x = nint((x - x_min) / (x_max - x_min) * real(plot_width, wp))
            text_y = nint((y_max - y) / (y_max - y_min) * real(plot_height, wp))
        end if
        
        ! Clamp to canvas bounds
        ! For legend text (already in screen coordinates), don't truncate based on length
        if (x >= 1.0_wp .and. x <= real(plot_width, wp) .and. &
            y >= 1.0_wp .and. y <= real(plot_height, wp)) then
            ! For legend text, only clamp starting position, let text extend as needed
            text_x = max(1, min(text_x, plot_width))
        else
            ! For other text, prevent overflow
            text_x = max(1, min(text_x, plot_width - processed_len))
        end if
        text_y = max(1, min(text_y, plot_height))
        
        text = processed_text(1:processed_len)
        
        ! Note: Color values are passed but not used for storage here
        ! They should be stored by the calling routine if needed
        associate(unused_sum => current_r + current_g + current_b); end associate
    end subroutine ascii_draw_text_primitive

end module fortplot_ascii_primitives
