module fortplot_pdf_drawing
    !! PDF-specific drawing utility functions
    !!
    !! This module provides PDF vector graphics drawing primitives
    !! including markers, shapes, and specialized vector operations.
    !!
    !! Author: fortplot contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    use fortplot_vector, only: vector_stream_writer, vector_graphics_state
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS
    use fortplot_logging, only: log_debug
    implicit none
    
    private
    public :: draw_pdf_circle_with_outline, draw_pdf_square_with_outline
    public :: draw_pdf_diamond_with_outline, draw_pdf_x_marker, draw_pdf_arrow
    public :: pdf_stream_writer
    
    type, extends(vector_stream_writer) :: pdf_stream_writer
    contains
        procedure :: write_command => pdf_write_command
        procedure :: write_move => pdf_write_move
        procedure :: write_line => pdf_write_line
        procedure :: write_stroke => pdf_write_stroke
        procedure :: write_color => pdf_write_color
        procedure :: write_line_width => pdf_write_line_width
        procedure :: save_state => pdf_save_state
        procedure :: restore_state => pdf_restore_state
    end type pdf_stream_writer

contains

    subroutine pdf_write_command(this, command)
        !! Write PDF graphics command to stream
        class(pdf_stream_writer), intent(inout) :: this
        character(len=*), intent(in) :: command
        call this%add_to_stream(command)
    end subroutine pdf_write_command

    subroutine pdf_write_move(this, x, y)
        !! Write PDF move command with robust validation
        !! Validates coordinates and handles NaN, infinity gracefully
        !! Logs debug information when corrections are applied
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: x, y
        real(wp) :: x_safe, y_safe
        character(len=64) :: cmd
        character(len=256) :: debug_msg
        logical :: x_corrected, y_corrected
        
        x_corrected = .false.
        y_corrected = .false.
        
        ! Validate and correct X coordinate
        if (ieee_is_nan(x) .or. .not. ieee_is_finite(x)) then
            x_safe = 0.0_wp  ! Default to origin for invalid values
            x_corrected = .true.
            call log_debug("Coordinate correction: X=invalid -> 0.000")
        else
            x_safe = x
        end if
        
        ! Validate and correct Y coordinate
        if (ieee_is_nan(y) .or. .not. ieee_is_finite(y)) then
            y_safe = 0.0_wp  ! Default to origin for invalid values
            y_corrected = .true.
            call log_debug("Coordinate correction: Y=invalid -> 0.000")
        else
            y_safe = y
        end if
        
        ! Log summary if any corrections were made
        if (x_corrected .or. y_corrected) then
            write(debug_msg, '("Final coordinates: (", F0.3, ", ", F0.3, ")")') &
                x_safe, y_safe
            call log_debug(trim(debug_msg))
        end if
        
        ! Write validated coordinates
        write(cmd, '(F0.3,1X,F0.3," m")') x_safe, y_safe
        call this%add_to_stream(trim(cmd))
    end subroutine pdf_write_move

    subroutine pdf_write_line(this, x, y)
        !! Write PDF line command with robust validation
        !! Validates coordinates and handles NaN, infinity gracefully
        !! Logs debug information when corrections are applied
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: x, y
        real(wp) :: x_safe, y_safe
        character(len=64) :: cmd
        character(len=256) :: debug_msg
        logical :: x_corrected, y_corrected
        
        x_corrected = .false.
        y_corrected = .false.
        
        ! Validate and correct X coordinate
        if (ieee_is_nan(x) .or. .not. ieee_is_finite(x)) then
            x_safe = 0.0_wp  ! Default to origin for invalid values
            x_corrected = .true.
            call log_debug("Coordinate correction: X=invalid -> 0.000")
        else
            x_safe = x
        end if
        
        ! Validate and correct Y coordinate
        if (ieee_is_nan(y) .or. .not. ieee_is_finite(y)) then
            y_safe = 0.0_wp  ! Default to origin for invalid values
            y_corrected = .true.
            call log_debug("Coordinate correction: Y=invalid -> 0.000")
        else
            y_safe = y
        end if
        
        ! Log summary if any corrections were made
        if (x_corrected .or. y_corrected) then
            write(debug_msg, '("Final coordinates: (", F0.3, ", ", F0.3, ")")') &
                x_safe, y_safe
            call log_debug(trim(debug_msg))
        end if
        
        ! Write validated coordinates
        write(cmd, '(F0.3,1X,F0.3," l")') x_safe, y_safe
        call this%add_to_stream(trim(cmd))
    end subroutine pdf_write_line

    subroutine pdf_write_stroke(this)
        !! Write PDF stroke command
        class(pdf_stream_writer), intent(inout) :: this
        call this%add_to_stream("S")
    end subroutine pdf_write_stroke

    subroutine pdf_write_color(this, r, g, b)
        !! Write PDF color command with robust validation
        !! Validates and clamps RGB values to [0.0, 1.0] range
        !! Handles NaN, infinity, and out-of-range values gracefully
        !! Logs debug information when corrections are applied
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        real(wp) :: r_safe, g_safe, b_safe
        character(len=64) :: cmd
        character(len=256) :: debug_msg
        logical :: r_corrected, g_corrected, b_corrected
        
        r_corrected = .false.
        g_corrected = .false.
        b_corrected = .false.
        
        ! Validate and clamp R component
        if (ieee_is_nan(r) .or. .not. ieee_is_finite(r)) then
            r_safe = 0.0_wp  ! Default to black for invalid values
            r_corrected = .true.
            call log_debug("RGB correction: R=invalid -> 0.000")
        else if (r < 0.0_wp .or. r > 1.0_wp) then
            r_safe = max(0.0_wp, min(1.0_wp, r))  ! Clamp to [0, 1]
            r_corrected = .true.
            if (abs(r) > 999.0_wp) then
                call log_debug("RGB correction: R=out-of-range (large) -> clamped")
            else
                write(debug_msg, '("RGB correction: R=", F0.3, " (out-of-range) -> ", F0.3)') r, r_safe
                call log_debug(trim(debug_msg))
            end if
        else
            r_safe = r
        end if
        
        ! Validate and clamp G component
        if (ieee_is_nan(g) .or. .not. ieee_is_finite(g)) then
            g_safe = 0.0_wp  ! Default to black for invalid values
            g_corrected = .true.
            call log_debug("RGB correction: G=invalid -> 0.000")
        else if (g < 0.0_wp .or. g > 1.0_wp) then
            g_safe = max(0.0_wp, min(1.0_wp, g))  ! Clamp to [0, 1]
            g_corrected = .true.
            if (abs(g) > 999.0_wp) then
                call log_debug("RGB correction: G=out-of-range (large) -> clamped")
            else
                write(debug_msg, '("RGB correction: G=", F0.3, " (out-of-range) -> ", F0.3)') g, g_safe
                call log_debug(trim(debug_msg))
            end if
        else
            g_safe = g
        end if
        
        ! Validate and clamp B component
        if (ieee_is_nan(b) .or. .not. ieee_is_finite(b)) then
            b_safe = 0.0_wp  ! Default to black for invalid values
            b_corrected = .true.
            call log_debug("RGB correction: B=invalid -> 0.000")
        else if (b < 0.0_wp .or. b > 1.0_wp) then
            b_safe = max(0.0_wp, min(1.0_wp, b))  ! Clamp to [0, 1]
            b_corrected = .true.
            if (abs(b) > 999.0_wp) then
                call log_debug("RGB correction: B=out-of-range (large) -> clamped")
            else
                write(debug_msg, '("RGB correction: B=", F0.3, " (out-of-range) -> ", F0.3)') b, b_safe
                call log_debug(trim(debug_msg))
            end if
        else
            b_safe = b
        end if
        
        ! Log summary if any corrections were made
        if (r_corrected .or. g_corrected .or. b_corrected) then
            write(debug_msg, '("Final RGB: (", F0.3, ", ", F0.3, ", ", F0.3, ")")') &
                r_safe, g_safe, b_safe
            call log_debug(trim(debug_msg))
        end if
        
        ! Write validated color values
        write(cmd, '(F0.3,1X,F0.3,1X,F0.3," RG")') r_safe, g_safe, b_safe
        call this%add_to_stream(trim(cmd))
    end subroutine pdf_write_color

    subroutine pdf_write_line_width(this, width)
        !! Write PDF line width command with robust validation
        !! Validates width > 0 and handles NaN, infinity gracefully
        !! Logs debug information when corrections are applied
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: width
        real(wp) :: width_safe
        character(len=32) :: cmd
        character(len=256) :: debug_msg
        logical :: width_corrected
        
        width_corrected = .false.
        
        ! Validate and correct width
        if (ieee_is_nan(width) .or. .not. ieee_is_finite(width)) then
            width_safe = 1.0_wp  ! Default to 1.0 for invalid values
            width_corrected = .true.
            call log_debug("Line width correction: width=invalid -> 1.000")
        else if (width <= 0.0_wp) then
            width_safe = 1.0_wp  ! Ensure positive width
            width_corrected = .true.
            if (abs(width) > 999.0_wp) then
                call log_debug("Line width correction: width=large negative -> 1.000")
            else
                write(debug_msg, '("Line width correction: width=", F0.3, " (non-positive) -> 1.000")') width
                call log_debug(trim(debug_msg))
            end if
        else
            width_safe = width
        end if
        
        ! Log final width if corrected
        if (width_corrected) then
            write(debug_msg, '("Final line width: ", F0.3)') width_safe
            call log_debug(trim(debug_msg))
        end if
        
        ! Write validated width
        write(cmd, '(F0.3," w")') width_safe
        call this%add_to_stream(trim(cmd))
    end subroutine pdf_write_line_width

    subroutine pdf_save_state(this)
        !! Write PDF save graphics state command
        class(pdf_stream_writer), intent(inout) :: this
        call this%add_to_stream("q")
    end subroutine pdf_save_state

    subroutine pdf_restore_state(this)
        !! Write PDF restore graphics state command
        class(pdf_stream_writer), intent(inout) :: this
        call this%add_to_stream("Q")
    end subroutine pdf_restore_state

    subroutine draw_pdf_circle_with_outline(this, cx, cy, radius)
        !! Draw filled circle with outline in PDF
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: cx, cy, radius
        
        character(len=256) :: cmd
        real(wp), parameter :: KAPPA = 0.552284749831_wp  ! 4/3 * (sqrt(2) - 1)
        real(wp) :: kappa_r
        
        kappa_r = KAPPA * radius
        
        ! Move to start point (right side of circle)
        call this%write_move(cx + radius, cy)
        
        ! Draw circle using Bezier curves (4 cubic curves)
        write(cmd, '(6(F0.3,1X),"c")') &
            cx + radius, cy + kappa_r, cx + kappa_r, cy + radius, cx, cy + radius
        call this%write_command(trim(cmd))
        
        write(cmd, '(6(F0.3,1X),"c")') &
            cx - kappa_r, cy + radius, cx - radius, cy + kappa_r, cx - radius, cy
        call this%write_command(trim(cmd))
        
        write(cmd, '(6(F0.3,1X),"c")') &
            cx - radius, cy - kappa_r, cx - kappa_r, cy - radius, cx, cy - radius
        call this%write_command(trim(cmd))
        
        write(cmd, '(6(F0.3,1X),"c")') &
            cx + kappa_r, cy - radius, cx + radius, cy - kappa_r, cx + radius, cy
        call this%write_command(trim(cmd))
        
        ! Close and fill with stroke
        call this%write_command("h")  ! Close path
        call this%write_command("B")  ! Fill and stroke
    end subroutine draw_pdf_circle_with_outline

    subroutine draw_pdf_square_with_outline(this, cx, cy, size)
        !! Draw filled square with outline in PDF
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: cx, cy, size
        
        real(wp) :: half_size, x1, y1, x2, y2
        
        half_size = size * 0.5_wp
        x1 = cx - half_size
        y1 = cy - half_size
        x2 = cx + half_size
        y2 = cy + half_size
        
        ! Draw rectangle
        call this%write_move(x1, y1)
        call this%write_line(x2, y1)
        call this%write_line(x2, y2)
        call this%write_line(x1, y2)
        call this%write_command("h")  ! Close path
        call this%write_command("B")  ! Fill and stroke
    end subroutine draw_pdf_square_with_outline

    subroutine draw_pdf_diamond_with_outline(this, cx, cy, size)
        !! Draw filled diamond with outline in PDF
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: cx, cy, size
        
        real(wp) :: half_size
        
        half_size = size * 0.5_wp
        
        ! Draw diamond
        call this%write_move(cx, cy - half_size)      ! Top
        call this%write_line(cx + half_size, cy)      ! Right
        call this%write_line(cx, cy + half_size)      ! Bottom
        call this%write_line(cx - half_size, cy)      ! Left
        call this%write_command("h")                  ! Close path
        call this%write_command("B")                  ! Fill and stroke
    end subroutine draw_pdf_diamond_with_outline

    subroutine draw_pdf_x_marker(this, cx, cy, size)
        !! Draw X-shaped marker in PDF
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: cx, cy, size
        
        real(wp) :: half_size
        
        half_size = size * 0.5_wp
        
        ! Draw first diagonal
        call this%write_move(cx - half_size, cy - half_size)
        call this%write_line(cx + half_size, cy + half_size)
        call this%write_stroke()
        
        ! Draw second diagonal
        call this%write_move(cx - half_size, cy + half_size)
        call this%write_line(cx + half_size, cy - half_size)
        call this%write_stroke()
    end subroutine draw_pdf_x_marker

    subroutine draw_pdf_arrow(this, x, y, dx, dy, size, style)
        !! Draw arrow marker in PDF
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        
        real(wp) :: arrow_length, arrow_angle, tip_x, tip_y
        real(wp) :: left_x, left_y, right_x, right_y
        real(wp), parameter :: ARROW_HEAD_ANGLE = 0.523599_wp  ! 30 degrees in radians
        
        arrow_length = size * 0.3_wp
        arrow_angle = atan2(dy, dx)
        
        ! Arrow tip
        tip_x = x + dx
        tip_y = y + dy
        
        ! Arrow wings
        left_x = tip_x - arrow_length * cos(arrow_angle - ARROW_HEAD_ANGLE)
        left_y = tip_y - arrow_length * sin(arrow_angle - ARROW_HEAD_ANGLE)
        right_x = tip_x - arrow_length * cos(arrow_angle + ARROW_HEAD_ANGLE)
        right_y = tip_y - arrow_length * sin(arrow_angle + ARROW_HEAD_ANGLE)
        
        ! Draw arrow shaft
        call this%write_move(x, y)
        call this%write_line(tip_x, tip_y)
        call this%write_stroke()
        
        ! Draw arrow head based on style
        if (style == 'filled' .or. style == 'open') then
            call this%write_move(tip_x, tip_y)
            call this%write_line(left_x, left_y)
            call this%write_line(right_x, right_y)
            call this%write_command("h")  ! Close path
            
            if (style == 'filled') then
                call this%write_command("B")  ! Fill and stroke
            else
                call this%write_stroke()     ! Just stroke
            end if
        end if
    end subroutine draw_pdf_arrow

end module fortplot_pdf_drawing