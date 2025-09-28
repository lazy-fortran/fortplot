module fortplot_ascii_drawing
    !! ASCII terminal plotting backend - Basic Drawing Elements
    !!
    !! This module contains core drawing functions for ASCII plotting
    !! including markers, arrows, heatmaps, and line drawing.
    !!
    !! Author: fortplot contributors

    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_ascii_utils, only: get_char_density, ASCII_CHARS
    use fortplot_ascii_utils, only: get_blend_char
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: draw_ascii_marker, fill_ascii_heatmap, draw_ascii_arrow
    public :: draw_line_on_canvas

contains

    subroutine draw_ascii_marker(canvas, x, y, style, x_min, x_max, y_min, y_max, plot_width, plot_height)
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        integer :: px, py
        character(len=1) :: marker_char

        ! Map to usable plot area (excluding 1-char border on each side)
        px = int((x - x_min) / (x_max - x_min) * real(plot_width - 3, wp)) + 2
        py = (plot_height - 1) - int((y - y_min) / (y_max - y_min) * real(plot_height - 3, wp))

        ! Map marker styles to distinct ASCII characters for visual differentiation
        select case (trim(style))
        case ('o')
            marker_char = 'o'  ! Circle
        case ('s')
            marker_char = '#'  ! Square
        case ('D', 'd')
            marker_char = '%'  ! Diamond (ASCII representation)
        case ('x')
            marker_char = 'x'  ! Cross
        case ('+')
            marker_char = '+'  ! Plus
        case ('*')
            marker_char = '*'  ! Star
        case ('^')
            marker_char = '^'  ! Triangle up
        case ('v')
            marker_char = 'v'  ! Triangle down
        case ('<')
            marker_char = '<'  ! Triangle left
        case ('>')
            marker_char = '>'  ! Triangle right
        case ('p')
            marker_char = 'P'  ! Pentagon
        case ('h', 'H')
            marker_char = 'H'  ! Hexagon
        case default
            marker_char = '*'  ! Default fallback
        end select

        if (px >= 2 .and. px <= plot_width - 1 .and. py >= 2 .and. py <= plot_height - 1) then
            canvas(py, px) = marker_char
        end if
    end subroutine draw_ascii_marker

    subroutine fill_ascii_heatmap(canvas, x_grid, y_grid, z_grid, z_min, z_max, &
                                  x_min, x_max, y_min, y_max, plot_width, plot_height)
        !! Fill ASCII canvas with heatmap representation of 2D data
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height

        integer :: nx, ny, i, j, px, py
        real(wp) :: z_normalized
        integer :: char_idx

        nx = size(x_grid)
        ny = size(y_grid)

        ! z_grid should have dimensions (ny, nx) - rows by columns
        if (size(z_grid, 1) /= ny .or. size(z_grid, 2) /= nx) return

        ! Fill the canvas with density characters based on z values
        do i = 1, nx
            do j = 1, ny
                ! Map grid coordinates to canvas coordinates
                px = int((x_grid(i) - x_min) / (x_max - x_min) * &
                        real(plot_width - 3, wp)) + 2
                py = (plot_height - 1) - int((y_grid(j) - y_min) / &
                        (y_max - y_min) * real(plot_height - 3, wp))

                ! Check bounds
                if (px >= 2 .and. px <= plot_width - 1 .and. py >= 2 .and. py <= plot_height - 1) then
                    ! Normalize z value to character index
                    ! z_grid is (ny, nx) so access as z_grid(j, i)
                    if (abs(z_max - z_min) > EPSILON_COMPARE) then
                        z_normalized = (z_grid(j, i) - z_min) / (z_max - z_min)
                    else
                        z_normalized = 0.5_wp
                    end if

                    ! Map to character index (1 to len(ASCII_CHARS))
                    char_idx = min(len(ASCII_CHARS), max(1, int(z_normalized * real(len(ASCII_CHARS) - 1, wp)) + 1))

                    ! Only overwrite if current position is empty or has lower density
                    if (canvas(py, px) == ' ' .or. char_idx > index(ASCII_CHARS, canvas(py, px))) then
                        canvas(py, px) = ASCII_CHARS(char_idx:char_idx)
                    end if
                end if
            end do
        end do
    end subroutine fill_ascii_heatmap

    subroutine draw_ascii_arrow(canvas, x, y, dx, dy, size, style, &
                                x_min, x_max, y_min, y_max, width, height, &
                                has_rendered_arrows, uses_vector_arrows, has_triangular_arrows)
        !! Draw arrow using Unicode directional characters for ASCII backend
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: width, height
        logical, intent(out) :: has_rendered_arrows, uses_vector_arrows, has_triangular_arrows

        integer :: px, py
        character(len=1) :: arrow_char
        real(wp) :: angle

        ! Reference otherwise-unused parameters without unreachable branches
        associate(unused_s => size, unused_ls => len_trim(style)); end associate

        ! Convert world coordinates to pixel coordinates
        px = int((x - x_min) / (x_max - x_min) * real(width, wp))
        py = int((y - y_min) / (y_max - y_min) * real(height, wp))

        ! Ensure coordinates are within bounds
        if (px < 1 .or. px > width .or. py < 1 .or. py > height) return

        ! Calculate angle for direction
        angle = atan2(dy, dx)

        ! Choose ASCII-compatible arrow character based on direction
        if (abs(angle) < 0.393_wp) then          ! 0 ± 22.5 degrees (right)
            arrow_char = '>'
        else if (angle >= 0.393_wp .and. angle < 1.178_wp) then  ! 22.5-67.5 degrees (up-right)
            arrow_char = '/'
        else if (angle >= 1.178_wp .and. angle < 1.963_wp) then  ! 67.5-112.5 degrees (up)
            arrow_char = '^'
        else if (angle >= 1.963_wp .and. angle < 2.749_wp) then  ! 112.5-157.5 degrees (up-left)
            arrow_char = '\'
        else if (abs(angle) >= 2.749_wp) then    ! 157.5-180 degrees (left)
            arrow_char = '<'
        else if (angle <= -0.393_wp .and. angle > -1.178_wp) then  ! -22.5 to -67.5 degrees (down-right)
            arrow_char = '\'
        else if (angle <= -1.178_wp .and. angle > -1.963_wp) then  ! -67.5 to -112.5 degrees (down)
            arrow_char = 'v'
        else  ! -112.5 to -157.5 degrees (down-left)
            arrow_char = '/'
        end if

        ! Place arrow character on canvas
        canvas(py, px) = arrow_char

        ! Mark that arrows have been rendered
        has_rendered_arrows = .true.
        uses_vector_arrows = .false.
        has_triangular_arrows = .false.
    end subroutine draw_ascii_arrow

    subroutine draw_line_on_canvas(canvas, x1, y1, x2, y2, x_min, x_max, y_min, y_max, plot_width, plot_height, line_char)
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        character(len=1), intent(in) :: line_char

        real(wp) :: dx, dy, length, step_x, step_y, x, y
        integer :: steps, i, px, py

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
    end subroutine draw_line_on_canvas

end module fortplot_ascii_drawing