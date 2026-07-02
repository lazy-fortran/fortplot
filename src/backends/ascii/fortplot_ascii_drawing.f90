module fortplot_ascii_drawing
    !! ASCII terminal plotting backend - Basic Drawing Elements
    !!
    !! This module contains core drawing functions for ASCII plotting
    !! including markers, arrows, heatmaps, and line drawing.
    !!
    !! Author: fortplot contributors

    use fortplot_constants, only: EPSILON_COMPARE, ASCII_CHAR_ASPECT
    use fortplot_margins, only: plot_area_t
    use fortplot_ascii_utils, only: get_char_density, ASCII_CHARS
    use fortplot_ascii_utils, only: get_blend_char
    use fortplot_ascii_axis_policy, only: put_cell, LAYER_GRID, LAYER_AXIS, LAYER_TICK
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: draw_ascii_marker, fill_ascii_heatmap, draw_ascii_arrow
    public :: draw_line_on_canvas
    public :: draw_text_axis_frame, draw_text_axis_tick, draw_text_grid_lines

contains

    subroutine draw_ascii_marker(canvas, x, y, style, x_min, x_max, y_min, y_max, &
                                 plot_area, plot_width, plot_height)
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height
        integer :: px, py
        character(len=1) :: marker_char
        logical :: use_plot_area

        use_plot_area = plot_area%width > 0 .and. plot_area%height > 0
        if (use_plot_area) then
            call map_to_plot_area(x, y, x_min, x_max, y_min, y_max, plot_area, px, py)
        else
            px = int((x - x_min) / (x_max - x_min) * real(plot_width - 3, wp)) + 2
            py = (plot_height - 1) - int((y - y_min) / (y_max - y_min) * real(plot_height - 3, wp))
        end if

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

        if (use_plot_area) then
            if (px >= plot_area%left + 1 .and. px <= plot_area%left + plot_area%width - 1 .and. &
                py >= plot_area%bottom + 1 .and. py <= plot_area%bottom + plot_area%height - 1) then
                canvas(py, px) = marker_char
            end if
        else if (px >= 2 .and. px <= plot_width - 1 .and. py >= 2 .and. py <= plot_height - 1) then
            canvas(py, px) = marker_char
        end if
    end subroutine draw_ascii_marker

    subroutine fill_ascii_heatmap(canvas, x_grid, y_grid, z_grid, z_min, z_max, &
                                  x_min, x_max, y_min, y_max, plot_area, plot_width, plot_height)
        !! Fill ASCII canvas with heatmap representation of 2D data
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height

        integer :: nx, ny, i, j, px, py
        real(wp) :: z_normalized
        integer :: char_idx
        logical :: use_plot_area

        nx = size(x_grid)
        ny = size(y_grid)

        ! z_grid should have dimensions (ny, nx) - rows by columns
        if (size(z_grid, 1) /= ny .or. size(z_grid, 2) /= nx) return
        use_plot_area = plot_area%width > 0 .and. plot_area%height > 0

        ! Fill the canvas with density characters based on z values
        do i = 1, nx
            do j = 1, ny
                if (use_plot_area) then
                    call map_to_plot_area(x_grid(i), y_grid(j), x_min, x_max, y_min, y_max, &
                                          plot_area, px, py)
                else
                    px = int((x_grid(i) - x_min) / (x_max - x_min) * real(plot_width - 3, wp)) + 2
                    py = (plot_height - 1) - int((y_grid(j) - y_min) / (y_max - y_min) * real(plot_height - 3, wp))
                end if

                ! Check bounds
                if (use_plot_area) then
                    if (px >= plot_area%left + 1 .and. px <= plot_area%left + plot_area%width - 1 .and. &
                        py >= plot_area%bottom + 1 .and. py <= plot_area%bottom + plot_area%height - 1) then
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
                else if (px >= 2 .and. px <= plot_width - 1 .and. &
                         py >= 2 .and. py <= plot_height - 1) then
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
                                x_min, x_max, y_min, y_max, plot_area, width, height, &
                                has_rendered_arrows, uses_vector_arrows, has_triangular_arrows)
        !! Draw arrow using Unicode directional characters for ASCII backend
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: width, height
        logical, intent(out) :: has_rendered_arrows, uses_vector_arrows, has_triangular_arrows

        integer :: px, py
        character(len=1) :: arrow_char
        real(wp) :: angle
        logical :: use_plot_area

        ! Reference otherwise-unused parameters without unreachable branches
        associate(unused_s => size, unused_ls => len_trim(style)); end associate

        use_plot_area = plot_area%width > 0 .and. plot_area%height > 0
        if (use_plot_area) then
            call map_to_plot_area(x, y, x_min, x_max, y_min, y_max, plot_area, px, py)
            ! Ensure coordinates stay inside the frame border (1-char margin)
            if (px < plot_area%left + 1 .or. px > plot_area%left + plot_area%width - 1 .or. &
                py < plot_area%bottom + 1 .or. py > plot_area%bottom + plot_area%height - 1) return
        else
            px = int((x - x_min) / (x_max - x_min) * real(width, wp))
            py = int((y - y_min) / (y_max - y_min) * real(height, wp))
            if (px < 2 .or. px > width - 1 .or. py < 2 .or. py > height - 1) return
        end if

        ! Calculate angle for direction in screen space. The canvas compresses y
        ! by ASCII_CHAR_ASPECT relative to x (a cell is that many times taller
        ! than wide), so scale dy by 1/ASCII_CHAR_ASPECT before atan2 to pick the
        ! glyph that matches the visible flow direction (#1965).
        angle = atan2(dy / ASCII_CHAR_ASPECT, dx)

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

    subroutine draw_line_on_canvas(canvas, x1, y1, x2, y2, x_min, x_max, y_min, y_max, &
                                   plot_area, plot_width, plot_height, line_char)
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
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
            if (plot_area%width > 0 .and. plot_area%height > 0) then
                call map_to_plot_area(x, y, x_min, x_max, y_min, y_max, plot_area, px, py)
                if (px >= plot_area%left + 1 .and. px <= plot_area%left + plot_area%width - 1 .and. &
                    py >= plot_area%bottom + 1 .and. py <= plot_area%bottom + plot_area%height - 1) then
                    if (canvas(py, px) == ' ') then
                        canvas(py, px) = line_char
                    else if (canvas(py, px) /= line_char) then
                        canvas(py, px) = get_blend_char(canvas(py, px), line_char)
                    end if
                end if
            else
                px = int((x - x_min) / (x_max - x_min) * real(plot_width - 3, wp)) + 2
                py = (plot_height - 1) - int((y - y_min) / (y_max - y_min) * real(plot_height - 3, wp))
                if (px >= 2 .and. px <= plot_width - 1 .and. py >= 2 .and. py <= plot_height - 1) then
                    if (canvas(py, px) == ' ') then
                        canvas(py, px) = line_char
                    else if (canvas(py, px) /= line_char) then
                        canvas(py, px) = get_blend_char(canvas(py, px), line_char)
                    end if
                end if
            end if

            x = x + step_x
            y = y + step_y
        end do
    end subroutine draw_line_on_canvas

    subroutine draw_text_axis_frame(canvas, axis_col, bottom_row, top_row, right_col)
        !! Draw the solid left and bottom axis spines plus the corner tick.
        !! Spines are filled cell-by-cell in screen space so they stay
        !! continuous regardless of the data range (issue #2069).
        character(len=1), intent(inout) :: canvas(:, :)
        integer, intent(in) :: axis_col, bottom_row, top_row, right_col
        integer :: r, c

        do r = top_row, bottom_row
            call put_cell(canvas, r, axis_col, '|', LAYER_AXIS)
        end do
        do c = axis_col, right_col
            call put_cell(canvas, bottom_row, c, '-', LAYER_AXIS)
        end do
        call put_cell(canvas, bottom_row, axis_col, '+', LAYER_TICK)
    end subroutine draw_text_axis_frame

    subroutine draw_text_axis_tick(canvas, row, col)
        !! Draw a single tick mark on a spine at a labeled tick position.
        character(len=1), intent(inout) :: canvas(:, :)
        integer, intent(in) :: row, col

        call put_cell(canvas, row, col, '+', LAYER_TICK)
    end subroutine draw_text_axis_tick

    subroutine draw_text_grid_lines(canvas, x_cols, num_x, y_rows, num_y, &
                                    top_row, bottom_row, left_col, right_col)
        !! Fill interior grid glyphs aligned to major-tick columns and rows.
        !! Grid cells are drawn at LAYER_GRID, the lowest drawable layer, so
        !! put_cell leaves data, axis spines, tick marks, and labels intact and
        !! only paints otherwise-blank interior cells (issue #2074).
        character(len=1), intent(inout) :: canvas(:, :)
        integer, intent(in) :: x_cols(:), y_rows(:)
        integer, intent(in) :: num_x, num_y
        integer, intent(in) :: top_row, bottom_row, left_col, right_col
        integer :: i, r, c

        do i = 1, num_x
            do r = top_row, bottom_row
                call put_cell(canvas, r, x_cols(i), ':', LAYER_GRID)
            end do
        end do
        do i = 1, num_y
            do c = left_col, right_col
                call put_cell(canvas, y_rows(i), c, '.', LAYER_GRID)
            end do
        end do
    end subroutine draw_text_grid_lines

    subroutine map_to_plot_area(x, y, x_min, x_max, y_min, y_max, plot_area, px, py)
        real(wp), intent(in) :: x, y, x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(out) :: px, py
        integer :: inner_width, inner_height

        if (plot_area%width > 0 .and. plot_area%height > 0) then
            inner_width = max(1, plot_area%width - 2)
            inner_height = max(1, plot_area%height - 2)
            px = plot_area%left + 1 + nint((x - x_min)/(x_max - x_min)*real(inner_width, wp))
            py = plot_area%bottom + plot_area%height - 1 - &
                 nint((y - y_min)/(y_max - y_min)*real(inner_height, wp))
        else
            px = int((x - x_min) / (x_max - x_min) * real(1, wp)) + 2
            py = 1 - int((y - y_min) / (y_max - y_min) * real(1, wp))
        end if
    end subroutine map_to_plot_area

end module fortplot_ascii_drawing
