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
    use fortplot_ascii_axis_policy, only: put_cell, glyph_layer, LAYER_EMPTY, &
                                          LAYER_GRID, LAYER_DATA, LAYER_AXIS, &
                                          LAYER_TICK
    use fortplot_colormap, only: colormap_value_to_color
    use fortplot_text_color, only: pack_rgb
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: draw_ascii_marker, fill_ascii_heatmap, draw_ascii_arrow
    public :: draw_ascii_vector_arrow
    public :: draw_line_on_canvas, draw_ascii_stream_segment
    public :: draw_text_axis_frame, draw_text_axis_tick, draw_text_grid_lines
    public :: fill_ascii_contour, ascii_contour_glyph
    public :: text_charset_t, unicode_glyphs
    public :: normalize_text_charset, resolve_text_charset_from_environment
    public :: charset_is_unicode, text_frame_line, translate_text_cell

    !! Glyph table selecting the concrete characters the text backend paints for
    !! frames, axes, ticks, markers, and arrows. The Unicode table swaps in
    !! box-drawing and symbol glyphs; the ASCII default keeps the historical
    !! literal '+', '-', '|' output and does not need a table (issue #2060).
    type :: text_charset_t
        character(len=:), allocatable :: hline, vline
        character(len=:), allocatable :: corner_tl, corner_tr
        character(len=:), allocatable :: corner_bl, corner_br
        character(len=:), allocatable :: marker_square, marker_diamond
        character(len=:), allocatable :: marker_star
        character(len=:), allocatable :: arrow_right, arrow_left
        character(len=:), allocatable :: arrow_up, arrow_ne, arrow_se
    end type text_charset_t

    !! UTF-8 byte sequences are built with achar() so the source stays pure
    !! ASCII, matching the existing convention in fortplot_unicode.
    character(len=*), parameter :: U_HLINE = achar(226)//achar(148)//achar(128)
    character(len=*), parameter :: U_VLINE = achar(226)//achar(148)//achar(130)
    character(len=*), parameter :: U_TL = achar(226)//achar(148)//achar(140)
    character(len=*), parameter :: U_TR = achar(226)//achar(148)//achar(144)
    character(len=*), parameter :: U_BL = achar(226)//achar(148)//achar(148)
    character(len=*), parameter :: U_BR = achar(226)//achar(148)//achar(152)
    character(len=*), parameter :: U_SQUARE = achar(226)//achar(150)//achar(160)
    character(len=*), parameter :: U_DIAMOND = achar(226)//achar(151)//achar(134)
    character(len=*), parameter :: U_STAR = achar(226)//achar(136)//achar(151)
    character(len=*), parameter :: U_RIGHT = achar(226)//achar(134)//achar(146)
    character(len=*), parameter :: U_LEFT = achar(226)//achar(134)//achar(144)
    character(len=*), parameter :: U_UP = achar(226)//achar(134)//achar(145)
    character(len=*), parameter :: U_NE = achar(226)//achar(134)//achar(151)
    character(len=*), parameter :: U_SE = achar(226)//achar(134)//achar(152)

    !! Shortest projected shaft (in text cells) that still earns a quiver glyph.
    !! Vectors shorter than this are dropped so that lowering the user scale
    !! visibly thins the field instead of leaving a constant glyph per point.
    real(wp), parameter :: MIN_QUIVER_SHAFT_CELLS = 0.5_wp

    !! Ordered low-to-high density glyph ramp for filled-contour bands. Every
    !! glyph classifies as LAYER_DATA (fortplot_ascii_axis_policy), so band fills
    !! never masquerade as an axis spine, tick, or label cell (issue #2077).
    character(len=*), parameter :: ASCII_CONTOUR_RAMP = '.:=o*#%@'

    character(len=1), parameter :: STREAM_LINE_GLYPH = '.'
    integer, parameter :: STREAM_MIN_GAP = 3

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
                                  x_min, x_max, y_min, y_max, plot_area, plot_width, &
                                  plot_height, canvas_color, colormap_name)
        !! Fill ASCII canvas with heatmap representation of 2D data
        character(len=1), intent(inout) :: canvas(:,:)
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height
        integer, intent(inout), optional :: canvas_color(:,:)
        character(len=*), intent(in), optional :: colormap_name

        integer :: nx, ny, i, j, px, py
        real(wp) :: z_normalized
        integer :: char_idx
        logical :: use_plot_area
        character(len=32) :: cmap

        nx = size(x_grid)
        ny = size(y_grid)
        cmap = 'viridis'
        if (present(colormap_name)) cmap = trim(colormap_name)

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
                            call set_heatmap_color(py, px, z_grid(j, i), z_min, z_max, &
                                                   trim(cmap), canvas_color)
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
                        call set_heatmap_color(py, px, z_grid(j, i), z_min, z_max, &
                                               trim(cmap), canvas_color)
                    end if
                end if
            end do
        end do
    end subroutine fill_ascii_heatmap

    subroutine set_heatmap_color(py, px, z_value, z_min, z_max, colormap_name, canvas_color)
        integer, intent(in) :: py, px
        real(wp), intent(in) :: z_value, z_min, z_max
        character(len=*), intent(in) :: colormap_name
        integer, intent(inout), optional :: canvas_color(:,:)
        real(wp) :: rgb(3)

        if (.not. present(canvas_color)) return
        call colormap_value_to_color(z_value, z_min, z_max, colormap_name, rgb)
        canvas_color(py, px) = pack_rgb(rgb(1), rgb(2), rgb(3))
    end subroutine set_heatmap_color

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

        ! Place the direction marker through the layer policy so it never
        ! overwrites axis spines, tick marks, or tick/axis labels (issue #2070).
        call put_cell(canvas, py, px, arrow_char, LAYER_DATA)

        ! Mark that arrows have been rendered
        has_rendered_arrows = .true.
        uses_vector_arrows = .false.
        has_triangular_arrows = .false.
    end subroutine draw_ascii_arrow

    subroutine draw_ascii_vector_arrow(canvas, x, y, u, v, x_min, x_max, &
                                       y_min, y_max, plot_area, plot_width, plot_height)
        !! Project a quiver vector to a text cell, clip it to the interior plot
        !! area, and stamp an eight-direction ASCII glyph at data-layer priority
        !! so it never overwrites axis spines, ticks, or label text (issue #2071).
        !! ``u``/``v`` are the already scaled vector components in data units, so
        !! the caller's scale factor still governs which arrows survive the
        !! minimum-shaft cut. The interior clip drops vectors that would land on
        !! the frame or in the tick/axis label margin.
        character(len=1), intent(inout) :: canvas(:, :)
        real(wp), intent(in) :: x, y, u, v
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height

        integer :: px, py, left, right, top, bottom
        integer :: span_x, span_y
        real(wp) :: cell_dx, cell_dy, shaft_cells
        logical :: use_plot_area

        if (x_max <= x_min .or. y_max <= y_min) return

        use_plot_area = plot_area%width > 0 .and. plot_area%height > 0
        if (use_plot_area) then
            span_x = max(1, plot_area%width - 2)
            span_y = max(1, plot_area%height - 2)
            call map_to_plot_area(x, y, x_min, x_max, y_min, y_max, plot_area, px, py)
            left = plot_area%left + 1
            right = plot_area%left + plot_area%width - 1
            top = plot_area%bottom + 1
            bottom = plot_area%bottom + plot_area%height - 1
        else
            span_x = max(1, plot_width - 3)
            span_y = max(1, plot_height - 3)
            px = int((x - x_min)/(x_max - x_min)*real(span_x, wp)) + 2
            py = (plot_height - 1) - int((y - y_min)/(y_max - y_min)*real(span_y, wp))
            left = 2
            right = plot_width - 1
            top = 2
            bottom = plot_height - 1
        end if

        cell_dx = u/(x_max - x_min)*real(span_x, wp)
        cell_dy = v/(y_max - y_min)*real(span_y, wp)
        shaft_cells = sqrt(cell_dx*cell_dx + cell_dy*cell_dy)
        if (shaft_cells < MIN_QUIVER_SHAFT_CELLS) return

        if (px <= left .or. px >= right) return
        if (py <= top .or. py >= bottom) return

        call put_cell(canvas, py, px, quiver_direction_glyph(u, v), LAYER_DATA)
    end subroutine draw_ascii_vector_arrow

    pure character(len=1) function quiver_direction_glyph(dx, dy) result(glyph)
        !! Map a vector direction to one of eight ASCII compass glyphs. dy is
        !! de-squashed by ASCII_CHAR_ASPECT so the chosen glyph matches the
        !! visible flow direction on the aspect-compressed canvas (#1965).
        real(wp), intent(in) :: dx, dy
        real(wp) :: angle

        angle = atan2(dy/ASCII_CHAR_ASPECT, dx)
        if (abs(angle) < 0.393_wp) then
            glyph = '>'
        else if (angle >= 0.393_wp .and. angle < 1.178_wp) then
            glyph = '/'
        else if (angle >= 1.178_wp .and. angle < 1.963_wp) then
            glyph = '^'
        else if (angle >= 1.963_wp .and. angle < 2.749_wp) then
            glyph = '\'
        else if (abs(angle) >= 2.749_wp) then
            glyph = '<'
        else if (angle <= -0.393_wp .and. angle > -1.178_wp) then
            glyph = '\'
        else if (angle <= -1.178_wp .and. angle > -1.963_wp) then
            glyph = 'v'
        else
            glyph = '/'
        end if
    end function quiver_direction_glyph

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

    subroutine draw_ascii_stream_segment(canvas, x1, y1, x2, y2, &
                                         x_min, x_max, y_min, y_max, plot_area, &
                                         plot_width, plot_height)
        !! Rasterize a streamplot trajectory segment thinned to terminal-cell
        !! resolution (issue #2070). Flow cells are placed through the layer
        !! policy so they never overwrite axes, ticks, or labels, and a minimum
        !! horizontal gap keeps any single row from being flooded with flow
        !! glyphs. The result is a sparse dotted flow field instead of a dense
        !! run of hyphen fragments.
        character(len=1), intent(inout) :: canvas(:, :)
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height

        real(wp) :: dx, dy, length, step_x, step_y, x, y
        integer :: steps, i, px, py, col_lo, col_hi, row_lo, row_hi
        logical :: use_plot_area

        use_plot_area = plot_area%width > 0 .and. plot_area%height > 0
        if (use_plot_area) then
            col_lo = plot_area%left + 1
            col_hi = plot_area%left + plot_area%width - 1
            row_lo = plot_area%bottom + 1
            row_hi = plot_area%bottom + plot_area%height - 1
        else
            col_lo = 2
            col_hi = plot_width - 1
            row_lo = 2
            row_hi = plot_height - 1
        end if

        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)
        if (length < 1.0e-6_wp) return

        steps = max(int(length*4), max(abs(int(dx)), abs(int(dy)))) + 1
        step_x = dx/real(steps, wp)
        step_y = dy/real(steps, wp)

        x = x1
        y = y1
        do i = 0, steps
            if (use_plot_area) then
                call map_to_plot_area(x, y, x_min, x_max, y_min, y_max, &
                                      plot_area, px, py)
            else
                px = int((x - x_min)/(x_max - x_min)*real(plot_width - 3, wp)) + 2
                py = (plot_height - 1) - &
                     int((y - y_min)/(y_max - y_min)*real(plot_height - 3, wp))
            end if
            call place_stream_cell(canvas, px, py, col_lo, col_hi, row_lo, row_hi)
            x = x + step_x
            y = y + step_y
        end do
    end subroutine draw_ascii_stream_segment

    subroutine place_stream_cell(canvas, px, py, col_lo, col_hi, row_lo, row_hi)
        !! Place one thinned flow glyph if the cell is empty and no other flow
        !! glyph sits within STREAM_MIN_GAP cells to its left.
        character(len=1), intent(inout) :: canvas(:, :)
        integer, intent(in) :: px, py, col_lo, col_hi, row_lo, row_hi
        integer :: g

        if (px < col_lo .or. px > col_hi) return
        if (py < row_lo .or. py > row_hi) return
        if (glyph_layer(canvas(py, px)) /= LAYER_EMPTY) return

        do g = 1, STREAM_MIN_GAP
            if (px - g >= 1) then
                if (canvas(py, px - g) == STREAM_LINE_GLYPH) return
            end if
            if (px + g <= size(canvas, 2)) then
                if (canvas(py, px + g) == STREAM_LINE_GLYPH) return
            end if
        end do

        call put_cell(canvas, py, px, STREAM_LINE_GLYPH, LAYER_DATA)
    end subroutine place_stream_cell

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

    subroutine fill_ascii_contour(canvas, x_grid, y_grid, z_grid, levels, &
                                  x_min, x_max, y_min, y_max, plot_width, &
                                  plot_height)
        !! Paint filled-contour bands into the ASCII canvas with an ordered glyph
        !! ramp (issue #2077). Each interior character cell is mapped back to a
        !! data coordinate (same screen mapping as ascii_fill_quad_primitive), its
        !! scalar value is looked up on the grid, and the band index selects a
        !! ramp glyph. Cells are written through put_cell at LAYER_DATA so axis
        !! spines, ticks, and labels stay reserved.
        character(len=1), intent(inout) :: canvas(:, :)
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: levels(:)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height

        integer :: nx, ny, nlev, n_bands
        integer :: px, py, band, gi, gj
        real(wp) :: fx, fy, xd, yd, zv
        character(len=1) :: glyph

        nx = size(x_grid)
        ny = size(y_grid)
        nlev = size(levels)
        if (nx < 1 .or. ny < 1 .or. nlev < 1) return
        if (size(z_grid, 1) /= ny .or. size(z_grid, 2) /= nx) return
        if (plot_width <= 3 .or. plot_height <= 3) return
        if (x_max <= x_min .or. y_max <= y_min) return

        n_bands = max(1, nlev - 1)

        do py = 2, plot_height - 1
            fy = real(plot_height - 1 - py, wp)/real(plot_height - 3, wp)
            yd = y_min + fy*(y_max - y_min)
            gj = nearest_grid_index(yd, y_grid)
            do px = 2, plot_width - 1
                fx = real(px - 2, wp)/real(plot_width - 3, wp)
                xd = x_min + fx*(x_max - x_min)
                gi = nearest_grid_index(xd, x_grid)
                zv = z_grid(gj, gi)
                band = compute_level_index(zv, levels)
                glyph = ascii_contour_glyph(band, n_bands)
                call put_cell(canvas, py, px, glyph, LAYER_DATA)
            end do
        end do
    end subroutine fill_ascii_contour

    pure integer function compute_level_index(z_value, levels) result(band)
        !! Band containing z_value for ascending levels: [levels(k), levels(k+1))
        !! maps to band k. Values below the first or above the last level clamp
        !! to the nearest interior band so the field fills without gaps.
        real(wp), intent(in) :: z_value
        real(wp), intent(in) :: levels(:)
        integer :: k, nlev

        nlev = size(levels)
        band = 1
        if (nlev < 2) return
        do k = 1, nlev - 1
            if (z_value >= levels(k)) band = k
        end do
    end function compute_level_index

    pure character(len=1) function ascii_contour_glyph(band, n_bands) result(glyph)
        !! Map a 1-based band index onto the ordered density ramp.
        integer, intent(in) :: band, n_bands
        integer :: ramp_len, idx, nb, b

        ramp_len = len(ASCII_CONTOUR_RAMP)
        nb = max(1, n_bands)
        b = max(1, min(band, nb))
        if (nb <= 1) then
            idx = 1
        else
            idx = nint(real(b - 1, wp)/real(nb - 1, wp)*real(ramp_len - 1, wp)) + 1
        end if
        idx = max(1, min(ramp_len, idx))
        glyph = ASCII_CONTOUR_RAMP(idx:idx)
    end function ascii_contour_glyph

    pure integer function nearest_grid_index(value, grid) result(best)
        !! Index of the grid node closest to value (grid is monotonic).
        real(wp), intent(in) :: value, grid(:)
        integer :: i, n
        real(wp) :: d, best_d

        n = size(grid)
        best = 1
        best_d = abs(value - grid(1))
        do i = 2, n
            d = abs(value - grid(i))
            if (d < best_d) then
                best_d = d
                best = i
            end if
        end do
    end function nearest_grid_index

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

    function unicode_glyphs() result(glyphs)
        !! Unicode charset table: box-drawing frame glyphs plus Unicode markers
        !! and arrows. Only bytes that never occur in numeric tick labels or
        !! letters are remapped, so labels and annotations stay intact.
        type(text_charset_t) :: glyphs

        glyphs%hline = U_HLINE
        glyphs%vline = U_VLINE
        glyphs%corner_tl = U_TL
        glyphs%corner_tr = U_TR
        glyphs%corner_bl = U_BL
        glyphs%corner_br = U_BR
        glyphs%marker_square = U_SQUARE
        glyphs%marker_diamond = U_DIAMOND
        glyphs%marker_star = U_STAR
        glyphs%arrow_right = U_RIGHT
        glyphs%arrow_left = U_LEFT
        glyphs%arrow_up = U_UP
        glyphs%arrow_ne = U_NE
        glyphs%arrow_se = U_SE
    end function unicode_glyphs

    function normalize_text_charset(name) result(mode)
        !! Canonicalize a user charset selection to 'ascii' or 'unicode'.
        !! 'auto' resolves through the environment; anything unrecognized falls
        !! back to the ASCII compatibility charset.
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: mode
        character(len=:), allocatable :: lowered
        integer :: i, c

        lowered = trim(adjustl(name))
        do i = 1, len(lowered)
            c = iachar(lowered(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) then
                lowered(i:i) = achar(c + 32)
            end if
        end do

        select case (lowered)
        case ('unicode')
            mode = 'unicode'
        case ('auto')
            mode = resolve_text_charset_from_environment()
        case default
            mode = 'ascii'
        end select
    end function normalize_text_charset

    function resolve_text_charset_from_environment() result(mode)
        !! Resolve 'auto' deterministically from FORTPLOT_TEXT_CHARSET. Absent or
        !! unset, the file default stays ASCII so saved .txt bytes are stable and
        !! never depend on the host locale (issue #2060 non-goal).
        character(len=:), allocatable :: mode
        character(len=64) :: value
        integer :: length, stat

        mode = 'ascii'
        call get_environment_variable('FORTPLOT_TEXT_CHARSET', value, length, stat)
        if (stat /= 0) return
        if (length <= 0) return
        mode = normalize_text_charset(value(1:length))
        if (mode == 'auto') mode = 'ascii'
    end function resolve_text_charset_from_environment

    pure logical function charset_is_unicode(mode) result(is_unicode)
        character(len=*), intent(in) :: mode
        is_unicode = (trim(mode) == 'unicode')
    end function charset_is_unicode

    function text_frame_line(width, glyphs, is_top) result(line)
        !! Build the top or bottom frame border for the given charset. In ASCII
        !! mode this reproduces the historical '+' // '-'*width // '+' border.
        integer, intent(in) :: width
        type(text_charset_t), intent(in) :: glyphs
        logical, intent(in) :: is_top
        character(len=:), allocatable :: line

        if (is_top) then
            line = glyphs%corner_tl//repeat(glyphs%hline, width)//glyphs%corner_tr
        else
            line = glyphs%corner_bl//repeat(glyphs%hline, width)//glyphs%corner_br
        end if
    end function text_frame_line

    function translate_text_cell(ch, glyphs) result(token)
        !! Map a single canvas byte to its charset glyph. Only structural,
        !! marker, and arrow symbols are remapped; digits, letters, '.', '-',
        !! and '+' pass through so numeric tick labels and text annotations
        !! survive Unicode output unchanged.
        character(len=1), intent(in) :: ch
        type(text_charset_t), intent(in) :: glyphs
        character(len=:), allocatable :: token

        select case (ch)
        case ('|')
            token = glyphs%vline
        case ('#')
            token = glyphs%marker_square
        case ('%')
            token = glyphs%marker_diamond
        case ('*')
            token = glyphs%marker_star
        case ('>')
            token = glyphs%arrow_right
        case ('<')
            token = glyphs%arrow_left
        case ('^')
            token = glyphs%arrow_up
        case ('/')
            token = glyphs%arrow_ne
        case ('\')
            token = glyphs%arrow_se
        case default
            token = ch
        end select
    end function translate_text_cell

end module fortplot_ascii_drawing
