module fortplot_legend_drawing
    !! Legend drawing and rendering procedures
    !!
    !! Single Responsibility: Legend box drawing, entry rendering, and positioning

    use fortplot_ascii_mathtext, only: sanitize_ascii_text
    use fortplot_context, only: plot_context
    use fortplot_legend_layout, only: legend_box_t, calculate_legend_box
    use fortplot_legend_state, only: legend_t, legend_entry_t
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_text, only: calculate_text_height, get_font_ascent_ratio
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: render_ascii_legend, render_standard_legend, &
              calculate_legend_position, backend_is_ascii

    integer, parameter :: ASCII_LEGEND_HANDLE_WIDTH = 3
    !! Fixed-width text handle so every legend row aligns its label at the
    !! same column regardless of line style or marker.
    integer, parameter :: ASCII_LEGEND_BOTTOM_RESERVE = 2
    !! Rows kept clear at the bottom (axis line plus tick labels) so a stacked
    !! legend never lands on the x-axis frame.
    integer, parameter :: ASCII_LEGEND_YAXIS_GUTTER = 3
    !! Columns kept clear at the left (y-axis tick labels) so a left-anchored
    !! legend handle is not clobbered by the tick digits sharing its row.

contains

    subroutine render_ascii_legend(legend, backend, legend_x, legend_y)
        !! Render compact ASCII legend with style-specific handles
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: legend_x, legend_y
        integer :: i
        real(wp) :: text_x, text_y
        character(len=:), allocatable :: legend_line
        integer :: available_width, block_width
        character(len=256) :: sanitized_label
        integer :: sanitized_len
        character(len=ASCII_LEGEND_HANDLE_WIDTH) :: handle

        available_width = max(1, backend%width - int(legend_x) + 1)
        block_width = ascii_legend_block_width(legend, available_width)

        do i = 1, legend%num_entries
            text_x = legend_x
            text_y = legend_y + real(i-1, wp)

            text_y = max(1.0_wp, min(text_y, real(max(2, backend%height - 2), wp)))

            call backend%clear_text_background(text_x - 1.0_wp, text_y, block_width + 2)

            call backend%color(legend%entries(i)%color(1), &
                              legend%entries(i)%color(2), &
                              legend%entries(i)%color(3))

            call sanitize_ascii_text(legend%entries(i)%label, sanitized_label, sanitized_len)

            handle = ascii_legend_handle(legend%entries(i))
            legend_line = handle // ' ' // trim(sanitized_label(1:sanitized_len))

            if (len(legend_line) > available_width) then
                legend_line = legend_line(1:available_width)
            end if

            call backend%draw_text_overlay(text_x, text_y, legend_line)
        end do
    end subroutine render_ascii_legend

    function ascii_legend_block_width(legend, available_width) result(block_width)
        !! Uniform blank-background width covering the widest handle+label row so
        !! the whole legend reads as one clear rectangle.
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: available_width
        integer :: block_width
        integer :: i, label_len, row_len
        character(len=256) :: sanitized_label

        block_width = 0
        do i = 1, legend%num_entries
            call sanitize_ascii_text(legend%entries(i)%label, sanitized_label, label_len)
            row_len = ASCII_LEGEND_HANDLE_WIDTH + 1 + label_len
            block_width = max(block_width, row_len)
        end do
        block_width = min(block_width, available_width)
    end function ascii_legend_block_width

    pure function ascii_legend_handle(entry) result(handle)
        !! Build a fixed-width ASCII handle that reflects the entry's line style
        !! and marker so legend rows read distinctly:
        !!   solid ``---``  dashed ``- -``  dotted ``...``  dash-dot ``-.-``
        !!   marker-only `` o ``  invisible line ``   `` (blank).
        type(legend_entry_t), intent(in) :: entry
        character(len=ASCII_LEGEND_HANDLE_WIDTH) :: handle
        logical :: has_line, has_marker
        character(len=1) :: marker_char

        handle = repeat(' ', ASCII_LEGEND_HANDLE_WIDTH)

        has_line = .false.
        if (allocated(entry%linestyle)) then
            if (len_trim(entry%linestyle) > 0 .and. &
                trim(entry%linestyle) /= 'None' .and. &
                trim(entry%linestyle) /= 'none') then
                has_line = .true.
            end if
        end if

        has_marker = .false.
        if (allocated(entry%marker)) then
            if (len_trim(entry%marker) > 0 .and. &
                trim(entry%marker) /= 'None' .and. &
                trim(entry%marker) /= 'none') then
                has_marker = .true.
            end if
        end if

        if (has_line) then
            select case (trim(entry%linestyle))
            case ('--')
                handle = '- -'
            case (':')
                handle = '...'
            case ('-.')
                handle = '-.-'
            case default
                handle = '---'
            end select
        end if

        if (has_marker) then
            marker_char = get_ascii_marker_char(entry%marker)
            if (has_line) then
                handle(2:2) = marker_char
            else
                handle = ' '//marker_char//' '
            end if
        end if
    end function ascii_legend_handle

    subroutine render_standard_legend(legend, backend, legend_x, legend_y)
        !! Render standard legend for PNG/PDF backends with improved sizing
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: legend_x, legend_y

        type(legend_box_t) :: box
        character(len=:), allocatable :: labels(:)
        real(wp) :: data_width, data_height

        call initialize_legend_rendering(legend, backend, box, labels, data_width, data_height)
        call draw_legend_frame(backend, legend_x, legend_y, box)
        call render_legend_entries(legend, backend, legend_x, legend_y, box)
    end subroutine render_standard_legend

    subroutine initialize_legend_rendering(legend, backend, box, labels, data_width, data_height)
        !! Initialize legend rendering components
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(in) :: backend
        type(legend_box_t), intent(out) :: box
        character(len=:), allocatable, intent(out) :: labels(:)
        real(wp), intent(out) :: data_width, data_height

        integer :: i
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        integer :: px_w, px_h

        allocate(character(len=256) :: labels(legend%num_entries))
        do i = 1, legend%num_entries
            labels(i) = legend%entries(i)%label
        end do

        data_width = backend%x_max - backend%x_min
        data_height = backend%y_max - backend%y_min

        call calculate_plot_area(backend%width, backend%height, margins, plot_area)
        px_w = max(1, plot_area%width)
        px_h = max(1, plot_area%height)

        box = calculate_legend_box(labels, data_width, data_height, &
                                  legend%num_entries, legend%position, px_w, px_h)
    end subroutine initialize_legend_rendering

    subroutine draw_legend_frame(backend, legend_x, legend_y, box)
        !! Draw legend background box and border
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: legend_x, legend_y
        type(legend_box_t), intent(in) :: box

        real(wp) :: box_x1, box_y1, box_x2, box_y2

        box_x1 = legend_x
        box_y1 = legend_y
        box_x2 = box_x1 + box%width
        box_y2 = box_y1 - box%height

        call backend%color(1.0_wp, 1.0_wp, 1.0_wp)
        call draw_legend_box(backend, box_x1, box_y1, box_x2, box_y2)
        call backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        call draw_legend_border(backend, box_x1, box_y1, box_x2, box_y2)
    end subroutine draw_legend_frame

    subroutine render_legend_entries(legend, backend, legend_x, legend_y, box)
        !! Render all legend entries (lines, markers, text)
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: legend_x, legend_y
        type(legend_box_t), intent(in) :: box

        real(wp) :: ascent_ratio, line_x1, line_x2, line_center_y, text_x, text_baseline
        integer :: i

        ascent_ratio = get_font_ascent_ratio()

        do i = 1, legend%num_entries
            call calculate_entry_positions(legend_x, legend_y, box, ascent_ratio, i, &
                                         line_x1, line_x2, line_center_y, text_x, &
                                         text_baseline)

            if (legend%entries(i)%is_patch) then
                call render_legend_patch(legend%entries(i), backend, line_x1, &
                                         line_x2, line_center_y, &
                                         0.5_wp*box%entry_height)
            else
                call render_legend_line(legend%entries(i), backend, line_x1, &
                                        line_x2, line_center_y)
                call render_legend_marker(legend%entries(i), backend, line_x1, &
                                          line_x2, line_center_y)
            end if
            call render_legend_text(legend%entries(i), backend, text_x, text_baseline)
        end do
    end subroutine render_legend_entries

    subroutine calculate_entry_positions(legend_x, legend_y, box, ascent_ratio, entry_idx, &
                                         line_x1, line_x2, line_center_y, text_x, &
                                         text_baseline)
        !! Calculate positions for legend entry components
        real(wp), intent(in) :: legend_x, legend_y, ascent_ratio
        type(legend_box_t), intent(in) :: box
        integer, intent(in) :: entry_idx
        real(wp), intent(out) :: line_x1, line_x2, line_center_y, text_x, text_baseline
        real(wp) :: entry_stride, entry_top_y, entry_baseline, entry_offset

        line_x1 = legend_x + box%padding_x
        line_x2 = line_x1 + box%line_length

        entry_stride = box%entry_height + box%entry_spacing
        entry_top_y = legend_y - box%padding - real(entry_idx - 1, wp) * entry_stride

        entry_baseline = entry_top_y - ascent_ratio * box%entry_height
        entry_offset = (ascent_ratio - 0.5_wp) * box%entry_height

        line_center_y = entry_baseline + entry_offset

        text_x = line_x2 + box%text_spacing
        text_baseline = entry_baseline
    end subroutine calculate_entry_positions

    subroutine render_legend_line(entry, backend, line_x1, line_x2, line_center_y)
        !! Render legend line for entry
        type(legend_entry_t), intent(in) :: entry
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: line_x1, line_x2, line_center_y

        call backend%color(entry%color(1), entry%color(2), entry%color(3))

        if (allocated(entry%linestyle)) then
            if (trim(entry%linestyle) /= 'None' .and. trim(entry%linestyle) /= 'none') then
                call backend%set_line_style(entry%linestyle)
                call backend%line(line_x1, line_center_y, line_x2, line_center_y)
            end if
        else
            call backend%set_line_style('-')
            call backend%line(line_x1, line_center_y, line_x2, line_center_y)
        end if
    end subroutine render_legend_line

    subroutine render_legend_patch(entry, backend, line_x1, line_x2, line_center_y, &
                                   half_height)
        !! Render a filled rectangle swatch for patch entries (bars), matching
        !! matplotlib's bar legend handle.
        type(legend_entry_t), intent(in) :: entry
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: line_x1, line_x2, line_center_y, half_height
        real(wp) :: x_quad(4), y_quad(4)

        call backend%color(entry%color(1), entry%color(2), entry%color(3))
        x_quad = [line_x1, line_x2, line_x2, line_x1]
        y_quad = [line_center_y - half_height, line_center_y - half_height, &
                  line_center_y + half_height, line_center_y + half_height]
        call backend%fill_quad(x_quad, y_quad)
    end subroutine render_legend_patch

    subroutine render_legend_marker(entry, backend, line_x1, line_x2, line_center_y)
        !! Render legend marker for entry
        type(legend_entry_t), intent(in) :: entry
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: line_x1, line_x2, line_center_y

        if (allocated(entry%marker)) then
            if (trim(entry%marker) /= 'None' .and. trim(entry%marker) /= 'none' .and. &
                len_trim(entry%marker) > 0) then
                call backend%set_marker_colors(entry%color(1), entry%color(2), entry%color(3), &
                                               entry%color(1), entry%color(2), entry%color(3))
                call backend%draw_marker((line_x1 + line_x2) / 2.0_wp, &
                                        line_center_y, entry%marker)
            end if
        end if
    end subroutine render_legend_marker

    subroutine render_legend_text(entry, backend, text_x, text_y)
        !! Render legend text for entry
        type(legend_entry_t), intent(in) :: entry
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: text_x, text_y

        call backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        call backend%text(text_x, text_y, entry%label)
    end subroutine render_legend_text

    subroutine calculate_legend_position(legend, backend, x, y)
        !! Calculate legend position based on backend and position setting
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(in) :: backend
        real(wp), intent(out) :: x, y
        real(wp) :: data_width, data_height
        type(legend_box_t) :: box
        character(len=:), allocatable :: labels(:)
        integer :: i
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        integer :: px_w, px_h
        logical :: ascii_mode
        integer :: screen_width, screen_height
        integer :: margin_x, margin_y
        integer :: longest_entry, entry_len, prefix_len
        integer :: ascii_x, ascii_y, total_lines

        data_width = backend%x_max - backend%x_min
        data_height = backend%y_max - backend%y_min

        ascii_mode = backend_is_ascii(backend)
        if (ascii_mode) then
            screen_width = max(1, backend%width)
            screen_height = max(1, backend%height)
            margin_x = 3
            margin_y = 0
            longest_entry = 0

            prefix_len = ASCII_LEGEND_HANDLE_WIDTH + 1
            do i = 1, legend%num_entries
                entry_len = len_trim(legend%entries(i)%label)
                longest_entry = max(longest_entry, prefix_len + entry_len)
            end do

            if (longest_entry == 0) longest_entry = 1
            longest_entry = min(longest_entry, max(1, screen_width - margin_x))
            total_lines = max(legend%num_entries, 1)

            select case (legend%position)
            case (1)
                ascii_x = margin_x + ASCII_LEGEND_YAXIS_GUTTER
                ascii_y = margin_y + 1
            case (2)
                ascii_x = max(margin_x, screen_width - longest_entry - margin_x + 1)
                ascii_y = margin_y + 1
            case (3)
                ascii_x = margin_x + ASCII_LEGEND_YAXIS_GUTTER
                ascii_y = max(margin_y + 1, screen_height - total_lines - margin_y + 2)
            case (4)
                ascii_x = max(margin_x, screen_width - longest_entry - margin_x + 1)
                ascii_y = max(margin_y + 1, screen_height - total_lines - margin_y + 2)
            case (5)
                ascii_x = max(margin_x, screen_width - longest_entry - margin_x + 1)
                ascii_y = (screen_height - total_lines)*0.5_wp + 1
            case default
                ascii_x = max(margin_x, screen_width - longest_entry - margin_x + 1)
                ascii_y = margin_y + 1
            end select

            if (legend%position == 1 .or. legend%position == 2) then
                ascii_y = max(ascii_y, margin_y + 4)
            end if

            if (legend%num_entries > 0) then
                if (ascii_y + legend%num_entries - 1 > &
                    screen_height - ASCII_LEGEND_BOTTOM_RESERVE) then
                    ascii_y = max(2, screen_height - ASCII_LEGEND_BOTTOM_RESERVE - &
                                     legend%num_entries + 1)
                end if
            end if

            ascii_x = max(2, min(ascii_x, max(2, screen_width - 1)))
            ascii_y = max(1, min(ascii_y, max(2, screen_height - 1)))

            x = real(ascii_x, wp)
            y = real(ascii_y, wp)
        else
            allocate(character(len=256) :: labels(legend%num_entries))
            do i = 1, legend%num_entries
                labels(i) = legend%entries(i)%label
            end do

            call calculate_plot_area(backend%width, backend%height, margins, plot_area)
            px_w = max(1, plot_area%width)
            px_h = max(1, plot_area%height)

            box = calculate_legend_box(labels, data_width, data_height, &
                                     legend%num_entries, legend%position, px_w, px_h)

            x = backend%x_min + box%x
            y = backend%y_min + box%y
        end if
    end subroutine calculate_legend_position

    subroutine draw_legend_box(backend, x1, y1, x2, y2)
        !! Draw filled white rectangle for legend background
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x1, y1, x2, y2

        real(wp) :: x_quad(4), y_quad(4)

        call backend%set_line_style('-')
        call backend%color(1.0_wp, 1.0_wp, 1.0_wp)

        x_quad = [x1, x2, x2, x1]
        y_quad = [y1, y1, y2, y2]
        call backend%fill_quad(x_quad, y_quad)
    end subroutine draw_legend_box

    subroutine draw_legend_border(backend, x1, y1, x2, y2)
        !! Draw thin border around legend box matching matplotlib's legend frame
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp), parameter :: LEGEND_EDGE_GRAY = 0.8_wp
            !! matplotlib's legend.edgecolor default (0.8 gray), lighter than
            !! the black axes frame.

        if (backend%width > 80 .or. backend%height > 24) then
            ! 0.8pt matches the axes frame width (matplotlib axes.linewidth
            ! default) so the legend border keeps the same stroke weight as
            ! the surrounding axes box.
            call backend%set_line_width(0.8_wp)
        end if
        call backend%set_line_style('-')
        call backend%color(LEGEND_EDGE_GRAY, LEGEND_EDGE_GRAY, LEGEND_EDGE_GRAY)

        call backend%line(x1, y1, x2, y1)
        call backend%line(x2, y1, x2, y2)
        call backend%line(x2, y2, x1, y2)
        call backend%line(x1, y2, x1, y1)
    end subroutine draw_legend_border

    pure function get_ascii_marker_char(marker_style) result(marker_char)
        !! Convert marker style to ASCII character
        character(len=*), intent(in) :: marker_style
        character(len=1) :: marker_char

        select case (trim(marker_style))
        case ('o')
            marker_char = 'o'
        case ('s')
            marker_char = '#'
        case ('D', 'd')
            marker_char = '%'
        case ('x')
            marker_char = 'x'
        case ('+')
            marker_char = '+'
        case ('*')
            marker_char = '*'
        case ('^')
            marker_char = '^'
        case ('v')
            marker_char = 'v'
        case ('<')
            marker_char = '<'
        case ('>')
            marker_char = '>'
        case ('p')
            marker_char = 'P'
        case ('h', 'H')
            marker_char = 'H'
        case ('-')
            marker_char = '-'
        case ('=')
            marker_char = '='
        case ('%')
            marker_char = '%'
        case ('@')
            marker_char = '@'
        case ('#')
            marker_char = '#'
        case default
            marker_char = '*'
        end select
    end function get_ascii_marker_char

    pure logical function backend_is_ascii(backend)
        !! Detect whether backend is operating in ASCII mode based on canvas size
        class(plot_context), intent(in) :: backend

        backend_is_ascii = backend%width <= 120 .and. backend%height <= 60
    end function backend_is_ascii

end module fortplot_legend_drawing
