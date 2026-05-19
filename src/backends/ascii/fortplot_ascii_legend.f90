module fortplot_ascii_legend
    !! ASCII terminal plotting backend - Legend Management
    !!
    !! This module handles ASCII-specific legend rendering, positioning,
    !! and management including specialized autopct support.
    !!
    !! Author: fortplot contributors

    use fortplot_legend, only: legend_t, render_ascii_legend
    use fortplot_legend, only: LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT, &
                           LEGEND_EAST
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_ascii_utils, only: is_legend_entry_text, is_registered_legend_label, is_autopct_text
    use fortplot_ascii_mathtext, only: sanitize_ascii_text
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: render_ascii_legend_specialized, calculate_ascii_legend_dimensions
    public :: set_ascii_legend_border_width, calculate_ascii_legend_position
    public :: reset_ascii_legend_lines_helper, append_ascii_legend_line_helper
    public :: register_legend_entry_helper, assign_pending_autopct_helper
    public :: enqueue_pie_autopct, dequeue_pie_autopct
    public :: clear_pie_legend_entries, get_pie_autopct
    public :: decode_ascii_legend_line
    public :: ascii_render_legend_impl, ascii_calc_legend_dims_impl
    public :: ascii_set_legend_border_impl, ascii_calc_legend_pos_impl
    public :: ascii_add_legend_entry_impl, ascii_clear_legend_impl
    public :: ascii_clear_pie_legend_impl, ascii_register_pie_legend_impl

contains

    subroutine render_ascii_legend_specialized(legend, canvas_context, legend_x, legend_y)
        !! Render legend using ASCII-specific compact layout
        use fortplot_context, only: plot_context
        type(legend_t), intent(in) :: legend
        class(plot_context), intent(inout) :: canvas_context
        real(wp), intent(in) :: legend_x, legend_y

        ! Use ASCII-specific legend rendering
        call render_ascii_legend(legend, canvas_context, legend_x, legend_y)
    end subroutine render_ascii_legend_specialized

    subroutine calculate_ascii_legend_dimensions(legend, width, legend_width, legend_height)
        !! Calculate ASCII-specific legend dimensions
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width
        real(wp), intent(out) :: legend_width, legend_height
        integer :: i
        character(len=512) :: processed_label
        integer :: processed_len

        ! Calculate actual legend width based on longest entry
        legend_width = 15.0_wp  ! Default minimum width
        do i = 1, legend%num_entries
            ! Process LaTeX commands for accurate width calculation
            call process_latex_in_text(legend%entries(i)%label, processed_label, processed_len)
            legend_width = max(legend_width, real(processed_len + 5, wp))  ! +5 for "-- " prefix and margin
        end do

        ! For ASCII backend, limit legend width to prevent overflow
        if (legend_width > real(width, wp) * 0.3) then
            legend_width = real(width, wp) * 0.3
        end if

        legend_height = real(legend%num_entries + 2, wp)  ! Each entry + border
    end subroutine calculate_ascii_legend_dimensions

    subroutine set_ascii_legend_border_width()
        !! ASCII doesn't use line widths - no-op
        ! ASCII backend doesn't have line widths - no operation needed
    end subroutine set_ascii_legend_border_width

    subroutine calculate_ascii_legend_position(legend, width, height, x, y)
        !! Calculate ASCII-specific legend position using character coordinates
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width, height
        real(wp), intent(out) :: x, y
        real(wp) :: legend_width, legend_height, margin_x, margin_y

        ! Get ASCII-specific dimensions
        call calculate_ascii_legend_dimensions(legend, width, legend_width, legend_height)

        margin_x = 2.0_wp      ! 2 character margin
        margin_y = 1.0_wp      ! 1 line margin

        select case (legend%position)
        case (LEGEND_UPPER_LEFT)
            x = margin_x
            y = margin_y
        case (LEGEND_UPPER_RIGHT)
            ! Position legend so its text fits within the canvas
            ! For ASCII, be more conservative to avoid clipping
            x = real(width, wp) - legend_width - margin_x - 5.0_wp
            x = max(margin_x, x)  ! But not too far left
            y = margin_y + 2.0_wp  ! Start lower to leave room for multiple entries
        case (LEGEND_LOWER_LEFT)
            x = margin_x
            y = real(height, wp) - legend_height - margin_y
        case (LEGEND_LOWER_RIGHT)
            x = real(width, wp) - legend_width - margin_x
            y = real(height, wp) - legend_height - margin_y
        case (LEGEND_EAST)
            x = real(width, wp) - legend_width - margin_x
            y = (real(height, wp) - legend_height)*0.5_wp
        case default
            ! Default to upper right corner
            x = real(width, wp) - legend_width - margin_x
            y = margin_y
        end select
    end subroutine calculate_ascii_legend_position

    subroutine reset_ascii_legend_lines_helper(legend_lines, num_legend_lines)
        !! Reset ASCII legend lines array
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines

        if (.not. allocated(legend_lines)) then
            allocate(character(len=96) :: legend_lines(0))
        end if

        num_legend_lines = 0
    end subroutine reset_ascii_legend_lines_helper

    subroutine append_ascii_legend_line_helper(legend_lines, num_legend_lines, raw_line)
        !! Append a legend line to ASCII backend array
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines
        character(len=*), intent(in) :: raw_line

        integer :: current_size, new_size
        character(len=96), allocatable :: tmp(:)
        character(len=96) :: line_buffer

        if (.not. allocated(legend_lines)) then
            allocate(character(len=96) :: legend_lines(0))
        end if

        current_size = size(legend_lines)
        if (num_legend_lines == current_size) then
            new_size = max(4, max(1, current_size) * 2)
            allocate(tmp(new_size))
            tmp = ' '
            if (num_legend_lines > 0) tmp(1:num_legend_lines) = legend_lines
            call move_alloc(tmp, legend_lines)
        end if

        num_legend_lines = num_legend_lines + 1
        line_buffer = adjustl(raw_line)
        legend_lines(num_legend_lines) = line_buffer
    end subroutine append_ascii_legend_line_helper

    subroutine register_legend_entry_helper(legend_entry_indices, legend_entry_has_autopct, &
                                          legend_entry_labels, legend_entry_count, &
                                          legend_autopct_cursor, &
                                          line_idx, label, has_autopct)
        !! Register a legend entry for processing
        integer, allocatable, intent(inout) :: legend_entry_indices(:)
        logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
        character(len=64), allocatable, intent(inout) :: legend_entry_labels(:)
        integer, intent(inout) :: legend_entry_count, legend_autopct_cursor
        integer, intent(in) :: line_idx
        character(len=*), intent(in) :: label
        logical, intent(in) :: has_autopct

        integer :: current_size, new_size
        integer, allocatable :: idx_tmp(:)
        logical, allocatable :: flag_tmp(:)
        character(len=64), allocatable :: label_tmp(:)

        if (.not. allocated(legend_entry_indices)) then
            allocate(legend_entry_indices(0))
            allocate(legend_entry_has_autopct(0))
            allocate(legend_entry_labels(0))
        end if

        current_size = size(legend_entry_indices)
        if (legend_entry_count == current_size) then
            new_size = max(4, max(1, current_size) * 2)
            allocate(idx_tmp(new_size))
            idx_tmp = 0
            if (legend_entry_count > 0) then
                idx_tmp(1:legend_entry_count) = legend_entry_indices
            end if
            call move_alloc(idx_tmp, legend_entry_indices)

            allocate(flag_tmp(new_size))
            flag_tmp = .false.
            if (legend_entry_count > 0) then
                flag_tmp(1:legend_entry_count) = legend_entry_has_autopct
            end if
            call move_alloc(flag_tmp, legend_entry_has_autopct)

            allocate(label_tmp(new_size))
            label_tmp = ''
            if (legend_entry_count > 0) then
                label_tmp(1:legend_entry_count) = legend_entry_labels
            end if
            call move_alloc(label_tmp, legend_entry_labels)
        end if

        legend_entry_count = legend_entry_count + 1
        legend_entry_indices(legend_entry_count) = line_idx
        legend_entry_has_autopct(legend_entry_count) = has_autopct
        legend_entry_labels(legend_entry_count) = adjustl(trim(label))

        if (has_autopct) then
            if (legend_autopct_cursor == legend_entry_count) then
                legend_autopct_cursor = legend_autopct_cursor + 1
            end if
        end if
    end subroutine register_legend_entry_helper

    subroutine assign_pending_autopct_helper(legend_lines, num_legend_lines, &
                                           pie_autopct_queue, pie_autopct_count, &
                                           legend_entry_indices, legend_entry_has_autopct, &
                                           legend_autopct_cursor, legend_entry_count)
        !! Assign pending autopct values to legend lines
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
        integer, intent(inout) :: pie_autopct_count
        integer, allocatable, intent(inout) :: legend_entry_indices(:)
        logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
        integer, intent(inout) :: legend_autopct_cursor
        integer, intent(in) :: legend_entry_count

        character(len=32) :: value
        integer :: target_idx

        do while (pie_autopct_count > 0 .and. legend_autopct_cursor <= legend_entry_count)
            if (legend_entry_has_autopct(legend_autopct_cursor)) then
                legend_autopct_cursor = legend_autopct_cursor + 1
                cycle
            end if

            ! Dequeue autopct value
            if (pie_autopct_count > 0) then
                value = pie_autopct_queue(1)
                ! Shift queue
                if (pie_autopct_count > 1) then
                    pie_autopct_queue(1:pie_autopct_count-1) = pie_autopct_queue(2:pie_autopct_count)
                end if
                pie_autopct_count = pie_autopct_count - 1
            else
                value = ''
            end if

            target_idx = legend_entry_indices(legend_autopct_cursor)
            call append_autopct_to_line_helper(legend_lines, num_legend_lines, target_idx, value)
            legend_entry_has_autopct(legend_autopct_cursor) = .true.
            legend_autopct_cursor = legend_autopct_cursor + 1
        end do
    end subroutine assign_pending_autopct_helper

    subroutine append_autopct_to_line_helper(legend_lines, num_legend_lines, line_idx, value)
        !! Append autopct value to a legend line
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(in) :: num_legend_lines
        integer, intent(in) :: line_idx
        character(len=*), intent(in) :: value

        character(len=96) :: updated_line

        if (len_trim(value) == 0) return
        if (line_idx < 1 .or. line_idx > num_legend_lines) return

        updated_line = trim(legend_lines(line_idx))
        if (index(updated_line, '(') > 0) then
            legend_lines(line_idx) = updated_line
        else
            updated_line = trim(updated_line) // ' (' // trim(value) // ')'
            legend_lines(line_idx) = adjustl(updated_line)
        end if
    end subroutine append_autopct_to_line_helper

    ! Pie chart autopct queue management

    subroutine enqueue_pie_autopct(autopct_queue, autopct_count, value)
        !! Enqueue a pie autopct value for later assignment
        character(len=32), allocatable, intent(inout) :: autopct_queue(:)
        integer, intent(inout) :: autopct_count
        character(len=*), intent(in) :: value

        integer :: current_size, new_size
        character(len=32), allocatable :: tmp(:)

        if (.not. allocated(autopct_queue)) then
            allocate(character(len=32) :: autopct_queue(0))
        end if

        current_size = size(autopct_queue)
        if (autopct_count == current_size) then
            new_size = max(4, max(1, current_size) * 2)
            allocate(tmp(new_size))
            tmp = ''
            if (autopct_count > 0) tmp(1:autopct_count) = autopct_queue
            call move_alloc(tmp, autopct_queue)
        end if

        autopct_count = autopct_count + 1
        autopct_queue(autopct_count) = adjustl(trim(value))
    end subroutine enqueue_pie_autopct

    function dequeue_pie_autopct(autopct_queue, autopct_count) result(value)
        !! Dequeue the next pie autopct value from the queue
        character(len=32), allocatable, intent(inout) :: autopct_queue(:)
        integer, intent(inout) :: autopct_count
        character(len=32) :: value

        integer :: idx

        if (.not. allocated(autopct_queue)) then
            allocate(character(len=32) :: autopct_queue(0))
        end if

        if (autopct_count <= 0) then
            value = ''
            return
        end if

        value = autopct_queue(1)
        if (autopct_count > 1) then
            do idx = 1, autopct_count - 1
                autopct_queue(idx) = autopct_queue(idx + 1)
            end do
        end if
        autopct_queue(autopct_count) = ''
        autopct_count = autopct_count - 1
    end function dequeue_pie_autopct

    subroutine clear_pie_legend_entries(pie_legend_labels, pie_legend_values, &
                                        pie_legend_count, autopct_queue, autopct_count)
        !! Clear all pie chart legend entries and autopct data
        character(len=64), allocatable, intent(inout) :: pie_legend_labels(:)
        character(len=32), allocatable, intent(inout) :: pie_legend_values(:)
        integer, intent(inout) :: pie_legend_count
        character(len=32), allocatable, intent(inout) :: autopct_queue(:)
        integer, intent(inout) :: autopct_count

        if (.not. allocated(pie_legend_labels)) then
            allocate(character(len=64) :: pie_legend_labels(0))
        else
            if (size(pie_legend_labels) > 0) pie_legend_labels = ''
        end if

        if (.not. allocated(pie_legend_values)) then
            allocate(character(len=32) :: pie_legend_values(0))
        else
            if (size(pie_legend_values) > 0) pie_legend_values = ''
        end if

        pie_legend_count = 0
        autopct_count = 0
        if (allocated(autopct_queue)) then
            if (size(autopct_queue) > 0) autopct_queue = ''
        end if
    end subroutine clear_pie_legend_entries

    function get_pie_autopct(pie_legend_labels, pie_legend_values, &
                             pie_legend_count, label) result(value)
        !! Look up an autopct value by label name
        character(len=64), allocatable, intent(inout) :: pie_legend_labels(:)
        character(len=32), allocatable, intent(inout) :: pie_legend_values(:)
        integer, intent(in) :: pie_legend_count
        character(len=*), intent(in) :: label
        character(len=32) :: value

        integer :: i
        character(len=:), allocatable :: normalized

        value = ''
        if (.not. allocated(pie_legend_labels)) return
        if (pie_legend_count <= 0) return

        normalized = adjustl(trim(label))

        do i = 1, pie_legend_count
            if (adjustl(trim(pie_legend_labels(i))) == normalized) then
                value = trim(pie_legend_values(i))
                pie_legend_values(i) = ''
                return
            end if
        end do
    end function get_pie_autopct

    subroutine decode_ascii_legend_line(raw_text, formatted_line, entry_label)
        !! Decode a raw legend line into formatted output and entry label
        character(len=*), intent(in) :: raw_text
        character(len=96), intent(out) :: formatted_line
        character(len=64), intent(out) :: entry_label

        character(len=:), allocatable :: trimmed_text
        character(len=256) :: sanitized
        integer :: sanitized_len, first_space

        formatted_line = ''
        entry_label = ''

        trimmed_text = trim(adjustl(raw_text))
        if (len_trim(trimmed_text) == 0) return

        if (len(trimmed_text) >= 3 .and. trimmed_text(1:3) == '-- ') then
            if (len(trimmed_text) > 3) then
                call sanitize_ascii_text(trim(adjustl(trimmed_text(4:))), sanitized, sanitized_len)
                entry_label = trim(sanitized(1:sanitized_len))
            else
                entry_label = ''
            end if
            formatted_line = '  - '//trim(entry_label)
        else
            formatted_line = '  '//trim(trimmed_text)
            first_space = index(trimmed_text, ' ')
            if (first_space > 0 .and. first_space < len(trimmed_text)) then
                call sanitize_ascii_text(trim(adjustl(trimmed_text(first_space + 1:))), sanitized, sanitized_len)
                entry_label = trim(sanitized(1:sanitized_len))
            else
                call sanitize_ascii_text(trim(trimmed_text), sanitized, sanitized_len)
                entry_label = trim(sanitized(1:sanitized_len))
            end if
        end if
    end subroutine decode_ascii_legend_line

    subroutine ascii_render_legend_impl(legend, legend_lines, num_legend_lines)
        use fortplot_legend, only: legend_t
        type(legend_t), intent(in) :: legend
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines

        integer :: i, label_len
        character(len=96) :: line_buffer
        character(len=256) :: sanitized_label
        character(len=:), allocatable :: label_text
        character(len=1) :: marker_char

        call reset_ascii_legend_lines_helper(legend_lines, num_legend_lines)

        if (legend%num_entries <= 0) return

        call append_ascii_legend_line_helper(legend_lines, num_legend_lines, 'Legend:')

        do i = 1, legend%num_entries
            if (allocated(legend%entries(i)%label)) then
                call sanitize_ascii_text(legend%entries(i)%label, sanitized_label, label_len)
                label_text = sanitized_label(1:label_len)
            else
                label_text = ''
            end if

            if (len_trim(label_text) == 0) then
                write (line_buffer, '("Series ",I0)') i
                label_text = trim(line_buffer)
            end if

            marker_char = '*'
            if (allocated(legend%entries(i)%marker)) then
                if (trim(legend%entries(i)%marker) /= 'None' .and. &
                    len_trim(legend%entries(i)%marker) > 0) then
                    marker_char = trim(legend%entries(i)%marker)
                end if
            end if

            line_buffer = '  '//marker_char//' '//label_text
            call append_ascii_legend_line_helper(legend_lines, num_legend_lines, trim(line_buffer))
        end do
    end subroutine ascii_render_legend_impl

    subroutine ascii_calc_legend_dims_impl(legend, width, legend_width, legend_height)
        use fortplot_legend, only: legend_t
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width
        real(wp), intent(out) :: legend_width, legend_height

        call calculate_ascii_legend_dimensions(legend, width, legend_width, legend_height)
    end subroutine ascii_calc_legend_dims_impl

    subroutine ascii_set_legend_border_impl()
    end subroutine ascii_set_legend_border_impl

    subroutine ascii_calc_legend_pos_impl(legend, width, height, legend_width, legend_height, x, y)
        use fortplot_legend, only: legend_t
        type(legend_t), intent(in) :: legend
        integer, intent(in) :: width, height
        real(wp), intent(in) :: legend_width, legend_height
        real(wp), intent(out) :: x, y
        real(wp) :: margin_x, margin_y
        real(wp) :: rw, rh, lw, lh

        margin_x = 2.0_wp
        margin_y = 1.0_wp
        rw = real(width, wp)
        rh = real(height, wp)
        lw = legend_width
        lh = legend_height

        select case (legend%position)
        case (LEGEND_UPPER_LEFT)
            x = margin_x
            y = margin_y
        case (LEGEND_UPPER_RIGHT)
            x = rw - lw - margin_x - 5.0_wp
            x = max(margin_x, x)
            y = margin_y + 2.0_wp
        case (LEGEND_LOWER_LEFT)
            x = margin_x
            y = rh - lh - margin_y
        case (LEGEND_LOWER_RIGHT)
            x = rw - lw - margin_x
            y = rh - lh - margin_y
        case (LEGEND_EAST)
            x = rw - lw - margin_x
            y = (rh - lh)*0.5_wp
        case default
            x = rw - lw - margin_x
            y = margin_y
        end select
    end subroutine ascii_calc_legend_pos_impl

    subroutine ascii_add_legend_entry_impl(label, value_text, legend_lines, num_legend_lines)
        character(len=*), intent(in) :: label
        character(len=*), intent(in), optional :: value_text
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines

        character(len=96) :: line_buffer
        character(len=256) :: sanitized
        integer :: sanitized_len
        character(len=:), allocatable :: value_trimmed

        call sanitize_ascii_text(label, sanitized, sanitized_len)
        line_buffer = '  '//trim(sanitized(1:sanitized_len))

        if (present(value_text)) then
            value_trimmed = trim(value_text)
            if (len_trim(value_trimmed) > 0) then
                line_buffer = trim(line_buffer)//' ('//value_trimmed//')'
            end if
        end if

        call append_ascii_legend_line_helper(legend_lines, num_legend_lines, trim(line_buffer))
    end subroutine ascii_add_legend_entry_impl

    subroutine ascii_clear_legend_impl(legend_lines, num_legend_lines, header)
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines
        character(len=*), intent(in), optional :: header

        call reset_ascii_legend_lines_helper(legend_lines, num_legend_lines)

        if (present(header)) then
            if (len_trim(header) > 0) then
                call append_ascii_legend_line_helper(legend_lines, num_legend_lines, trim(header))
            end if
        end if
    end subroutine ascii_clear_legend_impl

    subroutine ascii_clear_pie_legend_impl(legend_lines, num_legend_lines)
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines

        call ascii_clear_legend_impl(legend_lines, num_legend_lines)
    end subroutine ascii_clear_pie_legend_impl

    subroutine ascii_register_pie_legend_impl(label, value_text, legend_lines, num_legend_lines)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: value_text
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines

        call ascii_add_legend_entry_impl(label, value_text, legend_lines, num_legend_lines)
    end subroutine ascii_register_pie_legend_impl

end module fortplot_ascii_legend