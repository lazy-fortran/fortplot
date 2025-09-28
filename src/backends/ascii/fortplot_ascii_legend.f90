module fortplot_ascii_legend
    !! ASCII terminal plotting backend - Legend Management
    !!
    !! This module handles ASCII-specific legend rendering, positioning,
    !! and management including specialized autopct support.
    !!
    !! Author: fortplot contributors

    use fortplot_legend, only: legend_t, render_ascii_legend
    use fortplot_legend, only: LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_ascii_utils, only: is_legend_entry_text, is_registered_legend_label, is_autopct_text
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: render_ascii_legend_specialized, calculate_ascii_legend_dimensions
    public :: set_ascii_legend_border_width, calculate_ascii_legend_position
    public :: reset_ascii_legend_lines_helper, append_ascii_legend_line_helper
    public :: register_legend_entry_helper, assign_pending_autopct_helper

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

end module fortplot_ascii_legend