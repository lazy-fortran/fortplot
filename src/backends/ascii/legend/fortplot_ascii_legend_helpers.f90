submodule (fortplot_ascii_legend) fortplot_ascii_legend_helpers

    !! ASCII legend helper functions
    !!
    !! Single Responsibility: Handle legend line management, entry registration,
    !! and autopct assignment helpers.

    implicit none

contains

    module subroutine reset_ascii_legend_lines_helper(legend_lines, num_legend_lines)
        !! Reset ASCII legend lines array
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines

        if (.not. allocated(legend_lines)) then
            allocate(character(len=96) :: legend_lines(0))
        end if

        num_legend_lines = 0
    end subroutine reset_ascii_legend_lines_helper

    module subroutine append_ascii_legend_line_helper(legend_lines, num_legend_lines, raw_line)
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

    module subroutine register_legend_entry_helper(legend_entry_indices, legend_entry_has_autopct, &
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

    module subroutine assign_pending_autopct_helper(legend_lines, num_legend_lines, &
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

            if (pie_autopct_count > 0) then
                value = pie_autopct_queue(1)
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

    module subroutine append_autopct_to_line_helper(legend_lines, num_legend_lines, line_idx, value)
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

    module subroutine ascii_add_legend_entry_impl(label, value_text, legend_lines, num_legend_lines)
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

    module subroutine ascii_clear_legend_impl(legend_lines, num_legend_lines, header)
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

end submodule fortplot_ascii_legend_helpers
