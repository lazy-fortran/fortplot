submodule (fortplot_ascii_legend) fortplot_ascii_legend_pie

    !! Pie chart autopct queue management
    !!
    !! Single Responsibility: Handle pie chart autopct value queuing,
    !! dequeuing, clearing, and lookup.

    implicit none

contains

    module subroutine enqueue_pie_autopct(autopct_queue, autopct_count, value)
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

    module function dequeue_pie_autopct(autopct_queue, autopct_count) result(value)
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

    module subroutine clear_pie_legend_entries(pie_legend_labels, pie_legend_values, &
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

    module function get_pie_autopct(pie_legend_labels, pie_legend_values, &
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

    module subroutine ascii_clear_pie_legend_impl(legend_lines, num_legend_lines)
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines

        call ascii_clear_legend_impl(legend_lines, num_legend_lines)
    end subroutine ascii_clear_pie_legend_impl

    module subroutine ascii_register_pie_legend_impl(label, value_text, legend_lines, num_legend_lines)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: value_text
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines

        call ascii_add_legend_entry_impl(label, value_text, legend_lines, num_legend_lines)
    end subroutine ascii_register_pie_legend_impl

end submodule fortplot_ascii_legend_pie
