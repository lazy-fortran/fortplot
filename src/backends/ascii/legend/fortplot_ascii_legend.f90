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
    use fortplot_context, only: plot_context
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

    interface
        module subroutine render_ascii_legend_specialized(legend, canvas_context, legend_x, legend_y)
            use fortplot_context, only: plot_context
            type(legend_t), intent(in) :: legend
            class(plot_context), intent(inout) :: canvas_context
            real(wp), intent(in) :: legend_x, legend_y
        end subroutine render_ascii_legend_specialized

        module subroutine calculate_ascii_legend_dimensions(legend, width, legend_width, legend_height)
            type(legend_t), intent(in) :: legend
            integer, intent(in) :: width
            real(wp), intent(out) :: legend_width, legend_height
        end subroutine calculate_ascii_legend_dimensions

        module subroutine set_ascii_legend_border_width()
        end subroutine set_ascii_legend_border_width

        module subroutine calculate_ascii_legend_position(legend, width, height, x, y)
            type(legend_t), intent(in) :: legend
            integer, intent(in) :: width, height
            real(wp), intent(out) :: x, y
        end subroutine calculate_ascii_legend_position

        module subroutine reset_ascii_legend_lines_helper(legend_lines, num_legend_lines)
            character(len=96), allocatable, intent(inout) :: legend_lines(:)
            integer, intent(inout) :: num_legend_lines
        end subroutine reset_ascii_legend_lines_helper

        module subroutine append_ascii_legend_line_helper(legend_lines, num_legend_lines, raw_line)
            character(len=96), allocatable, intent(inout) :: legend_lines(:)
            integer, intent(inout) :: num_legend_lines
            character(len=*), intent(in) :: raw_line
        end subroutine append_ascii_legend_line_helper

        module subroutine register_legend_entry_helper(legend_entry_indices, legend_entry_has_autopct, &
                                              legend_entry_labels, legend_entry_count, &
                                              legend_autopct_cursor, &
                                              line_idx, label, has_autopct)
            integer, allocatable, intent(inout) :: legend_entry_indices(:)
            logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
            character(len=64), allocatable, intent(inout) :: legend_entry_labels(:)
            integer, intent(inout) :: legend_entry_count, legend_autopct_cursor
            integer, intent(in) :: line_idx
            character(len=*), intent(in) :: label
            logical, intent(in) :: has_autopct
        end subroutine register_legend_entry_helper

        module subroutine assign_pending_autopct_helper(legend_lines, num_legend_lines, &
                                               pie_autopct_queue, pie_autopct_count, &
                                               legend_entry_indices, legend_entry_has_autopct, &
                                               legend_autopct_cursor, legend_entry_count)
            character(len=96), allocatable, intent(inout) :: legend_lines(:)
            integer, intent(in) :: num_legend_lines
            character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
            integer, intent(inout) :: pie_autopct_count
            integer, allocatable, intent(inout) :: legend_entry_indices(:)
            logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
            integer, intent(inout) :: legend_autopct_cursor
            integer, intent(in) :: legend_entry_count
        end subroutine assign_pending_autopct_helper

        module subroutine append_autopct_to_line_helper(legend_lines, num_legend_lines, line_idx, value)
            character(len=96), allocatable, intent(inout) :: legend_lines(:)
            integer, intent(in) :: num_legend_lines
            integer, intent(in) :: line_idx
            character(len=*), intent(in) :: value
        end subroutine append_autopct_to_line_helper

        module subroutine enqueue_pie_autopct(autopct_queue, autopct_count, value)
            character(len=32), allocatable, intent(inout) :: autopct_queue(:)
            integer, intent(inout) :: autopct_count
            character(len=*), intent(in) :: value
        end subroutine enqueue_pie_autopct

        module function dequeue_pie_autopct(autopct_queue, autopct_count) result(value)
            character(len=32), allocatable, intent(inout) :: autopct_queue(:)
            integer, intent(inout) :: autopct_count
            character(len=32) :: value
        end function dequeue_pie_autopct

        module subroutine clear_pie_legend_entries(pie_legend_labels, pie_legend_values, &
                                            pie_legend_count, autopct_queue, autopct_count)
            character(len=64), allocatable, intent(inout) :: pie_legend_labels(:)
            character(len=32), allocatable, intent(inout) :: pie_legend_values(:)
            integer, intent(inout) :: pie_legend_count
            character(len=32), allocatable, intent(inout) :: autopct_queue(:)
            integer, intent(inout) :: autopct_count
        end subroutine clear_pie_legend_entries

        module function get_pie_autopct(pie_legend_labels, pie_legend_values, &
                                 pie_legend_count, label) result(value)
            character(len=64), allocatable, intent(inout) :: pie_legend_labels(:)
            character(len=32), allocatable, intent(inout) :: pie_legend_values(:)
            integer, intent(in) :: pie_legend_count
            character(len=*), intent(in) :: label
            character(len=32) :: value
        end function get_pie_autopct

        module subroutine decode_ascii_legend_line(raw_text, formatted_line, entry_label)
            character(len=*), intent(in) :: raw_text
            character(len=96), intent(out) :: formatted_line
            character(len=64), intent(out) :: entry_label
        end subroutine decode_ascii_legend_line

        module subroutine ascii_render_legend_impl(legend, legend_lines, num_legend_lines, raw_labels)
            use fortplot_legend, only: legend_t
            type(legend_t), intent(in) :: legend
            character(len=96), allocatable, intent(inout) :: legend_lines(:)
            integer, intent(inout) :: num_legend_lines
            logical, intent(in), optional :: raw_labels
        end subroutine ascii_render_legend_impl

        module subroutine ascii_calc_legend_dims_impl(legend, width, legend_width, legend_height)
            use fortplot_legend, only: legend_t
            type(legend_t), intent(in) :: legend
            integer, intent(in) :: width
            real(wp), intent(out) :: legend_width, legend_height
        end subroutine ascii_calc_legend_dims_impl

        module subroutine ascii_set_legend_border_impl()
        end subroutine ascii_set_legend_border_impl

        module subroutine ascii_calc_legend_pos_impl(legend, width, height, legend_width, legend_height, x, y)
            use fortplot_legend, only: legend_t
            type(legend_t), intent(in) :: legend
            integer, intent(in) :: width, height
            real(wp), intent(in) :: legend_width, legend_height
            real(wp), intent(out) :: x, y
        end subroutine ascii_calc_legend_pos_impl

        module subroutine ascii_add_legend_entry_impl(label, value_text, legend_lines, num_legend_lines)
            character(len=*), intent(in) :: label
            character(len=*), intent(in), optional :: value_text
            character(len=96), allocatable, intent(inout) :: legend_lines(:)
            integer, intent(inout) :: num_legend_lines
        end subroutine ascii_add_legend_entry_impl

        module subroutine ascii_clear_legend_impl(legend_lines, num_legend_lines, header)
            character(len=96), allocatable, intent(inout) :: legend_lines(:)
            integer, intent(inout) :: num_legend_lines
            character(len=*), intent(in), optional :: header
        end subroutine ascii_clear_legend_impl

        module subroutine ascii_clear_pie_legend_impl(legend_lines, num_legend_lines)
            character(len=96), allocatable, intent(inout) :: legend_lines(:)
            integer, intent(inout) :: num_legend_lines
        end subroutine ascii_clear_pie_legend_impl

        module subroutine ascii_register_pie_legend_impl(label, value_text, legend_lines, num_legend_lines)
            character(len=*), intent(in) :: label
            character(len=*), intent(in) :: value_text
            character(len=96), allocatable, intent(inout) :: legend_lines(:)
            integer, intent(inout) :: num_legend_lines
        end subroutine ascii_register_pie_legend_impl
    end interface

end module fortplot_ascii_legend
