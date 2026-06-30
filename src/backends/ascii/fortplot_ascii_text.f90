module fortplot_ascii_text
     !! ASCII terminal plotting backend - Text and Layout Management
     !!
     !! This module handles ASCII text rendering, axes labels, and complex
     !! text processing. Legend management is delegated to fortplot_ascii_legend.
     !!
     !! Author: fortplot contributors

     use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
     use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                          format_tick_value_consistent
     use fortplot_ascii_mathtext, only: sanitize_ascii_text
     use fortplot_margins, only: plot_area_t
     use fortplot_ascii_utils, only: is_legend_entry_text, &
                                     is_registered_legend_label, is_autopct_text, &
                                     get_blend_char, text_element_t
     use fortplot_ascii_legend, only: reset_ascii_legend_lines_helper, &
                                      append_ascii_legend_line_helper, &
                                      register_legend_entry_helper, &
                                      assign_pending_autopct_helper, &
                                      enqueue_pie_autopct, dequeue_pie_autopct, &
                                      clear_pie_legend_entries, get_pie_autopct, &
                                      decode_ascii_legend_line
     use fortplot_ascii_text_elements, only: add_text_element, store_text_element
     use, intrinsic :: iso_fortran_env, only: wp => real64
     implicit none

     private
     public :: draw_ascii_axes_and_labels, ascii_draw_text_helper

contains

    subroutine draw_ascii_axes_and_labels(canvas, xscale, yscale, symlog_threshold, &
                                           x_min, x_max, y_min, y_max, &
                                           title, xlabel, ylabel, &
                                           x_date_format, y_date_format, &
                                           z_min, z_max, has_3d_plots, &
                                           current_r, current_g, current_b, &
                                           plot_area, plot_width, plot_height, &
                                           title_text, xlabel_text, ylabel_text, &
                                           text_elements, num_text_elements, &
                                           custom_xticks, custom_xtick_labels)
        !! Draw axes and labels for ASCII backend
        character(len=1), intent(inout) :: canvas(:, :)
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        real(wp), intent(in) :: current_r, current_g, current_b
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height
        character(len=:), allocatable, intent(inout) :: title_text, xlabel_text, &
                                                        ylabel_text
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(inout) :: num_text_elements
        real(wp), intent(in), optional :: custom_xticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)

        real(wp) :: x_tick_positions(MAX_TICKS), y_tick_positions(MAX_TICKS)
        character(len=500) :: processed_title
        integer :: processed_len

        ! Reference optional parameters without unreachable branches
        if (present(z_min)) then; associate (unused_zmin => z_min); end associate; end if
        if (present(z_max)) then; associate (unused_zmax => z_max); end associate; end if
        associate (unused_h3d => has_3d_plots); end associate

        call process_axis_labels(title, processed_title, processed_len, title_text)
        call draw_ascii_axis_lines(canvas, x_min, x_max, y_min, y_max, plot_area, &
                                   plot_width, plot_height)
        call render_ascii_x_ticks(xscale, x_min, x_max, y_min, y_max, symlog_threshold, &
                                  x_tick_positions, custom_xticks, custom_xtick_labels, &
                                  x_date_format, plot_area, plot_width, plot_height, &
                                  text_elements, num_text_elements, current_r, current_g, current_b)
        call render_ascii_y_ticks(yscale, x_min, x_max, y_min, y_max, symlog_threshold, &
                                  y_tick_positions, y_date_format, plot_area, plot_width, plot_height, &
                                  text_elements, num_text_elements, current_r, current_g, current_b)
        call process_axis_labels(xlabel, processed_title, processed_len, xlabel_text)
        call process_axis_labels(ylabel, processed_title, processed_len, ylabel_text)
    end subroutine draw_ascii_axes_and_labels

    subroutine process_axis_labels(label, processed_title, processed_len, output_text)
        !! Process a single axis label (title, xlabel, or ylabel) for ASCII output
        character(len=:), allocatable, intent(in), optional :: label
        character(len=500), intent(inout) :: processed_title
        integer, intent(inout) :: processed_len
        character(len=:), allocatable, intent(out) :: output_text

        output_text = ''
        if (present(label)) then
            if (allocated(label)) then
                call sanitize_ascii_text(label, processed_title, processed_len)
                output_text = processed_title(1:processed_len)
            end if
        end if
    end subroutine process_axis_labels

    subroutine draw_ascii_axis_lines(canvas, x_min, x_max, y_min, y_max, plot_area, &
                                     plot_width, plot_height)
        !! Draw horizontal and vertical axis lines on ASCII canvas
        character(len=1), intent(inout) :: canvas(:, :)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height

        call draw_line_on_canvas_local(canvas, x_min, y_min, x_max, y_min, &
                                       x_min, x_max, y_min, y_max, plot_area, &
                                       plot_width, plot_height, '-')
        call draw_line_on_canvas_local(canvas, x_min, y_min, x_min, y_max, &
                                       x_min, x_max, y_min, y_max, plot_area, &
                                       plot_width, plot_height, '|')
    end subroutine draw_ascii_axis_lines

subroutine render_ascii_x_ticks(xscale, x_min, x_max, y_min, y_max, symlog_threshold, &
                                 x_tick_positions, custom_xticks, custom_xtick_labels, &
                                 x_date_format, plot_area, plot_width, plot_height, &
                                 text_elements, num_text_elements, current_r, current_g, current_b)
        !! Render X-axis tick marks and labels on ASCII canvas
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, symlog_threshold
        real(wp), contiguous, intent(inout) :: x_tick_positions(:)
        real(wp), intent(in), optional :: custom_xticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: x_date_format
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(inout) :: num_text_elements
        real(wp), intent(in) :: current_r, current_g, current_b

        integer :: num_x_ticks, i
        character(len=50) :: tick_label
        real(wp) :: tick_x
        integer :: decimals
        logical :: use_custom_xticks
        logical :: use_plot_area

        use_custom_xticks = .false.
        if (present(custom_xticks) .and. present(custom_xtick_labels)) then
            if (size(custom_xticks) > 0 .and. &
                size(custom_xticks) == size(custom_xtick_labels)) then
                use_custom_xticks = .true.
                num_x_ticks = min(size(custom_xticks), MAX_TICKS)
                x_tick_positions(1:num_x_ticks) = custom_xticks(1:num_x_ticks)
            end if
        end if

        if (.not. use_custom_xticks) then
            call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, &
                                     x_tick_positions, num_x_ticks)
        else
            num_x_ticks = min(size(custom_xticks), MAX_TICKS)
        end if

        decimals = 0
        if (trim(xscale) == 'linear' .and. num_x_ticks >= 2 .and. &
            .not. use_custom_xticks) then
            decimals = determine_decimals_from_ticks(x_tick_positions, num_x_ticks)
        end if
        use_plot_area = plot_area%width > 0 .and. plot_area%height > 0

        do i = 1, num_x_ticks
            tick_x = x_tick_positions(i)
            if (use_custom_xticks) then
                tick_label = custom_xtick_labels(i)
            else if (trim(xscale) == 'linear') then
                tick_label = format_tick_value_consistent(tick_x, decimals)
            else
                tick_label = format_tick_label(tick_x, xscale, &
                                               date_format=x_date_format, &
                                               data_min=x_min, data_max=x_max)
            end if
            if (use_plot_area) then
                associate (sx => nint((tick_x - x_min)/(x_max - x_min)* &
                               real(max(1, plot_area%width - 2), wp)) + plot_area%left + 1, &
                             sy => plot_area%bottom + plot_area%height - 1)
                    ! Center the label on the tick column rather than left-anchoring it,
                    ! so labels sit under the bar/tick they name (issue #1957).
                    associate (start_col => sx - len_trim(tick_label)/2)
                        call add_text_element(text_elements, num_text_elements, &
                                              real(max(plot_area%left + 1, &
                                                       min(start_col, plot_area%left + plot_area%width - 1)), wp), &
                                              real(sy, wp), &
                                              trim(tick_label), &
                                              current_r, current_g, current_b, &
                                              x_min, x_max, y_min, y_max, plot_area, plot_width, plot_height)
                    end associate
                end associate
            else
                associate (sx => nint((tick_x - x_min)/(x_max - x_min)* &
                               real(plot_width - 2, wp)) + 1, &
                             sy => plot_height)
                    associate (start_col => sx - len_trim(tick_label)/2)
                        call add_text_element(text_elements, num_text_elements, &
                                              real(max(1, min(start_col, plot_width - 1)), wp), &
                                              real(sy, wp), &
                                              trim(tick_label), &
                                              current_r, current_g, current_b, &
                                              x_min, x_max, y_min, y_max, plot_area, plot_width, plot_height)
                    end associate
                end associate
            end if
        end do
    end subroutine render_ascii_x_ticks

subroutine render_ascii_y_ticks(yscale, x_min, x_max, y_min, y_max, symlog_threshold, &
                                   y_tick_positions, y_date_format, plot_area, plot_width, plot_height, &
                                   text_elements, num_text_elements, current_r, current_g, current_b)
        !! Render Y-axis tick marks with row-based de-duplication on ASCII canvas
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, symlog_threshold
        real(wp), contiguous, intent(inout) :: y_tick_positions(:)
        character(len=*), intent(in), optional :: y_date_format
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(inout) :: num_text_elements
        real(wp), intent(in) :: current_r, current_g, current_b

        integer :: num_y_ticks, i, row
        character(len=50) :: tick_label
        real(wp) :: tick_y
        integer :: decimals
        integer, allocatable :: row_best_len(:)
        character(len=64), allocatable :: row_best_label(:)
        logical :: use_plot_area

        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, &
                                 y_tick_positions, num_y_ticks)
        decimals = 0
        if (trim(yscale) == 'linear' .and. num_y_ticks >= 2) then
            decimals = determine_decimals_from_ticks(y_tick_positions, num_y_ticks)
        end if
        use_plot_area = plot_area%width > 0 .and. plot_area%height > 0

        if (use_plot_area) then
            allocate (row_best_len(max(1, plot_area%bottom + plot_area%height + 1)))
            allocate (row_best_label(max(1, plot_area%bottom + plot_area%height + 1)))
            row_best_len = 0
            row_best_label = ''
        else
            allocate (row_best_len(max(1, plot_height + 1)))
            allocate (row_best_label(max(1, plot_height + 1)))
            row_best_len = 0
            row_best_label = ''
        end if

        do i = 1, num_y_ticks
            tick_y = y_tick_positions(i)
            if (trim(yscale) == 'linear') then
                tick_label = format_tick_value_consistent(tick_y, decimals)
            else
                tick_label = format_tick_label(tick_y, yscale, &
                                               date_format=y_date_format, &
                                               data_min=y_min, data_max=y_max)
            end if
            if (use_plot_area) then
                row = plot_area%bottom + plot_area%height - 1 - &
                      nint((y_max - tick_y)/(y_max - y_min)*real(max(1, plot_area%height - 2), wp))
                row = max(plot_area%bottom + 1, min(row, plot_area%bottom + plot_area%height - 1))
            else
                row = nint((y_max - tick_y)/(y_max - y_min)*real(plot_height, wp))
                row = max(1, min(row, plot_height))
            end if
            if (len_trim(tick_label) > row_best_len(row)) then
                row_best_len(row) = len_trim(tick_label)
                row_best_label(row) = adjustl(tick_label)
            end if
        end do

        if (use_plot_area) then
            do row = plot_area%bottom + 1, plot_area%bottom + plot_area%height - 1
                if (row_best_len(row) > 0) then
                    call add_text_element(text_elements, num_text_elements, &
                                          real(plot_area%left + 1, wp), real(row, wp), &
                                          trim(row_best_label(row)), &
                                          current_r, current_g, current_b, &
                                          x_min, x_max, y_min, y_max, plot_area, plot_width, plot_height)
                end if
            end do
        else
            do row = 1, plot_height
                if (row_best_len(row) > 0 .and. row < plot_height) then
                    call add_text_element(text_elements, num_text_elements, &
                                          2.0_wp, real(row, wp), &
                                          trim(row_best_label(row)), &
                                          current_r, current_g, current_b, &
                                          x_min, x_max, y_min, y_max, plot_area, plot_width, plot_height)
                end if
            end do
        end if
    end subroutine render_ascii_y_ticks

    subroutine draw_line_on_canvas_local(canvas, x1, y1, x2, y2, x_min, x_max, y_min, &
                                         y_max, plot_area, plot_width, plot_height, line_char)
        character(len=1), intent(inout) :: canvas(:, :)
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: plot_width, plot_height
        character(len=1), intent(in) :: line_char

        real(wp) :: dx, dy, length, step_x, step_y, x, y
        integer :: steps, i, px, py
        logical :: use_plot_area

        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)

        if (length < 1e-6_wp) return

        steps = max(int(length*4), max(abs(int(dx)), abs(int(dy)))) + 1
        step_x = dx/real(steps, wp)
        step_y = dy/real(steps, wp)

        x = x1
        y = y1
        use_plot_area = plot_area%width > 0 .and. plot_area%height > 0

        do i = 0, steps
            if (use_plot_area) then
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
                px = int((x - x_min)/(x_max - x_min)*real(plot_width - 3, wp)) + 2
                py = (plot_height - 1) - int((y - y_min)/(y_max - y_min)*real(plot_height - 3, wp))
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
    end subroutine draw_line_on_canvas_local

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
            px = 1
            py = 1
        end if
    end subroutine map_to_plot_area

    subroutine ascii_draw_text_helper(text_elements, num_text_elements, &
                                      legend_lines, num_legend_lines, &
                                      capturing_legend, &
                                      pie_legend_labels, pie_legend_values, &
                                      pie_legend_count, &
                                      pie_autopct_queue, pie_autopct_count, &
                                      legend_entry_indices, legend_entry_has_autopct, &
                                      legend_entry_labels, legend_entry_count, &
                                      legend_autopct_cursor, &
                                      x, y, text, x_min, x_max, y_min, y_max, &
                                      plot_width, plot_height, current_r, &
                                      current_g, current_b)
        !! ASCII text drawing with legend processing (moved from main module)
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(inout) :: num_text_elements
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines
        logical, intent(inout) :: capturing_legend
        character(len=64), allocatable, intent(inout) :: pie_legend_labels(:)
        character(len=32), allocatable, intent(inout) :: pie_legend_values(:)
        integer, intent(inout) :: pie_legend_count
        character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
        integer, intent(inout) :: pie_autopct_count
        integer, allocatable, intent(inout) :: legend_entry_indices(:)
        logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
        character(len=64), allocatable, intent(inout) :: legend_entry_labels(:)
        integer, intent(inout) :: legend_entry_count, legend_autopct_cursor
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        real(wp), intent(in) :: current_r, current_g, current_b

        integer :: text_x, text_y
        character(len=:), allocatable :: processed_text, trimmed_text
        character(len=96) :: formatted_line
        character(len=64) :: entry_label
        character(len=32) :: autopct_value
        logical :: handled

        trimmed_text = trim(adjustl(text))
        handled = .false.

        if (trimmed_text == 'ASCII Legend') then
            call handle_ascii_legend_init(legend_lines, num_legend_lines, &
                                          capturing_legend, legend_entry_count, &
                                          legend_autopct_cursor)
            handled = .true.
        else if (is_autopct_text(trimmed_text)) then
            call handle_ascii_autopct(trimmed_text, pie_autopct_queue, pie_autopct_count, &
                                      legend_lines, num_legend_lines, &
                                      legend_entry_indices, legend_entry_has_autopct, &
                                      legend_autopct_cursor, legend_entry_count)
            handled = .true.
        else if (capturing_legend) then
            call handle_ascii_legend_capture(trimmed_text, formatted_line, entry_label, &
                                             autopct_value, pie_autopct_queue, pie_autopct_count, &
                                             pie_legend_labels, pie_legend_values, pie_legend_count, &
                                             legend_lines, num_legend_lines, capturing_legend, &
                                             legend_entry_indices, legend_entry_has_autopct, &
                                             legend_entry_labels, legend_entry_count, &
                                             legend_autopct_cursor, handled)
        end if

        if (.not. handled .and. legend_entry_count > 0) then
            if (is_registered_legend_label(legend_entry_labels, legend_entry_count, &
                                           trimmed_text)) return
        end if

        if (.not. handled) then
            call store_text_element(text_elements, num_text_elements, text_x, text_y, &
                                    processed_text, x, y, text, x_min, x_max, y_min, y_max, &
                                    plot_width=plot_width, plot_height=plot_height, &
                                    current_r=current_r, current_g=current_g, current_b=current_b)
        end if
    end subroutine ascii_draw_text_helper

    subroutine handle_ascii_legend_init(legend_lines, num_legend_lines, &
                                        capturing_legend, legend_entry_count, &
                                        legend_autopct_cursor)
        !! Initialize ASCII legend state
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines
        logical, intent(inout) :: capturing_legend
        integer, intent(inout) :: legend_entry_count
        integer, intent(inout) :: legend_autopct_cursor

        call reset_ascii_legend_lines_helper(legend_lines, num_legend_lines)
        call append_ascii_legend_line_helper(legend_lines, num_legend_lines, 'Legend:')
        capturing_legend = .true.
        legend_entry_count = 0
        legend_autopct_cursor = 1
    end subroutine handle_ascii_legend_init

    subroutine handle_ascii_autopct(trimmed_text, pie_autopct_queue, pie_autopct_count, &
                                     legend_lines, num_legend_lines, &
                                     legend_entry_indices, legend_entry_has_autopct, &
                                     legend_autopct_cursor, legend_entry_count)
        !! Handle autopct text for pie charts
        character(len=*), intent(in) :: trimmed_text
        character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
        integer, intent(inout) :: pie_autopct_count
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines
        integer, allocatable, intent(inout) :: legend_entry_indices(:)
        logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
        integer, intent(inout) :: legend_autopct_cursor
        integer, intent(inout) :: legend_entry_count

        call enqueue_pie_autopct(pie_autopct_queue, pie_autopct_count, trimmed_text)
        call assign_pending_autopct_helper(legend_lines, num_legend_lines, &
                                           pie_autopct_queue, pie_autopct_count, &
                                           legend_entry_indices, legend_entry_has_autopct, &
                                           legend_autopct_cursor, legend_entry_count)
    end subroutine handle_ascii_autopct

    subroutine handle_ascii_legend_capture(trimmed_text, formatted_line, entry_label, &
                                            autopct_value, pie_autopct_queue, pie_autopct_count, &
                                            pie_legend_labels, pie_legend_values, pie_legend_count, &
                                            legend_lines, num_legend_lines, capturing_legend, &
                                            legend_entry_indices, legend_entry_has_autopct, &
                                            legend_entry_labels, legend_entry_count, &
                                            legend_autopct_cursor, handled)
        !! Handle legend content capture phase
        character(len=*), intent(in) :: trimmed_text
        character(len=96), intent(inout) :: formatted_line
        character(len=64), intent(inout) :: entry_label
        character(len=32), intent(inout) :: autopct_value
        character(len=32), allocatable, intent(inout) :: pie_autopct_queue(:)
        integer, intent(inout) :: pie_autopct_count
        character(len=64), allocatable, intent(inout) :: pie_legend_labels(:)
        character(len=32), allocatable, intent(inout) :: pie_legend_values(:)
        integer, intent(inout) :: pie_legend_count
        character(len=96), allocatable, intent(inout) :: legend_lines(:)
        integer, intent(inout) :: num_legend_lines
        logical, intent(inout) :: capturing_legend
        integer, allocatable, intent(inout) :: legend_entry_indices(:)
        logical, allocatable, intent(inout) :: legend_entry_has_autopct(:)
        character(len=64), allocatable, intent(inout) :: legend_entry_labels(:)
        integer, intent(inout) :: legend_entry_count
        integer, intent(inout) :: legend_autopct_cursor
        logical, intent(out) :: handled

        handled = .false.

        if (len_trim(trimmed_text) == 0) then
            capturing_legend = .false.
            call clear_pie_legend_entries(pie_legend_labels, pie_legend_values, &
                                              pie_legend_count, pie_autopct_queue, pie_autopct_count)
            handled = .true.
            return
        end if

        if (.not. is_legend_entry_text(trimmed_text)) then
            capturing_legend = .false.
            call clear_pie_legend_entries(pie_legend_labels, pie_legend_values, &
                                              pie_legend_count, pie_autopct_queue, pie_autopct_count)
            return
        end if

        call decode_ascii_legend_line(trimmed_text, formatted_line, entry_label)
        if (len_trim(entry_label) > 0 .and. len_trim(formatted_line) > 0) then
            autopct_value = ''
            if (pie_autopct_count > 0) then
                autopct_value = dequeue_pie_autopct(pie_autopct_queue, pie_autopct_count)
            else
                autopct_value = get_pie_autopct(pie_legend_labels, pie_legend_values, &
                                                pie_legend_count, entry_label)
            end if
            if (len_trim(autopct_value) > 0) then
                formatted_line = trim(formatted_line)//' ('//trim(autopct_value)//')'
            end if
            call append_ascii_legend_line_helper(legend_lines, num_legend_lines, &
                                                 trim(formatted_line))
            call register_legend_entry_helper(legend_entry_indices, &
                                             legend_entry_has_autopct, &
                                             legend_entry_labels, legend_entry_count, &
                                             legend_autopct_cursor, num_legend_lines, &
                                             entry_label, len_trim(autopct_value) > 0)
            call assign_pending_autopct_helper(legend_lines, num_legend_lines, &
                                               pie_autopct_queue, pie_autopct_count, &
                                               legend_entry_indices, legend_entry_has_autopct, &
                                               legend_autopct_cursor, legend_entry_count)
      handled = .true.
         end if
     end subroutine handle_ascii_legend_capture

end module fortplot_ascii_text
