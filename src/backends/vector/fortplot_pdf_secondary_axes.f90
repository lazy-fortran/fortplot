module fortplot_pdf_secondary_axes
    !! PDF secondary axes rendering for twin axes support
    !! Handles drawing of secondary Y-axis (right side) and X-axis (top)

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, PDF_TICK_SIZE, &
                                 PDF_TICK_LABEL_SIZE, PDF_LABEL_SIZE
    use fortplot_pdf_text, only: estimate_pdf_text_width
    use fortplot_pdf_axes, only: setup_axes_data_ranges, generate_tick_data, &
                                 render_mixed_text
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_pdf_text_segments, only: process_text_segments
    implicit none
    private

    public :: draw_pdf_secondary_y_axis
    public :: draw_pdf_secondary_x_axis_top

contains

    subroutine draw_pdf_secondary_y_axis(ctx, yscale, symlog_threshold, y_min, y_max, &
                                         plot_area_left, plot_area_bottom, &
                                         plot_area_width, plot_area_height, ylabel)
        !! Draw secondary Y axis on the right side of the plot
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: y_min, y_max
        real(wp), intent(in) :: plot_area_left, plot_area_bottom
        real(wp), intent(in) :: plot_area_width, plot_area_height
        character(len=:), allocatable, intent(in), optional :: ylabel

        real(wp), allocatable :: x_positions(:), y_positions(:)
        character(len=32), allocatable :: x_labels(:), y_labels(:)
        integer :: num_x_ticks, num_y_ticks, i
        real(wp) :: y_min_adj, y_max_adj
        real(wp) :: right_edge, label_x, label_y
        real(wp) :: y_label_w, max_y_w
        character(len=2048) :: tick_cmd
        real(wp), parameter :: Y_TICK_GAP = 3.0_wp
        real(wp), parameter :: YLABEL_PAD = 5.0_wp
        real(wp), parameter :: Y_TICK_BASELINE_NUDGE = 0.35_wp

        call setup_axes_data_ranges(ctx, 0.0_wp, 1.0_wp, y_min, y_max, &
                                    y_min_adj, y_max_adj, y_min_adj, y_max_adj, &
                                    yscale=yscale)

        call generate_tick_data(ctx, 0.0_wp, 1.0_wp, y_min, y_max, &
                                x_positions, y_positions, x_labels, y_labels, &
                                num_x_ticks, num_y_ticks, yscale=yscale, &
                                plot_area_left=plot_area_left, &
                                plot_area_bottom=plot_area_bottom, &
                                plot_area_width=plot_area_width, &
                                plot_area_height=plot_area_height, &
                                symlog_threshold=symlog_threshold)

        right_edge = plot_area_left + plot_area_width

        call ctx%set_color(0.0_wp, 0.0_wp, 0.0_wp)
        call ctx%set_line_width(1.0_wp)
        ctx%stream_data = ctx%stream_data//'[] 0 d'//new_line('a')

        do i = 1, num_y_ticks
            write (tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                right_edge, y_positions(i), &
                right_edge - PDF_TICK_SIZE, y_positions(i)
            ctx%stream_data = ctx%stream_data//trim(adjustl(tick_cmd))//new_line('a')
        end do

        max_y_w = 0.0_wp
        do i = 1, num_y_ticks
            y_label_w = estimate_pdf_text_width(trim(y_labels(i)), PDF_TICK_LABEL_SIZE)
            max_y_w = max(max_y_w, y_label_w)
            label_x = right_edge + Y_TICK_GAP
            label_y = y_positions(i) - Y_TICK_BASELINE_NUDGE*PDF_TICK_LABEL_SIZE
            call render_mixed_text(ctx, label_x, label_y, trim(y_labels(i)))
        end do

        if (present(ylabel)) then
            if (allocated(ylabel)) then
                if (len_trim(ylabel) > 0) then
                    call draw_rotated_ylabel_right(ctx, trim(ylabel), right_edge, &
                                                   plot_area_bottom, plot_area_height, &
                                                   max_y_w + Y_TICK_GAP + YLABEL_PAD)
                end if
            end if
        end if
    end subroutine draw_pdf_secondary_y_axis

    subroutine draw_pdf_secondary_x_axis_top(ctx, xscale, symlog_threshold, &
                                             x_min, x_max, &
                                             plot_area_left, plot_area_bottom, &
                                             plot_area_width, plot_area_height, xlabel)
        !! Draw secondary X axis at the top of the plot
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max
        real(wp), intent(in) :: plot_area_left, plot_area_bottom
        real(wp), intent(in) :: plot_area_width, plot_area_height
        character(len=:), allocatable, intent(in), optional :: xlabel

        real(wp), allocatable :: x_positions(:), y_positions(:)
        character(len=32), allocatable :: x_labels(:), y_labels(:)
        integer :: num_x_ticks, num_y_ticks, i
        real(wp) :: x_min_adj, x_max_adj
        real(wp) :: top_edge, label_x, label_y
        real(wp) :: width_pt, xlabel_x, xlabel_y
        character(len=2048) :: tick_cmd
        character(len=512) :: processed_xlabel
        integer :: processed_len
        real(wp), parameter :: X_TICK_GAP = 3.0_wp
        real(wp), parameter :: XLABEL_PAD = 15.0_wp

        call setup_axes_data_ranges(ctx, x_min, x_max, 0.0_wp, 1.0_wp, &
                                    x_min_adj, x_max_adj, x_min_adj, x_max_adj, &
                                    xscale=xscale)

        call generate_tick_data(ctx, x_min, x_max, 0.0_wp, 1.0_wp, &
                                x_positions, y_positions, x_labels, y_labels, &
                                num_x_ticks, num_y_ticks, xscale=xscale, &
                                plot_area_left=plot_area_left, &
                                plot_area_bottom=plot_area_bottom, &
                                plot_area_width=plot_area_width, &
                                plot_area_height=plot_area_height, &
                                symlog_threshold=symlog_threshold)

        top_edge = plot_area_bottom + plot_area_height

        call ctx%set_color(0.0_wp, 0.0_wp, 0.0_wp)
        call ctx%set_line_width(1.0_wp)
        ctx%stream_data = ctx%stream_data//'[] 0 d'//new_line('a')

        do i = 1, num_x_ticks
            write (tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                x_positions(i), top_edge, &
                x_positions(i), top_edge - PDF_TICK_SIZE
            ctx%stream_data = ctx%stream_data//trim(adjustl(tick_cmd))//new_line('a')
        end do

        do i = 1, num_x_ticks
            label_x = x_positions(i) - &
                      0.5_wp*estimate_pdf_text_width(trim(x_labels(i)), &
                                                     PDF_TICK_LABEL_SIZE)
            label_y = top_edge + X_TICK_GAP
            call render_mixed_text(ctx, label_x, label_y, trim(x_labels(i)))
        end do

        if (present(xlabel)) then
            if (allocated(xlabel)) then
                if (len_trim(xlabel) > 0) then
                    call process_latex_in_text(trim(xlabel), processed_xlabel, &
                                               processed_len)
                    width_pt = estimate_pdf_text_width( &
                               processed_xlabel(1:processed_len), PDF_LABEL_SIZE)
                    xlabel_x = plot_area_left + 0.5_wp*plot_area_width - 0.5_wp*width_pt
                    xlabel_y = top_edge + XLABEL_PAD + PDF_TICK_LABEL_SIZE
                    call render_mixed_text(ctx, xlabel_x, xlabel_y, trim(xlabel))
                end if
            end if
        end if
    end subroutine draw_pdf_secondary_x_axis_top

    subroutine draw_rotated_ylabel_right(ctx, text, right_edge, plot_bottom, &
                                         plot_height, offset)
        !! Draw rotated ylabel on the right side
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: right_edge, plot_bottom, plot_height, offset

        real(wp) :: width_pt, ylabel_x, ylabel_y
        character(len=512) :: processed
        integer :: plen

        call process_latex_in_text(text, processed, plen)
        width_pt = estimate_pdf_text_width(processed(1:plen), PDF_LABEL_SIZE)
        ylabel_x = right_edge + offset + PDF_LABEL_SIZE
        ylabel_y = plot_bottom + 0.5_wp*plot_height - 0.5_wp*width_pt
        call draw_rotated_mixed_font_text_clockwise(ctx, ylabel_x, ylabel_y, text)
    end subroutine draw_rotated_ylabel_right

    subroutine draw_rotated_mixed_font_text_clockwise(ctx, x, y, text)
        !! Draw text rotated 90 degrees clockwise (for right ylabel)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=512) :: processed
        integer :: plen
        character(len=1024) :: matrix_cmd
        logical :: in_symbol_font

        call process_latex_in_text(text, processed, plen)

        ctx%stream_data = ctx%stream_data//'BT'//new_line('a')

        write (matrix_cmd, '("0 -1 1 0 ", F0.3, 1X, F0.3, " Tm")') x, y
        ctx%stream_data = ctx%stream_data//trim(adjustl(matrix_cmd))//new_line('a')

        write (matrix_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            ctx%fonts%get_helvetica_obj(), PDF_LABEL_SIZE
        ctx%stream_data = ctx%stream_data//trim(adjustl(matrix_cmd))//new_line('a')

        in_symbol_font = .false.
        call process_text_segments(ctx, processed(1:plen), in_symbol_font, &
                                   PDF_LABEL_SIZE)

        ctx%stream_data = ctx%stream_data//'ET'//new_line('a')
    end subroutine draw_rotated_mixed_font_text_clockwise

end module fortplot_pdf_secondary_axes
