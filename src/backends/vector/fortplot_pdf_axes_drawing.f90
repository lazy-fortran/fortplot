module fortplot_pdf_axes_drawing
    !! PDF axis drawing module
    !!
    !! Handles plot frame, tick marks, minor ticks, and tick label rendering.
    !! Depends on fortplot_pdf_axes_tick_data for tick data generation.

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, PDF_MARGIN, &
                                 PDF_TICK_SIZE, PDF_LABEL_SIZE, &
                                 PDF_TICK_LABEL_SIZE, PDF_TITLE_SIZE
    use fortplot_pdf_axes_tick_data, only: initialize_tick_arrays, &
                                           generate_x_axis_ticks, &
                                           generate_y_axis_ticks
    use fortplot_pdf_text, only: estimate_pdf_text_width
    use fortplot_pdf_axes_text, only: render_mixed_text
    implicit none
    private

    ! Public procedures
    public :: draw_pdf_frame_with_area
    public :: draw_pdf_tick_marks_with_area
    public :: draw_pdf_tick_labels_with_area
    public :: draw_pdf_minor_tick_marks

    real(wp), parameter :: PDF_MINOR_TICK_SIZE = 3.0_wp

contains

    subroutine draw_pdf_frame_with_area(ctx, plot_left, plot_bottom, plot_width, &
                                        plot_height)
        !! Draw the plot frame using actual plot area coordinates (FIXED version)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: plot_left, plot_bottom, plot_width, plot_height
        character(len=2048) :: frame_cmd
        real(wp) :: x1, y1

        ! PDF coordinates: Y=0 at bottom (same as our data coordinates)
        x1 = plot_left
        y1 = plot_bottom  ! No conversion needed - PDF Y=0 is at bottom

        ! Force solid stroke for the frame regardless of prior dash settings
        ctx%stream_data = ctx%stream_data//'[] 0 d'//new_line('a')

        ! Draw rectangle frame
        write (frame_cmd, '(F0.3, 1X, F0.3, " ", F0.3, 1X, F0.3, " re S")') &
            x1, y1, plot_width, plot_height
        ctx%stream_data = ctx%stream_data//trim(adjustl(frame_cmd))//new_line('a')
    end subroutine draw_pdf_frame_with_area

    subroutine draw_pdf_tick_marks_with_area(ctx, x_positions, y_positions, &
                                             num_x, num_y, plot_left, plot_bottom)
        !! Draw tick marks using actual plot area coordinates (FIXED version)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in), contiguous :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x, num_y
        real(wp), intent(in) :: plot_left, plot_bottom

        integer :: i
        character(len=2048) :: tick_cmd
        real(wp) :: tick_length, bottom_y

        ! Ensure tick marks are stroked in black and solid regardless of prior
        ! drawing state.
        call ctx%set_color(0.0_wp, 0.0_wp, 0.0_wp)
        call ctx%set_line_width(1.0_wp)
        ctx%stream_data = ctx%stream_data//'[] 0 d'//new_line('a')

        tick_length = PDF_TICK_SIZE
        bottom_y = plot_bottom  ! PDF Y=0 is at bottom, no conversion needed

        ! Draw X-axis ticks (bottom of plot area) — draw inward into frame
        do i = 1, num_x
            write (tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                x_positions(i), bottom_y, &
                x_positions(i), bottom_y + tick_length
            ctx%stream_data = ctx%stream_data//trim(adjustl(tick_cmd))//new_line('a')
        end do

        ! Draw Y-axis ticks (left side of plot area) — draw inward into frame
        do i = 1, num_y
            write (tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                plot_left, y_positions(i), &
                plot_left + tick_length, y_positions(i)
            ctx%stream_data = ctx%stream_data//trim(adjustl(tick_cmd))//new_line('a')
        end do
    end subroutine draw_pdf_tick_marks_with_area

    subroutine draw_pdf_tick_labels_with_area(ctx, x_positions, y_positions, x_labels, &
                                              y_labels, num_x, num_y, plot_left, &
                                              plot_bottom, plot_height, &
                                              max_y_tick_label_width)
        !! Draw tick labels using actual plot area coordinates (FIXED version)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in), contiguous :: x_positions(:), y_positions(:)
        character(len=*), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x, num_y
        real(wp), intent(in) :: plot_left, plot_bottom, plot_height
        real(wp), intent(out), optional :: max_y_tick_label_width

        integer :: i, prev_idx
        real(wp) :: label_x, label_y, bottom_y
        real(wp) :: y_label_w
        real(wp) :: max_y_w
        real(wp) :: min_spacing
        real(wp) :: label_budget
        integer :: max_labels
        integer :: idx
        integer :: k
        ! Legacy fallback width (unused for mathtext)
        real(wp), parameter :: X_TICK_GAP = 15.0_wp
        ! Distance below plot for X tick labels
        real(wp), parameter :: Y_TICK_GAP_LOCAL = 1.0_wp
        real(wp), parameter :: Y_TICK_BASELINE_NUDGE = 0.35_wp
        bottom_y = plot_bottom  ! PDF Y=0 is at bottom, no conversion needed

        ! Draw X-axis labels (center-aligned under each tick)
        do i = 1, num_x
            ! Estimate label width in points for proper centering (handles mathtext)
            label_x = x_positions(i) - &
                      0.5_wp*estimate_pdf_text_width(trim(x_labels(i)), &
                                                     PDF_TICK_LABEL_SIZE)
            label_y = bottom_y - X_TICK_GAP
            call render_mixed_text(ctx, label_x, label_y, trim(x_labels(i)))
        end do

        max_y_w = 0.0_wp
        min_spacing = max(9.0_wp, 1.15_wp*PDF_TICK_LABEL_SIZE)
        label_budget = max(0.0_wp, plot_height - PDF_TICK_LABEL_SIZE)
        max_labels = max(2, int(label_budget/min_spacing) + 1)

        if (num_y <= max_labels) then
            do i = 1, num_y
                label_y = y_positions(i) - Y_TICK_BASELINE_NUDGE*PDF_TICK_LABEL_SIZE
                y_label_w = estimate_pdf_text_width(trim(y_labels(i)), &
                                                    PDF_TICK_LABEL_SIZE)
                max_y_w = max(max_y_w, y_label_w)
                label_x = plot_left - Y_TICK_GAP_LOCAL - y_label_w
                call render_mixed_text(ctx, label_x, label_y, trim(y_labels(i)))
            end do
        else
            prev_idx = 0
            do k = 1, max_labels
                idx = 1 + int(nint(real(k - 1, wp)*real(num_y - 1, wp)/ &
                                   real(max_labels - 1, wp)))
                idx = max(idx, prev_idx + 1)
                idx = min(idx, num_y - (max_labels - k))
                prev_idx = idx
                label_y = y_positions(idx) - Y_TICK_BASELINE_NUDGE*PDF_TICK_LABEL_SIZE
                y_label_w = estimate_pdf_text_width(trim(y_labels(idx)), &
                                                    PDF_TICK_LABEL_SIZE)
                max_y_w = max(max_y_w, y_label_w)
                label_x = plot_left - Y_TICK_GAP_LOCAL - y_label_w
                call render_mixed_text(ctx, label_x, label_y, trim(y_labels(idx)))
            end do
        end if

        if (present(max_y_tick_label_width)) then
            max_y_tick_label_width = max_y_w
        end if
    end subroutine draw_pdf_tick_labels_with_area

    subroutine draw_pdf_minor_tick_marks(ctx, x_minor_positions, y_minor_positions, &
                                         num_x_minor, num_y_minor, &
                                         plot_left, plot_bottom)
        !! Draw minor tick marks (shorter than major ticks)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in), contiguous :: x_minor_positions(:), y_minor_positions(:)
        integer, intent(in) :: num_x_minor, num_y_minor
        real(wp), intent(in) :: plot_left, plot_bottom

        integer :: i
        character(len=2048) :: tick_cmd
        real(wp) :: bottom_y

        call ctx%set_color(0.0_wp, 0.0_wp, 0.0_wp)
        call ctx%set_line_width(0.5_wp)
        ctx%stream_data = ctx%stream_data//'[] 0 d'//new_line('a')

        bottom_y = plot_bottom

        do i = 1, num_x_minor
            write (tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                x_minor_positions(i), bottom_y, &
                x_minor_positions(i), bottom_y + PDF_MINOR_TICK_SIZE
            ctx%stream_data = ctx%stream_data//trim(adjustl(tick_cmd))//new_line('a')
        end do

        do i = 1, num_y_minor
            write (tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                plot_left, y_minor_positions(i), &
                plot_left + PDF_MINOR_TICK_SIZE, y_minor_positions(i)
            ctx%stream_data = ctx%stream_data//trim(adjustl(tick_cmd))//new_line('a')
        end do
    end subroutine draw_pdf_minor_tick_marks

end module fortplot_pdf_axes_drawing
