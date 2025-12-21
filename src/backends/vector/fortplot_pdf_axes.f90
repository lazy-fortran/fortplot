module fortplot_pdf_axes
    !! PDF axes, grid, and tick drawing operations
    !! Handles plot frame, axes, tick marks, and grid lines

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, PDF_MARGIN, &
                                 PDF_TICK_SIZE, PDF_LABEL_SIZE, &
                                 PDF_TICK_LABEL_SIZE, PDF_TITLE_SIZE
    use fortplot_constants, only: XLABEL_VERTICAL_OFFSET
    use fortplot_pdf_drawing, only: pdf_stream_writer
    use fortplot_pdf_text, only: draw_pdf_text, draw_pdf_text_bold, &
                                 draw_mixed_font_text, draw_rotated_mixed_font_text, &
                                 draw_pdf_mathtext, estimate_pdf_text_width
    use fortplot_text_helpers, only: prepare_mathtext_if_needed
    use fortplot_text_layout, only: has_mathtext, preprocess_math_text
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext
    use fortplot_pdf_mathtext_render, only: render_mathtext_element_pdf
    use fortplot_unicode, only: utf8_char_length, utf8_to_codepoint
    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                         format_tick_value_consistent
    use fortplot_scales, only: apply_scale_transform
    implicit none
    private

    ! Public procedures
    public :: draw_pdf_axes_and_labels
    public :: draw_pdf_3d_axes_frame
    public :: draw_pdf_frame_with_area
    public :: draw_pdf_tick_marks_with_area
    public :: draw_pdf_tick_labels_with_area
    public :: draw_pdf_title_and_labels
    public :: setup_axes_data_ranges
    public :: generate_tick_data
    public :: render_mixed_text

contains

    subroutine setup_axes_data_ranges(ctx, x_min_orig, x_max_orig, y_min_orig, &
                                      y_max_orig, &
                                      x_min_adj, x_max_adj, y_min_adj, y_max_adj, &
                                      xscale, yscale)
        !! Set up data ranges for axes with optional log scaling
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        real(wp), intent(out) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj
        character(len=*), intent(in), optional :: xscale, yscale

        real(wp) :: x_range, y_range
        associate (dummy_ctx => ctx%width); end associate

        ! Initialize adjusted values
        x_min_adj = x_min_orig
        x_max_adj = x_max_orig
        y_min_adj = y_min_orig
        y_max_adj = y_max_orig

        ! Apply log scaling if requested
        if (present(xscale)) then
            if (xscale == 'log' .and. x_min_adj > 0.0_wp) then
                x_min_adj = log10(x_min_adj)
                x_max_adj = log10(x_max_adj)
            end if
        end if

        if (present(yscale)) then
            if (yscale == 'log' .and. y_min_adj > 0.0_wp) then
                y_min_adj = log10(y_min_adj)
                y_max_adj = log10(y_max_adj)
            end if
        end if

        ! Ensure valid ranges
        x_range = x_max_adj - x_min_adj
        y_range = y_max_adj - y_min_adj

        if (abs(x_range) < 1.0e-10_wp) then
            x_min_adj = x_min_adj - 0.5_wp
            x_max_adj = x_max_adj + 0.5_wp
        end if

        if (abs(y_range) < 1.0e-10_wp) then
            y_min_adj = y_min_adj - 0.5_wp
            y_max_adj = y_max_adj + 0.5_wp
        end if
    end subroutine setup_axes_data_ranges

    subroutine generate_tick_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                                  x_positions, y_positions, x_labels, y_labels, &
                                  num_x_ticks, num_y_ticks, xscale, yscale, &
                                  plot_area_left, plot_area_bottom, plot_area_width, &
                                  plot_area_height, &
                                  symlog_threshold)
        !! Generate tick positions and labels for axes
        !! Refactored to be under 100 lines (QADS compliance)
        type(pdf_context_core), intent(in) :: ctx
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=32), allocatable, intent(out) :: x_labels(:), y_labels(:)
        integer, intent(out) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height
        real(wp), intent(in), optional :: symlog_threshold

        associate (dummy_ctx => ctx%width); end associate
        ! Calculate number of ticks and allocate arrays
        call initialize_tick_arrays(plot_area_width, plot_area_height, num_x_ticks, &
                                    num_y_ticks, &
                                    x_positions, y_positions, x_labels, y_labels)

        ! Generate X axis ticks
        call generate_x_axis_ticks(data_x_min, data_x_max, num_x_ticks, &
                                   plot_area_left, &
                                   plot_area_width, x_positions, x_labels, xscale, &
                                   symlog_threshold)

        ! Generate Y axis ticks
        call generate_y_axis_ticks(data_y_min, data_y_max, num_y_ticks, &
                                   plot_area_bottom, &
                                   plot_area_height, y_positions, y_labels, yscale, &
                                   symlog_threshold)

    end subroutine generate_tick_data

    subroutine initialize_tick_arrays(plot_width, plot_height, num_x_ticks, &
                                      num_y_ticks, &
                                      x_positions, y_positions, x_labels, y_labels)
        !! Initialize tick count and allocate arrays
        real(wp), intent(in) :: plot_width, plot_height
        integer, intent(out) :: num_x_ticks, num_y_ticks
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=32), allocatable, intent(out) :: x_labels(:), y_labels(:)

        integer, parameter :: TARGET_TICKS = 8

        ! Determine number of ticks using plot area dimensions
        num_x_ticks = min(TARGET_TICKS, max(2, int(plot_width/50.0_wp)))
        num_y_ticks = min(TARGET_TICKS, max(2, int(plot_height/40.0_wp)))

        ! Allocate arrays
        allocate (x_positions(num_x_ticks))
        allocate (y_positions(num_y_ticks))
        allocate (x_labels(num_x_ticks))
        allocate (y_labels(num_y_ticks))
    end subroutine initialize_tick_arrays

    subroutine generate_x_axis_ticks(data_min, data_max, num_ticks, plot_left, &
                                     plot_width, &
                                     positions, labels, scale_type, symlog_threshold)
        !! Generate X axis tick positions and labels
        real(wp), intent(in) :: data_min, data_max, plot_left, plot_width
        integer, intent(inout) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=32), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        real(wp), intent(in), optional :: symlog_threshold

        call generate_axis_ticks_internal(data_min, data_max, num_ticks, plot_left, &
                                          plot_width, &
                                          positions, labels, scale_type, &
                                          symlog_threshold, 'x')
    end subroutine generate_x_axis_ticks

    subroutine generate_y_axis_ticks(data_min, data_max, num_ticks, plot_bottom, &
                                     plot_height, &
                                     positions, labels, scale_type, symlog_threshold)
        !! Generate Y axis tick positions and labels
        real(wp), intent(in) :: data_min, data_max, plot_bottom, plot_height
        integer, intent(inout) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=32), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        real(wp), intent(in), optional :: symlog_threshold

        call generate_axis_ticks_internal(data_min, data_max, num_ticks, plot_bottom, &
                                          plot_height, &
                                          positions, labels, scale_type, &
                                          symlog_threshold, 'y')
    end subroutine generate_y_axis_ticks

    subroutine generate_axis_ticks_internal(data_min, data_max, num_ticks, plot_start, &
                                            plot_size, &
                                            positions, labels, scale_type, &
                                            symlog_threshold, axis)
        !! Internal helper to generate axis tick positions and labels
        real(wp), intent(in) :: data_min, data_max, plot_start, plot_size
        integer, intent(inout) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=32), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        real(wp), intent(in), optional :: symlog_threshold
        character, intent(in) :: axis  ! 'x' or 'y'

        real(wp) :: tvals(MAX_TICKS)
        integer :: nt
        character(len=16) :: scale
        real(wp) :: thr
        integer :: used_ticks

        scale = 'linear'
        if (present(scale_type)) scale = scale_type
        thr = 1.0_wp
        if (present(symlog_threshold)) thr = symlog_threshold

        call compute_scale_ticks(scale, data_min, data_max, thr, tvals, nt)
        if (nt <= 0) then
            num_ticks = min(num_ticks, size(positions))
            if (num_ticks <= 0) then
                num_ticks = 0
                return
            end if
            call handle_zero_range_ticks(data_min, num_ticks, plot_start + &
                                         plot_size*0.5_wp, &
                                         positions, labels, scale)
            return
        end if

        used_ticks = min(nt, min(num_ticks, size(positions)))
        if (used_ticks <= 0) then
            num_ticks = 0
            return
        end if

        call fill_tick_positions_and_labels(tvals, nt, data_min, data_max, plot_start, &
                                            plot_size, &
                                            used_ticks, positions, labels, scale, thr)
        num_ticks = used_ticks
    end subroutine generate_axis_ticks_internal

    subroutine fill_tick_positions_and_labels(tvals, nt, data_min, data_max, &
                                              plot_start, plot_size, &
                                              num_ticks, positions, labels, &
                                              scale, threshold)
        !! Fill tick positions and labels arrays
        real(wp), intent(in) :: tvals(:), data_min, data_max, plot_start, &
                                plot_size, threshold
        integer, intent(in) :: nt, num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=32), intent(out) :: labels(:)
        character(len=*), intent(in) :: scale

        real(wp) :: min_t, max_t, tv_t
        integer :: i, limit, decimals

        min_t = apply_scale_transform(data_min, scale, threshold)
        max_t = apply_scale_transform(data_max, scale, threshold)

        decimals = 0
        if (trim(scale) == 'linear' .and. nt >= 2) then
            decimals = determine_decimals_from_ticks(tvals, nt)
        end if

        limit = min(num_ticks, size(positions))
        do i = 1, limit
            if (i > nt) exit
            tv_t = apply_scale_transform(tvals(i), scale, threshold)
            if (max_t > min_t) then
                positions(i) = plot_start + (tv_t - min_t)/(max_t - min_t)*plot_size
            else
                positions(i) = plot_start + 0.5_wp*plot_size
            end if
            if (trim(scale) == 'linear') then
                labels(i) = adjustl(format_tick_value_consistent(tvals(i), decimals))
            else
                labels(i) = adjustl(format_tick_label(tvals(i), scale))
            end if
        end do
        do i = nt + 1, limit
            labels(i) = ''
        end do
    end subroutine fill_tick_positions_and_labels

    subroutine handle_zero_range_ticks(data_value, num_ticks, center_position, &
                                       positions, labels, scale_type)
        !! Handle ticks for zero or near-zero range data
        real(wp), intent(in) :: data_value, center_position
        integer, intent(in) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=32), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type

        integer :: i

        do i = 1, num_ticks
            positions(i) = center_position
            if (present(scale_type)) then
                labels(i) = adjustl(format_tick_label(data_value, scale_type))
            else
                labels(i) = adjustl(format_tick_label(data_value, 'linear'))
            end if
        end do
    end subroutine handle_zero_range_ticks

    subroutine draw_pdf_axes_and_labels(ctx, xscale, yscale, symlog_threshold, &
                                        data_x_min, data_x_max, data_y_min, &
                                        data_y_max, title, xlabel, ylabel, &
                                        plot_area_left, plot_area_bottom, &
                                        plot_area_width, plot_area_height)
        !! Draw complete axes system with labels using actual plot area coordinates
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height

        real(wp), allocatable :: x_positions(:), y_positions(:)
        character(len=32), allocatable :: x_labels(:), y_labels(:)
        integer :: num_x_ticks, num_y_ticks
        real(wp) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj

        ! Setup data ranges and generate ticks
        call prepare_axes_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                               x_min_adj, x_max_adj, y_min_adj, y_max_adj, &
                               x_positions, y_positions, x_labels, y_labels, &
                               num_x_ticks, num_y_ticks, xscale, yscale, &
                               plot_area_left, plot_area_bottom, plot_area_width, &
                               plot_area_height, &
                               symlog_threshold)

        ! Draw axes elements
        call draw_axes_elements(ctx, x_positions, y_positions, x_labels, y_labels, &
                                num_x_ticks, num_y_ticks, title, xlabel, ylabel, &
                                plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height)
    end subroutine draw_pdf_axes_and_labels

    subroutine prepare_axes_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                                 x_min_adj, x_max_adj, y_min_adj, y_max_adj, &
                                 x_positions, y_positions, x_labels, y_labels, &
                                 num_x_ticks, num_y_ticks, xscale, yscale, &
                                 plot_area_left, plot_area_bottom, plot_area_width, &
                                 plot_area_height, &
                                 symlog_threshold)
        !! Prepare axes data ranges and generate tick positions
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        real(wp), intent(out) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=32), allocatable, intent(out) :: x_labels(:), y_labels(:)
        integer, intent(out) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height
        real(wp), intent(in), optional :: symlog_threshold

        call setup_axes_data_ranges(ctx, data_x_min, data_x_max, data_y_min, &
                                    data_y_max, &
                                    x_min_adj, x_max_adj, y_min_adj, y_max_adj, &
                                    xscale, yscale)

        call generate_tick_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                                x_positions, y_positions, x_labels, y_labels, &
                                num_x_ticks, num_y_ticks, xscale, yscale, &
                                plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height, &
                                symlog_threshold)
    end subroutine prepare_axes_data

    subroutine draw_axes_elements(ctx, x_positions, y_positions, x_labels, y_labels, &
                                  num_x_ticks, num_y_ticks, title, xlabel, ylabel, &
                                  plot_area_left, plot_area_bottom, plot_area_width, &
                                  plot_area_height)
        !! Draw axes frame, ticks, and labels
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=32), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height
        real(wp) :: max_y_tick_label_width

        ! Ensure axes are drawn in black independent of prior plot color state
        call ctx%set_color(0.0_wp, 0.0_wp, 0.0_wp)
        call ctx%set_line_width(1.0_wp)

        call draw_pdf_frame_with_area(ctx, plot_area_left, plot_area_bottom, &
                                      plot_area_width, plot_area_height)

        call draw_pdf_tick_marks_with_area(ctx, x_positions, y_positions, num_x_ticks, &
                                           num_y_ticks, &
                                           plot_area_left, plot_area_bottom)

        max_y_tick_label_width = 0.0_wp
        call draw_pdf_tick_labels_with_area(ctx, x_positions, y_positions, x_labels, &
                                            y_labels, &
                                            num_x_ticks, num_y_ticks, plot_area_left, &
                                            plot_area_bottom, &
                                            plot_area_height, &
                                            max_y_tick_label_width)

        if (present(title) .or. present(xlabel) .or. present(ylabel)) then
            call draw_pdf_title_and_labels(ctx, title, xlabel, ylabel, &
                                           plot_area_left, plot_area_bottom, &
                                           plot_area_width, plot_area_height, &
                                           max_y_tick_label_width)
        end if
    end subroutine draw_axes_elements

    subroutine draw_pdf_3d_axes_frame(ctx, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw 3D axes frame - see issue #494 for implementation roadmap
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max

        ! Reference unused arguments to keep interface stable
        associate (d1 => ctx%width, d2 => x_min, d3 => x_max, d4 => y_min, &
                   d5 => y_max, d6 => z_min, d7 => z_max)
        end associate
        ! PDF backend does not support 3D axes projection
        ! 3D plots in PDF backend fall back to 2D projections handled by the
        ! standard 2D axes drawing functions. This is consistent with many
        ! PDF plotting libraries that focus on vector graphics in 2D space.
        ! 3D visualization is better suited to raster backends (PNG) that can
        ! render projected 3D axes with proper visual depth cues.
        ! Implementation needed - see issue #494
    end subroutine draw_pdf_3d_axes_frame

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
        real(wp), intent(in) :: x_positions(:), y_positions(:)
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
        real(wp), intent(in) :: x_positions(:), y_positions(:)
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

    subroutine draw_pdf_title_and_labels(ctx, title, xlabel, ylabel, &
                                         plot_area_left, plot_area_bottom, &
                                         plot_area_width, plot_area_height, &
                                         y_tick_label_max_width)
        !! Draw plot title and axis labels
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height
        real(wp), intent(in), optional :: y_tick_label_max_width

        real(wp) :: title_x, title_y
        real(wp) :: xlabel_x, xlabel_y
        real(wp) :: ylabel_x, ylabel_y
        real(wp) :: width_pt
        real(wp) :: y_tick_w
        character(len=512) :: processed_title, processed_xlabel, processed_ylabel
        integer :: processed_len
        real(wp), parameter :: TITLE_GAP = 6.0_wp
        real(wp), parameter :: Y_TICK_GAP_LOCAL = 1.0_wp
        real(wp), parameter :: YLABEL_PAD = 1.0_wp
        real(wp), parameter :: LABEL_THICKNESS = 1.2_wp*PDF_LABEL_SIZE

        ! Draw title (centered at top)
        if (present(title)) then
            if (len_trim(title) > 0) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(title), processed_title, processed_len)

                width_pt = estimate_pdf_text_width(processed_title(1:processed_len), &
                                                   PDF_TITLE_SIZE)
                title_x = plot_area_left + 0.5_wp*plot_area_width - 0.5_wp*width_pt
                title_y = plot_area_bottom + plot_area_height + TITLE_GAP
                ! Process LaTeX commands to Unicode and render with mixed fonts
                ! Use mathtext rendering for title to handle superscripts properly
                call draw_pdf_mathtext(ctx, title_x, title_y, trim(title), &
                                       PDF_TITLE_SIZE)
            end if
        end if

        ! Draw X-axis label (centered at bottom).
        if (present(xlabel)) then
            if (len_trim(xlabel) > 0) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(xlabel), processed_xlabel, &
                                           processed_len)

                width_pt = estimate_pdf_text_width(processed_xlabel(1:processed_len), &
                                                   PDF_LABEL_SIZE)
                xlabel_x = plot_area_left + 0.5_wp*plot_area_width - 0.5_wp*width_pt
                xlabel_y = plot_area_bottom - real(XLABEL_VERTICAL_OFFSET, wp)
                call render_mixed_text(ctx, xlabel_x, xlabel_y, trim(xlabel))
            end if
        end if

        ! Draw Y-axis label (rotated on left) - anchor point is the right edge of the
        ! rotated glyphs (text extends to the left in device X).
        if (present(ylabel)) then
            if (len_trim(ylabel) > 0) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(ylabel), processed_ylabel, &
                                           processed_len)

                y_tick_w = 0.0_wp
                if (present(y_tick_label_max_width)) y_tick_w = y_tick_label_max_width

                ! Place y-label block left of the y-tick label block by a fixed padding.
                ! Account for rotated text matrix: glyphs extend left of (ylabel_x).
                ylabel_x = plot_area_left - Y_TICK_GAP_LOCAL - y_tick_w - YLABEL_PAD

                ! Vertically center rotated label: its extent along Y equals the
                ! unrotated text width in points.
                width_pt = estimate_pdf_text_width(processed_ylabel(1:processed_len), &
                                                   PDF_LABEL_SIZE)
                ylabel_y = plot_area_bottom + 0.5_wp*plot_area_height - 0.5_wp*width_pt
                call render_rotated_mixed_text(ctx, ylabel_x, ylabel_y, trim(ylabel))
            end if
        end if
    end subroutine draw_pdf_title_and_labels

    subroutine render_mixed_text(ctx, x, y, text, font_size)
        !! Helper: process LaTeX and render mixed-font text (with mathtext support)
        !! PDF needs Unicode superscripts converted to mathtext for proper rendering
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: font_size
        character(len=512) :: processed
        integer :: plen
        character(len=600) :: math_ready
        integer :: mlen

        ! For PDF, we need to handle Unicode superscripts properly
        ! The mathtext system will render them as superscripts
        call process_latex_in_text(text, processed, plen)
        ! Mirror raster backend behavior: pass trimmed text through as-is
        ! Mathtext engages only when callers supply explicit $...$ delimiters
        call prepare_mathtext_if_needed(processed(1:plen), math_ready, mlen)

        ! Route through mathtext renderer only when math segments are present
        if (has_mathtext(math_ready(1:mlen))) then
            if (present(font_size)) then
                call draw_pdf_mathtext(ctx, x, y, math_ready(1:mlen), font_size)
            else
                call draw_pdf_mathtext(ctx, x, y, math_ready(1:mlen))
            end if
        else
            if (present(font_size)) then
                call draw_mixed_font_text(ctx, x, y, processed(1:plen), font_size)
            else
                call draw_mixed_font_text(ctx, x, y, processed(1:plen))
            end if
        end if
    end subroutine render_mixed_text

    subroutine render_rotated_mixed_text(ctx, x, y, text)
        !! Helper: process LaTeX and render rotated mixed-font ylabel
        !! Now supports mathtext rendering for ylabel with $...$ delimiters
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=512) :: processed
        integer :: plen
        character(len=600) :: math_ready
        integer :: mlen

        ! Process LaTeX commands
        call process_latex_in_text(text, processed, plen)

        ! Check if mathtext is present ($...$ delimiters)
        call prepare_mathtext_if_needed(processed(1:plen), math_ready, mlen)

        if (has_mathtext(math_ready(1:mlen))) then
            ! For mathtext, we need to use a rotated mathtext renderer
            ! Since draw_pdf_mathtext doesn't support rotation, we'll use
            ! the text matrix approach with mathtext rendering
            call draw_rotated_pdf_mathtext(ctx, x, y, math_ready(1:mlen))
        else
            call draw_rotated_mixed_font_text(ctx, x, y, processed(1:plen))
        end if
    end subroutine render_rotated_mixed_text

    subroutine draw_rotated_pdf_mathtext(ctx, x, y, text)
        !! Draw rotated mathtext for ylabel
        !! Uses rotation matrix with manual text positioning for subscripts/superscripts
        use fortplot_pdf_text_segments, only: process_text_segments
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=1024) :: matrix_cmd, td_cmd
        character(len=2048) :: preprocessed_text
        integer :: processed_len
        character(len=4096) :: math_ready
        integer :: mlen
        type(mathtext_element_t), allocatable :: elements(:)
        integer :: i
        real(wp) :: elem_font_size, elem_y_offset
        real(wp) :: char_width
        integer :: j, codepoint, char_len, text_len
        logical :: in_symbol_font

        ! Process text for mathtext
        call process_latex_in_text(text, preprocessed_text, processed_len)
        call preprocess_math_text(preprocessed_text(1:processed_len), math_ready, mlen)

        ! Parse mathtext elements
        elements = parse_mathtext(math_ready(1:mlen))

        ! Begin text object with rotation matrix (90 degrees counterclockwise)
        ctx%stream_data = ctx%stream_data//'BT'//new_line('a')

        ! Set rotation matrix: [0 1 -1 0 x y] for 90-degree rotation
        write (matrix_cmd, '("0 1 -1 0 ", F0.3, 1X, F0.3, " Tm")') x, y
        ctx%stream_data = ctx%stream_data//trim(adjustl(matrix_cmd))//new_line('a')

        ! Render each mathtext element with proper font size and vertical offset
        in_symbol_font = .false.
        do i = 1, size(elements)
            if (len_trim(elements(i)%text) > 0) then
                ! Calculate element font size and vertical offset
                elem_font_size = PDF_LABEL_SIZE*elements(i)%font_size_ratio
                elem_y_offset = elements(i)%vertical_offset*PDF_LABEL_SIZE

                ! Move to position for this element using Td (relative positioning)
                ! The rotation matrix transforms these:
                ! x->forward along text, y->perpendicular.
                if (i > 1) then
                    ! Move horizontally by previous element width, vertically by
                    ! offset difference.
                    write (td_cmd, '(F0.3, 1X, F0.3, " Td")') char_width, &
                        elem_y_offset - (elements(i - 1)%vertical_offset*PDF_LABEL_SIZE)
                    ctx%stream_data = &
                        ctx%stream_data//trim(adjustl(td_cmd))//new_line('a')
                else if (abs(elem_y_offset) > 0.01_wp) then
                    ! First element with non-zero offset
                    write (td_cmd, '("0 ", F0.3, " Td")') elem_y_offset
                    ctx%stream_data = &
                        ctx%stream_data//trim(adjustl(td_cmd))//new_line('a')
                end if

                ! Set font size for this element
                write (matrix_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
                    ctx%fonts%get_helvetica_obj(), elem_font_size
                ctx%stream_data = &
                    ctx%stream_data//trim(adjustl(matrix_cmd))//new_line('a')

                ! Render text segments
                call process_text_segments(ctx, elements(i)%text, in_symbol_font, &
                                           elem_font_size)

                ! Calculate width for next element positioning
                char_width = 0.0_wp
                j = 1
                text_len = len_trim(elements(i)%text)
                do while (text_len < len(elements(i)%text))
                    if (elements(i)%text(text_len + 1:text_len + 1) == ' ') then
                        text_len = text_len + 1
                    else
                        exit
                    end if
                end do

                do while (j <= text_len)
                    char_len = utf8_char_length(elements(i)%text(j:j))
                    if (char_len == 0) then
                        codepoint = iachar(elements(i)%text(j:j))
                        char_len = 1
                    else
                        codepoint = utf8_to_codepoint(elements(i)%text, j)
                    end if

                    if (codepoint >= 48 .and. codepoint <= 57) then
                        char_width = char_width + elem_font_size*0.55_wp
                    else if (codepoint >= 65 .and. codepoint <= 90) then
                        char_width = char_width + elem_font_size*0.65_wp
                    else if (codepoint >= 97 .and. codepoint <= 122) then
                        char_width = char_width + elem_font_size*0.5_wp
                    else if (codepoint == 32) then
                        char_width = char_width + elem_font_size*0.3_wp
                    else
                        char_width = char_width + elem_font_size*0.5_wp
                    end if

                    j = j + char_len
                end do
            end if
        end do

        ctx%stream_data = ctx%stream_data//'ET'//new_line('a')
    end subroutine draw_rotated_pdf_mathtext

end module fortplot_pdf_axes
