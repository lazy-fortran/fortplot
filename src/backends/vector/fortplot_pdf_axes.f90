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
                                draw_pdf_mathtext
    use fortplot_latex_parser, only: process_latex_in_text
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

    subroutine setup_axes_data_ranges(ctx, x_min_orig, x_max_orig, y_min_orig, y_max_orig, &
                                     x_min_adj, x_max_adj, y_min_adj, y_max_adj, xscale, yscale)
        !! Set up data ranges for axes with optional log scaling
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        real(wp), intent(out) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj
        character(len=*), intent(in), optional :: xscale, yscale

        real(wp) :: x_range, y_range
        associate(dummy_ctx => ctx%width); end associate

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
                                 plot_area_left, plot_area_bottom, plot_area_width, plot_area_height, &
                                 symlog_threshold)
        !! Generate tick positions and labels for axes
        !! Refactored to be under 100 lines (QADS compliance)
        type(pdf_context_core), intent(in) :: ctx
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=32), allocatable, intent(out) :: x_labels(:), y_labels(:)
        integer, intent(out) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, plot_area_height
        real(wp), intent(in), optional :: symlog_threshold

        associate(dummy_ctx => ctx%width); end associate
        ! Calculate number of ticks and allocate arrays
        call initialize_tick_arrays(plot_area_width, plot_area_height, num_x_ticks, num_y_ticks, &
                                   x_positions, y_positions, x_labels, y_labels)

        ! Generate X axis ticks
        call generate_x_axis_ticks(data_x_min, data_x_max, num_x_ticks, plot_area_left, &
                                  plot_area_width, x_positions, x_labels, xscale, symlog_threshold)

        ! Generate Y axis ticks
        call generate_y_axis_ticks(data_y_min, data_y_max, num_y_ticks, plot_area_bottom, &
                                  plot_area_height, y_positions, y_labels, yscale, symlog_threshold)

    end subroutine generate_tick_data

    subroutine initialize_tick_arrays(plot_width, plot_height, num_x_ticks, num_y_ticks, &
                                     x_positions, y_positions, x_labels, y_labels)
        !! Initialize tick count and allocate arrays
        real(wp), intent(in) :: plot_width, plot_height
        integer, intent(out) :: num_x_ticks, num_y_ticks
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=32), allocatable, intent(out) :: x_labels(:), y_labels(:)

        integer, parameter :: TARGET_TICKS = 8

        ! Determine number of ticks using plot area dimensions
        num_x_ticks = min(TARGET_TICKS, max(2, int(plot_width / 50.0_wp)))
        num_y_ticks = min(TARGET_TICKS, max(2, int(plot_height / 40.0_wp)))

        ! Allocate arrays
        allocate(x_positions(num_x_ticks))
        allocate(y_positions(num_y_ticks))
        allocate(x_labels(num_x_ticks))
        allocate(y_labels(num_y_ticks))
    end subroutine initialize_tick_arrays

    subroutine generate_x_axis_ticks(data_min, data_max, num_ticks, plot_left, plot_width, &
                                    positions, labels, scale_type, symlog_threshold)
        !! Generate X axis tick positions and labels
        real(wp), intent(in) :: data_min, data_max, plot_left, plot_width
        integer, intent(in) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=32), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        real(wp), intent(in), optional :: symlog_threshold

        call generate_axis_ticks_internal(data_min, data_max, num_ticks, plot_left, plot_width, &
                                        positions, labels, scale_type, symlog_threshold, 'x')
    end subroutine generate_x_axis_ticks

    subroutine generate_y_axis_ticks(data_min, data_max, num_ticks, plot_bottom, plot_height, &
                                    positions, labels, scale_type, symlog_threshold)
        !! Generate Y axis tick positions and labels
        real(wp), intent(in) :: data_min, data_max, plot_bottom, plot_height
        integer, intent(in) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=32), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        real(wp), intent(in), optional :: symlog_threshold

        call generate_axis_ticks_internal(data_min, data_max, num_ticks, plot_bottom, plot_height, &
                                        positions, labels, scale_type, symlog_threshold, 'y')
    end subroutine generate_y_axis_ticks

    subroutine generate_axis_ticks_internal(data_min, data_max, num_ticks, plot_start, plot_size, &
                                          positions, labels, scale_type, symlog_threshold, axis)
        !! Internal helper to generate axis tick positions and labels
        real(wp), intent(in) :: data_min, data_max, plot_start, plot_size
        integer, intent(in) :: num_ticks
        real(wp), intent(out) :: positions(:)
        character(len=32), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale_type
        real(wp), intent(in), optional :: symlog_threshold
        character, intent(in) :: axis  ! 'x' or 'y'

        real(wp) :: tvals(MAX_TICKS)
        integer :: nt
        character(len=16) :: scale
        real(wp) :: thr

        scale = 'linear'
        if (present(scale_type)) scale = scale_type
        thr = 1.0_wp
        if (present(symlog_threshold)) thr = symlog_threshold

        call compute_scale_ticks(scale, data_min, data_max, thr, tvals, nt)
        if (nt <= 0) then
            call handle_zero_range_ticks(data_min, num_ticks, plot_start + plot_size * 0.5_wp, &
                                        positions, labels, scale_type)
            return
        end if

        call fill_tick_positions_and_labels(tvals, nt, data_min, data_max, plot_start, plot_size, &
                                           num_ticks, positions, labels, scale, thr)
    end subroutine generate_axis_ticks_internal

    subroutine fill_tick_positions_and_labels(tvals, nt, data_min, data_max, plot_start, plot_size, &
                                             num_ticks, positions, labels, scale, threshold)
        !! Fill tick positions and labels arrays
        real(wp), intent(in) :: tvals(:), data_min, data_max, plot_start, plot_size, threshold
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
                positions(i) = plot_start + (tv_t - min_t) / (max_t - min_t) * plot_size
            else
                positions(i) = plot_start + 0.5_wp * plot_size
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
                                       data_x_min, data_x_max, data_y_min, data_y_max, &
                                       title, xlabel, ylabel, plot_area_left, plot_area_bottom, &
                                       plot_area_width, plot_area_height, canvas_height)
        !! Draw complete axes system with labels using actual plot area coordinates
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, plot_area_height, canvas_height

        real(wp), allocatable :: x_positions(:), y_positions(:)
        character(len=32), allocatable :: x_labels(:), y_labels(:)
        integer :: num_x_ticks, num_y_ticks
        real(wp) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj

        ! Setup data ranges and generate ticks
        call prepare_axes_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                             x_min_adj, x_max_adj, y_min_adj, y_max_adj, &
                             x_positions, y_positions, x_labels, y_labels, &
                             num_x_ticks, num_y_ticks, xscale, yscale, &
                             plot_area_left, plot_area_bottom, plot_area_width, plot_area_height, &
                             symlog_threshold)

        ! Draw axes elements
        call draw_axes_elements(ctx, x_positions, y_positions, x_labels, y_labels, &
                              num_x_ticks, num_y_ticks, title, xlabel, ylabel, &
                              plot_area_left, plot_area_bottom, plot_area_width, plot_area_height, &
                              canvas_height)
    end subroutine draw_pdf_axes_and_labels

    subroutine prepare_axes_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                                x_min_adj, x_max_adj, y_min_adj, y_max_adj, &
                                x_positions, y_positions, x_labels, y_labels, &
                                num_x_ticks, num_y_ticks, xscale, yscale, &
                                plot_area_left, plot_area_bottom, plot_area_width, plot_area_height, &
                                symlog_threshold)
        !! Prepare axes data ranges and generate tick positions
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        real(wp), intent(out) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=32), allocatable, intent(out) :: x_labels(:), y_labels(:)
        integer, intent(out) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, plot_area_height
        real(wp), intent(in), optional :: symlog_threshold

        call setup_axes_data_ranges(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                                   x_min_adj, x_max_adj, y_min_adj, y_max_adj, xscale, yscale)

        call generate_tick_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                               x_positions, y_positions, x_labels, y_labels, &
                               num_x_ticks, num_y_ticks, xscale, yscale, &
                               plot_area_left, plot_area_bottom, plot_area_width, plot_area_height, &
                               symlog_threshold)
    end subroutine prepare_axes_data

    subroutine draw_axes_elements(ctx, x_positions, y_positions, x_labels, y_labels, &
                                 num_x_ticks, num_y_ticks, title, xlabel, ylabel, &
                                 plot_area_left, plot_area_bottom, plot_area_width, plot_area_height, &
                                 canvas_height)
        !! Draw axes frame, ticks, and labels
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=32), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, plot_area_height, canvas_height

        ! Ensure axes are drawn in black independent of prior plot color state
        call ctx%set_color(0.0_wp, 0.0_wp, 0.0_wp)
        call ctx%set_line_width(1.0_wp)

        call draw_pdf_frame_with_area(ctx, plot_area_left, plot_area_bottom, &
                                     plot_area_width, plot_area_height, canvas_height)

        call draw_pdf_tick_marks_with_area(ctx, x_positions, y_positions, num_x_ticks, num_y_ticks, &
                                          plot_area_left, plot_area_bottom, canvas_height)

        call draw_pdf_tick_labels_with_area(ctx, x_positions, y_positions, x_labels, y_labels, &
                                           num_x_ticks, num_y_ticks, plot_area_left, plot_area_bottom, canvas_height)

        if (present(title) .or. present(xlabel) .or. present(ylabel)) then
            call draw_pdf_title_and_labels(ctx, title, xlabel, ylabel, &
                                      plot_area_left, plot_area_bottom, &
                                      plot_area_width, plot_area_height)
        end if
    end subroutine draw_axes_elements

    subroutine draw_pdf_3d_axes_frame(ctx, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw 3D axes frame - see issue #494 for implementation roadmap
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max

        ! Reference unused arguments to keep interface stable
        associate(d1=>ctx%width, d2=>x_min, d3=>x_max, d4=>y_min, d5=>y_max, d6=>z_min, d7=>z_max); end associate
        ! PDF backend does not support 3D axes projection
        ! 3D plots in PDF backend fall back to 2D projections handled by the
        ! standard 2D axes drawing functions. This is consistent with many
        ! PDF plotting libraries that focus on vector graphics in 2D space.
        ! 3D visualization is better suited to raster backends (PNG) that can
        ! render projected 3D axes with proper visual depth cues.
        ! Implementation needed - see issue #494
    end subroutine draw_pdf_3d_axes_frame

    subroutine draw_pdf_frame_with_area(ctx, plot_left, plot_bottom, plot_width, plot_height, canvas_height)
        !! Draw the plot frame using actual plot area coordinates (FIXED version)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: plot_left, plot_bottom, plot_width, plot_height, canvas_height
        character(len=2048) :: frame_cmd
        real(wp) :: x1, y1
        associate(dch=>canvas_height); end associate

        ! PDF coordinates: Y=0 at bottom (same as our data coordinates)
        x1 = plot_left
        y1 = plot_bottom  ! No conversion needed - PDF Y=0 is at bottom

        ! Force solid stroke for the frame regardless of prior dash settings
        ctx%stream_data = ctx%stream_data // '[] 0 d' // new_line('a')

        ! Draw rectangle frame
        write(frame_cmd, '(F0.3, 1X, F0.3, " ", F0.3, 1X, F0.3, " re S")') &
            x1, y1, plot_width, plot_height
        ctx%stream_data = ctx%stream_data // trim(adjustl(frame_cmd)) // new_line('a')
    end subroutine draw_pdf_frame_with_area


    subroutine draw_pdf_tick_marks_with_area(ctx, x_positions, y_positions, num_x, num_y, &
                                           plot_left, plot_bottom, canvas_height)
        !! Draw tick marks using actual plot area coordinates (FIXED version)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x, num_y
        real(wp), intent(in) :: plot_left, plot_bottom, canvas_height

        integer :: i
        character(len=2048) :: tick_cmd
        real(wp) :: tick_length, bottom_y
        associate(dch=>canvas_height); end associate

        ! Ensure tick marks are stroked in black and solid regardless of prior drawing state
        call ctx%set_color(0.0_wp, 0.0_wp, 0.0_wp)
        call ctx%set_line_width(1.0_wp)
        ctx%stream_data = ctx%stream_data // '[] 0 d' // new_line('a')

        tick_length = PDF_TICK_SIZE
        bottom_y = plot_bottom  ! PDF Y=0 is at bottom, no conversion needed

        ! Draw X-axis ticks (bottom of plot area) — draw inward into frame
        do i = 1, num_x
            write(tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                x_positions(i), bottom_y, &
                x_positions(i), bottom_y + tick_length
            ctx%stream_data = ctx%stream_data // trim(adjustl(tick_cmd)) // new_line('a')
        end do

        ! Draw Y-axis ticks (left side of plot area) — draw inward into frame
        do i = 1, num_y
            write(tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                plot_left, y_positions(i), &
                plot_left + tick_length, y_positions(i)
            ctx%stream_data = ctx%stream_data // trim(adjustl(tick_cmd)) // new_line('a')
        end do
    end subroutine draw_pdf_tick_marks_with_area


    subroutine draw_pdf_tick_labels_with_area(ctx, x_positions, y_positions, x_labels, y_labels, &
                                            num_x, num_y, plot_left, plot_bottom, canvas_height)
        !! Draw tick labels using actual plot area coordinates (FIXED version)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=*), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x, num_y
        real(wp), intent(in) :: plot_left, plot_bottom, canvas_height

        integer :: i
        real(wp) :: label_x, label_y, bottom_y
        real(wp), parameter :: TICK_CHAR_W = 6.0_wp   ! Approximate glyph width at PDF_TICK_LABEL_SIZE
        real(wp), parameter :: X_TICK_GAP = 15.0_wp   ! Distance below plot for X tick labels
        real(wp), parameter :: Y_TICK_GAP = 19.0_wp   ! Distance left of plot edge to end of Y tick labels

        associate(dch=>canvas_height); end associate
        bottom_y = plot_bottom  ! PDF Y=0 is at bottom, no conversion needed

        ! Draw X-axis labels (center-aligned under each tick)
        do i = 1, num_x
            label_x = x_positions(i) - 0.5_wp * TICK_CHAR_W * real(len_trim(x_labels(i)), wp)
            label_y = bottom_y - X_TICK_GAP
            call draw_pdf_text(ctx, label_x, label_y, trim(x_labels(i)))
        end do

        ! Draw Y-axis labels with overlap detection (right-aligned to end at plot_left - Y_TICK_GAP)
        call draw_pdf_y_labels_with_overlap_detection(ctx, y_positions, y_labels, num_y, &
                                                     plot_left - Y_TICK_GAP, 0.0_wp)
    end subroutine draw_pdf_tick_labels_with_area

    subroutine draw_pdf_title_and_labels(ctx, title, xlabel, ylabel, &
                                         plot_area_left, plot_area_bottom, &
                                         plot_area_width, plot_area_height)
        !! Draw plot title and axis labels
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, plot_area_height

        real(wp) :: title_x, title_y
        real(wp) :: xlabel_x, xlabel_y
        real(wp) :: ylabel_x, ylabel_y
        character(len=512) :: processed_title, processed_xlabel, processed_ylabel
        integer :: processed_len

        ! Draw title (centered at top)
        if (present(title)) then
            if (len_trim(title) > 0) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(title), processed_title, processed_len)
                
                title_x = plot_area_left + plot_area_width * 0.5_wp - &
                         real(processed_len, wp) * 3.5_wp
                title_y = plot_area_bottom + plot_area_height + 20.0_wp
                ! Process LaTeX commands to Unicode and render with mixed fonts
                call render_mixed_text(ctx, title_x, title_y, trim(title), PDF_TITLE_SIZE)
            end if
        end if

        ! Draw X-axis label (centered at bottom) - use consistent offset below plot bottom
        if (present(xlabel)) then
            if (len_trim(xlabel) > 0) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(xlabel), processed_xlabel, processed_len)
                
                xlabel_x = plot_area_left + plot_area_width * 0.5_wp - &
                          real(processed_len, wp) * 3.0_wp
                xlabel_y = plot_area_bottom - real(XLABEL_VERTICAL_OFFSET, wp)
                call render_mixed_text(ctx, xlabel_x, xlabel_y, trim(xlabel))
            end if
        end if

        ! Draw Y-axis label (rotated on left) - closer to frame inside left margin
        if (present(ylabel)) then
            if (len_trim(ylabel) > 0) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(ylabel), processed_ylabel, processed_len)
                
                ! Place y-label 45px left of plot frame (reduced from 60px)
                ylabel_x = plot_area_left - 45.0_wp
                ylabel_y = plot_area_bottom + plot_area_height * 0.5_wp - &
                          real(processed_len, wp) * 3.0_wp
                call render_rotated_mixed_text(ctx, ylabel_x, ylabel_y, trim(ylabel))
            end if
        end if
    end subroutine draw_pdf_title_and_labels

    subroutine render_mixed_text(ctx, x, y, text, font_size)
        !! Helper: process LaTeX and render mixed-font text (with mathtext support)
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: font_size
        character(len=512) :: processed
        integer :: plen

        ! ALWAYS process LaTeX commands first to convert to Unicode
        call process_latex_in_text(text, processed, plen)
        
        ! Now check if the processed text contains mathematical notation
        if (index(processed(1:plen), '^') > 0 .or. index(processed(1:plen), '_') > 0) then
            ! Use mathtext rendering for superscripts/subscripts on the processed text
            if (present(font_size)) then
                call draw_pdf_mathtext(ctx, x, y, processed(1:plen), font_size)
            else
                call draw_pdf_mathtext(ctx, x, y, processed(1:plen))
            end if
        else
            ! Use regular mixed-font rendering for the processed text
            if (present(font_size)) then
                call draw_mixed_font_text(ctx, x, y, processed(1:plen), font_size)
            else
                call draw_mixed_font_text(ctx, x, y, processed(1:plen))
            end if
        end if
    end subroutine render_mixed_text

    subroutine render_rotated_mixed_text(ctx, x, y, text)
        !! Helper: process LaTeX and render rotated mixed-font ylabel
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=512) :: processed
        integer :: plen

        call process_latex_in_text(text, processed, plen)
        call draw_rotated_mixed_font_text(ctx, x, y, processed(1:plen))
    end subroutine render_rotated_mixed_text

    subroutine draw_pdf_y_labels_with_overlap_detection(ctx, y_positions, y_labels, num_y, plot_left, canvas_height)
        !! Draw Y-axis labels with overlap detection to prevent clustering
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: y_positions(:)
        character(len=*), intent(in) :: y_labels(:)
        integer, intent(in) :: num_y
        real(wp), intent(in) :: plot_left, canvas_height

        real(wp) :: last_y_drawn
        real(wp) :: min_spacing
        integer :: i
        real(wp) :: label_x, label_y
        character(len=512) :: processed_label
        integer :: processed_len
        associate(dch=>canvas_height); end associate

        min_spacing = 15.0_wp  ! Minimum vertical spacing between labels
        last_y_drawn = -1000.0_wp  ! Initialize to ensure first label is drawn

        do i = 1, num_y
            label_y = y_positions(i) - 3.0_wp  ! PDF Y=0 is at bottom, no conversion needed

            ! Only draw if sufficient spacing from last label
            if (abs(label_y - last_y_drawn) >= min_spacing) then
                ! Process LaTeX commands for accurate width calculation
                call process_latex_in_text(trim(y_labels(i)), processed_label, processed_len)
                label_x = plot_left - real(processed_len, wp) * 5.0_wp
                call draw_pdf_text(ctx, label_x, label_y, trim(y_labels(i)))
                last_y_drawn = label_y
            end if
        end do
    end subroutine draw_pdf_y_labels_with_overlap_detection

end module fortplot_pdf_axes
