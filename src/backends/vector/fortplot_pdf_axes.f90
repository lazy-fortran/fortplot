module fortplot_pdf_axes
    !! PDF axes, grid, tick drawing, and plot frame operations
    !!
    !! Thin coordinator module that delegates to specialized sub-modules:
    !! - fortplot_pdf_axes_tick_data: tick position/label computation
    !! - fortplot_pdf_axes_drawing: frame, tick marks, minor ticks
    !! - fortplot_pdf_axes_text: title/label text rendering with mathtext

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
    ! Sub-modules
    use fortplot_pdf_axes_tick_data, only: initialize_tick_arrays, &
                                           generate_x_axis_ticks, &
                                           generate_y_axis_ticks, &
                                           apply_custom_axis_ticks, &
                                           generate_axis_ticks_internal, &
                                           subsample_ticks, &
                                           fill_tick_positions_and_labels, &
                                           handle_zero_range_ticks
    use fortplot_pdf_axes_drawing, only: draw_pdf_frame_with_area, &
                                         draw_pdf_tick_marks_with_area, &
                                         draw_pdf_tick_labels_with_area, &
                                         draw_pdf_minor_tick_marks
    use fortplot_pdf_axes_text, only: draw_pdf_title_and_labels, &
                                      render_mixed_text, &
                                      render_rotated_mixed_text
    implicit none
    private

    ! Public procedures (re-exported from sub-modules)
    public :: draw_pdf_axes_and_labels
    public :: draw_pdf_frame_with_area
    public :: draw_pdf_tick_marks_with_area
    public :: draw_pdf_tick_labels_with_area
    public :: draw_pdf_title_and_labels
    public :: setup_axes_data_ranges
    public :: generate_tick_data
    public :: render_mixed_text
    public :: draw_pdf_minor_tick_marks

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
                                   x_date_format, y_date_format, &
                                   plot_area_left, plot_area_bottom, plot_area_width, &
                                   plot_area_height, &
                                   symlog_threshold, &
                                   custom_xticks, custom_xtick_labels, &
                                   custom_yticks, custom_ytick_labels)
        !! Generate tick positions and labels for axes
        type(pdf_context_core), intent(in) :: ctx
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=50), allocatable, intent(out) :: x_labels(:), y_labels(:)
        integer, intent(out) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: xscale, yscale
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)

        ! Calculate number of ticks and allocate arrays
        call initialize_tick_arrays(plot_area_width, plot_area_height, num_x_ticks, &
                                    num_y_ticks, &
                                    x_positions, y_positions, x_labels, y_labels)

        ! Generate X axis ticks
        call generate_x_axis_ticks(data_x_min, data_x_max, num_x_ticks, &
                                   plot_area_left, &
                                   plot_area_width, x_positions, x_labels, xscale, &
                                   date_format=x_date_format, &
                                   symlog_threshold=symlog_threshold, &
                                   custom_xticks=custom_xticks, &
                                   custom_xtick_labels=custom_xtick_labels)

        ! Generate Y axis ticks
        call generate_y_axis_ticks(data_y_min, data_y_max, num_y_ticks, &
                                   plot_area_bottom, &
                                   plot_area_height, y_positions, y_labels, yscale, &
                                   date_format=y_date_format, &
                                   symlog_threshold=symlog_threshold, &
                                   custom_yticks=custom_yticks, &
                                   custom_ytick_labels=custom_ytick_labels)

    end subroutine generate_tick_data

    subroutine draw_pdf_axes_and_labels(ctx, xscale, yscale, symlog_threshold, &
                                        data_x_min, data_x_max, data_y_min, &
                                        data_y_max, title, xlabel, ylabel, &
                                        x_date_format, y_date_format, &
                                        plot_area_left, plot_area_bottom, &
                                        plot_area_width, plot_area_height, &
                                        custom_xticks, custom_xtick_labels, &
                                        custom_yticks, custom_ytick_labels)
        !! Draw complete axes system with labels using actual plot area coordinates
        type(pdf_context_core), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)

        real(wp), allocatable :: x_positions(:), y_positions(:)
        character(len=50), allocatable :: x_labels(:), y_labels(:)
        integer :: num_x_ticks, num_y_ticks
        real(wp) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj

        ! Setup data ranges and generate ticks
        call prepare_axes_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                               x_min_adj, x_max_adj, y_min_adj, y_max_adj, &
                               x_positions, y_positions, x_labels, y_labels, &
                               num_x_ticks, num_y_ticks, xscale, yscale, &
                               x_date_format, y_date_format, &
                               plot_area_left, plot_area_bottom, plot_area_width, &
                               plot_area_height, &
                               symlog_threshold, &
                               custom_xticks=custom_xticks, &
                               custom_xtick_labels=custom_xtick_labels, &
                               custom_yticks=custom_yticks, &
                               custom_ytick_labels=custom_ytick_labels)

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
                                 x_date_format, y_date_format, &
                                 plot_area_left, plot_area_bottom, plot_area_width, &
                                 plot_area_height, &
                                 symlog_threshold, &
                                 custom_xticks, custom_xtick_labels, &
                                 custom_yticks, custom_ytick_labels)
        !! Prepare axes data ranges and generate tick positions
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), intent(in) :: data_x_min, data_x_max, data_y_min, data_y_max
        real(wp), intent(out) :: x_min_adj, x_max_adj, y_min_adj, y_max_adj
        real(wp), allocatable, intent(out) :: x_positions(:), y_positions(:)
        character(len=50), allocatable, intent(out) :: x_labels(:), y_labels(:)
        integer, intent(out) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: xscale, yscale
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in) :: plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)

        call setup_axes_data_ranges(ctx, data_x_min, data_x_max, data_y_min, &
                                    data_y_max, &
                                    x_min_adj, x_max_adj, y_min_adj, y_max_adj, &
                                    xscale, yscale)

        call generate_tick_data(ctx, data_x_min, data_x_max, data_y_min, data_y_max, &
                                x_positions, y_positions, x_labels, y_labels, &
                                num_x_ticks, num_y_ticks, xscale, yscale, &
                                x_date_format, y_date_format, &
                                plot_area_left, plot_area_bottom, plot_area_width, &
                                plot_area_height, &
                                symlog_threshold, &
                                custom_xticks=custom_xticks, &
                                custom_xtick_labels=custom_xtick_labels, &
                                custom_yticks=custom_yticks, &
                                custom_ytick_labels=custom_ytick_labels)
    end subroutine prepare_axes_data

    subroutine draw_axes_elements(ctx, x_positions, y_positions, x_labels, y_labels, &
                                  num_x_ticks, num_y_ticks, title, xlabel, ylabel, &
                                  plot_area_left, plot_area_bottom, plot_area_width, &
                                  plot_area_height)
        !! Draw axes frame, ticks, and labels
        type(pdf_context_core), intent(inout) :: ctx
        real(wp), contiguous, intent(in) :: x_positions(:), y_positions(:)
        character(len=50), intent(in) :: x_labels(:), y_labels(:)
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

end module fortplot_pdf_axes
