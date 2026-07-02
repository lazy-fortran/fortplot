module fortplot_ascii_secondary_axes
    !! ASCII backend secondary axis rendering
    !!
    !! Handles twin-axes tick labels for the ASCII backend.
    !! Separated from the rendering pipeline to keep module size compliant.
    !!
    !! Secondary labels are placed in reserved bands outside the primary data
    !! area: the secondary y-axis labels go in the right-hand columns beyond the
    !! data plot area, and the secondary x-axis (top) labels go in the reserved
    !! rows above it (issue #2066). The reservation itself shrinks the ASCII
    !! plot area before data is drawn, so labels never overwrite plotted glyphs
    !! or the axis spines.

    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                         format_tick_value_consistent
    use fortplot_ascii, only: ascii_context
    use fortplot_ascii_mathtext, only: sanitize_ascii_text
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: ascii_draw_secondary_y_axis, ascii_draw_secondary_x_axis_top
    public :: ascii_secondary_y_tick_width

    integer, parameter :: SECONDARY_TICK_PAD = 1

contains

    integer function ascii_secondary_y_tick_width(yscale, symlog_threshold, &
                                                  y_min, y_max, date_format) &
        result(width)
        !! Width in character cells needed to hold the widest secondary y tick
        !! label, plus one column of padding from the data area. Used to reserve
        !! the right-hand band before data is drawn.
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: y_min, y_max
        character(len=*), intent(in), optional :: date_format

        real(wp) :: positions(MAX_TICKS)
        character(len=50) :: labels(MAX_TICKS)
        integer :: num, max_len

        call compute_secondary_y_labels(yscale, symlog_threshold, y_min, y_max, &
                                        date_format, positions, labels, num, max_len)
        if (num <= 0 .or. max_len <= 0) then
            width = 0
        else
            width = max_len + SECONDARY_TICK_PAD
        end if
    end function ascii_secondary_y_tick_width

    subroutine compute_secondary_y_labels(yscale, symlog_threshold, y_min, y_max, &
                                          date_format, positions, labels, num, max_len)
        !! Compute secondary y tick positions and their rendered labels once so
        !! the reserved band width and the drawn labels stay consistent.
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: y_min, y_max
        character(len=*), intent(in), optional :: date_format
        real(wp), intent(out) :: positions(MAX_TICKS)
        character(len=50), intent(out) :: labels(MAX_TICKS)
        integer, intent(out) :: num, max_len

        integer :: decimals, i, sanitized_len
        character(len=50) :: raw_label
        character(len=500) :: sanitized

        labels = ''
        max_len = 0
        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, positions, num)
        if (num <= 0) return

        decimals = 0
        if (trim(yscale) == 'linear' .and. num >= 2) then
            decimals = determine_decimals_from_ticks(positions, num)
        end if

        do i = 1, num
            if (trim(yscale) == 'linear') then
                raw_label = format_tick_value_consistent(positions(i), decimals)
            else
                raw_label = format_tick_label(positions(i), yscale, &
                    date_format=date_format, data_min=y_min, data_max=y_max)
            end if
            ! Store the ASCII-rendered form so the reserved band width matches
            ! what actually prints (LaTeX markup is stripped on render).
            call sanitize_ascii_text(trim(raw_label), sanitized, sanitized_len)
            labels(i) = sanitized(1:min(sanitized_len, len(labels(i))))
            max_len = max(max_len, len_trim(labels(i)))
        end do
    end subroutine compute_secondary_y_labels

    subroutine ascii_draw_secondary_x_axis_top(backend, xscale, symlog_threshold, &
                                               x_min, x_max, xlabel, date_format)
        !! Draw top x-axis tick labels for ASCII backend in the reserved top band
        type(ascii_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max
        character(len=:), allocatable, intent(in), optional :: xlabel
        character(len=*), intent(in), optional :: date_format

        real(wp) :: x_tick_positions(MAX_TICKS)
        character(len=50) :: tick_label
        integer :: num_x_ticks, decimals, i
        integer :: text_x, tick_row, label_row, start_col, label_len
        integer :: left, width, inner
        real(wp) :: frac

        call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, &
                                 x_tick_positions, num_x_ticks)
        if (num_x_ticks <= 0) return
        if (x_max <= x_min) return

        decimals = 0
        if (trim(xscale) == 'linear' .and. num_x_ticks >= 2) then
            decimals = determine_decimals_from_ticks(x_tick_positions, num_x_ticks)
        end if

        left = backend%plot_area%left
        width = max(1, backend%plot_area%width)
        inner = max(1, width - 2)

        tick_row = max(1, backend%plot_area%bottom)
        label_row = 1

        do i = 1, num_x_ticks
            if (trim(xscale) == 'linear') then
                tick_label = format_tick_value_consistent(x_tick_positions(i), decimals)
            else
                tick_label = format_tick_label(x_tick_positions(i), xscale, &
                    date_format=date_format, data_min=x_min, data_max=x_max)
            end if

            frac = (x_tick_positions(i) - x_min)/(x_max - x_min)
            text_x = left + 1 + nint(frac*real(inner, wp))
            label_len = len_trim(tick_label)
            start_col = text_x - label_len/2
            start_col = max(left + 1, min(start_col, left + width - label_len))
            start_col = max(1, min(start_col, backend%plot_width - label_len))

            call backend%text(real(start_col, wp), real(tick_row, wp), &
                              trim(tick_label))
        end do

        ! Split the optional guards: .and. does not short-circuit, so allocated()
        ! and len_trim() must not be reached when xlabel is absent.
        if (present(xlabel)) then
            if (allocated(xlabel)) then
                if (len_trim(xlabel) > 0) then
                    label_len = len_trim(xlabel)
                    start_col = left + max(1, (width - label_len)/2)
                    start_col = max(1, min(start_col, backend%plot_width - label_len))
                    call backend%text(real(start_col, wp), real(label_row, wp), &
                                      trim(xlabel))
                end if
            end if
        end if
    end subroutine ascii_draw_secondary_x_axis_top

    subroutine ascii_draw_secondary_y_axis(backend, yscale, symlog_threshold, &
                                           y_min, y_max, ylabel, date_format)
        !! Draw right y-axis tick labels for ASCII backend in the reserved band
        !! to the right of the primary data area.
        type(ascii_context), intent(inout) :: backend
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: y_min, y_max
        character(len=:), allocatable, intent(in), optional :: ylabel
        character(len=*), intent(in), optional :: date_format

        real(wp) :: y_tick_positions(MAX_TICKS)
        character(len=50) :: labels(MAX_TICKS)
        integer :: num_y_ticks, i, max_len
        integer :: text_x, text_y, label_len
        integer :: bottom, height, band_x

        ! ylabel is part of the shared secondary-axis interface; the ASCII
        ! backend renders it via the figure ylabel line, not the tick band.
        ! Guard present/allocated separately (.and. does not short-circuit) and
        ! associate an integer: flang rejects associating the bare deferred-
        ! length optional dummy in a specification expression.
        if (present(ylabel)) then
            if (allocated(ylabel)) then
                associate (unused => len_trim(ylabel)); end associate
            end if
        end if

        call compute_secondary_y_labels(yscale, symlog_threshold, y_min, y_max, &
                                        date_format, y_tick_positions, labels, &
                                        num_y_ticks, max_len)
        if (num_y_ticks <= 0) return
        if (y_max <= y_min) return

        bottom = backend%plot_area%bottom
        height = max(1, backend%plot_area%height)
        band_x = backend%plot_area%left + backend%plot_area%width + SECONDARY_TICK_PAD

        do i = 1, num_y_ticks
            label_len = len_trim(labels(i))
            if (label_len <= 0) cycle

            text_y = bottom + height - 1 - &
                     nint((y_tick_positions(i) - y_min)/(y_max - y_min)* &
                     real(max(1, height - 2), wp))
            text_y = max(bottom + 1, min(text_y, bottom + height - 1))
            text_y = max(1, min(text_y, backend%plot_height))

            text_x = min(band_x, backend%plot_width - label_len)
            text_x = max(1, text_x)

            call backend%text(real(text_x, wp), real(text_y, wp), trim(labels(i)))
        end do
    end subroutine ascii_draw_secondary_y_axis

end module fortplot_ascii_secondary_axes
