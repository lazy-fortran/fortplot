module fortplot_ascii_secondary_axes
    !! ASCII backend secondary axis rendering
    !!
    !! Handles twin-axes tick labels for the ASCII backend.
    !! Separated from the rendering pipeline to keep module size compliant.

    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                         format_tick_value_consistent
    use fortplot_ascii, only: ascii_context
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: ascii_draw_secondary_y_axis, ascii_draw_secondary_x_axis_top

contains

    subroutine ascii_draw_secondary_x_axis_top(backend, xscale, symlog_threshold, &
                                               x_min, x_max, xlabel, date_format)
        !! Draw top x-axis tick labels for ASCII backend
        type(ascii_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max
        character(len=:), allocatable, intent(in), optional :: xlabel
        character(len=*), intent(in), optional :: date_format

        real(wp) :: x_tick_positions(MAX_TICKS)
        character(len=50) :: tick_label
        integer :: num_x_ticks, decimals, i
        integer :: text_x, text_y, label_len
        real(wp) :: frac

        call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, &
                                 x_tick_positions, num_x_ticks)
        if (num_x_ticks <= 0) return

        decimals = 0
        if (trim(xscale) == 'linear' .and. num_x_ticks >= 2) then
            decimals = determine_decimals_from_ticks(x_tick_positions, num_x_ticks)
        end if

        text_y = 1

        do i = 1, num_x_ticks
            if (trim(xscale) == 'linear') then
                tick_label = format_tick_value_consistent(x_tick_positions(i), decimals)
            else
                tick_label = format_tick_label(x_tick_positions(i), xscale, &
                    date_format=date_format, data_min=x_min, data_max=x_max)
            end if

            frac = (x_tick_positions(i) - x_min) / (x_max - x_min)
            text_x = nint(frac * real(backend%plot_width - 2, wp)) + 1
            text_x = max(1, min(text_x, backend%plot_width - 1))
            label_len = len_trim(tick_label)
            text_x = max(1, min(text_x, backend%plot_width - label_len))

            call backend%text(real(text_x, wp), real(text_y, wp), trim(tick_label))
        end do

        if (present(xlabel) .and. allocated(xlabel) .and. len_trim(xlabel) > 0) then
            call backend%text(real(backend%plot_width / 2, wp), real(text_y, wp), &
                              trim(xlabel))
        end if
    end subroutine ascii_draw_secondary_x_axis_top

    subroutine ascii_draw_secondary_y_axis(backend, yscale, symlog_threshold, &
                                           y_min, y_max, ylabel, date_format)
        !! Draw right y-axis tick labels for ASCII backend
        type(ascii_context), intent(inout) :: backend
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: y_min, y_max
        character(len=:), allocatable, intent(in), optional :: ylabel
        character(len=*), intent(in), optional :: date_format

        real(wp) :: y_tick_positions(MAX_TICKS)
        character(len=50) :: tick_label
        integer :: num_y_ticks, decimals, i
        integer :: text_x, text_y, label_len
        real(wp) :: frac

        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, &
                                 y_tick_positions, num_y_ticks)
        if (num_y_ticks <= 0) return

        decimals = 0
        if (trim(yscale) == 'linear' .and. num_y_ticks >= 2) then
            decimals = determine_decimals_from_ticks(y_tick_positions, num_y_ticks)
        end if

        do i = 1, num_y_ticks
            if (trim(yscale) == 'linear') then
                tick_label = format_tick_value_consistent(y_tick_positions(i), decimals)
            else
                tick_label = format_tick_label(y_tick_positions(i), yscale, &
                    date_format=date_format, data_min=y_min, data_max=y_max)
            end if

            frac = (y_tick_positions(i) - y_min) / (y_max - y_min)
            text_y = nint((1.0_wp - frac) * real(backend%plot_height - 2, wp)) + 1
            text_y = max(1, min(text_y, backend%plot_height - 1))

            label_len = len_trim(tick_label)
            text_x = backend%plot_width - label_len - 1
            text_x = max(1, text_x)

            call backend%text(real(text_x, wp), real(text_y, wp), trim(tick_label))
        end do
    end subroutine ascii_draw_secondary_y_axis

end module fortplot_ascii_secondary_axes
