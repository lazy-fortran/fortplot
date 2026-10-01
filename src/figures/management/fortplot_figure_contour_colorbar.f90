module fortplot_figure_contour_colorbar
    !! Discrete line-contour colorbars with Matplotlib's uniform level spacing.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_colormap, only: colormap_value_to_color
    use fortplot_utils_sort, only: sort_array
    use fortplot_plot_data, only: plot_data_t
    use fortplot_contour_level_calculation, only: compute_default_contour_levels
    use fortplot_tick_formatting, only: remove_trailing_zeros
    use fortplot_unicode, only: ascii_minus_to_unicode
    implicit none
    private
    public :: render_contour_colorbar_lines, contour_colorbar_tick_positions
    public :: contour_colorbar_default_ticks, get_contour_colorbar_levels
    public :: format_contour_colorbar_tick

contains

    subroutine render_contour_colorbar_lines(backend, vertical, levels, &
            colormap, line_width)
        class(plot_context), intent(inout) :: backend
        logical, intent(in) :: vertical
        real(wp), intent(in) :: levels(:), line_width
        character(len=*), intent(in) :: colormap
        real(wp) :: upper, color(3), position
        integer :: i

        upper = real(max(1, size(levels) - 1), wp)
        if (vertical) then
            call backend%set_coordinates(0.0_wp, 1.0_wp, 0.0_wp, upper)
        else
            call backend%set_coordinates(0.0_wp, upper, 0.0_wp, 1.0_wp)
        end if
        call backend%set_line_width(line_width)
        do i = 1, size(levels)
            call colormap_value_to_color(levels(i), minval(levels), &
                maxval(levels), colormap, color)
            call backend%color(color(1), color(2), color(3))
            position = real(i - 1, wp)
            if (size(levels) == 1) position = 0.5_wp
            if (vertical) then
                call backend%line(0.0_wp, position, 1.0_wp, position)
            else
                call backend%line(position, 0.0_wp, position, 1.0_wp)
            end if
        end do
    end subroutine render_contour_colorbar_lines

    subroutine contour_colorbar_tick_positions(levels, ticks, positions)
        real(wp), intent(in) :: levels(:), ticks(:)
        real(wp), allocatable, intent(out) :: positions(:)
        real(wp) :: fraction
        integer :: i, j

        allocate (positions(size(ticks)), source=0.0_wp)
        if (size(levels) == 1) then
            positions = 0.5_wp
            return
        end if
        do i = 1, size(ticks)
            do j = 1, size(levels) - 1
                if (ticks(i) > levels(j + 1)) cycle
                fraction = (ticks(i) - levels(j))/(levels(j + 1) - levels(j))
                positions(i) = real(j - 1, wp) + fraction
                exit
            end do
        end do
    end subroutine contour_colorbar_tick_positions

    subroutine contour_colorbar_default_ticks(levels, ticks)
        !! Match Matplotlib's FixedLocator(levels, nbins=10), including its phase.
        real(wp), intent(in) :: levels(:)
        real(wp), allocatable, intent(out) :: ticks(:)
        integer :: stride, phase, i

        stride = max(1, ceiling(real(size(levels), wp)/10.0_wp))
        phase = 1
        do i = 2, stride
            if (minval(abs(levels(i::stride))) < &
                minval(abs(levels(phase::stride)))) phase = i
        end do
        ticks = levels(phase::stride)
    end subroutine contour_colorbar_default_ticks

    subroutine get_contour_colorbar_levels(plot, levels)
        !! Use a private sorted/unique view; preserve accepted native plot inputs.
        type(plot_data_t), intent(in) :: plot
        real(wp), allocatable, intent(out) :: levels(:)
        real(wp), allocatable :: sorted(:)
        integer :: i, count

        if (allocated(plot%contour_levels)) then
            sorted = plot%contour_levels
        else
            if (.not. allocated(plot%z_grid)) then
                allocate (levels(0))
                return
            end if
            if (size(plot%z_grid) == 0) then
                allocate (levels(0))
                return
            end if
            call compute_default_contour_levels(minval(plot%z_grid), &
                maxval(plot%z_grid), sorted)
        end if
        call sort_array(sorted)
        count = 0
        do i = 1, size(sorted)
            if (count > 0) then
                if (sorted(i) <= sorted(count)) cycle
            end if
            count = count + 1
            sorted(count) = sorted(i)
        end do
        levels = sorted(:count)
    end subroutine get_contour_colorbar_levels

    function format_contour_colorbar_tick(value, ticks) result(label)
        !! Value-aware precision also retains fractional offsets and tiny values.
        real(wp), intent(in) :: value, ticks(:)
        character(len=50) :: label, mantissa
        character(len=16) :: format
        integer :: i, j, precision, exponent
        real(wp) :: spacing, difference, magnitude

        spacing = huge(1.0_wp)
        do i = 1, size(ticks)
            do j = i + 1, size(ticks)
                difference = abs(ticks(i) - ticks(j))
                if (difference > 0.0_wp) spacing = min(spacing, difference)
            end do
        end do
        precision = 12
        if (spacing < huge(1.0_wp)) then
            magnitude = maxval(abs(ticks))
            if (magnitude > 0.0_wp) precision = min(17, max(6, &
                ceiling(log10(magnitude) - log10(spacing)) + 2))
        end if
        write (format, '(A,I0,A)') '(G0.', precision, ')'
        write (label, format) value
        exponent = scan(label, 'Ee')
        if (exponent > 0) then
            mantissa = label(:exponent - 1)
            call remove_trailing_zeros(mantissa)
            label = trim(mantissa)//trim(label(exponent:))
        else
            call remove_trailing_zeros(label)
        end if
        label = ascii_minus_to_unicode(adjustl(label))
    end function format_contour_colorbar_tick

end module fortplot_figure_contour_colorbar
