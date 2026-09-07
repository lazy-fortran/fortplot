module fortplot_figure_minor_ticks
    !! Shared minor-tick generation for raster and PDF figure rendering.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_raster, only: raster_context
    use fortplot_pdf, only: pdf_context
    use fortplot_raster_axes, only: raster_draw_x_minor_ticks, &
        raster_draw_y_minor_ticks
    use fortplot_figure_tick_budget, only: backend_tick_budget
    use fortplot_axes, only: compute_scale_ticks, MAX_TICKS
    use fortplot_tick_calculation, only: calculate_minor_tick_positions, &
        calculate_log_minor_tick_positions
    use fortplot_scales, only: apply_scale_transform
    implicit none
    private
    public :: render_minor_ticks_raster, render_minor_ticks_pdf

contains

    subroutine render_minor_ticks_raster(backend, xscale, yscale, threshold, &
            x_min, x_max, y_min, y_max, state)
        type(raster_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: threshold, x_min, x_max, y_min, y_max
        type(figure_state_t), intent(in) :: state
        real(wp) :: ticks(MAX_TICKS*10), lo, hi
        integer :: n

        call minor_axis_ticks(xscale, threshold, x_min, x_max, &
            backend%x_min, backend%x_max, state%minor_ticks_x, &
            state%minor_tick_count, ticks, n, lo, hi, &
            backend_tick_budget(backend, .true.))
        if (n > 0) call raster_draw_x_minor_ticks(backend%raster, backend%width, &
            backend%height, backend%plot_area, &
            xscale, threshold, ticks(:n), lo, hi)
        call minor_axis_ticks(yscale, threshold, y_min, y_max, &
            backend%y_min, backend%y_max, state%minor_ticks_y, &
            state%minor_tick_count, ticks, n, lo, hi, &
            backend_tick_budget(backend, .false.))
        if (n > 0) call raster_draw_y_minor_ticks(backend%raster, backend%width, &
            backend%height, backend%plot_area, &
            yscale, threshold, ticks(:n), lo, hi)
    end subroutine render_minor_ticks_raster

    subroutine render_minor_ticks_pdf(backend, xscale, yscale, threshold, &
            x_min, x_max, y_min, y_max, state)
        type(pdf_context), intent(inout) :: backend
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: threshold, x_min, x_max, y_min, y_max
        type(figure_state_t), intent(in) :: state
        real(wp) :: xs(MAX_TICKS*10), ys(MAX_TICKS*10), lo, hi
        integer :: nx, ny

        call minor_axis_ticks(xscale, threshold, x_min, x_max, &
            backend%x_min, backend%x_max, state%minor_ticks_x, &
            state%minor_tick_count, xs, nx, lo, hi, &
            backend_tick_budget(backend, .true.))
        call map_pdf_ticks(xs, nx, xscale, threshold, lo, hi, &
            backend%plot_area%left, backend%plot_area%width)
        call minor_axis_ticks(yscale, threshold, y_min, y_max, &
            backend%y_min, backend%y_max, state%minor_ticks_y, &
            state%minor_tick_count, ys, ny, lo, hi, &
            backend_tick_budget(backend, .false.))
        call map_pdf_ticks(ys, ny, yscale, threshold, lo, hi, &
            backend%plot_area%bottom, backend%plot_area%height)
        if (nx + ny == 0) return
        call backend%draw_minor_ticks(xs(:nx), ys(:ny))
    end subroutine render_minor_ticks_pdf

    subroutine minor_axis_ticks(scale, threshold, data_min, data_max, &
            view_min, view_max, enabled, minor_count, &
            ticks, n, lo, hi, max_intervals)
        character(len=*), intent(in) :: scale
        real(wp), intent(in) :: threshold, data_min, data_max, view_min, view_max
        logical, intent(in) :: enabled
        integer, intent(in) :: minor_count, max_intervals
        real(wp), intent(out) :: ticks(:), lo, hi
        integer, intent(out) :: n
        real(wp) :: major(MAX_TICKS)
        integer :: nmajor, first_decade, last_decade, i, count
        real(wp) :: step, normalized_step

        n = 0
        lo = data_min
        hi = data_max
        if (.not. enabled .and. trim(scale) /= 'log') return
        if (trim(scale) == 'linear') then
            lo = view_min
            hi = view_max
        end if
        if (hi <= lo) return
        if (trim(scale) == 'log') then
            if (lo <= 0.0_wp) return
            first_decade = floor(log10(lo))
            last_decade = ceiling(log10(hi))
            nmajor = min(MAX_TICKS, last_decade - first_decade + 1)
            do i = 1, nmajor
                major(i) = 10.0_wp**(first_decade + i - 1)
            end do
            call calculate_log_minor_tick_positions(major, nmajor, lo, hi, ticks, n)
        else
            call compute_scale_ticks(scale, lo, hi, threshold, major, nmajor, &
                step_min=data_min, step_max=data_max, max_intervals=max_intervals)
            if (nmajor < 2) return
            count = minor_count
            if (count == 0) then
                ! AutoMinorLocator uses five subdivisions for 1, 2.5, 5, 10;
                ! all other major steps use four subdivisions.
                step = abs(major(2) - major(1))
                if (step <= 0.0_wp) return
                normalized_step = step/10.0_wp**floor(log10(step))
                count = 3
                if (any(abs(normalized_step - &
                    [1.0_wp, 2.5_wp, 5.0_wp, 10.0_wp]) < 1.0e-8_wp)) &
                    count = 4
            end if
            call calculate_minor_tick_positions(major, nmajor, count, &
                lo, hi, ticks, n)
        end if
    end subroutine minor_axis_ticks

    subroutine map_pdf_ticks(ticks, n, scale, threshold, lo, hi, origin, span)
        real(wp), intent(inout) :: ticks(:)
        integer, intent(in) :: n, origin, span
        character(len=*), intent(in) :: scale
        real(wp), intent(in) :: threshold, lo, hi
        real(wp) :: lower, upper
        integer :: i

        lower = apply_scale_transform(lo, scale, threshold)
        upper = apply_scale_transform(hi, scale, threshold)
        if (upper <= lower) return
        do i = 1, n
            ticks(i) = real(origin, wp) + real(span, wp)* &
                (apply_scale_transform(ticks(i), scale, threshold) - lower)/ &
                (upper - lower)
        end do
    end subroutine map_pdf_ticks

end module fortplot_figure_minor_ticks
