module fortplot_errorbar_rendering
    !! Errorbar rendering module
    !!
    !! Renders symmetric/asymmetric error bars for X/Y with optional caps.
    !! Draws using backend line primitives in data (transformed) space and
    !! respects current plot color.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: render_errorbar_plot

contains

    subroutine render_errorbar_plot(backend, plot_data, xscale, yscale, symlog_threshold)
        !! Render error bars for a plot
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: i, n
        real(wp) :: x_val, y_val
        real(wp) :: x_scaled, y_scaled
        real(wp) :: y_low, y_high, x_low, x_high
        real(wp) :: y_low_t, y_high_t, x_low_t, x_high_t
        real(wp) :: cap_half

        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) return
        if (size(plot_data%x) == 0 .or. size(plot_data%y) == 0) return
        if (size(plot_data%x) /= size(plot_data%y)) return

        n = size(plot_data%x)

        ! Set color from plot_data to match markers/lines
        call backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))

        ! Capsize: interpret in data (transformed) coordinates as a small fraction
        ! of the current view width for robustness across scales.
        cap_half = 0.0_wp
        if (plot_data%capsize > 0.0_wp) then
            cap_half = 0.01_wp * abs(backend%x_max - backend%x_min)
        end if

        do i = 1, n
            x_val = plot_data%x(i)
            y_val = plot_data%y(i)
            x_scaled = apply_scale_transform(x_val, xscale, symlog_threshold)
            y_scaled = apply_scale_transform(y_val, yscale, symlog_threshold)

            ! Y error bars (vertical)
            if (plot_data%has_yerr) then
                if (plot_data%asymmetric_yerr .and. allocated(plot_data%yerr_lower) &
                    .and. allocated(plot_data%yerr_upper)) then
                    y_low = y_val - plot_data%yerr_lower(i)
                    y_high = y_val + plot_data%yerr_upper(i)
                else if (allocated(plot_data%yerr)) then
                    y_low = y_val - plot_data%yerr(i)
                    y_high = y_val + plot_data%yerr(i)
                else
                    y_low = y_val
                    y_high = y_val
                end if

                y_low_t = apply_scale_transform(y_low, yscale, symlog_threshold)
                y_high_t = apply_scale_transform(y_high, yscale, symlog_threshold)
                call backend%line(x_scaled, y_low_t, x_scaled, y_high_t)
                if (cap_half > 0.0_wp) then
                    call backend%line(x_scaled - cap_half, y_low_t,  x_scaled + cap_half, y_low_t)
                    call backend%line(x_scaled - cap_half, y_high_t, x_scaled + cap_half, y_high_t)
                end if
            end if

            ! X error bars (horizontal)
            if (plot_data%has_xerr) then
                if (plot_data%asymmetric_xerr .and. allocated(plot_data%xerr_lower) &
                    .and. allocated(plot_data%xerr_upper)) then
                    x_low = x_val - plot_data%xerr_lower(i)
                    x_high = x_val + plot_data%xerr_upper(i)
                else if (allocated(plot_data%xerr)) then
                    x_low = x_val - plot_data%xerr(i)
                    x_high = x_val + plot_data%xerr(i)
                else
                    x_low = x_val
                    x_high = x_val
                end if

                x_low_t = apply_scale_transform(x_low, xscale, symlog_threshold)
                x_high_t = apply_scale_transform(x_high, xscale, symlog_threshold)
                call backend%line(x_low_t, y_scaled, x_high_t, y_scaled)
                if (cap_half > 0.0_wp) then
                    call backend%line(x_low_t,  y_scaled - cap_half, x_low_t,  y_scaled + cap_half)
                    call backend%line(x_high_t, y_scaled - cap_half, x_high_t, y_scaled + cap_half)
                end if
            end if
        end do
    end subroutine render_errorbar_plot

end module fortplot_errorbar_rendering

