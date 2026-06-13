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
    use fortplot_constants, only: REFERENCE_DPI
    implicit none

    private
    public :: render_errorbar_plot

contains

    subroutine render_errorbar_plot(backend, plot_data, xscale, yscale, symlog_threshold, &
                                    default_line_width, width, height, &
                                    margin_left, margin_right, margin_bottom, margin_top)
        !! Render error bars for a plot
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in), optional :: default_line_width
        integer, intent(in), optional :: width, height
        real(wp), intent(in), optional :: margin_left, margin_right
        real(wp), intent(in), optional :: margin_bottom, margin_top

        integer :: i, n
        real(wp) :: x_val, y_val
        real(wp) :: x_scaled, y_scaled
        real(wp) :: y_low, y_high, x_low, x_high
        real(wp) :: y_low_t, y_high_t, x_low_t, x_high_t
        real(wp) :: cap_half_x, cap_half_y
        real(wp) :: base_line_width, cap_line_width, restore_width

        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) return
        if (size(plot_data%x) == 0 .or. size(plot_data%y) == 0) return
        if (size(plot_data%x) /= size(plot_data%y)) return

        n = size(plot_data%x)

        ! Set color from plot_data to match markers/lines
        call backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))

        base_line_width = max(plot_data%elinewidth, 1.0e-6_wp)
        cap_line_width = base_line_width
        if (plot_data%capthick > 0.0_wp) cap_line_width = plot_data%capthick

        if (present(default_line_width)) then
            restore_width = max(default_line_width, 1.0e-6_wp)
        else
            restore_width = 1.0_wp
        end if

        call backend%set_line_width(base_line_width)

        ! Capsize is given in points (matplotlib convention). Convert the total
        ! cap length to data units per axis so the horizontal caps on Y error
        ! bars and the vertical caps on X error bars both render at the correct
        ! point size regardless of the data range or aspect ratio.
        call compute_cap_half_lengths(backend, plot_data%capsize, &
                                      width, height, margin_left, margin_right, &
                                      margin_bottom, margin_top, &
                                      cap_half_x, cap_half_y)

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
                if (cap_half_x > 0.0_wp) then
                    if (cap_line_width /= base_line_width) then
                        call backend%set_line_width(cap_line_width)
                    end if
                    call backend%line(x_scaled - cap_half_x, y_low_t,  &
                                       x_scaled + cap_half_x, y_low_t)
                    call backend%line(x_scaled - cap_half_x, y_high_t, &
                                       x_scaled + cap_half_x, y_high_t)
                    if (cap_line_width /= base_line_width) then
                        call backend%set_line_width(base_line_width)
                    end if
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
                if (cap_half_y > 0.0_wp) then
                    if (cap_line_width /= base_line_width) then
                        call backend%set_line_width(cap_line_width)
                    end if
                    call backend%line(x_low_t,  y_scaled - cap_half_y, &
                                       x_low_t,  y_scaled + cap_half_y)
                    call backend%line(x_high_t, y_scaled - cap_half_y, &
                                       x_high_t, y_scaled + cap_half_y)
                    if (cap_line_width /= base_line_width) then
                        call backend%set_line_width(base_line_width)
                    end if
                end if
            end if
        end do

        call backend%set_line_width(restore_width)
    end subroutine render_errorbar_plot

    subroutine compute_cap_half_lengths(backend, capsize, width, height, &
                                        margin_left, margin_right, &
                                        margin_bottom, margin_top, &
                                        cap_half_x, cap_half_y)
        !! Convert a point-valued capsize into per-axis half-lengths in data
        !! coordinates. Returns 0 when caps are disabled or sizing data is
        !! unavailable. capsize is the total cap length in points (matplotlib).
        class(plot_context), intent(in) :: backend
        real(wp), intent(in) :: capsize
        integer, intent(in), optional :: width, height
        real(wp), intent(in), optional :: margin_left, margin_right
        real(wp), intent(in), optional :: margin_bottom, margin_top
        real(wp), intent(out) :: cap_half_x, cap_half_y

        real(wp) :: plot_w_px, plot_h_px, cap_px
        real(wp) :: x_range, y_range

        cap_half_x = 0.0_wp
        cap_half_y = 0.0_wp
        if (capsize <= 0.0_wp) return
        if (.not. present(width) .or. .not. present(height)) return
        if (.not. present(margin_left) .or. .not. present(margin_right)) return
        if (.not. present(margin_bottom) .or. .not. present(margin_top)) return

        plot_w_px = real(width, wp) * (1.0_wp - margin_left - margin_right)
        plot_h_px = real(height, wp) * (1.0_wp - margin_bottom - margin_top)
        if (plot_w_px <= 0.0_wp .or. plot_h_px <= 0.0_wp) return

        ! Total cap length in pixels at the reference DPI, then half on each side.
        cap_px = capsize * REFERENCE_DPI / 72.0_wp

        x_range = abs(backend%x_max - backend%x_min)
        y_range = abs(backend%y_max - backend%y_min)
        if (x_range > 0.0_wp) cap_half_x = 0.5_wp * cap_px * x_range / plot_w_px
        if (y_range > 0.0_wp) cap_half_y = 0.5_wp * cap_px * y_range / plot_h_px
    end subroutine compute_cap_half_lengths

end module fortplot_errorbar_rendering
