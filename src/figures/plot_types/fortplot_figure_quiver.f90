module fortplot_figure_quiver
    !! Quiver plot (discrete vector arrows) functionality module
    !!
    !! Single Responsibility: Handle quiver plot creation for discrete vector fields
    !! Provides arrow visualization at grid points showing direction and magnitude

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_QUIVER
    use fortplot_figure_initialization, only: figure_state_t
    implicit none

    private
    public :: quiver_basic_validation, quiver_figure

contains

    function quiver_basic_validation(x, y, u, v) result(is_valid)
        !! Validate quiver input arrays have matching dimensions
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        logical :: is_valid

        is_valid = .true.

        if (size(x) /= size(y)) then
            is_valid = .false.
            return
        end if

        if (size(x) /= size(u)) then
            is_valid = .false.
            return
        end if

        if (size(x) /= size(v)) then
            is_valid = .false.
            return
        end if

        if (size(x) == 0) then
            is_valid = .false.
            return
        end if
    end function quiver_basic_validation

    subroutine quiver_figure(plots, state, plot_count, x, y, u, v, &
                             scale, color, width, headwidth, headlength, units)
        !! Add quiver plot to figure
        !! Creates discrete vector arrows at (x,y) positions with (u,v) directions
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units

        integer :: n, plot_idx
        real(wp) :: arrow_color(3)

        if (.not. quiver_basic_validation(x, y, u, v)) then
            state%has_error = .true.
            return
        end if

        n = size(x)
        plot_count = plot_count + 1
        plot_idx = plot_count

        if (plot_idx > size(plots)) then
            state%has_error = .true.
            return
        end if

        plots(plot_idx)%plot_type = PLOT_TYPE_QUIVER

        allocate(plots(plot_idx)%x(n))
        allocate(plots(plot_idx)%y(n))
        allocate(plots(plot_idx)%quiver_u(n))
        allocate(plots(plot_idx)%quiver_v(n))

        plots(plot_idx)%x = x
        plots(plot_idx)%y = y
        plots(plot_idx)%quiver_u = u
        plots(plot_idx)%quiver_v = v

        if (present(scale)) then
            plots(plot_idx)%quiver_scale = scale
        else
            plots(plot_idx)%quiver_scale = 1.0_wp
        end if

        if (present(width)) then
            plots(plot_idx)%quiver_width = width
        end if

        if (present(headwidth)) then
            plots(plot_idx)%quiver_headwidth = headwidth
        end if

        if (present(headlength)) then
            plots(plot_idx)%quiver_headlength = headlength
        end if

        if (present(units)) then
            plots(plot_idx)%quiver_units = trim(adjustl(units))
        end if

        arrow_color = [0.0_wp, 0.0_wp, 0.0_wp]
        if (present(color)) arrow_color = color
        plots(plot_idx)%color = arrow_color

        if (.not. state%xlim_set) then
            if (state%x_min > minval(x)) state%x_min = minval(x)
            if (state%x_max < maxval(x)) state%x_max = maxval(x)
        end if
        if (.not. state%ylim_set) then
            if (state%y_min > minval(y)) state%y_min = minval(y)
            if (state%y_max < maxval(y)) state%y_max = maxval(y)
        end if
    end subroutine quiver_figure

end module fortplot_figure_quiver
