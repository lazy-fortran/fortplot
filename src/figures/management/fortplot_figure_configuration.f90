module fortplot_figure_configuration
    !! Figure configuration module
    !!
    !! Single Responsibility: Configure figure dimensions, backend, labels,
    !! scales, and axis limits after initialization.
    !! Extracted from fortplot_figure_initialization to respect module size limits.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_utils, only: initialize_backend, normalize_backend_name
    use fortplot_plot_data, only: AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t

    implicit none

    private
    public :: setup_figure_backend, configure_figure_dimensions
    public :: set_figure_labels, set_figure_scales, set_figure_limits

contains

    subroutine setup_figure_backend(state, backend_name)
        !! Setup or change the figure backend
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: backend_name

        character(len=20) :: canonical

        canonical = normalize_backend_name(backend_name)

        ! Reinitialize backend; initialize_backend has intent(out) and will handle
        ! deallocation.
        call initialize_backend(state%backend, canonical, state%width, &
                                state%height, state%dpi)

        ! Update the backend_name field to match the current backend
        state%backend_name = canonical

        ! Force re-rendering with new backend
        state%rendered = .false.
    end subroutine setup_figure_backend

    subroutine configure_figure_dimensions(state, width, height)
        !! Configure figure dimensions
        type(figure_state_t), intent(inout) :: state
        integer, intent(in), optional :: width, height

        if (present(width)) state%width = width
        if (present(height)) state%height = height

        ! If backend exists, reinitialize with new dimensions
        if (allocated(state%backend)) then
            ! Get current backend type and reinitialize
            state%rendered = .false.
        end if
    end subroutine configure_figure_dimensions

    subroutine set_figure_labels(state, title, xlabel, ylabel)
        !! Set figure labels
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in), optional :: title, xlabel, ylabel

        if (present(title)) state%title = title

        if (present(xlabel)) then
            select case (state%active_axis)
            case (AXIS_TWINY)
                state%twiny_xlabel = xlabel
                state%has_twiny = .true.
            case default
                state%xlabel = xlabel
            end select
        end if

        if (present(ylabel)) then
            select case (state%active_axis)
            case (AXIS_TWINX)
                state%twinx_ylabel = ylabel
                state%has_twinx = .true.
            case default
                state%ylabel = ylabel
            end select
        end if
    end subroutine set_figure_labels

    subroutine set_figure_scales(state, xscale, yscale, threshold, base, linscale)
        !! Set axis scale types
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: threshold, base, linscale

        if (present(xscale)) then
            if (state%active_axis == AXIS_TWINY) then
                state%twiny_xscale = xscale
                state%has_twiny = .true.
            else
                state%xscale = xscale
            end if
        end if

        if (present(yscale)) then
            if (state%active_axis == AXIS_TWINX) then
                state%twinx_yscale = yscale
                state%has_twinx = .true.
            else
                state%yscale = yscale
            end if
        end if
        if (present(threshold)) state%symlog_threshold = threshold
        if (present(base)) state%symlog_base = base
        if (present(linscale)) state%symlog_linscale = linscale
    end subroutine set_figure_scales

    subroutine set_figure_limits(state, x_min, x_max, y_min, y_max)
        !! Set axis limits
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in), optional :: x_min, x_max, y_min, y_max

        if (present(x_min) .and. present(x_max)) then
            if (state%active_axis == AXIS_TWINY) then
                state%twiny_x_min = x_min
                state%twiny_x_max = x_max
                state%twiny_xlim_set = .true.
                state%has_twiny = .true.
            else
                state%x_min = x_min
                state%x_max = x_max
                state%xlim_set = .true.
            end if
        end if

        if (present(y_min) .and. present(y_max)) then
            if (state%active_axis == AXIS_TWINX) then
                state%twinx_y_min = y_min
                state%twinx_y_max = y_max
                state%twinx_ylim_set = .true.
                state%has_twinx = .true.
            else
                state%y_min = y_min
                state%y_max = y_max
                state%ylim_set = .true.
            end if
        end if
    end subroutine set_figure_limits

end module fortplot_figure_configuration
