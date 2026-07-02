module fortplot_figure_core_config
    !! Figure configuration operations module
    !!
    !! This module contains figure configuration functionality (labels, scales, limits)
    !! extracted from fortplot_figure_core for architectural compliance
    !!
    !! ARCHITECTURAL REFACTORING (Issue #678):
    !! - Focused module for configuration operations
    !! - Single Responsibility Principle compliance
    !! - Clean separation from plot data management

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization
    use fortplot_figure_grid
    use fortplot_plot_data, only: AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_configuration, only: set_figure_labels, set_figure_scales, set_figure_limits
    use fortplot_string_utils, only: to_lowercase
    use fortplot_logging, only: log_warning
    use fortplot_text_color, only: is_valid_text_color_mode
    implicit none

    private
    public :: set_xlabel_figure, set_ylabel_figure, set_title_figure
    public :: set_xscale_figure, set_yscale_figure, set_xlim_figure, set_ylim_figure
    public :: set_xaxis_date_format_figure, set_yaxis_date_format_figure
    public :: set_line_width_figure, grid_figure
    ! Configuration wrapper procedures for core module delegation
    public :: core_set_xlabel, core_set_ylabel, core_set_title
    public :: core_set_xscale, core_set_yscale, core_set_xlim, core_set_ylim
    public :: core_set_xaxis_date_format, core_set_yaxis_date_format
    public :: core_set_line_width, core_grid
    public :: set_view_figure, core_set_view
    public :: core_set_text_charset, core_set_text_color_mode

contains

    subroutine set_xlabel_figure(state, xlabel_compat, label)
        !! Set x-axis label
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: xlabel_compat
        character(len=*), intent(in) :: label
        call set_figure_labels(state, xlabel=label)
        ! Update backward compatibility member
        xlabel_compat = label
    end subroutine set_xlabel_figure

    subroutine set_ylabel_figure(state, ylabel_compat, label)
        !! Set y-axis label
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: ylabel_compat
        character(len=*), intent(in) :: label
        call set_figure_labels(state, ylabel=label)
        ! Update backward compatibility member
        ylabel_compat = label
    end subroutine set_ylabel_figure

    subroutine set_title_figure(state, title_compat, title)
        !! Set figure title
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: title_compat
        character(len=*), intent(in) :: title
        call set_figure_labels(state, title=title)
        ! Update backward compatibility member
        title_compat = title
    end subroutine set_title_figure

    subroutine set_xscale_figure(state, scale, threshold, base, linscale)
        !! Set x-axis scale type
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold, base, linscale

        call set_figure_scales(state, xscale=scale, threshold=threshold, &
                               base=base, linscale=linscale)
    end subroutine set_xscale_figure

    subroutine set_yscale_figure(state, scale, threshold, base, linscale)
        !! Set y-axis scale type
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold, base, linscale

        call set_figure_scales(state, yscale=scale, threshold=threshold, &
                               base=base, linscale=linscale)
    end subroutine set_yscale_figure

    subroutine set_xaxis_date_format_figure(state, format)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: format

        select case (state%active_axis)
        case (AXIS_TWINY)
            if (trim(state%twiny_xscale) /= 'date_jd') state%twiny_xscale = 'date'
            state%twiny_xaxis_date_format = trim(format)
        case default
            if (trim(state%xscale) /= 'date_jd') state%xscale = 'date'
            state%xaxis_date_format = trim(format)
        end select

        state%rendered = .false.
    end subroutine set_xaxis_date_format_figure

    subroutine set_yaxis_date_format_figure(state, format)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: format

        select case (state%active_axis)
        case (AXIS_TWINX)
            if (trim(state%twinx_yscale) /= 'date_jd') state%twinx_yscale = 'date'
            state%twinx_yaxis_date_format = trim(format)
        case default
            if (trim(state%yscale) /= 'date_jd') state%yscale = 'date'
            state%yaxis_date_format = trim(format)
        end select

        state%rendered = .false.
    end subroutine set_yaxis_date_format_figure

    subroutine set_xlim_figure(state, x_min, x_max)
        !! Set x-axis limits
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_min, x_max

        call set_figure_limits(state, x_min=x_min, x_max=x_max)
    end subroutine set_xlim_figure

    subroutine set_ylim_figure(state, y_min, y_max)
        !! Set y-axis limits
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: y_min, y_max

        call set_figure_limits(state, y_min=y_min, y_max=y_max)
    end subroutine set_ylim_figure

    subroutine set_view_figure(state, elev, azim, dist)
        !! Set the 3D view angles (degrees) and optional camera distance.
        !! Mirrors matplotlib's Axes3D.view_init(elev, azim).
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in), optional :: elev, azim, dist
        real(wp), parameter :: DEG2RAD = 3.14159265358979323846_wp / 180.0_wp

        if (present(elev)) state%view_elev = elev * DEG2RAD
        if (present(azim)) state%view_azim = azim * DEG2RAD
        if (present(dist)) state%view_dist = dist
        state%rendered = .false.
    end subroutine set_view_figure

    subroutine set_line_width_figure(state, width)
        !! Set line width for subsequent plots
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: width
        state%current_line_width = width
    end subroutine set_line_width_figure

    subroutine grid_figure(state, enabled, which, axis, alpha, linestyle)
        !! Enable/disable and configure grid lines
        type(figure_state_t), intent(inout) :: state
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha

        call configure_grid(state%grid_enabled, state%grid_which, &
                            state%grid_axis, state%grid_alpha, &
                            state%grid_linestyle, enabled, which, axis, &
                            alpha, linestyle)
    end subroutine grid_figure

    !! CORE MODULE DELEGATION PROCEDURES
    !! Simple wrapper procedures for core module delegation pattern

    subroutine core_set_xlabel(state, xlabel_compat, label)
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: xlabel_compat
        character(len=*), intent(in) :: label
        call set_xlabel_figure(state, xlabel_compat, label)
    end subroutine core_set_xlabel

    subroutine core_set_ylabel(state, ylabel_compat, label)
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: ylabel_compat
        character(len=*), intent(in) :: label
        call set_ylabel_figure(state, ylabel_compat, label)
    end subroutine core_set_ylabel

    subroutine core_set_title(state, title_compat, title)
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: title_compat
        character(len=*), intent(in) :: title
        call set_title_figure(state, title_compat, title)
    end subroutine core_set_title

    subroutine core_set_xscale(state, scale, threshold, base, linscale)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold, base, linscale
        call set_xscale_figure(state, scale, threshold, base, linscale)
    end subroutine core_set_xscale

    subroutine core_set_yscale(state, scale, threshold, base, linscale)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold, base, linscale
        call set_yscale_figure(state, scale, threshold, base, linscale)
    end subroutine core_set_yscale

    subroutine core_set_text_charset(state, charset)
        !! Store the text-backend charset: 'ascii' (default), 'unicode', 'auto'
        !! (resolved from the environment at save time), or 'braille'. Unknown
        !! names warn and fall back to 'ascii' (#2060, #2061).
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: charset
        character(len=:), allocatable :: normalized

        normalized = to_lowercase(trim(adjustl(charset)))
        select case (normalized)
        case ('ascii', 'unicode', 'auto', 'braille')
            state%text_charset = normalized
        case default
            call log_warning("Unknown text charset '" // trim(charset) // &
                             "', using 'ascii'")
            state%text_charset = 'ascii'
        end select
    end subroutine core_set_text_charset

    subroutine core_set_text_color_mode(state, mode)
        !! Store the text-backend ANSI color mode: 'never' (default), 'ansi16',
        !! 'ansi256', 'truecolor', or 'auto' (resolved at output time). Unknown
        !! names warn and fall back to 'never' so escapes never leak (#2062).
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: mode
        character(len=:), allocatable :: normalized

        normalized = to_lowercase(trim(adjustl(mode)))
        if (is_valid_text_color_mode(normalized)) then
            state%text_color_mode = normalized
        else
            call log_warning("Unknown text color mode '" // trim(mode) // &
                             "', using 'never'")
            state%text_color_mode = 'never'
        end if
    end subroutine core_set_text_color_mode

    subroutine core_set_xaxis_date_format(state, format)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: format
        call set_xaxis_date_format_figure(state, format)
    end subroutine core_set_xaxis_date_format

    subroutine core_set_yaxis_date_format(state, format)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: format
        call set_yaxis_date_format_figure(state, format)
    end subroutine core_set_yaxis_date_format

    subroutine core_set_xlim(state, x_min, x_max)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_min, x_max
        call set_xlim_figure(state, x_min, x_max)
    end subroutine core_set_xlim

    subroutine core_set_view(state, elev, azim, dist)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in), optional :: elev, azim, dist
        call set_view_figure(state, elev, azim, dist)
    end subroutine core_set_view

    subroutine core_set_ylim(state, y_min, y_max)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: y_min, y_max
        call set_ylim_figure(state, y_min, y_max)
    end subroutine core_set_ylim

    subroutine core_set_line_width(state, width)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: width
        call set_line_width_figure(state, width)
    end subroutine core_set_line_width

    subroutine core_grid(state, enabled, which, axis, alpha, linestyle)
        type(figure_state_t), intent(inout) :: state
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        call grid_figure(state, enabled, which, axis, alpha, linestyle)
    end subroutine core_grid

end module fortplot_figure_core_config
