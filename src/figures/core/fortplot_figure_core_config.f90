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
    implicit none

    private
    public :: set_xlabel_figure, set_ylabel_figure, set_title_figure
    public :: set_xscale_figure, set_yscale_figure, set_xlim_figure, set_ylim_figure
    public :: set_line_width_figure, grid_figure
    ! Configuration wrapper procedures for core module delegation
    public :: core_set_xlabel, core_set_ylabel, core_set_title
    public :: core_set_xscale, core_set_yscale, core_set_xlim, core_set_ylim
    public :: core_set_line_width, core_grid

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

    subroutine set_xscale_figure(state, scale, threshold)
        !! Set x-axis scale type
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        call set_figure_scales(state, xscale=scale, threshold=threshold)
    end subroutine set_xscale_figure

    subroutine set_yscale_figure(state, scale, threshold)
        !! Set y-axis scale type
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        call set_figure_scales(state, yscale=scale, threshold=threshold)
    end subroutine set_yscale_figure

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
                           state%grid_linestyle, enabled, which, axis, alpha, linestyle)
    end subroutine grid_figure

    !!=============================================================================
    !! CORE MODULE DELEGATION PROCEDURES
    !! Simple wrapper procedures for core module delegation pattern
    !!=============================================================================

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

    subroutine core_set_xscale(state, scale, threshold)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call set_xscale_figure(state, scale, threshold)
    end subroutine core_set_xscale

    subroutine core_set_yscale(state, scale, threshold)
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call set_yscale_figure(state, scale, threshold)
    end subroutine core_set_yscale

    subroutine core_set_xlim(state, x_min, x_max)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_min, x_max
        call set_xlim_figure(state, x_min, x_max)
    end subroutine core_set_xlim

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