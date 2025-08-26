module fortplot_figure_initialization
    !! Figure initialization and configuration module
    !! 
    !! Single Responsibility: Initialize figures and manage basic configuration
    !! Extracted from fortplot_figure_core to reduce file size and improve modularity
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_utils, only: initialize_backend
    use fortplot_legend, only: legend_t
    use fortplot_plot_data, only: plot_data_t
    implicit none
    
    private
    public :: figure_state_t, initialize_figure_state, reset_figure_state
    public :: setup_figure_backend, configure_figure_dimensions
    public :: set_figure_labels, set_figure_scales, set_figure_limits
    
    type :: figure_state_t
        !! Figure state and configuration data
        !! Encapsulates all configuration and state management
        class(plot_context), allocatable :: backend
        integer :: plot_count = 0
        logical :: rendered = .false.
        
        ! Figure dimensions
        integer :: width = 640
        integer :: height = 480
        
        ! Plot area settings
        real(wp) :: margin_left = 0.15_wp
        real(wp) :: margin_right = 0.05_wp
        real(wp) :: margin_bottom = 0.15_wp
        real(wp) :: margin_top = 0.05_wp
        
        ! Scale settings
        character(len=10) :: xscale = 'linear'
        character(len=10) :: yscale = 'linear'
        real(wp) :: symlog_threshold = 1.0_wp
        
        ! Axis limits
        real(wp) :: x_min, x_max, y_min, y_max
        real(wp) :: x_min_transformed, x_max_transformed
        real(wp) :: y_min_transformed, y_max_transformed
        logical :: xlim_set = .false., ylim_set = .false.
        
        ! Labels
        character(len=:), allocatable :: title
        character(len=:), allocatable :: xlabel
        character(len=:), allocatable :: ylabel
        
        ! Color palette: seaborn colorblind palette
        real(wp), dimension(3,6) :: colors = reshape([ &
            0.0_wp,   0.447_wp, 0.698_wp,  & ! #0072B2 (blue)
            0.0_wp,   0.619_wp, 0.451_wp,  & ! #009E73 (green)
            0.835_wp, 0.369_wp, 0.0_wp,    & ! #D55E00 (orange)
            0.8_wp,   0.475_wp, 0.655_wp,  & ! #CC79A7 (purple)
            0.941_wp, 0.894_wp, 0.259_wp,  & ! #F0E442 (yellow)
            0.337_wp, 0.702_wp, 0.914_wp], & ! #56B4E9 (cyan)
            [3,6])
        
        ! Legend support
        type(legend_t) :: legend_data
        logical :: show_legend = .false.
        integer :: max_plots = 500
        
        ! Drawing properties
        real(wp) :: current_line_width = 1.0_wp
        logical :: has_error = .false.
        
        ! Grid settings
        logical :: grid_enabled = .false.
        character(len=10) :: grid_which = 'both'
        character(len=1) :: grid_axis = 'b'
        real(wp) :: grid_alpha = 0.3_wp
        character(len=10) :: grid_linestyle = '-'
    end type figure_state_t
    
contains
    
    subroutine initialize_figure_state(state, width, height, backend)
        !! Initialize figure state with specified parameters
        type(figure_state_t), intent(inout) :: state
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        
        if (present(width)) state%width = width
        if (present(height)) state%height = height
        
        ! Initialize backend - default to PNG if not specified
        if (present(backend)) then
            call initialize_backend(state%backend, backend, state%width, state%height)
        else
            ! Default to PNG backend to prevent uninitialized backend
            if (.not. allocated(state%backend)) then
                call initialize_backend(state%backend, 'png', state%width, state%height)
            end if
        end if
        
        ! Reset state
        ! Reset state manually to avoid legend issues
        state%plot_count = 0
        state%rendered = .false.
        state%show_legend = .false.
        state%xlim_set = .false.
        state%ylim_set = .false.
        state%has_error = .false.
        
        ! Simple legend initialization
        state%legend_data%num_entries = 0
    end subroutine initialize_figure_state
    
    subroutine reset_figure_state(state)
        !! Reset figure state to initial values
        type(figure_state_t), intent(inout) :: state
        
        state%plot_count = 0
        state%rendered = .false.
        state%show_legend = .false.
        
        ! Initialize legend data (safe initialization)
        state%legend_data%num_entries = 0
        if (allocated(state%legend_data%entries)) then
            deallocate(state%legend_data%entries)
        end if
        
        ! Reset axis limits and labels
        state%xlim_set = .false.
        state%ylim_set = .false.
        if (allocated(state%title)) deallocate(state%title)
        if (allocated(state%xlabel)) deallocate(state%xlabel)
        if (allocated(state%ylabel)) deallocate(state%ylabel)
        
        state%has_error = .false.
    end subroutine reset_figure_state
    
    subroutine setup_figure_backend(state, backend_name)
        !! Setup or change the figure backend
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: backend_name
        
        ! Deallocate current backend and initialize new one
        if (allocated(state%backend)) deallocate(state%backend)
        call initialize_backend(state%backend, backend_name, state%width, state%height)
        
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
        if (present(xlabel)) state%xlabel = xlabel
        if (present(ylabel)) state%ylabel = ylabel
    end subroutine set_figure_labels
    
    subroutine set_figure_scales(state, xscale, yscale, threshold)
        !! Set axis scale types
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: threshold
        
        if (present(xscale)) state%xscale = xscale
        if (present(yscale)) state%yscale = yscale
        if (present(threshold)) state%symlog_threshold = threshold
    end subroutine set_figure_scales
    
    subroutine set_figure_limits(state, x_min, x_max, y_min, y_max)
        !! Set axis limits
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in), optional :: x_min, x_max, y_min, y_max
        
        if (present(x_min) .and. present(x_max)) then
            state%x_min = x_min
            state%x_max = x_max
            state%xlim_set = .true.
        end if
        
        if (present(y_min) .and. present(y_max)) then
            state%y_min = y_min
            state%y_max = y_max
            state%ylim_set = .true.
        end if
    end subroutine set_figure_limits

end module fortplot_figure_initialization