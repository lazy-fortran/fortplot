module fortplot_figure_initialization
    !! Figure initialization and configuration module
    !! 
    !! Single Responsibility: Initialize figures and manage basic configuration
    !! Extracted from fortplot_figure_core to reduce file size and improve modularity
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_utils, only: initialize_backend
    use fortplot_legend, only: legend_t
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    implicit none
    
    private
    public :: figure_state_t, initialize_figure_state, reset_figure_state
    public :: setup_figure_backend, configure_figure_dimensions
    public :: set_figure_labels, set_figure_scales, set_figure_limits
    
    type :: figure_state_t
        !! Figure state and configuration data
        !! Encapsulates all configuration and state management
        class(plot_context), allocatable :: backend
        character(len=10) :: backend_name = 'png'
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

        ! Secondary axis support
        integer :: active_axis = AXIS_PRIMARY
        logical :: has_twinx = .false.
        logical :: has_twiny = .false.
        character(len=:), allocatable :: twinx_ylabel
        character(len=:), allocatable :: twiny_xlabel
        character(len=10) :: twinx_yscale = 'linear'
        character(len=10) :: twiny_xscale = 'linear'
        logical :: twinx_ylim_set = .false.
        logical :: twiny_xlim_set = .false.
        real(wp) :: twinx_y_min = 0.0_wp
        real(wp) :: twinx_y_max = 1.0_wp
        real(wp) :: twiny_x_min = 0.0_wp
        real(wp) :: twiny_x_max = 1.0_wp
        real(wp) :: twinx_y_min_transformed = 0.0_wp
        real(wp) :: twinx_y_max_transformed = 1.0_wp
        real(wp) :: twiny_x_min_transformed = 0.0_wp
        real(wp) :: twiny_x_max_transformed = 1.0_wp
        
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

        ! Streamplot arrow storage (rendered after plots)
        type(arrow_data_t), allocatable :: stream_arrows(:)
    end type figure_state_t
    
contains
    
    subroutine initialize_figure_state(state, width, height, backend)
        !! Initialize figure state with specified parameters
        !! Added Issue #854: Parameter validation for user input safety
        use fortplot_parameter_validation, only: validate_plot_dimensions, validate_file_path, &
                                               parameter_validation_result_t, validation_warning
        type(figure_state_t), intent(inout) :: state
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        
        type(parameter_validation_result_t) :: validation
        real(wp) :: width_real, height_real
        
        ! Set dimensions with validation
        if (present(width)) then
            width_real = real(width, wp)
            height_real = real(state%height, wp)
            if (present(height)) height_real = real(height, wp)
            
            validation = validate_plot_dimensions(width_real, height_real, "figure_initialization")
            if (validation%is_valid) then
                state%width = width
            else
                ! Use default width on validation failure
                state%width = 640
            end if
        end if
        
        if (present(height)) then
            width_real = real(state%width, wp)
            height_real = real(height, wp)
            
            validation = validate_plot_dimensions(width_real, height_real, "figure_initialization")
            if (validation%is_valid) then
                state%height = height
            else
                ! Use default height on validation failure
                state%height = 480
            end if
        end if
        
        ! Initialize backend - default to PNG if not specified
        if (present(backend)) then
            ! Validate backend name (basic check for supported backends)
            if (len_trim(backend) == 0) then
                call validation_warning("Empty backend name provided, using default 'png'", &
                                      "figure_initialization")
                state%backend_name = 'png'
                call initialize_backend(state%backend, 'png', state%width, state%height)
            else if (backend /= 'png' .and. backend /= 'pdf' .and. backend /= 'ascii') then
                call validation_warning("Unknown backend '" // trim(backend) // &
                                      "', using default 'png'", "figure_initialization")
                state%backend_name = 'png'
                call initialize_backend(state%backend, 'png', state%width, state%height)
            else
                state%backend_name = backend
                call initialize_backend(state%backend, backend, state%width, state%height)
            end if
        else
            ! Default to PNG backend to prevent uninitialized backend
            if (.not. allocated(state%backend)) then
                state%backend_name = 'png'
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
        
        ! Proper legend initialization without manual deallocate
        state%legend_data%num_entries = 0
        block
            use fortplot_legend, only: legend_entry_t
            type(legend_entry_t), allocatable :: new_entries(:)
            allocate(new_entries(0))
            call move_alloc(new_entries, state%legend_data%entries)
        end block

        state%active_axis = AXIS_PRIMARY
        state%has_twinx = .false.
        state%has_twiny = .false.
        state%twinx_ylim_set = .false.
        state%twiny_xlim_set = .false.
        state%twinx_yscale = 'linear'
        state%twiny_xscale = 'linear'
        state%twinx_y_min = 0.0_wp
        state%twinx_y_max = 1.0_wp
        state%twiny_x_min = 0.0_wp
        state%twiny_x_max = 1.0_wp
        state%twinx_y_min_transformed = 0.0_wp
        state%twinx_y_max_transformed = 1.0_wp
        state%twiny_x_min_transformed = 0.0_wp
        state%twiny_x_max_transformed = 1.0_wp
        if (allocated(state%twinx_ylabel)) deallocate(state%twinx_ylabel)
        if (allocated(state%twiny_xlabel)) deallocate(state%twiny_xlabel)
    end subroutine initialize_figure_state
    
    subroutine reset_figure_state(state)
        !! Reset figure state to initial values
        type(figure_state_t), intent(inout) :: state
        
        state%plot_count = 0
        state%rendered = .false.
        state%show_legend = .false.
        
        ! Initialize legend data (safe initialization without manual deallocate)
        state%legend_data%num_entries = 0
        block
            use fortplot_legend, only: legend_entry_t
            type(legend_entry_t), allocatable :: new_entries(:)
            allocate(new_entries(0))
            call move_alloc(new_entries, state%legend_data%entries)
        end block
        
        ! Reset axis limits and labels
        state%xlim_set = .false.
        state%ylim_set = .false.
        state%title = ''
        state%xlabel = ''
        state%ylabel = ''

        state%active_axis = AXIS_PRIMARY
        state%has_twinx = .false.
        state%has_twiny = .false.
        state%twinx_ylim_set = .false.
        state%twiny_xlim_set = .false.
        state%twinx_yscale = 'linear'
        state%twiny_xscale = 'linear'
        state%twinx_y_min = 0.0_wp
        state%twinx_y_max = 1.0_wp
        state%twiny_x_min = 0.0_wp
        state%twiny_x_max = 1.0_wp
        state%twinx_y_min_transformed = 0.0_wp
        state%twinx_y_max_transformed = 1.0_wp
        state%twiny_x_min_transformed = 0.0_wp
        state%twiny_x_max_transformed = 1.0_wp
        if (allocated(state%twinx_ylabel)) deallocate(state%twinx_ylabel)
        if (allocated(state%twiny_xlabel)) deallocate(state%twiny_xlabel)
        
        state%has_error = .false.

        if (allocated(state%stream_arrows)) deallocate(state%stream_arrows)
    end subroutine reset_figure_state
    
    subroutine setup_figure_backend(state, backend_name)
        !! Setup or change the figure backend
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: backend_name
        
        ! Reinitialize backend; initialize_backend has intent(out) and will handle deallocation
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
    
    subroutine set_figure_scales(state, xscale, yscale, threshold)
        !! Set axis scale types
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: threshold
        
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

end module fortplot_figure_initialization
