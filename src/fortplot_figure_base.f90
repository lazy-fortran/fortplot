module fortplot_figure_base
    !! Core figure type definition and initialization (SOLID principles compliance)
    !! 
    !! This module provides the main figure type and basic initialization/lifecycle
    !! management. Separated from rendering and plot addition for better modularity.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, subplot_t
    use fortplot_legend
    use fortplot_annotations, only: text_annotation_t
    use fortplot_logging, only: log_warning

    implicit none

    private
    public :: figure_t

    type :: figure_t
        !! Main figure class - coordinates plotting operations
        !! Follows Open/Closed Principle by using composition over inheritance
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
        
        ! Axis limits - separate original and transformed ranges
        real(wp) :: x_min, x_max, y_min, y_max  ! Original data ranges for tick generation
        real(wp) :: z_min, z_max  ! Z-axis limits for 3D plots
        real(wp) :: x_min_transformed, x_max_transformed, y_min_transformed, y_max_transformed  ! Transformed for rendering
        logical :: xlim_set = .false., ylim_set = .false.
        
        ! Figure and axis labels
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

        ! Store all plot data for deferred rendering
        type(plot_data_t), allocatable :: plots(:)
        
        ! Legend support following SOLID principles
        type(legend_t) :: legend_data
        logical :: legend_added = .false.
        integer :: legend_location = 1  ! default: upper right
        
        ! Streamline data for vector field plots
        type(plot_data_t), allocatable :: streamlines(:)
        
        ! Arrow data for streamplot annotations
        type(arrow_data_t), allocatable :: arrow_data(:)
        
        ! Error tracking
        logical :: has_error = .false.
        
        ! Text annotations support (Issue #184)
        type(text_annotation_t), allocatable :: annotations(:)
        integer :: annotation_count = 0
        integer :: max_annotations = 1000
        
        ! Subplot management
        integer :: subplot_rows = 1
        integer :: subplot_cols = 1
        integer :: current_subplot = 1
        type(subplot_t), allocatable :: subplots(:)
        
        ! Plot limits
        integer :: max_plots = 50
        
        ! Tick count configuration (Issue #238)
        integer :: x_tick_count = 0  ! 0 means use dynamic calculation
        integer :: y_tick_count = 0  ! 0 means use dynamic calculation

    contains
        procedure :: initialize
        procedure :: destroy
        procedure :: set_xlabel
        procedure :: set_ylabel
        procedure :: set_title
        procedure :: set_xscale
        procedure :: set_yscale
        procedure :: set_xlim
        procedure :: set_ylim
        procedure :: set_subplot
        procedure :: set_line_width
        procedure :: initialize_default_subplot
        procedure :: set_ydata
        procedure :: add_plot
        procedure :: clear
        procedure :: set_tick_count
    end type figure_t

contains

    subroutine initialize(self, width, height, backend)
        !! Initialize figure with backend and dimensions
        !! 
        !! Arguments:
        !!   width, height: Figure dimensions in pixels
        !!   backend: Rendering backend name ('ascii', 'png', 'pdf')
        use fortplot_utils, only: initialize_backend
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        
        character(len=20) :: backend_type
        
        ! Set dimensions
        if (present(width)) self%width = width
        if (present(height)) self%height = height
        
        ! Set backend
        backend_type = 'ascii'
        if (present(backend)) backend_type = trim(backend)
        
        ! Create backend (deallocate first if already allocated)
        if (allocated(self%backend)) deallocate(self%backend)
        call initialize_backend(self%backend, backend_type, self%width, self%height)
        
        ! Initialize plot storage (deallocate first if already allocated)
        if (allocated(self%plots)) deallocate(self%plots)
        allocate(self%plots(self%max_plots))
        
        ! Initialize subplot system
        call self%initialize_default_subplot()
        
        self%plot_count = 0
        self%rendered = .false.
        self%has_error = .false.
    end subroutine initialize

    subroutine destroy(self)
        !! Clean up figure resources
        class(figure_t), intent(inout) :: self
        
        if (allocated(self%backend)) then
            ! Note: plot_context doesn't have a destroy method
            deallocate(self%backend)
        end if
        
        if (allocated(self%plots)) deallocate(self%plots)
        if (allocated(self%streamlines)) deallocate(self%streamlines)
        if (allocated(self%arrow_data)) deallocate(self%arrow_data)
        if (allocated(self%annotations)) deallocate(self%annotations)
        if (allocated(self%subplots)) deallocate(self%subplots)
        if (allocated(self%title)) deallocate(self%title)
        if (allocated(self%xlabel)) deallocate(self%xlabel)
        if (allocated(self%ylabel)) deallocate(self%ylabel)
    end subroutine destroy

    subroutine set_xlabel(self, label)
        !! Set x-axis label
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        
        self%xlabel = trim(label)
    end subroutine set_xlabel

    subroutine set_ylabel(self, label)
        !! Set y-axis label
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        
        self%ylabel = trim(label)
    end subroutine set_ylabel

    subroutine set_title(self, title)
        !! Set figure title
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: title
        
        self%title = trim(title)
    end subroutine set_title

    subroutine set_xscale(self, scale, threshold)
        !! Set x-axis scale
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        self%xscale = trim(scale)
        if (present(threshold)) self%symlog_threshold = threshold
    end subroutine set_xscale

    subroutine set_yscale(self, scale, threshold)
        !! Set y-axis scale
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        self%yscale = trim(scale)
        if (present(threshold)) self%symlog_threshold = threshold
    end subroutine set_yscale

    subroutine set_xlim(self, x_min, x_max)
        !! Set x-axis limits
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_min, x_max
        
        self%x_min = x_min
        self%x_max = x_max
        self%xlim_set = .true.
    end subroutine set_xlim

    subroutine set_ylim(self, y_min, y_max)
        !! Set y-axis limits
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y_min, y_max
        
        self%y_min = y_min
        self%y_max = y_max
        self%ylim_set = .true.
    end subroutine set_ylim

    subroutine set_subplot(self, rows, cols, index)
        !! Configure subplot layout with warning generation for excessive subplots
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: rows, cols, index
        
        integer :: total_subplots, i
        character(len=100) :: warning_msg
        
        if (rows < 1 .or. cols < 1 .or. index < 1 .or. index > rows * cols) then
            self%has_error = .true.
            return
        end if
        
        self%subplot_rows = rows
        self%subplot_cols = cols
        self%current_subplot = index
        
        total_subplots = rows * cols
        
        ! Generate warning for excessive subplot counts
        if (total_subplots > 25) then
            write(warning_msg, '(A,I0,A,I0,A,I0,A)') &
                'Large number of subplots requested (', rows, 'x', cols, '=', total_subplots, &
                '), performance may be degraded'
            call log_warning(trim(warning_msg))
        end if
        
        ! Generate warning for extreme subplot indices
        if (index > 100) then
            write(warning_msg, '(A,I0,A)') &
                'Subplot index ', index, ' exceeds typical display capacity'
            call log_warning(trim(warning_msg))
        end if
        
        ! Reallocate subplots if needed
        if (.not. allocated(self%subplots) .or. size(self%subplots) /= total_subplots) then
            if (allocated(self%subplots)) deallocate(self%subplots)
            allocate(self%subplots(total_subplots))
            
            ! Initialize all subplots
            do i = 1, total_subplots
                allocate(self%subplots(i)%plots(self%subplots(i)%max_plots))
                self%subplots(i)%plot_count = 0
            end do
        end if
    end subroutine set_subplot

    subroutine set_line_width(self, width)
        !! Set default line width for subsequent plots
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: width
        
        ! Store in backend
        call self%backend%set_line_width(width)
    end subroutine set_line_width

    subroutine initialize_default_subplot(self)
        !! Initialize single subplot layout
        class(figure_t), intent(inout) :: self
        
        self%subplot_rows = 1
        self%subplot_cols = 1
        self%current_subplot = 1
        
        if (allocated(self%subplots)) deallocate(self%subplots)
        allocate(self%subplots(1))
        allocate(self%subplots(1)%plots(self%subplots(1)%max_plots))
        self%subplots(1)%plot_count = 0
    end subroutine initialize_default_subplot

    subroutine set_ydata(self, plot_index, y_new)
        !! Update y-data for an existing plot
        !! Useful for animations and interactive updates
        !! 
        !! Arguments:
        !!   plot_index: Index of the plot to update (1-based)
        !!   y_new: New y-data array (must match size of original)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        real(wp), dimension(:), intent(in) :: y_new
        
        integer :: subplot_idx
        
        ! Get current subplot (default to 1 if not set)
        subplot_idx = self%current_subplot
        if (subplot_idx < 1 .or. subplot_idx > size(self%subplots)) then
            subplot_idx = 1
        end if
        
        ! Check if plot exists
        if (plot_index < 1 .or. plot_index > self%subplots(subplot_idx)%plot_count) then
            call log_warning("set_ydata: Invalid plot index")
            return
        end if
        
        ! Check if sizes match
        if (size(y_new) /= size(self%subplots(subplot_idx)%plots(plot_index)%y)) then
            call log_warning("set_ydata: Size mismatch between new and existing y data")
            return
        end if
        
        ! Update the y data
        self%subplots(subplot_idx)%plots(plot_index)%y = y_new
        
        ! Mark figure as needing re-render
        self%rendered = .false.
    end subroutine set_ydata
    
    subroutine add_plot(self, x, y, label, linestyle, color, marker)
        !! Add a line plot to the figure (OO API)
        !! Implements inline to avoid circular dependency
        use fortplot_plot_data, only: PLOT_TYPE_LINE
        use fortplot_colors, only: parse_color
        
        class(figure_t), intent(inout) :: self
        real(wp), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: linestyle
        character(len=*), intent(in), optional :: color
        character(len=*), intent(in), optional :: marker
        
        integer :: plot_idx, color_idx, subplot_idx
        real(wp) :: rgb(3)
        logical :: success
        
        ! Get current subplot
        subplot_idx = self%current_subplot
        plot_idx = self%subplots(subplot_idx)%plot_count + 1
        self%subplots(subplot_idx)%plot_count = plot_idx
        
        self%subplots(subplot_idx)%plots(plot_idx)%plot_type = PLOT_TYPE_LINE
        
        ! Store data
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%x(size(x)))
        allocate(self%subplots(subplot_idx)%plots(plot_idx)%y(size(y)))
        self%subplots(subplot_idx)%plots(plot_idx)%x = x
        self%subplots(subplot_idx)%plots(plot_idx)%y = y
        
        ! Set label
        if (present(label)) then
            if (len_trim(label) > 0) then
                self%subplots(subplot_idx)%plots(plot_idx)%label = label
            end if
            ! If label is empty or not provided, leave it unallocated
        end if
        
        ! Set linestyle 
        if (present(linestyle)) then
            self%subplots(subplot_idx)%plots(plot_idx)%linestyle = linestyle
        else
            self%subplots(subplot_idx)%plots(plot_idx)%linestyle = '-'
        end if
        
        ! Set color 
        if (present(color)) then
            call parse_color(color, rgb, success)
            if (success) then
                self%subplots(subplot_idx)%plots(plot_idx)%color = rgb
            else
                ! Use default color from palette
                color_idx = mod(plot_idx - 1, 6) + 1
                self%subplots(subplot_idx)%plots(plot_idx)%color = self%colors(:, color_idx)
            end if
        else
            ! No color specified, use default from palette
            color_idx = mod(plot_idx - 1, 6) + 1
            self%subplots(subplot_idx)%plots(plot_idx)%color = self%colors(:, color_idx)
        end if
        
        ! Set marker 
        if (present(marker)) then
            self%subplots(subplot_idx)%plots(plot_idx)%marker = marker
        else
            self%subplots(subplot_idx)%plots(plot_idx)%marker = ''
        end if
    end subroutine add_plot
    
    subroutine clear(self)
        !! Clear all plots and reset figure to initial state
        class(figure_t), intent(inout) :: self
        integer :: i
        
        ! Clear all plots
        self%plot_count = 0
        self%rendered = .false.
        
        ! Reset subplot plots
        if (allocated(self%subplots)) then
            do i = 1, size(self%subplots)
                self%subplots(i)%plot_count = 0
            end do
        end if
        
        ! Clear streamlines and annotations if allocated
        if (allocated(self%streamlines)) then
            deallocate(self%streamlines)
        end if
        
        if (allocated(self%annotations)) then
            deallocate(self%annotations)
            self%annotation_count = 0
        end if
        
        if (allocated(self%arrow_data)) then
            deallocate(self%arrow_data)
        end if
    end subroutine clear
    
    subroutine set_tick_count(self, x_ticks, y_ticks)
        !! Set explicit tick count for axes (Issue #238)
        !! Values are clamped to valid range [3, 15]
        !! 0 means use dynamic calculation
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: x_ticks, y_ticks
        
        ! Validate and clamp X tick count
        if (x_ticks == 0) then
            self%x_tick_count = 0  ! Use dynamic calculation
        else if (x_ticks < 3) then
            self%x_tick_count = 3  ! Minimum
        else if (x_ticks > 15) then
            self%x_tick_count = 15  ! Maximum
        else
            self%x_tick_count = x_ticks
        end if
        
        ! Validate and clamp Y tick count
        if (y_ticks == 0) then
            self%y_tick_count = 0  ! Use dynamic calculation
        else if (y_ticks < 3) then
            self%y_tick_count = 3  ! Minimum
        else if (y_ticks > 15) then
            self%y_tick_count = 15  ! Maximum
        else
            self%y_tick_count = y_ticks
        end if
    end subroutine set_tick_count

end module fortplot_figure_base