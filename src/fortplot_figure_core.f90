module fortplot_figure_core
    !! Core figure management module (refactored for SOLID principles)
    !! 
    !! This module provides the main user interface for creating scientific plots
    !! with support for line plots, contour plots, and mixed plotting across
    !! PNG, PDF, and ASCII backends. Uses deferred rendering for efficiency.
    !! 
    !! Refactored to follow Single Responsibility Principle by delegating
    !! specialized tasks to focused modules.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales
    use fortplot_utils
    use fortplot_axes
    implicit none

    private
    public :: figure_t, plot_data_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR

    integer, parameter :: PLOT_TYPE_LINE = 1
    integer, parameter :: PLOT_TYPE_CONTOUR = 2

    type :: plot_data_t
        !! Data container for individual plots
        !! Separated from figure to follow Single Responsibility Principle
        integer :: plot_type = PLOT_TYPE_LINE
        ! Line plot data
        real(wp), allocatable :: x(:), y(:)
        ! Contour plot data
        real(wp), allocatable :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), allocatable :: contour_levels(:)
        ! Common properties
        real(wp), dimension(3) :: color
        character(len=:), allocatable :: label
        character(len=:), allocatable :: linestyle
    end type plot_data_t

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
        
        ! Axis limits
        real(wp) :: x_min, x_max, y_min, y_max
        logical :: xlim_set = .false., ylim_set = .false.

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
        integer :: max_plots = 20

        ! Labels
        character(len=:), allocatable :: xlabel, ylabel, title

    contains
        procedure :: initialize
        procedure :: add_plot
        procedure :: add_contour
        procedure :: savefig
        procedure :: set_xlabel
        procedure :: set_ylabel
        procedure :: set_title
        procedure :: set_xscale
        procedure :: set_yscale
        procedure :: set_xlim
        procedure :: set_ylim
        procedure :: show
        final :: destroy
    end type figure_t

contains

    subroutine initialize(self, width, height, backend)
        !! Initialize figure with specified dimensions and optional backend
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        
        if (present(width)) self%width = width
        if (present(height)) self%height = height
        
        allocate(self%plots(self%max_plots))
        self%plot_count = 0
        self%rendered = .false.
        
        ! Initialize backend if specified
        if (present(backend)) then
            call initialize_backend(self%backend, backend, self%width, self%height)
        end if
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        !! Add line plot data to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        call add_line_plot_data(self, x, y, label, linestyle, color)
        call update_data_ranges(self)
    end subroutine add_plot

    subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        !! Add contour plot data to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        call add_contour_plot_data(self, x_grid, y_grid, z_grid, levels, label)
        call update_data_ranges(self)
    end subroutine add_contour

    subroutine savefig(self, filename)
        !! Save figure to file with backend auto-detection
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        character(len=20) :: backend_type
        
        backend_type = get_backend_from_filename(filename)
        
        if (.not. allocated(self%backend)) then
            call initialize_backend(self%backend, backend_type, self%width, self%height)
        end if
        
        call render_figure(self)
        call self%backend%save(filename)
        
        write(*, '(A, A, A)') 'Saved figure: ', trim(filename)
    end subroutine savefig

    subroutine show(self)
        !! Display figure in ASCII terminal
        class(figure_t), intent(inout) :: self
        
        if (.not. allocated(self%backend)) then
            call initialize_backend(self%backend, 'ascii', self%width, self%height)
        end if
        
        call render_figure(self)
        call self%backend%save("terminal")
    end subroutine show

    ! Label setters (following Interface Segregation Principle)
    
    subroutine set_xlabel(self, label)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        self%xlabel = label
    end subroutine set_xlabel

    subroutine set_ylabel(self, label)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        self%ylabel = label
    end subroutine set_ylabel

    subroutine set_title(self, title)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: title
        self%title = title
    end subroutine set_title

    subroutine set_xscale(self, scale, threshold)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        self%xscale = scale
        if (present(threshold)) self%symlog_threshold = threshold
    end subroutine set_xscale

    subroutine set_yscale(self, scale, threshold)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        self%yscale = scale
        if (present(threshold)) self%symlog_threshold = threshold
    end subroutine set_yscale

    subroutine set_xlim(self, x_min, x_max)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_min, x_max
        
        self%x_min = x_min
        self%x_max = x_max
        self%xlim_set = .true.
    end subroutine set_xlim

    subroutine set_ylim(self, y_min, y_max)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y_min, y_max
        
        self%y_min = y_min
        self%y_max = y_max
        self%ylim_set = .true.
    end subroutine set_ylim

    subroutine destroy(self)
        !! Clean up figure resources
        type(figure_t), intent(inout) :: self
        
        if (allocated(self%plots)) deallocate(self%plots)
        if (allocated(self%backend)) deallocate(self%backend)
    end subroutine destroy

    ! Private helper routines (implementation details)
    
    subroutine add_line_plot_data(self, x, y, label, linestyle, color)
        !! Add line plot data to internal storage
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        integer :: plot_idx, color_idx
        
        plot_idx = self%plot_count
        self%plots(plot_idx)%plot_type = PLOT_TYPE_LINE
        
        ! Store data
        allocate(self%plots(plot_idx)%x(size(x)))
        allocate(self%plots(plot_idx)%y(size(y)))
        self%plots(plot_idx)%x = x
        self%plots(plot_idx)%y = y
        
        ! Set properties
        if (present(label)) then
            self%plots(plot_idx)%label = label
        else
            self%plots(plot_idx)%label = ''
        end if
        
        if (present(linestyle)) then
            self%plots(plot_idx)%linestyle = linestyle
        else
            self%plots(plot_idx)%linestyle = 'solid'
        end if
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else
            color_idx = mod(plot_idx - 1, 6) + 1
            self%plots(plot_idx)%color = self%colors(:, color_idx)
        end if
    end subroutine add_line_plot_data

    subroutine add_contour_plot_data(self, x_grid, y_grid, z_grid, levels, label)
        !! Add contour plot data to internal storage
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        integer :: plot_idx
        
        plot_idx = self%plot_count
        self%plots(plot_idx)%plot_type = PLOT_TYPE_CONTOUR
        
        ! Store grid data
        allocate(self%plots(plot_idx)%x_grid(size(x_grid)))
        allocate(self%plots(plot_idx)%y_grid(size(y_grid)))
        allocate(self%plots(plot_idx)%z_grid(size(z_grid,1), size(z_grid,2)))
        
        self%plots(plot_idx)%x_grid = x_grid
        self%plots(plot_idx)%y_grid = y_grid
        self%plots(plot_idx)%z_grid = z_grid
        
        ! Handle label
        if (present(label)) then
            self%plots(plot_idx)%label = label
        else
            self%plots(plot_idx)%label = ''
        end if
        
        ! Handle contour levels
        if (present(levels)) then
            allocate(self%plots(plot_idx)%contour_levels(size(levels)))
            self%plots(plot_idx)%contour_levels = levels
        else
            call generate_default_contour_levels(self%plots(plot_idx))
        end if
    end subroutine add_contour_plot_data

    subroutine update_data_ranges(self)
        !! Update figure data ranges after adding plots
        class(figure_t), intent(inout) :: self
        
        ! Implementation delegates to range calculation utilities
        ! This follows Dependency Inversion Principle
        call calculate_figure_data_ranges(self)
    end subroutine update_data_ranges

    subroutine render_figure(self)
        !! Render all plots to the backend
        class(figure_t), intent(inout) :: self
        
        if (self%rendered) return
        
        ! Setup coordinate system using scales module
        call setup_coordinate_system(self)
        
        ! Render background and axes
        call render_figure_background(self)
        call render_figure_axes(self)
        
        ! Render individual plots
        call render_all_plots(self)
        
        self%rendered = .true.
    end subroutine render_figure

    ! Placeholder implementations for helper routines
    ! These will delegate to specialized modules
    
    subroutine generate_default_contour_levels(plot_data)
        type(plot_data_t), intent(inout) :: plot_data
        ! Implementation will use utilities from contour module
    end subroutine generate_default_contour_levels
    
    subroutine calculate_figure_data_ranges(self)
        class(figure_t), intent(inout) :: self
        ! Implementation will use utilities module
    end subroutine calculate_figure_data_ranges
    
    subroutine setup_coordinate_system(self)
        class(figure_t), intent(inout) :: self
        ! Implementation will use scales module
    end subroutine setup_coordinate_system
    
    subroutine render_figure_background(self)
        class(figure_t), intent(inout) :: self
        ! Implementation will use backend directly
    end subroutine render_figure_background
    
    subroutine render_figure_axes(self)
        class(figure_t), intent(inout) :: self
        ! Implementation will use axes module
    end subroutine render_figure_axes
    
    subroutine render_all_plots(self)
        class(figure_t), intent(inout) :: self
        ! Implementation will delegate to specialized renderers
    end subroutine render_all_plots

end module fortplot_figure_core