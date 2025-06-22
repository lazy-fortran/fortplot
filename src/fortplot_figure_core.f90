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
        
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%max_plots))
        end if
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
        
        ! Always reinitialize backend for correct format
        if (allocated(self%backend)) deallocate(self%backend)
        call initialize_backend(self%backend, backend_type, self%width, self%height)
        
        ! Reset rendered flag to force re-rendering for new backend
        self%rendered = .false.
        call render_figure(self)
        call self%backend%save(filename)
        
        write(*, '(A, A, A)') 'Saved figure: ', trim(filename)
    end subroutine savefig

    subroutine show(self)
        !! Display figure in ASCII terminal
        class(figure_t), intent(inout) :: self
        
        if (.not. allocated(self%backend)) then
            ! Use reasonable ASCII dimensions instead of figure dimensions
            call initialize_backend(self%backend, 'ascii', 80, 24)
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
        if (allocated(self%plots(plot_idx)%x)) deallocate(self%plots(plot_idx)%x)
        if (allocated(self%plots(plot_idx)%y)) deallocate(self%plots(plot_idx)%y)
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
        if (allocated(self%plots(plot_idx)%x_grid)) deallocate(self%plots(plot_idx)%x_grid)
        if (allocated(self%plots(plot_idx)%y_grid)) deallocate(self%plots(plot_idx)%y_grid)
        if (allocated(self%plots(plot_idx)%z_grid)) deallocate(self%plots(plot_idx)%z_grid)
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
            if (allocated(self%plots(plot_idx)%contour_levels)) deallocate(self%plots(plot_idx)%contour_levels)
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
        integer :: i
        real(wp) :: x_min, x_max, y_min, y_max
        logical :: first_plot
        
        if (self%plot_count == 0) return
        
        first_plot = .true.
        
        do i = 1, self%plot_count
            if (self%plots(i)%plot_type == PLOT_TYPE_LINE) then
                if (first_plot) then
                    ! Apply scale transformations to data ranges
                    x_min = apply_scale_transform(minval(self%plots(i)%x), self%xscale, self%symlog_threshold)
                    x_max = apply_scale_transform(maxval(self%plots(i)%x), self%xscale, self%symlog_threshold)
                    y_min = apply_scale_transform(minval(self%plots(i)%y), self%yscale, self%symlog_threshold)
                    y_max = apply_scale_transform(maxval(self%plots(i)%y), self%yscale, self%symlog_threshold)
                    first_plot = .false.
                else
                    x_min = min(x_min, apply_scale_transform(minval(self%plots(i)%x), self%xscale, self%symlog_threshold))
                    x_max = max(x_max, apply_scale_transform(maxval(self%plots(i)%x), self%xscale, self%symlog_threshold))
                    y_min = min(y_min, apply_scale_transform(minval(self%plots(i)%y), self%yscale, self%symlog_threshold))
                    y_max = max(y_max, apply_scale_transform(maxval(self%plots(i)%y), self%yscale, self%symlog_threshold))
                end if
            else if (self%plots(i)%plot_type == PLOT_TYPE_CONTOUR) then
                if (first_plot) then
                    ! Apply scale transformations to contour grid ranges
                    x_min = apply_scale_transform(minval(self%plots(i)%x_grid), self%xscale, self%symlog_threshold)
                    x_max = apply_scale_transform(maxval(self%plots(i)%x_grid), self%xscale, self%symlog_threshold)
                    y_min = apply_scale_transform(minval(self%plots(i)%y_grid), self%yscale, self%symlog_threshold)
                    y_max = apply_scale_transform(maxval(self%plots(i)%y_grid), self%yscale, self%symlog_threshold)
                    first_plot = .false.
                else
                    x_min = min(x_min, apply_scale_transform(minval(self%plots(i)%x_grid), self%xscale, self%symlog_threshold))
                    x_max = max(x_max, apply_scale_transform(maxval(self%plots(i)%x_grid), self%xscale, self%symlog_threshold))
                    y_min = min(y_min, apply_scale_transform(minval(self%plots(i)%y_grid), self%yscale, self%symlog_threshold))
                    y_max = max(y_max, apply_scale_transform(maxval(self%plots(i)%y_grid), self%yscale, self%symlog_threshold))
                end if
            end if
        end do
        
        if (.not. self%xlim_set) then
            self%x_min = x_min
            self%x_max = x_max
        end if
        
        if (.not. self%ylim_set) then
            self%y_min = y_min
            self%y_max = y_max
        end if
        
        ! print *, "DEBUG: Data ranges - X:", self%x_min, "to", self%x_max, "Y:", self%y_min, "to", self%y_max
    end subroutine calculate_figure_data_ranges
    
    subroutine setup_coordinate_system(self)
        class(figure_t), intent(inout) :: self
        
        if (.not. self%xlim_set .or. .not. self%ylim_set) then
            call calculate_figure_data_ranges(self)
        end if
        
        ! Set backend data coordinate ranges
        self%backend%x_min = self%x_min
        self%backend%x_max = self%x_max
        self%backend%y_min = self%y_min
        self%backend%y_max = self%y_max
    end subroutine setup_coordinate_system
    
    subroutine render_figure_background(self)
        class(figure_t), intent(inout) :: self
        ! Clear the background - backend-specific implementation not needed
        ! Background is handled by backend initialization
    end subroutine render_figure_background
    
    subroutine render_figure_axes(self)
        class(figure_t), intent(inout) :: self
        
        ! print *, "DEBUG: Rendering axes with ranges X:", self%x_min, "to", self%x_max, "Y:", self%y_min, "to", self%y_max
        
        ! Set axis color to black
        call self%backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! Draw X axis (horizontal line at y=0 or y_min)
        call self%backend%line(self%x_min, self%y_min, self%x_max, self%y_min)
        
        ! Draw Y axis (vertical line at x=0 or x_min)
        call self%backend%line(self%x_min, self%y_min, self%x_min, self%y_max)
    end subroutine render_figure_axes
    
    subroutine render_all_plots(self)
        class(figure_t), intent(inout) :: self
        integer :: i
        
        do i = 1, self%plot_count
            ! Set color for this plot
            call self%backend%color(self%plots(i)%color(1), self%plots(i)%color(2), self%plots(i)%color(3))
            
            if (self%plots(i)%plot_type == PLOT_TYPE_LINE) then
                call render_line_plot(self, i)
            else if (self%plots(i)%plot_type == PLOT_TYPE_CONTOUR) then
                call render_contour_plot(self, i)
            end if
        end do
    end subroutine render_all_plots

    subroutine render_line_plot(self, plot_idx)
        !! Render a single line plot with linestyle support
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        integer :: i
        real(wp) :: x1_screen, y1_screen, x2_screen, y2_screen
        character(len=:), allocatable :: linestyle
        
        if (plot_idx > self%plot_count) return
        if (.not. allocated(self%plots(plot_idx)%x)) return
        if (size(self%plots(plot_idx)%x) < 2) return
        
        ! Get linestyle for this plot
        linestyle = self%plots(plot_idx)%linestyle
        
        ! Skip drawing if linestyle is 'None'
        if (linestyle == 'None') return
        
        ! Draw line segments using transformed coordinates with linestyle
        call draw_line_with_style(self, plot_idx, linestyle)
    end subroutine render_line_plot

    subroutine render_contour_plot(self, plot_idx)
        !! Render a single contour plot using proper marching squares algorithm
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        integer :: level_idx
        real(wp) :: contour_level
        real(wp) :: z_min, z_max
        
        if (plot_idx > self%plot_count) return
        if (.not. allocated(self%plots(plot_idx)%z_grid)) return
        
        ! Get data range for filtering valid levels
        z_min = minval(self%plots(plot_idx)%z_grid)
        z_max = maxval(self%plots(plot_idx)%z_grid)
        
        ! Render each contour level that falls within data range
        if (allocated(self%plots(plot_idx)%contour_levels)) then
            do level_idx = 1, size(self%plots(plot_idx)%contour_levels)
                contour_level = self%plots(plot_idx)%contour_levels(level_idx)
                
                ! Only render levels within the data range
                if (contour_level > z_min .and. contour_level < z_max) then
                    call trace_contour_level(self, plot_idx, contour_level)
                end if
            end do
        else
            ! Draw a few default contour levels
            call trace_contour_level(self, plot_idx, z_min + 0.2_wp * (z_max - z_min))
            call trace_contour_level(self, plot_idx, z_min + 0.5_wp * (z_max - z_min))
            call trace_contour_level(self, plot_idx, z_min + 0.8_wp * (z_max - z_min))
        end if
    end subroutine render_contour_plot

    subroutine trace_contour_level(self, plot_idx, level)
        !! Trace a single contour level using marching squares
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: level
        integer :: nx, ny, i, j
        
        nx = size(self%plots(plot_idx)%x_grid)
        ny = size(self%plots(plot_idx)%y_grid)
        
        do i = 1, nx-1
            do j = 1, ny-1
                call process_contour_cell(self, plot_idx, i, j, level)
            end do
        end do
    end subroutine trace_contour_level

    subroutine process_contour_cell(self, plot_idx, i, j, level)
        !! Process a single grid cell for contour extraction
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx, i, j
        real(wp), intent(in) :: level
        real(wp) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp) :: z1, z2, z3, z4
        integer :: config
        real(wp), dimension(8) :: line_points
        integer :: num_lines

        call get_cell_coordinates(self, plot_idx, i, j, x1, y1, x2, y2, x3, y3, x4, y4)
        call get_cell_values(self, plot_idx, i, j, z1, z2, z3, z4)
        call calculate_marching_squares_config(z1, z2, z3, z4, level, config)
        call get_contour_lines(config, x1, y1, x2, y2, x3, y3, x4, y4, &
                             z1, z2, z3, z4, level, line_points, num_lines)
        call draw_contour_lines(self, line_points, num_lines)
    end subroutine process_contour_cell

    subroutine get_cell_coordinates(self, plot_idx, i, j, x1, y1, x2, y2, x3, y3, x4, y4)
        !! Get the coordinates of the four corners of a grid cell
        class(figure_t), intent(in) :: self
        integer, intent(in) :: plot_idx, i, j
        real(wp), intent(out) :: x1, y1, x2, y2, x3, y3, x4, y4

        x1 = self%plots(plot_idx)%x_grid(i)
        y1 = self%plots(plot_idx)%y_grid(j)
        x2 = self%plots(plot_idx)%x_grid(i+1)
        y2 = self%plots(plot_idx)%y_grid(j)
        x3 = self%plots(plot_idx)%x_grid(i+1)
        y3 = self%plots(plot_idx)%y_grid(j+1)
        x4 = self%plots(plot_idx)%x_grid(i)
        y4 = self%plots(plot_idx)%y_grid(j+1)
    end subroutine get_cell_coordinates

    subroutine get_cell_values(self, plot_idx, i, j, z1, z2, z3, z4)
        !! Get the data values at the four corners of a grid cell
        class(figure_t), intent(in) :: self
        integer, intent(in) :: plot_idx, i, j
        real(wp), intent(out) :: z1, z2, z3, z4

        z1 = self%plots(plot_idx)%z_grid(i, j)
        z2 = self%plots(plot_idx)%z_grid(i+1, j)
        z3 = self%plots(plot_idx)%z_grid(i+1, j+1)
        z4 = self%plots(plot_idx)%z_grid(i, j+1)
    end subroutine get_cell_values

    subroutine calculate_marching_squares_config(z1, z2, z3, z4, level, config)
        !! Calculate marching squares configuration for a cell
        real(wp), intent(in) :: z1, z2, z3, z4, level
        integer, intent(out) :: config

        config = 0
        if (z1 >= level) config = config + 1
        if (z2 >= level) config = config + 2
        if (z3 >= level) config = config + 4
        if (z4 >= level) config = config + 8
    end subroutine calculate_marching_squares_config

    subroutine get_contour_lines(config, x1, y1, x2, y2, x3, y3, x4, y4, &
                               z1, z2, z3, z4, level, line_points, num_lines)
        !! Get contour line segments for a cell based on marching squares configuration
        integer, intent(in) :: config
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp), intent(in) :: z1, z2, z3, z4, level
        real(wp), dimension(8), intent(out) :: line_points
        integer, intent(out) :: num_lines
        real(wp) :: xa, ya, xb, yb, xc, yc, xd, yd
        
        call interpolate_edge_crossings(x1, y1, x2, y2, x3, y3, x4, y4, &
                                       z1, z2, z3, z4, level, xa, ya, xb, yb, xc, yc, xd, yd)
        call apply_marching_squares_lookup(config, xa, ya, xb, yb, xc, yc, xd, yd, line_points, num_lines)
    end subroutine get_contour_lines

    subroutine interpolate_edge_crossings(x1, y1, x2, y2, x3, y3, x4, y4, &
                                         z1, z2, z3, z4, level, xa, ya, xb, yb, xc, yc, xd, yd)
        !! Interpolate where contour level crosses cell edges
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp), intent(in) :: z1, z2, z3, z4, level
        real(wp), intent(out) :: xa, ya, xb, yb, xc, yc, xd, yd

        ! Edge 1-2 (bottom)
        if (abs(z2 - z1) > 1e-10_wp) then
            xa = x1 + (level - z1) / (z2 - z1) * (x2 - x1)
            ya = y1 + (level - z1) / (z2 - z1) * (y2 - y1)
        else
            xa = (x1 + x2) * 0.5_wp
            ya = (y1 + y2) * 0.5_wp
        end if
        
        ! Edge 2-3 (right)
        if (abs(z3 - z2) > 1e-10_wp) then
            xb = x2 + (level - z2) / (z3 - z2) * (x3 - x2)
            yb = y2 + (level - z2) / (z3 - z2) * (y3 - y2)
        else
            xb = (x2 + x3) * 0.5_wp
            yb = (y2 + y3) * 0.5_wp
        end if
        
        ! Edge 3-4 (top)
        if (abs(z4 - z3) > 1e-10_wp) then
            xc = x3 + (level - z3) / (z4 - z3) * (x4 - x3)
            yc = y3 + (level - z3) / (z4 - z3) * (y4 - y3)
        else
            xc = (x3 + x4) * 0.5_wp
            yc = (y3 + y4) * 0.5_wp
        end if
        
        ! Edge 4-1 (left)
        if (abs(z1 - z4) > 1e-10_wp) then
            xd = x4 + (level - z4) / (z1 - z4) * (x1 - x4)
            yd = y4 + (level - z4) / (z1 - z4) * (y1 - y4)
        else
            xd = (x4 + x1) * 0.5_wp
            yd = (y4 + y1) * 0.5_wp
        end if
    end subroutine interpolate_edge_crossings

    subroutine apply_marching_squares_lookup(config, xa, ya, xb, yb, xc, yc, xd, yd, line_points, num_lines)
        !! Apply marching squares lookup table to get line segments
        integer, intent(in) :: config
        real(wp), intent(in) :: xa, ya, xb, yb, xc, yc, xd, yd
        real(wp), dimension(8), intent(out) :: line_points
        integer, intent(out) :: num_lines

        num_lines = 0
        line_points = 0.0_wp
        
        select case (config)
        case (1, 14)
            line_points(1:4) = [xa, ya, xd, yd]
            num_lines = 1
        case (2, 13)
            line_points(1:4) = [xa, ya, xb, yb]
            num_lines = 1
        case (3, 12)
            line_points(1:4) = [xd, yd, xb, yb]
            num_lines = 1
        case (4, 11)
            line_points(1:4) = [xb, yb, xc, yc]
            num_lines = 1
        case (5)
            line_points(1:8) = [xa, ya, xd, yd, xb, yb, xc, yc]
            num_lines = 2
        case (6, 9)
            line_points(1:4) = [xa, ya, xc, yc]
            num_lines = 1
        case (7, 8)
            line_points(1:4) = [xd, yd, xc, yc]
            num_lines = 1
        case (10)
            line_points(1:8) = [xa, ya, xb, yb, xc, yc, xd, yd]
            num_lines = 2
        case default
            num_lines = 0
        end select
    end subroutine apply_marching_squares_lookup

    subroutine draw_contour_lines(self, line_points, num_lines)
        !! Draw the contour line segments with proper coordinate transformation
        class(figure_t), intent(inout) :: self
        real(wp), dimension(8), intent(in) :: line_points
        integer, intent(in) :: num_lines
        integer :: i
        real(wp) :: x1_trans, y1_trans, x2_trans, y2_trans
        
        do i = 1, num_lines
            ! Apply scale transformations to contour line endpoints
            x1_trans = apply_scale_transform(line_points(4*i-3), self%xscale, self%symlog_threshold)
            y1_trans = apply_scale_transform(line_points(4*i-2), self%yscale, self%symlog_threshold)
            x2_trans = apply_scale_transform(line_points(4*i-1), self%xscale, self%symlog_threshold)
            y2_trans = apply_scale_transform(line_points(4*i), self%yscale, self%symlog_threshold)
            
            call self%backend%line(x1_trans, y1_trans, x2_trans, y2_trans)
        end do
    end subroutine draw_contour_lines

    subroutine draw_line_with_style(self, plot_idx, linestyle)
        !! Draw line segments with specified linestyle pattern using continuous pattern approach
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        character(len=*), intent(in) :: linestyle
        
        if (linestyle == '-' .or. linestyle == 'solid') then
            ! Solid line - draw all segments normally
            call render_solid_line(self, plot_idx)
        else
            ! Patterned line - render with continuous pattern
            call render_patterned_line(self, plot_idx, linestyle)
        end if
    end subroutine draw_line_with_style

    subroutine render_solid_line(self, plot_idx)
        !! Render solid line by drawing all segments
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        integer :: i
        real(wp) :: x1_screen, y1_screen, x2_screen, y2_screen
        
        do i = 1, size(self%plots(plot_idx)%x) - 1
            ! Apply scale transformations
            x1_screen = apply_scale_transform(self%plots(plot_idx)%x(i), self%xscale, self%symlog_threshold)
            y1_screen = apply_scale_transform(self%plots(plot_idx)%y(i), self%yscale, self%symlog_threshold)
            x2_screen = apply_scale_transform(self%plots(plot_idx)%x(i+1), self%xscale, self%symlog_threshold)
            y2_screen = apply_scale_transform(self%plots(plot_idx)%y(i+1), self%yscale, self%symlog_threshold)
            
            call self%backend%line(x1_screen, y1_screen, x2_screen, y2_screen)
        end do
    end subroutine render_solid_line

    subroutine render_patterned_line(self, plot_idx, linestyle)
        !! Render line with continuous pattern across segments (matplotlib-style)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        character(len=*), intent(in) :: linestyle
        
        real(wp) :: current_distance, segment_length
        real(wp) :: dash_len, dot_len, gap_len
        real(wp) :: pattern(20), pattern_length
        integer :: pattern_size, pattern_index
        logical :: drawing
        integer :: i
        real(wp) :: x1_screen, y1_screen, x2_screen, y2_screen, dx, dy
        
        ! Get transformed data range for proper pattern scaling
        real(wp) :: x_range, y_range, plot_scale
        real(wp), allocatable :: x_trans(:), y_trans(:)
        
        ! Transform all data points to get proper scaling
        allocate(x_trans(size(self%plots(plot_idx)%x)))
        allocate(y_trans(size(self%plots(plot_idx)%y)))
        
        do i = 1, size(self%plots(plot_idx)%x)
            x_trans(i) = apply_scale_transform(self%plots(plot_idx)%x(i), self%xscale, self%symlog_threshold)
            y_trans(i) = apply_scale_transform(self%plots(plot_idx)%y(i), self%yscale, self%symlog_threshold)
        end do
        
        x_range = maxval(x_trans) - minval(x_trans)
        y_range = maxval(y_trans) - minval(y_trans)
        plot_scale = max(x_range, y_range)
        
        ! Define pattern lengths (matplotlib-like)
        dash_len = plot_scale * 0.03_wp    ! 3% of range
        dot_len = plot_scale * 0.005_wp    ! 0.5% of range  
        gap_len = plot_scale * 0.015_wp    ! 1.5% of range
        
        ! Define patterns like matplotlib
        select case (trim(linestyle))
        case ('--')
            ! Dashed: [dash, gap, dash, gap, ...]
            pattern_size = 2
            pattern(1) = dash_len  ! dash
            pattern(2) = gap_len   ! gap
            
        case (':')
            ! Dotted: [dot, gap, dot, gap, ...]
            pattern_size = 2
            pattern(1) = dot_len   ! dot
            pattern(2) = gap_len   ! gap
            
        case ('-.')
            ! Dash-dot: [dash, gap, dot, gap, dash, gap, dot, gap, ...]
            pattern_size = 4
            pattern(1) = dash_len  ! dash
            pattern(2) = gap_len   ! gap
            pattern(3) = dot_len   ! dot
            pattern(4) = gap_len   ! gap
            
        case default
            ! Unknown pattern, fall back to solid
            call render_solid_line(self, plot_idx)
            deallocate(x_trans, y_trans)
            return
        end select
        
        ! Calculate total pattern length
        pattern_length = sum(pattern(1:pattern_size))
        
        ! Render with continuous pattern
        current_distance = 0.0_wp
        pattern_index = 1
        drawing = .true.  ! Start drawing
        
        do i = 1, size(self%plots(plot_idx)%x) - 1
            x1_screen = x_trans(i)
            y1_screen = y_trans(i)
            x2_screen = x_trans(i+1)
            y2_screen = y_trans(i+1)
            
            dx = x2_screen - x1_screen
            dy = y2_screen - y1_screen
            segment_length = sqrt(dx*dx + dy*dy)
            
            if (segment_length < 1e-10_wp) cycle
            
            call render_segment_with_pattern(self, x1_screen, y1_screen, x2_screen, y2_screen, segment_length, &
                                            pattern, pattern_size, pattern_length, &
                                            current_distance, pattern_index, drawing)
        end do
        
        ! Clean up
        deallocate(x_trans, y_trans)
    end subroutine render_patterned_line

    subroutine render_segment_with_pattern(self, x1, y1, x2, y2, segment_length, &
                                          pattern, pattern_size, pattern_length, &
                                          current_distance, pattern_index, drawing)
        !! Render single segment with continuous pattern state
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x1, y1, x2, y2, segment_length
        real(wp), intent(in) :: pattern(:), pattern_length
        integer, intent(in) :: pattern_size
        real(wp), intent(inout) :: current_distance
        integer, intent(inout) :: pattern_index
        logical, intent(inout) :: drawing
        
        real(wp) :: dx, dy, remaining_distance, pattern_remaining
        real(wp) :: t_start, t_end, seg_x1, seg_y1, seg_x2, seg_y2
        
        dx = x2 - x1
        dy = y2 - y1
        remaining_distance = segment_length
        t_start = 0.0_wp
        
        do while (remaining_distance > 1e-10_wp)
            ! How much of current pattern element is left?
            pattern_remaining = pattern(pattern_index) - current_distance
            
            if (pattern_remaining <= remaining_distance) then
                ! Complete this pattern element within current segment
                t_end = t_start + pattern_remaining / segment_length
                
                if (drawing) then
                    seg_x1 = x1 + t_start * dx
                    seg_y1 = y1 + t_start * dy
                    seg_x2 = x1 + t_end * dx
                    seg_y2 = y1 + t_end * dy
                    call self%backend%line(seg_x1, seg_y1, seg_x2, seg_y2)
                end if
                
                ! Move to next pattern element
                remaining_distance = remaining_distance - pattern_remaining
                t_start = t_end
                current_distance = 0.0_wp
                pattern_index = mod(pattern_index, pattern_size) + 1
                drawing = .not. drawing  ! Alternate between drawing and not drawing
            else
                ! Pattern element extends beyond this segment
                t_end = 1.0_wp
                
                if (drawing) then
                    seg_x1 = x1 + t_start * dx
                    seg_y1 = y1 + t_start * dy
                    seg_x2 = x2
                    seg_y2 = y2
                    call self%backend%line(seg_x1, seg_y1, seg_x2, seg_y2)
                end if
                
                current_distance = current_distance + remaining_distance
                remaining_distance = 0.0_wp
            end if
        end do
    end subroutine render_segment_with_pattern

end module fortplot_figure_core