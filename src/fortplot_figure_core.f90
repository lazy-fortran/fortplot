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
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_context
    use fortplot_scales
    use fortplot_utils
    use fortplot_axes
    use fortplot_logging, only: log_warning, log_info
    use fortplot_errors, only: fortplot_error_t, SUCCESS, ERROR_RESOURCE_LIMIT, log_error
    use fortplot_gltf, only: gltf_context
    use fortplot_colormap
    use fortplot_pcolormesh
    use fortplot_format_parser, only: parse_format_string, contains_format_chars
    use fortplot_legend
    use fortplot_png, only: png_context, draw_axes_and_labels
    use fortplot_raster, only: draw_rotated_ylabel_raster
    use fortplot_pdf, only: pdf_context, draw_pdf_axes_and_labels
    use fortplot_ascii, only: ascii_context
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    use fortplot_colors, only: parse_color, color_t
    use fortplot_annotations, only: text_annotation_t, COORD_DATA, COORD_FIGURE, COORD_AXIS
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, subplot_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                    PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
                                    PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, &
                                    HALF_WIDTH, IQR_WHISKER_MULTIPLIER
    implicit none

    private
    public :: figure_t, plot_data_t, subplot_t, arrow_data_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
              PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, PLOT_TYPE_BOXPLOT, &
              PLOT_TYPE_SCATTER
    public :: ensure_directory_exists
    public :: COORD_DATA, COORD_FIGURE, COORD_AXIS
    
    ! Note: add_plot supports both RGB arrays and color strings via different parameter names

    ! Histogram constants
    integer, parameter :: DEFAULT_HISTOGRAM_BINS = 10
    integer, parameter :: MAX_SAFE_BINS = 10000
    real(wp), parameter :: IDENTICAL_VALUE_PADDING = 0.5_wp
    real(wp), parameter :: BIN_EDGE_PADDING_FACTOR = 0.001_wp
    
    ! Box plot constants
    real(wp), parameter :: BOX_PLOT_LINE_WIDTH = 2.0_wp
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
        logical :: show_legend = .false.
        integer :: max_plots = 500
        
        ! Line drawing properties
        real(wp) :: current_line_width = 1.0_wp
        
        ! Streamline data (temporary placeholder)
        type(plot_data_t), allocatable :: streamlines(:)
        
        ! Arrow data for streamplot arrows
        type(arrow_data_t), allocatable :: arrow_data(:)
        
        logical :: has_error = .false.
        
        ! Text annotation support
        type(text_annotation_t), allocatable :: annotations(:)
        integer :: annotation_count = 0
        integer :: max_annotations = 100

    contains
        procedure :: initialize
        procedure :: add_plot
        procedure :: add_3d_plot
        procedure :: add_surface
        procedure :: add_scatter_2d
        procedure :: add_scatter_3d
        procedure :: add_scatter => add_scatter_2d  ! Default to 2D
        procedure :: add_contour
        procedure :: add_contour_filled
        procedure :: add_pcolormesh
        procedure :: errorbar
        procedure :: bar
        procedure :: barh
        procedure :: hist
        procedure :: boxplot
        procedure :: streamplot
        procedure :: savefig
        procedure :: set_xlabel
        procedure :: set_ylabel
        procedure :: set_title
        procedure :: set_xscale
        procedure :: set_yscale
        procedure :: set_xlim
        procedure :: set_ylim
        procedure :: set_line_width
        procedure :: set_ydata
        procedure :: legend => figure_legend
        procedure :: show
        procedure :: clear_streamlines
        procedure :: has_3d_plots
        procedure :: text => add_text_annotation
        procedure :: annotate => add_arrow_annotation
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
        self%has_error = .false.
        
        ! Initialize annotations
        if (.not. allocated(self%annotations)) then
            allocate(self%annotations(self%max_annotations))
        end if
        self%annotation_count = 0
        
        ! Initialize legend following SOLID principles  
        self%show_legend = .false.
        
        ! Initialize backend if specified
        if (present(backend)) then
            call initialize_backend(self%backend, backend, self%width, self%height)
        end if
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color_rgb, color_str, marker, markercolor)
        !! Add line plot data with matplotlib/pyplot-fortran format string support
        !! Supports both RGB color arrays (color_rgb) and color strings (color_str)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle, color_str, marker, markercolor
        real(wp), intent(in), optional :: color_rgb(3)
        
        character(len=20) :: parsed_marker, parsed_linestyle
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        if (present(linestyle) .and. contains_format_chars(linestyle)) then
            ! Parse format string and use those values
            call parse_format_string(linestyle, parsed_marker, parsed_linestyle)
            call add_line_plot_data(self, x, y, label, parsed_linestyle, color_rgb, color_str, parsed_marker, markercolor)
        else
            ! Use traditional linestyle with optional marker
            call add_line_plot_data(self, x, y, label, linestyle, color_rgb, color_str, marker, markercolor)
        end if
        call update_data_ranges(self)
    end subroutine add_plot
    
    subroutine add_3d_plot(self, x, y, z, label, linestyle, markersize, linewidth)
        !! Add 3D line plot - pyplot-fortran compatible API
        !! Natural extension of add_plot with z coordinate
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: markersize, linewidth
        
        character(len=20) :: parsed_marker, parsed_linestyle
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        ! Validate input sizes
        if (size(x) /= size(y) .or. size(x) /= size(z)) then
            write(*, '(A)') 'Error: x, y, z arrays must have same size'
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        ! Parse format string if needed
        if (present(linestyle) .and. contains_format_chars(linestyle)) then
            call parse_format_string(linestyle, parsed_marker, parsed_linestyle)
            call add_3d_line_plot_data(self, x, y, z, label, parsed_linestyle, &
                                      parsed_marker, markersize, linewidth)
        else
            call add_3d_line_plot_data(self, x, y, z, label, linestyle, &
                                      '', markersize, linewidth)
        end if
        
        call update_data_ranges(self)
    end subroutine add_3d_plot
    
    subroutine add_scatter_2d(self, x, y, s, c, label, marker, markersize, color, &
                              colormap, vmin, vmax, show_colorbar)
        !! Add enhanced 2D scatter plot with size and color mapping
        !! Supports variable marker sizes (s) and colors (c) for bubble charts
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:)     ! Size mapping array
        real(wp), intent(in), optional :: c(:)     ! Color mapping array
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        logical, intent(in), optional :: show_colorbar
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        call add_scatter_plot_data(self, x, y, s=s, c=c, label=label, marker=marker, &
                                  markersize=markersize, color=color, colormap=colormap, &
                                  vmin=vmin, vmax=vmax, show_colorbar=show_colorbar)
        
        call update_data_ranges(self)
    end subroutine add_scatter_2d
    
    subroutine add_scatter_3d(self, x, y, z, s, c, label, marker, markersize, color, &
                              colormap, vmin, vmax, show_colorbar)
        !! Add enhanced 3D scatter plot with size and color mapping
        !! Supports variable marker sizes (s) and colors (c) for 3D bubble charts
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)   ! z is required for 3D
        real(wp), intent(in), optional :: s(:)     ! Size mapping array
        real(wp), intent(in), optional :: c(:)     ! Color mapping array
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        logical, intent(in), optional :: show_colorbar
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        call add_scatter_plot_data(self, x, y, z=z, s=s, c=c, label=label, marker=marker, &
                                  markersize=markersize, color=color, colormap=colormap, &
                                  vmin=vmin, vmax=vmax, show_colorbar=show_colorbar)
        
        call update_data_ranges(self)
    end subroutine add_scatter_3d
    
    subroutine add_surface(self, x, y, z, label)
        !! Add surface plot - 3D grid data
        !! x, y: 1D arrays defining grid
        !! z: 2D array of values on grid
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:,:)
        character(len=*), intent(in), optional :: label
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        ! Validate grid dimensions
        if (size(z, 1) /= size(x) .or. size(z, 2) /= size(y)) then
            write(*, '(A,I0,A,I0,A,I0,A,I0)') 'Error: Surface z dimensions (', &
                size(z, 1), ',', size(z, 2), ') must match x size (', &
                size(x), ') and y size (', size(y), ')'
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        call add_surface_plot_data(self, x, y, z, label)
        call update_data_ranges(self)
    end subroutine add_surface

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

    subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Add filled contour plot with color levels
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        call add_colored_contour_plot_data(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        call update_data_ranges(self)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        !! Add pcolormesh plot to figure with matplotlib-compatible interface
        !!
        !! Arguments:
        !!   x, y: Coordinate arrays (1D for regular grid)
        !!   c: Color data array (2D)
        !!   colormap: Optional colormap name
        !!   vmin, vmax: Optional color scale limits
        !!   edgecolors: Optional edge color specification
        !!   linewidths: Optional edge line width
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        character(len=*), intent(in), optional :: edgecolors
        real(wp), intent(in), optional :: linewidths
        
        call add_pcolormesh_plot_data(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        call update_data_ranges_pcolormesh(self)
    end subroutine add_pcolormesh

    subroutine bar(self, x, heights, width, label, color)
        !! Add vertical bar chart to figure
        !!
        !! Arguments:
        !!   x: X-axis positions for bars
        !!   heights: Heights of bars
        !!   width: Optional - width of bars (default: 0.8)
        !!   label: Optional - bar chart label for legend
        !!   color: Optional - bar color
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), heights(:)
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        if (size(x) == 0 .or. size(heights) == 0) then
            write(*, '(A)') 'Warning: Cannot create bar chart from empty data'
            return
        end if
        
        if (size(x) /= size(heights)) then
            write(*, '(A)') 'Warning: x and heights arrays must have same size'
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        call add_bar_plot_data(self, x, heights, width, label, color, .false.)
        call update_data_ranges(self)
    end subroutine bar

    subroutine barh(self, y, widths, height, label, color)
        !! Add horizontal bar chart to figure
        !!
        !! Arguments:
        !!   y: Y-axis positions for bars
        !!   widths: Widths of bars
        !!   height: Optional - height of bars (default: 0.8)
        !!   label: Optional - bar chart label for legend
        !!   color: Optional - bar color
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y(:), widths(:)
        real(wp), intent(in), optional :: height
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        if (self%plot_count >= self%max_plots) then
            write(*, '(A)') 'Warning: Maximum number of plots reached'
            return
        end if
        
        if (size(y) == 0 .or. size(widths) == 0) then
            write(*, '(A)') 'Warning: Cannot create bar chart from empty data'
            return
        end if
        
        if (size(y) /= size(widths)) then
            write(*, '(A)') 'Warning: y and widths arrays must have same size'
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        call add_bar_plot_data(self, y, widths, height, label, color, .true.)
        call update_data_ranges(self)
    end subroutine barh

    subroutine hist(self, data, bins, density, label, color)
        !! Add histogram plot to figure with automatic or custom binning
        !!
        !! Arguments:
        !!   data: Input data array to create histogram from
        !!   bins: Optional - number of bins (integer, default: 10)
        !!   density: Optional - normalize to probability density (default: false)
        !!   label: Optional - histogram label for legend
        !!   color: Optional - histogram color
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        if (.not. validate_histogram_input(self, data, bins)) return
        
        self%plot_count = self%plot_count + 1
        
        call add_histogram_plot_data(self, data, bins, density, label, color)
        call update_data_ranges(self)
    end subroutine hist

    subroutine boxplot(self, data, position, width, label, show_outliers, horizontal, color)
        !! Add box plot to figure using statistical data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        real(wp), intent(in), optional :: color(3)
        
        ! Basic input validation (following NO DEFENSIVE PROGRAMMING principle)
        if (size(data) < 1) then
            print *, "Warning: Box plot requires at least 1 data point"
            return
        end if
        
        call add_boxplot_data(self, data, position, width, label, show_outliers, horizontal, color)
        call update_data_ranges_boxplot(self)
    end subroutine boxplot

    subroutine streamplot(self, x, y, u, v, density, color, linewidth, rtol, atol, max_time, arrowsize, arrowstyle)
        !! Add streamline plot to figure using matplotlib-compatible algorithm with arrow support
        use fortplot_streamplot_matplotlib
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth
        real(wp), intent(in), optional :: rtol        !! Relative tolerance for DOPRI5
        real(wp), intent(in), optional :: atol        !! Absolute tolerance for DOPRI5
        real(wp), intent(in), optional :: max_time    !! Maximum integration time
        real(wp), intent(in), optional :: arrowsize   !! Arrow size scaling factor (default: 1.0)
        character(len=*), intent(in), optional :: arrowstyle !! Arrow style (default: '->')
        
        real(wp) :: plot_density, arrow_size_val
        character(len=10) :: arrow_style_val
        real, allocatable :: trajectories(:,:,:)
        integer :: n_trajectories
        integer, allocatable :: trajectory_lengths(:)
        
        if (size(u,1) /= size(x) .or. size(u,2) /= size(y)) then
            self%has_error = .true.
            return
        end if
        
        if (size(v,1) /= size(x) .or. size(v,2) /= size(y)) then
            self%has_error = .true.
            return
        end if
        
        plot_density = 1.0_wp
        if (present(density)) plot_density = density
        
        ! Handle arrow parameters with validation
        arrow_size_val = 1.0_wp  ! Default matplotlib-compatible arrow size
        if (present(arrowsize)) then
            if (arrowsize < 0.0_wp) then
                self%has_error = .true.
                return
            end if
            arrow_size_val = arrowsize
        end if
        
        arrow_style_val = '->'  ! Default matplotlib-compatible arrow style
        if (present(arrowstyle)) then
            if (trim(arrowstyle) /= '->' .and. trim(arrowstyle) /= '-' .and. &
                trim(arrowstyle) /= '<-' .and. trim(arrowstyle) /= '<->') then
                self%has_error = .true.
                return
            end if
            arrow_style_val = trim(arrowstyle)
        end if
        
        ! Update data ranges
        if (.not. self%xlim_set) then
            self%x_min = minval(x)
            self%x_max = maxval(x)
        end if
        if (.not. self%ylim_set) then
            self%y_min = minval(y)
            self%y_max = maxval(y)
        end if
        
        ! Use matplotlib-compatible streamplot implementation
        call streamplot_matplotlib(x, y, u, v, plot_density, trajectories, n_trajectories, trajectory_lengths)
        
        ! Generate arrows along streamlines if arrow size > 0
        if (arrow_size_val > 0.0_wp .and. n_trajectories > 0) then
            call generate_streamplot_arrows(self, trajectories, n_trajectories, trajectory_lengths, &
                                          x, y, u, v, arrow_size_val, arrow_style_val)
        end if
        
        ! Add trajectories to figure
        call add_trajectories_to_figure(self, trajectories, n_trajectories, trajectory_lengths, color, x, y)
        
    contains
        
        subroutine add_trajectories_to_figure(fig, trajectories, n_trajectories, lengths, trajectory_color, x_grid, y_grid)
            !! Add streamline trajectories to figure as regular plots
            class(figure_t), intent(inout) :: fig
            real, intent(in) :: trajectories(:,:,:)
            integer, intent(in) :: n_trajectories
            integer, intent(in) :: lengths(:)
            real(wp), intent(in), optional :: trajectory_color(3)
            real(wp), intent(in) :: x_grid(:), y_grid(:)
            
            integer :: i, j, n_points
            real(wp), allocatable :: traj_x(:), traj_y(:)
            real(wp) :: line_color(3)
            
            ! Set default color (blue)
            line_color = [0.0_wp, 0.447_wp, 0.698_wp]
            if (present(trajectory_color)) line_color = trajectory_color
            
            do i = 1, n_trajectories
                n_points = lengths(i)
                
                if (n_points > 1) then
                    allocate(traj_x(n_points), traj_y(n_points))
                    
                    ! Convert from grid coordinates to data coordinates
                    ! trajectories are in grid coordinates (0 to nx-1, 0 to ny-1)
                    ! need to convert to data coordinates like matplotlib does
                    do j = 1, n_points
                        ! Convert grid coords to data coords: grid2data transformation
                        traj_x(j) = real(trajectories(i, j, 1), wp) * (x_grid(size(x_grid)) - x_grid(1)) / &
                                   real(size(x_grid) - 1, wp) + x_grid(1)
                        traj_y(j) = real(trajectories(i, j, 2), wp) * (y_grid(size(y_grid)) - y_grid(1)) / &
                                   real(size(y_grid) - 1, wp) + y_grid(1)
                    end do
                    
                    ! Add as regular plot
                    call fig%add_plot(traj_x, traj_y, color_rgb=line_color, linestyle='-')
                    
                    deallocate(traj_x, traj_y)
                end if
            end do
        end subroutine add_trajectories_to_figure
        
        subroutine generate_streamplot_arrows(fig, trajectories, n_trajectories, trajectory_lengths, &
                                            x_grid, y_grid, u_field, v_field, arrow_size, arrow_style)
            !! Generate arrows along streamlines using matplotlib-compatible placement algorithm
            class(figure_t), intent(inout) :: fig
            real, intent(in) :: trajectories(:,:,:)
            integer, intent(in) :: n_trajectories
            integer, intent(in) :: trajectory_lengths(:)
            real(wp), intent(in) :: x_grid(:), y_grid(:), u_field(:,:), v_field(:,:)
            real(wp), intent(in) :: arrow_size
            character(len=*), intent(in) :: arrow_style
            
            integer :: max_arrows, arrow_count, traj_idx, arrow_interval, point_idx
            real(wp) :: arrow_x, arrow_y, arrow_dx, arrow_dy, speed_mag
            
            ! Calculate maximum possible arrows based on density and trajectory count
            max_arrows = max(1, min(500, n_trajectories * 3))  ! Limit to prevent memory issues
            
            ! Allocate arrow data array
            if (allocated(fig%arrow_data)) deallocate(fig%arrow_data)
            allocate(fig%arrow_data(max_arrows))
            
            arrow_count = 0
            
            ! Place arrows along each trajectory at regular intervals
            do traj_idx = 1, n_trajectories
                if (trajectory_lengths(traj_idx) < 5) cycle  ! Skip very short trajectories
                
                ! Calculate arrow interval based on trajectory length (matplotlib-style)
                arrow_interval = max(1, trajectory_lengths(traj_idx) / 3)  ! ~3 arrows per trajectory
                
                ! Place arrows at intervals along the trajectory
                do point_idx = arrow_interval, trajectory_lengths(traj_idx) - 1, arrow_interval
                    if (arrow_count >= max_arrows) exit
                    
                    ! Get arrow position from trajectory
                    arrow_x = real(trajectories(traj_idx, point_idx, 1), wp)
                    arrow_y = real(trajectories(traj_idx, point_idx, 2), wp)
                    
                    ! Calculate arrow direction from velocity field at this position
                    call interpolate_velocity_at_point(arrow_x, arrow_y, x_grid, y_grid, &
                                                      u_field, v_field, arrow_dx, arrow_dy, speed_mag)
                    
                    ! Skip if velocity is too small
                    if (speed_mag < 1e-10_wp) cycle
                    
                    ! Normalize direction vector
                    arrow_dx = arrow_dx / speed_mag
                    arrow_dy = arrow_dy / speed_mag
                    
                    ! Store arrow data
                    arrow_count = arrow_count + 1
                    fig%arrow_data(arrow_count)%x = arrow_x
                    fig%arrow_data(arrow_count)%y = arrow_y
                    fig%arrow_data(arrow_count)%dx = arrow_dx
                    fig%arrow_data(arrow_count)%dy = arrow_dy
                    fig%arrow_data(arrow_count)%size = arrow_size
                    fig%arrow_data(arrow_count)%style = arrow_style
                end do
            end do
            
            ! Resize arrow array to actual count
            if (arrow_count > 0) then
                fig%arrow_data = fig%arrow_data(1:arrow_count)
            else
                deallocate(fig%arrow_data)
            end if
        end subroutine generate_streamplot_arrows
        
        subroutine interpolate_velocity_at_point(x_pos, y_pos, x_grid, y_grid, u_field, v_field, &
                                               u_interp, v_interp, speed_mag)
            !! Bilinear interpolation of velocity field at given position
            real(wp), intent(in) :: x_pos, y_pos
            real(wp), intent(in) :: x_grid(:), y_grid(:), u_field(:,:), v_field(:,:)
            real(wp), intent(out) :: u_interp, v_interp, speed_mag
            
            integer :: i, j, i_next, j_next
            real(wp) :: x_frac, y_frac, w00, w01, w10, w11
            real(wp) :: u00, u01, u10, u11, v00, v01, v10, v11
            
            ! Find grid indices
            i = 1
            do while (i < size(x_grid) .and. x_grid(i) < x_pos)
                i = i + 1
            end do
            i = max(1, min(size(x_grid) - 1, i - 1))
            
            j = 1
            do while (j < size(y_grid) .and. y_grid(j) < y_pos)
                j = j + 1
            end do
            j = max(1, min(size(y_grid) - 1, j - 1))
            
            i_next = min(size(x_grid), i + 1)
            j_next = min(size(y_grid), j + 1)
            
            ! Calculate interpolation weights
            if (i_next > i) then
                x_frac = (x_pos - x_grid(i)) / (x_grid(i_next) - x_grid(i))
            else
                x_frac = 0.0_wp
            end if
            
            if (j_next > j) then
                y_frac = (y_pos - y_grid(j)) / (y_grid(j_next) - y_grid(j))
            else
                y_frac = 0.0_wp
            end if
            
            ! Bilinear interpolation weights
            w00 = (1.0_wp - x_frac) * (1.0_wp - y_frac)
            w01 = x_frac * (1.0_wp - y_frac)
            w10 = (1.0_wp - x_frac) * y_frac
            w11 = x_frac * y_frac
            
            ! Get velocity values at grid corners
            u00 = u_field(i, j)
            u01 = u_field(i_next, j)
            u10 = u_field(i, j_next)
            u11 = u_field(i_next, j_next)
            
            v00 = v_field(i, j)
            v01 = v_field(i_next, j)
            v10 = v_field(i, j_next)
            v11 = v_field(i_next, j_next)
            
            ! Interpolate velocity components
            u_interp = w00 * u00 + w01 * u01 + w10 * u10 + w11 * u11
            v_interp = w00 * v00 + w01 * v01 + w10 * v10 + w11 * v11
            
            ! Calculate speed magnitude
            speed_mag = sqrt(u_interp**2 + v_interp**2)
        end subroutine interpolate_velocity_at_point
        
    end subroutine streamplot

    subroutine savefig(self, filename, blocking)
        !! Save figure to file with backend auto-detection
        !! 
        !! Arguments:
        !!   filename: Output filename (extension determines format)
        !!   blocking: Optional - if true, wait for user input after save (default: false)
        use fortplot_security, only: is_safe_path
        use fortplot_logging, only: log_error
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        character(len=20) :: backend_type
        logical :: do_block
        
        ! Validate filename security (Issue #135)
        if (.not. is_safe_path(filename)) then
            call log_error("Unsafe filename rejected: " // trim(filename))
            return
        end if
        
        ! Default to non-blocking
        do_block = .false.
        if (present(blocking)) do_block = blocking
        
        ! Create output directory if needed
        call ensure_directory_exists(filename)
        
        backend_type = get_backend_from_filename(filename)
        
        ! Always reinitialize backend for correct format
        if (allocated(self%backend)) deallocate(self%backend)
        call initialize_backend(self%backend, backend_type, self%width, self%height)
        
        ! Handle GLTF differently - needs 3D data not 2D rendering
        select case (trim(backend_type))
        case ('gltf', 'glb')
            ! Pass 3D plot data directly to GLTF backend
            select type (backend => self%backend)
            type is (gltf_context)
                call prepare_gltf_data(backend, self%plots(1:self%plot_count))
            end select
            call self%backend%save(filename)
        case default
            ! Reset rendered flag to force re-rendering for new backend
            self%rendered = .false.
            call render_figure(self)
            call self%backend%save(filename)
        end select
        
        call log_info('Saved figure: ' // trim(filename))
        
        ! If blocking requested, wait for user input
        if (do_block) then
            call log_info("Press Enter to continue...")
            read(*,*)
        end if
    end subroutine savefig

    subroutine show(self, blocking)
        !! Display figure in ASCII terminal
        !! 
        !! Arguments:
        !!   blocking: Optional - if true, wait for user input after display (default: false)
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: blocking
        logical :: do_block
        
        ! Default to non-blocking
        do_block = .false.
        if (present(blocking)) do_block = blocking
        
        ! Always reinitialize backend for ASCII output
        if (allocated(self%backend)) deallocate(self%backend)
        call initialize_backend(self%backend, 'ascii', 80, 24)
        
        ! Reset rendered flag to force re-rendering for new backend
        self%rendered = .false.
        call render_figure(self)
        call self%backend%save("terminal")
        
        ! If blocking requested, wait for user input
        if (do_block) then
            call log_info("Press Enter to continue...")
            read(*,*)
        end if
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

    subroutine set_line_width(self, width)
        !! Set line width for subsequent plot operations
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: width
        
        self%current_line_width = width
    end subroutine set_line_width

    subroutine destroy(self)
        !! Clean up figure resources
        type(figure_t), intent(inout) :: self
        
        if (allocated(self%plots)) deallocate(self%plots)
        if (allocated(self%backend)) deallocate(self%backend)
    end subroutine destroy

    ! Private helper routines (implementation details)
    
    subroutine add_line_plot_data(self, x, y, label, linestyle, color_rgb, color_str, marker, markercolor)
        !! Add line plot data to internal storage with support for both RGB arrays and color strings
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle, color_str, marker, markercolor
        real(wp), intent(in), optional :: color_rgb(3)
        
        integer :: plot_idx, color_idx
        real(wp) :: rgb(3)
        logical :: success
        
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

        if (present(marker)) then
            self%plots(plot_idx)%marker = marker
        else
            self%plots(plot_idx)%marker = 'None'
        end if
        
        ! Handle color specification - color string takes precedence over RGB array
        if (present(color_str)) then
            call parse_color(color_str, rgb, success)
            if (success) then
                self%plots(plot_idx)%color = rgb
            else
                call log_warning('Invalid color specification: ' // color_str // '. Using default color.')
                color_idx = mod(plot_idx - 1, 6) + 1
                self%plots(plot_idx)%color = self%colors(:, color_idx)
            end if
        else if (present(color_rgb)) then
            self%plots(plot_idx)%color = color_rgb
        else
            color_idx = mod(plot_idx - 1, 6) + 1
            self%plots(plot_idx)%color = self%colors(:, color_idx)
        end if
        
        ! TODO: Handle markercolor separately when marker colors are implemented
        if (present(markercolor)) then
            call log_warning('Separate marker colors not yet implemented. Using line color for markers.')
        end if
    end subroutine add_line_plot_data
    
    subroutine add_3d_line_plot_data(self, x, y, z, label, linestyle, marker, markersize, linewidth)
        !! Add 3D line plot data to internal storage
        !! Following SRP - only handles data storage
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in), optional :: markersize, linewidth
        
        integer :: plot_idx, color_idx
        
        plot_idx = self%plot_count
        self%plots(plot_idx)%plot_type = PLOT_TYPE_LINE
        
        ! Allocate and store coordinates
        if (allocated(self%plots(plot_idx)%x)) deallocate(self%plots(plot_idx)%x)
        if (allocated(self%plots(plot_idx)%y)) deallocate(self%plots(plot_idx)%y)
        if (allocated(self%plots(plot_idx)%z)) deallocate(self%plots(plot_idx)%z)
        
        allocate(self%plots(plot_idx)%x(size(x)))
        allocate(self%plots(plot_idx)%y(size(y)))
        allocate(self%plots(plot_idx)%z(size(z)))
        
        self%plots(plot_idx)%x = x
        self%plots(plot_idx)%y = y
        self%plots(plot_idx)%z = z
        
        ! Set optional properties
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
        
        if (present(marker)) then
            self%plots(plot_idx)%marker = marker
        else
            self%plots(plot_idx)%marker = 'None'
        end if
        
        ! Use default color from palette
        color_idx = mod(plot_idx - 1, 6) + 1
        self%plots(plot_idx)%color = self%colors(:, color_idx)
        
        ! Update data ranges for 3D plots
        if (.not. self%xlim_set) then
            if (plot_idx == 1) then
                self%x_min = minval(x)
                self%x_max = maxval(x)
            else
                self%x_min = min(self%x_min, minval(x))
                self%x_max = max(self%x_max, maxval(x))
            end if
        end if
        
        if (.not. self%ylim_set) then
            if (plot_idx == 1) then
                self%y_min = minval(y)
                self%y_max = maxval(y)
            else
                self%y_min = min(self%y_min, minval(y))
                self%y_max = max(self%y_max, maxval(y))
            end if
        end if
        
        ! Note: markersize and linewidth handled by backend
    end subroutine add_3d_line_plot_data
    
    subroutine add_surface_plot_data(self, x, y, z, label)
        !! Add surface plot data to internal storage
        !! Following SRP - only handles data storage
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:,:)
        character(len=*), intent(in), optional :: label
        
        integer :: plot_idx, color_idx
        
        plot_idx = self%plot_count
        
        ! For now, reuse contour plot type (surface is similar to contour)
        self%plots(plot_idx)%plot_type = PLOT_TYPE_CONTOUR
        
        ! Store grid data
        if (allocated(self%plots(plot_idx)%x_grid)) deallocate(self%plots(plot_idx)%x_grid)
        if (allocated(self%plots(plot_idx)%y_grid)) deallocate(self%plots(plot_idx)%y_grid)
        if (allocated(self%plots(plot_idx)%z_grid)) deallocate(self%plots(plot_idx)%z_grid)
        
        allocate(self%plots(plot_idx)%x_grid(size(x)))
        allocate(self%plots(plot_idx)%y_grid(size(y)))
        allocate(self%plots(plot_idx)%z_grid(size(z,1), size(z,2)))
        
        self%plots(plot_idx)%x_grid = x
        self%plots(plot_idx)%y_grid = y
        self%plots(plot_idx)%z_grid = z
        
        ! Set label
        if (present(label)) then
            self%plots(plot_idx)%label = label
        else
            self%plots(plot_idx)%label = ''
        end if
        
        ! Use default color from palette
        color_idx = mod(plot_idx - 1, 6) + 1
        self%plots(plot_idx)%color = self%colors(:, color_idx)
        
    end subroutine add_surface_plot_data

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
        
        ! Set default contour properties
        self%plots(plot_idx)%use_color_levels = .false.
        
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

    subroutine add_colored_contour_plot_data(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Add colored contour plot data to internal storage
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
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
        
        ! Set color contour properties
        self%plots(plot_idx)%use_color_levels = .true.
        
        if (present(colormap)) then
            self%plots(plot_idx)%colormap = colormap
        else
            self%plots(plot_idx)%colormap = 'crest'
        end if
        
        if (present(show_colorbar)) then
            self%plots(plot_idx)%show_colorbar = show_colorbar
        else
            self%plots(plot_idx)%show_colorbar = .true.
        end if
        
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
    end subroutine add_colored_contour_plot_data

    subroutine add_pcolormesh_plot_data(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths, error)
        !! Add pcolormesh data to plot array
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        character(len=*), intent(in), optional :: edgecolors
        real(wp), intent(in), optional :: linewidths
        type(fortplot_error_t), intent(out), optional :: error
        
        integer :: plot_idx
        
        if (self%plot_count >= self%max_plots) then
            if (present(error)) then
                call error%set_error(ERROR_RESOURCE_LIMIT, &
                    "Maximum number of plots exceeded")
            else
                call log_error(ERROR_RESOURCE_LIMIT, "add_pcolormesh_plot_data")
            end if
            return
        end if
        
        plot_idx = self%plot_count + 1
        self%plots(plot_idx)%plot_type = PLOT_TYPE_PCOLORMESH
        
        ! Initialize pcolormesh with regular grid
        call self%plots(plot_idx)%pcolormesh_data%initialize_regular_grid(x, y, c, colormap)
        
        ! Set vmin/vmax if provided
        if (present(vmin)) then
            self%plots(plot_idx)%pcolormesh_data%vmin = vmin
            self%plots(plot_idx)%pcolormesh_data%vmin_set = .true.
        end if
        if (present(vmax)) then
            self%plots(plot_idx)%pcolormesh_data%vmax = vmax
            self%plots(plot_idx)%pcolormesh_data%vmax_set = .true.
        end if
        
        ! Set edge properties
        if (present(edgecolors)) then
            if (trim(edgecolors) /= 'none' .and. trim(edgecolors) /= '') then
                self%plots(plot_idx)%pcolormesh_data%show_edges = .true.
                ! TODO: Parse color string
            end if
        end if
        if (present(linewidths)) then
            self%plots(plot_idx)%pcolormesh_data%edge_width = linewidths
        end if
        
        ! Update data range if needed
        call self%plots(plot_idx)%pcolormesh_data%get_data_range()
    end subroutine add_pcolormesh_plot_data

    subroutine add_bar_plot_data(self, positions, values, bar_size, label, color, horizontal)
        !! Add bar chart data to internal storage
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: positions(:), values(:)
        real(wp), intent(in), optional :: bar_size
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        logical, intent(in) :: horizontal
        
        integer :: plot_idx, color_idx
        
        plot_idx = self%plot_count
        self%plots(plot_idx)%plot_type = PLOT_TYPE_BAR
        
        ! Store bar data (deallocate first if already allocated)
        if (allocated(self%plots(plot_idx)%bar_x)) deallocate(self%plots(plot_idx)%bar_x)
        if (allocated(self%plots(plot_idx)%bar_heights)) deallocate(self%plots(plot_idx)%bar_heights)
        allocate(self%plots(plot_idx)%bar_x(size(positions)))
        allocate(self%plots(plot_idx)%bar_heights(size(values)))
        self%plots(plot_idx)%bar_x = positions
        self%plots(plot_idx)%bar_heights = values
        
        ! Set bar properties
        if (present(bar_size)) then
            self%plots(plot_idx)%bar_width = bar_size
        else
            self%plots(plot_idx)%bar_width = 0.8_wp
        end if
        
        self%plots(plot_idx)%bar_horizontal = horizontal
        
        ! Set label
        if (present(label)) then
            self%plots(plot_idx)%label = label
        else
            self%plots(plot_idx)%label = ''
        end if
        
        ! Set color
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else
            color_idx = mod(plot_idx - 1, 6) + 1
            self%plots(plot_idx)%color = self%colors(:, color_idx)
        end if
        
        ! Set default bar style
        self%plots(plot_idx)%linestyle = 'solid'
        self%plots(plot_idx)%marker = 'None'
        
        ! Create x,y data for rendering from bar data
        call create_bar_xy_data(self%plots(plot_idx)%bar_x, &
                               self%plots(plot_idx)%bar_heights, &
                               self%plots(plot_idx)%bar_width, &
                               horizontal, &
                               self%plots(plot_idx)%x, &
                               self%plots(plot_idx)%y)
    end subroutine add_bar_plot_data

    subroutine update_data_ranges_pcolormesh(self)
        !! Update figure data ranges after adding pcolormesh plot
        class(figure_t), intent(inout) :: self
        
        integer :: plot_idx
        real(wp) :: x_min_plot, x_max_plot, y_min_plot, y_max_plot
        
        plot_idx = self%plot_count + 1
        
        ! Get data ranges from pcolormesh vertices
        x_min_plot = minval(self%plots(plot_idx)%pcolormesh_data%x_vertices)
        x_max_plot = maxval(self%plots(plot_idx)%pcolormesh_data%x_vertices)
        y_min_plot = minval(self%plots(plot_idx)%pcolormesh_data%y_vertices)
        y_max_plot = maxval(self%plots(plot_idx)%pcolormesh_data%y_vertices)
        
        ! Update figure ranges
        if (self%plot_count == 0) then
            self%x_min = x_min_plot
            self%x_max = x_max_plot
            self%y_min = y_min_plot
            self%y_max = y_max_plot
        else
            self%x_min = min(self%x_min, x_min_plot)
            self%x_max = max(self%x_max, x_max_plot)
            self%y_min = min(self%y_min, y_min_plot)
            self%y_max = max(self%y_max, y_max_plot)
        end if
        
        self%plot_count = plot_idx
    end subroutine update_data_ranges_pcolormesh

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
        
        ! Render Y-axis label ABSOLUTELY LAST (after everything else)
        select type (backend => self%backend)
        type is (png_context)
            if (allocated(self%ylabel)) then
                call draw_rotated_ylabel_raster(backend, self%ylabel)
            end if
        type is (pdf_context)
            ! PDF handles this differently - already done in draw_pdf_axes_and_labels
        end select
        
        ! Render legend if requested (following SOLID principles)
        if (self%show_legend) then
            call legend_render(self%legend_data, self%backend)
        end if
        
        self%rendered = .true.
    end subroutine render_figure

    ! Placeholder implementations for helper routines
    ! These will delegate to specialized modules
    
    subroutine generate_default_contour_levels(plot_data)
        !! Generate default contour levels for a plot
        type(plot_data_t), intent(inout) :: plot_data
        real(wp) :: z_min, z_max, dz
        integer :: i, n_levels
        
        if (.not. allocated(plot_data%z_grid)) return
        
        z_min = minval(plot_data%z_grid)
        z_max = maxval(plot_data%z_grid)
        
        ! Generate 10 evenly spaced levels by default
        n_levels = 10
        if (allocated(plot_data%contour_levels)) deallocate(plot_data%contour_levels)
        allocate(plot_data%contour_levels(n_levels))
        
        dz = (z_max - z_min) / real(n_levels + 1, wp)
        
        do i = 1, n_levels
            plot_data%contour_levels(i) = z_min + real(i, wp) * dz
        end do
    end subroutine generate_default_contour_levels
    
    subroutine calculate_figure_data_ranges(self)
        class(figure_t), intent(inout) :: self
        integer :: i
        real(wp) :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        real(wp) :: x_min_trans, x_max_trans, y_min_trans, y_max_trans
        real(wp) :: plot_x_min, plot_x_max, plot_y_min, plot_y_max, plot_z_min, plot_z_max
        logical :: first_plot, first_3d_plot
        
        if (self%plot_count == 0) return
        
        first_plot = .true.
        first_3d_plot = .true.
        
        do i = 1, self%plot_count
            if (self%plots(i)%plot_type == PLOT_TYPE_LINE) then
                if (self%plots(i)%is_3d()) then
                    ! Handle 3D plots by projecting to 2D first
                    call calculate_3d_plot_ranges(self, i, x_min_orig, x_max_orig, &
                                                 y_min_orig, y_max_orig, first_plot)
                    
                    ! Calculate Z-axis ranges for 3D plots (safely handling NaN/Inf)
                    if (allocated(self%plots(i)%z)) then
                        call safe_minmax_arrays(self%plots(i)%z, plot_z_min, plot_z_max)
                        if (first_3d_plot) then
                            self%z_min = plot_z_min
                            self%z_max = plot_z_max
                            first_3d_plot = .false.
                        else
                            self%z_min = min(self%z_min, plot_z_min)
                            self%z_max = max(self%z_max, plot_z_max)
                        end if
                    end if
                    
                    ! Calculate transformed ranges for rendering
                    if (first_plot) then
                        x_min_trans = apply_scale_transform(x_min_orig, self%xscale, self%symlog_threshold)
                        x_max_trans = apply_scale_transform(x_max_orig, self%xscale, self%symlog_threshold)
                        y_min_trans = apply_scale_transform(y_min_orig, self%yscale, self%symlog_threshold)
                        y_max_trans = apply_scale_transform(y_max_orig, self%yscale, self%symlog_threshold)
                        first_plot = .false.
                    else
                        x_min_trans = min(x_min_trans, apply_scale_transform(x_min_orig, &
                                                                           self%xscale, self%symlog_threshold))
                        x_max_trans = max(x_max_trans, apply_scale_transform(x_max_orig, &
                                                                           self%xscale, self%symlog_threshold))
                        y_min_trans = min(y_min_trans, apply_scale_transform(y_min_orig, &
                                                                           self%yscale, self%symlog_threshold))
                        y_max_trans = max(y_max_trans, apply_scale_transform(y_max_orig, &
                                                                           self%yscale, self%symlog_threshold))
                    end if
                else
                    ! Handle 2D plots as before
                    if (first_plot) then
                        ! Store ORIGINAL data ranges for tick generation (safely handling NaN/Inf)
                        call safe_minmax_arrays(self%plots(i)%x, x_min_orig, x_max_orig)
                        call safe_minmax_arrays(self%plots(i)%y, y_min_orig, y_max_orig)
                        
                        ! Calculate transformed ranges for rendering
                        x_min_trans = apply_scale_transform(x_min_orig, self%xscale, self%symlog_threshold)
                        x_max_trans = apply_scale_transform(x_max_orig, self%xscale, self%symlog_threshold)
                        y_min_trans = apply_scale_transform(y_min_orig, self%yscale, self%symlog_threshold)
                        y_max_trans = apply_scale_transform(y_max_orig, self%yscale, self%symlog_threshold)
                        first_plot = .false.
                    else
                        ! Update original ranges (safely handling NaN/Inf)
                        call safe_minmax_arrays(self%plots(i)%x, plot_x_min, plot_x_max)
                        call safe_minmax_arrays(self%plots(i)%y, plot_y_min, plot_y_max)
                        x_min_orig = min(x_min_orig, plot_x_min)
                        x_max_orig = max(x_max_orig, plot_x_max)
                        y_min_orig = min(y_min_orig, plot_y_min)
                        y_max_orig = max(y_max_orig, plot_y_max)
                        
                        ! Update transformed ranges (using already calculated plot ranges)
                        x_min_trans = min(x_min_trans, apply_scale_transform(plot_x_min, &
                                                                             self%xscale, self%symlog_threshold))
                        x_max_trans = max(x_max_trans, apply_scale_transform(plot_x_max, &
                                                                             self%xscale, self%symlog_threshold))
                        y_min_trans = min(y_min_trans, apply_scale_transform(plot_y_min, &
                                                                             self%yscale, self%symlog_threshold))
                        y_max_trans = max(y_max_trans, apply_scale_transform(plot_y_max, &
                                                                             self%yscale, self%symlog_threshold))
                    end if
                end if
            else if (self%plots(i)%plot_type == PLOT_TYPE_CONTOUR) then
                if (first_plot) then
                    ! Store ORIGINAL contour grid ranges
                    x_min_orig = minval(self%plots(i)%x_grid)
                    x_max_orig = maxval(self%plots(i)%x_grid)
                    y_min_orig = minval(self%plots(i)%y_grid)
                    y_max_orig = maxval(self%plots(i)%y_grid)
                    
                    ! Calculate transformed ranges for rendering
                    x_min_trans = apply_scale_transform(x_min_orig, self%xscale, self%symlog_threshold)
                    x_max_trans = apply_scale_transform(x_max_orig, self%xscale, self%symlog_threshold)
                    y_min_trans = apply_scale_transform(y_min_orig, self%yscale, self%symlog_threshold)
                    y_max_trans = apply_scale_transform(y_max_orig, self%yscale, self%symlog_threshold)
                    first_plot = .false.
                else
                    ! Update original ranges
                    x_min_orig = min(x_min_orig, minval(self%plots(i)%x_grid))
                    x_max_orig = max(x_max_orig, maxval(self%plots(i)%x_grid))
                    y_min_orig = min(y_min_orig, minval(self%plots(i)%y_grid))
                    y_max_orig = max(y_max_orig, maxval(self%plots(i)%y_grid))
                    
                    ! Update transformed ranges
                    x_min_trans = min(x_min_trans, apply_scale_transform(minval(self%plots(i)%x_grid), &
                                                                         self%xscale, self%symlog_threshold))
                    x_max_trans = max(x_max_trans, apply_scale_transform(maxval(self%plots(i)%x_grid), &
                                                                         self%xscale, self%symlog_threshold))
                    y_min_trans = min(y_min_trans, apply_scale_transform(minval(self%plots(i)%y_grid), &
                                                                         self%yscale, self%symlog_threshold))
                    y_max_trans = max(y_max_trans, apply_scale_transform(maxval(self%plots(i)%y_grid), &
                                                                         self%yscale, self%symlog_threshold))
                end if
            else if (self%plots(i)%plot_type == PLOT_TYPE_PCOLORMESH) then
                if (first_plot) then
                    ! Store ORIGINAL pcolormesh grid ranges  
                    x_min_orig = minval(self%plots(i)%pcolormesh_data%x_vertices)
                    x_max_orig = maxval(self%plots(i)%pcolormesh_data%x_vertices)
                    y_min_orig = minval(self%plots(i)%pcolormesh_data%y_vertices)
                    y_max_orig = maxval(self%plots(i)%pcolormesh_data%y_vertices)
                    
                    ! Calculate transformed ranges for rendering
                    x_min_trans = apply_scale_transform(x_min_orig, self%xscale, self%symlog_threshold)
                    x_max_trans = apply_scale_transform(x_max_orig, self%xscale, self%symlog_threshold)
                    y_min_trans = apply_scale_transform(y_min_orig, self%yscale, self%symlog_threshold)
                    y_max_trans = apply_scale_transform(y_max_orig, self%yscale, self%symlog_threshold)
                    first_plot = .false.
                else
                    ! Update original ranges
                    x_min_orig = min(x_min_orig, minval(self%plots(i)%pcolormesh_data%x_vertices))
                    x_max_orig = max(x_max_orig, maxval(self%plots(i)%pcolormesh_data%x_vertices))
                    y_min_orig = min(y_min_orig, minval(self%plots(i)%pcolormesh_data%y_vertices))
                    y_max_orig = max(y_max_orig, maxval(self%plots(i)%pcolormesh_data%y_vertices))
                    
                    ! Update transformed ranges for rendering
                    x_min_trans = min(x_min_trans, apply_scale_transform(minval(self%plots(i)%pcolormesh_data%x_vertices), &
                                                                         self%xscale, self%symlog_threshold))
                    x_max_trans = max(x_max_trans, apply_scale_transform(maxval(self%plots(i)%pcolormesh_data%x_vertices), &
                                                                         self%xscale, self%symlog_threshold))
                    y_min_trans = min(y_min_trans, apply_scale_transform(minval(self%plots(i)%pcolormesh_data%y_vertices), &
                                                                         self%yscale, self%symlog_threshold))
                    y_max_trans = max(y_max_trans, apply_scale_transform(maxval(self%plots(i)%pcolormesh_data%y_vertices), &
                                                                         self%yscale, self%symlog_threshold))
                end if
            else if (self%plots(i)%plot_type == PLOT_TYPE_BAR) then
                if (first_plot) then
                    ! Store ORIGINAL bar chart ranges
                    x_min_orig = minval(self%plots(i)%x)
                    x_max_orig = maxval(self%plots(i)%x)
                    y_min_orig = minval(self%plots(i)%y)
                    y_max_orig = maxval(self%plots(i)%y)
                    
                    ! Calculate transformed ranges for rendering
                    x_min_trans = apply_scale_transform(x_min_orig, self%xscale, self%symlog_threshold)
                    x_max_trans = apply_scale_transform(x_max_orig, self%xscale, self%symlog_threshold)
                    y_min_trans = apply_scale_transform(y_min_orig, self%yscale, self%symlog_threshold)
                    y_max_trans = apply_scale_transform(y_max_orig, self%yscale, self%symlog_threshold)
                    first_plot = .false.
                else
                    ! Update original ranges
                    x_min_orig = min(x_min_orig, minval(self%plots(i)%x))
                    x_max_orig = max(x_max_orig, maxval(self%plots(i)%x))
                    y_min_orig = min(y_min_orig, minval(self%plots(i)%y))
                    y_max_orig = max(y_max_orig, maxval(self%plots(i)%y))
                    
                    ! Update transformed ranges
                    x_min_trans = min(x_min_trans, apply_scale_transform(minval(self%plots(i)%x), &
                                                                         self%xscale, self%symlog_threshold))
                    x_max_trans = max(x_max_trans, apply_scale_transform(maxval(self%plots(i)%x), &
                                                                         self%xscale, self%symlog_threshold))
                    y_min_trans = min(y_min_trans, apply_scale_transform(minval(self%plots(i)%y), &
                                                                         self%yscale, self%symlog_threshold))
                    y_max_trans = max(y_max_trans, apply_scale_transform(maxval(self%plots(i)%y), &
                                                                         self%yscale, self%symlog_threshold))
                end if
            end if
        end do
        
        if (.not. self%xlim_set) then
            self%x_min = x_min_orig  ! Backend gets ORIGINAL coordinates for tick generation
            self%x_max = x_max_orig
            self%x_min_transformed = x_min_trans  ! Store transformed for rendering
            self%x_max_transformed = x_max_trans
        end if
        
        if (.not. self%ylim_set) then
            self%y_min = y_min_orig  ! Backend gets ORIGINAL coordinates for tick generation  
            self%y_max = y_max_orig
            self%y_min_transformed = y_min_trans  ! Store transformed for rendering
            self%y_max_transformed = y_max_trans
        end if
    end subroutine calculate_figure_data_ranges
    
    subroutine setup_coordinate_system(self)
        class(figure_t), intent(inout) :: self
        
        if (.not. self%xlim_set .or. .not. self%ylim_set) then
            call calculate_figure_data_ranges(self)
        end if
        
        ! Set backend data coordinate ranges to TRANSFORMED coordinates for data rendering
        self%backend%x_min = self%x_min_transformed
        self%backend%x_max = self%x_max_transformed
        self%backend%y_min = self%y_min_transformed
        self%backend%y_max = self%y_max_transformed
    end subroutine setup_coordinate_system
    
    subroutine render_figure_background(self)
        class(figure_t), intent(inout) :: self
        ! Clear the background - backend-specific implementation not needed
        ! Background is handled by backend initialization
    end subroutine render_figure_background
    
    subroutine render_figure_axes(self)
        class(figure_t), intent(inout) :: self
        
        
        ! Set axis color to black
        call self%backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! Use matplotlib-style axes with margins for backends that support it
        select type (backend => self%backend)
        type is (png_context)
            call draw_axes_and_labels(backend, self%xscale, self%yscale, self%symlog_threshold, &
                                    self%x_min, self%x_max, self%y_min, self%y_max, &
                                    self%title, self%xlabel, self%ylabel, &
                                    self%z_min, self%z_max, self%has_3d_plots())
        type is (pdf_context)
            call draw_pdf_axes_and_labels(backend, self%xscale, self%yscale, self%symlog_threshold, &
                                        self%x_min, self%x_max, self%y_min, self%y_max, &
                                        self%title, self%xlabel, self%ylabel, &
                                        self%z_min, self%z_max, self%has_3d_plots())
        type is (ascii_context)
            ! ASCII backend: explicitly set title and draw simple axes
            if (allocated(self%title)) then
                call backend%set_title(self%title)
            end if
            call self%backend%line(self%x_min, self%y_min, self%x_max, self%y_min)
            call self%backend%line(self%x_min, self%y_min, self%x_min, self%y_max)
        class default
            ! For other backends, use simple axes
            call self%backend%line(self%x_min, self%y_min, self%x_max, self%y_min)
            call self%backend%line(self%x_min, self%y_min, self%x_min, self%y_max)
        end select
    end subroutine render_figure_axes
    
    subroutine render_all_plots(self)
        class(figure_t), intent(inout) :: self
        integer :: i
        
        ! Render regular plots
        do i = 1, self%plot_count
            ! Set color for this plot
            call self%backend%color(self%plots(i)%color(1), self%plots(i)%color(2), self%plots(i)%color(3))
            
            if (self%plots(i)%plot_type == PLOT_TYPE_LINE) then
                call render_line_plot(self, i)
            else if (self%plots(i)%plot_type == PLOT_TYPE_CONTOUR) then
                call render_contour_plot(self, i)
            else if (self%plots(i)%plot_type == PLOT_TYPE_PCOLORMESH) then
                call render_pcolormesh_plot(self, i)
            else if (self%plots(i)%plot_type == PLOT_TYPE_BAR) then
                call render_bar_plot(self, i)
            else if (self%plots(i)%plot_type == PLOT_TYPE_SCATTER) then
                call render_scatter_plot(self, i)
            end if
        end do
        
        ! Render arrows if they exist
        call render_arrows(self)
        
    end subroutine render_all_plots

    subroutine render_arrows(self)
        !! Render streamplot arrows to the backend
        class(figure_t), intent(inout) :: self
        integer :: i
        real(wp) :: screen_x, screen_y
        
        if (.not. allocated(self%arrow_data)) return
        if (size(self%arrow_data) == 0) return
        
        ! Set arrow color (black)
        call self%backend%color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! Render each arrow using the backend
        do i = 1, size(self%arrow_data)
            ! Transform world coordinates to screen coordinates
            screen_x = transform_x_coordinate(self%arrow_data(i)%x, self%x_min, self%x_max, &
                                            self%width)
            screen_y = transform_y_coordinate(self%arrow_data(i)%y, self%y_min, self%y_max, &
                                            self%height)
            
            ! Call backend-specific arrow rendering
            call self%backend%draw_arrow(screen_x, screen_y, &
                                       self%arrow_data(i)%dx, self%arrow_data(i)%dy, &
                                       self%arrow_data(i)%size, self%arrow_data(i)%style)
        end do
    end subroutine render_arrows

    subroutine render_streamlines(self)
        !! Render all streamlines in the streamlines array
        class(figure_t), intent(inout) :: self
        integer :: i
        
        do i = 1, size(self%streamlines)
            ! Set color for this streamline
            call self%backend%color(self%streamlines(i)%color(1), self%streamlines(i)%color(2), self%streamlines(i)%color(3))
            
            ! Render as line plot
            call render_streamline(self, i)
        end do
    end subroutine render_streamlines

    subroutine render_streamline(self, streamline_idx)
        !! Render a single streamline
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: streamline_idx
        integer :: i
        real(wp) :: x1_screen, y1_screen, x2_screen, y2_screen
        
        
        do i = 1, size(self%streamlines(streamline_idx)%x) - 1
            ! Apply scale transformations
            x1_screen = apply_scale_transform(self%streamlines(streamline_idx)%x(i), self%xscale, self%symlog_threshold)
            y1_screen = apply_scale_transform(self%streamlines(streamline_idx)%y(i), self%yscale, self%symlog_threshold)
            x2_screen = apply_scale_transform(self%streamlines(streamline_idx)%x(i+1), self%xscale, self%symlog_threshold)
            y2_screen = apply_scale_transform(self%streamlines(streamline_idx)%y(i+1), self%yscale, self%symlog_threshold)
            
            ! Draw line segment
            call self%backend%line(x1_screen, y1_screen, x2_screen, y2_screen)
        end do
    end subroutine render_streamline

    subroutine render_line_plot(self, plot_idx)
        !! Render a single line plot with linestyle support (handles 2D and 3D)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        integer :: i
        real(wp) :: x1_screen, y1_screen, x2_screen, y2_screen
        character(len=:), allocatable :: linestyle
        
        if (plot_idx > self%plot_count) return
        if (.not. allocated(self%plots(plot_idx)%x)) return
        if (size(self%plots(plot_idx)%x) < 1) return
        
        ! Get linestyle for this plot
        linestyle = self%plots(plot_idx)%linestyle
        
        ! Draw lines only if linestyle is not 'None' and we have at least 2 points
        if (linestyle /= 'None' .and. size(self%plots(plot_idx)%x) >= 2) then
            ! Set line width for all backends (2.0 for plot data, 1.0 for axes)
            call self%backend%set_line_width(2.0_wp)
            
            ! Check if this is a 3D plot and handle projection
            if (self%plots(plot_idx)%is_3d()) then
                call draw_3d_line_with_style(self, plot_idx, linestyle)
            else
                call draw_line_with_style(self, plot_idx, linestyle)
            end if
        end if

        ! Always render markers regardless of linestyle (matplotlib behavior)
        if (self%plots(plot_idx)%is_3d()) then
            call render_3d_markers(self, plot_idx)
        else
            call render_markers(self, plot_idx)
        end if
    end subroutine render_line_plot

    subroutine render_markers(self, plot_idx)
        !! Render markers at each data point
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        character(len=:), allocatable :: marker
        integer :: i
        real(wp) :: x_trans, y_trans

        if (plot_idx > self%plot_count) return
        if (.not. allocated(self%plots(plot_idx)%marker)) return

        marker = self%plots(plot_idx)%marker
        if (marker == 'None') return

        do i = 1, size(self%plots(plot_idx)%x)
            x_trans = apply_scale_transform(self%plots(plot_idx)%x(i), self%xscale, self%symlog_threshold)
            y_trans = apply_scale_transform(self%plots(plot_idx)%y(i), self%yscale, self%symlog_threshold)
            call self%backend%draw_marker(x_trans, y_trans, marker)
        end do

    end subroutine render_markers

    subroutine draw_3d_line_with_style(self, plot_idx, linestyle)
        !! Draw 3D line plot with projection to 2D
        use fortplot_raster, only: raster_context
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        character(len=*), intent(in) :: linestyle
        
        real(wp), allocatable :: x2d(:), y2d(:)
        real(wp), allocatable :: x_norm(:), y_norm(:), z_norm(:)
        real(wp) :: orig_x_min, orig_x_max, orig_y_min, orig_y_max
        integer :: n
        
        n = size(self%plots(plot_idx)%x)
        allocate(x2d(n), y2d(n))
        allocate(x_norm(n), y_norm(n), z_norm(n))
        
        call normalize_3d_data_for_projection(self, plot_idx, x_norm, y_norm, z_norm)
        call project_normalized_3d_data(x_norm, y_norm, z_norm, x2d, y2d)
        call setup_3d_coordinate_system(self, x2d, y2d, orig_x_min, orig_x_max, &
                                       orig_y_min, orig_y_max)
        call draw_projected_3d_lines(self, x2d, y2d)
        call restore_original_coordinate_system(self, orig_x_min, orig_x_max, &
                                               orig_y_min, orig_y_max)
    end subroutine draw_3d_line_with_style
    
    subroutine normalize_3d_data_for_projection(self, plot_idx, x_norm, y_norm, z_norm)
        !! Normalize 3D data to unit cube for consistent projection
        class(figure_t), intent(in) :: self
        integer, intent(in) :: plot_idx
        real(wp), intent(out) :: x_norm(:), y_norm(:), z_norm(:)
        
        integer :: i, n
        
        n = size(self%plots(plot_idx)%x)
        
        do i = 1, n
            call normalize_coordinate_value(self%plots(plot_idx)%x(i), self%x_min, &
                                          self%x_max, x_norm(i))
            call normalize_coordinate_value(self%plots(plot_idx)%y(i), self%y_min, &
                                          self%y_max, y_norm(i))
            call normalize_coordinate_value(self%plots(plot_idx)%z(i), self%z_min, &
                                          self%z_max, z_norm(i))
        end do
    end subroutine normalize_3d_data_for_projection
    
    subroutine normalize_coordinate_value(value, min_val, max_val, normalized_value)
        !! Normalize single coordinate value to [0,1] range
        real(wp), intent(in) :: value, min_val, max_val
        real(wp), intent(out) :: normalized_value
        
        if (max_val - min_val > 0.0_wp) then
            normalized_value = (value - min_val) / (max_val - min_val)
        else
            normalized_value = 0.5_wp  ! Center if no range
        end if
    end subroutine normalize_coordinate_value
    
    subroutine project_normalized_3d_data(x_norm, y_norm, z_norm, x2d, y2d)
        !! Project normalized 3D data to 2D coordinates
        real(wp), intent(in) :: x_norm(:), y_norm(:), z_norm(:)
        real(wp), intent(out) :: x2d(:), y2d(:)
        
        real(wp) :: azim, elev, dist
        
        call get_default_view_angles(azim, elev, dist)
        call project_3d_to_2d(x_norm, y_norm, z_norm, azim, elev, dist, x2d, y2d)
    end subroutine project_normalized_3d_data
    
    subroutine setup_3d_coordinate_system(self, x2d, y2d, orig_x_min, orig_x_max, &
                                         orig_y_min, orig_y_max)
        !! Setup coordinate system for 3D projection rendering
        use fortplot_raster, only: raster_context
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x2d(:), y2d(:)
        real(wp), intent(out) :: orig_x_min, orig_x_max, orig_y_min, orig_y_max
        
        real(wp) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        
        call calculate_projection_bounds(x2d, y2d, proj_x_min, proj_x_max, &
                                        proj_y_min, proj_y_max)
        call save_and_set_backend_coordinates(self, proj_x_min, proj_x_max, &
                                             proj_y_min, proj_y_max, &
                                             orig_x_min, orig_x_max, &
                                             orig_y_min, orig_y_max)
    end subroutine setup_3d_coordinate_system
    
    subroutine calculate_projection_bounds(x2d, y2d, proj_x_min, proj_x_max, &
                                          proj_y_min, proj_y_max)
        !! Calculate bounds of projected 2D data
        real(wp), intent(in) :: x2d(:), y2d(:)
        real(wp), intent(out) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        
        proj_x_min = minval(x2d)
        proj_x_max = maxval(x2d)
        proj_y_min = minval(y2d)
        proj_y_max = maxval(y2d)
    end subroutine calculate_projection_bounds
    
    subroutine save_and_set_backend_coordinates(self, proj_x_min, proj_x_max, &
                                               proj_y_min, proj_y_max, &
                                               orig_x_min, orig_x_max, &
                                               orig_y_min, orig_y_max)
        !! Save original backend coordinates and set to projection bounds
        use fortplot_raster, only: raster_context
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        real(wp), intent(out) :: orig_x_min, orig_x_max, orig_y_min, orig_y_max
        
        select type (ctx => self%backend)
        class is (raster_context)
            orig_x_min = ctx%x_min
            orig_x_max = ctx%x_max
            orig_y_min = ctx%y_min
            orig_y_max = ctx%y_max
            
            ctx%x_min = proj_x_min
            ctx%x_max = proj_x_max
            ctx%y_min = proj_y_min
            ctx%y_max = proj_y_max
        end select
    end subroutine save_and_set_backend_coordinates
    
    subroutine draw_projected_3d_lines(self, x2d, y2d)
        !! Draw lines using projected 2D coordinates
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x2d(:), y2d(:)
        
        integer :: i, n
        
        n = size(x2d)
        do i = 1, n-1
            call self%backend%line(x2d(i), y2d(i), x2d(i+1), y2d(i+1))
        end do
    end subroutine draw_projected_3d_lines
    
    subroutine restore_original_coordinate_system(self, orig_x_min, orig_x_max, &
                                                 orig_y_min, orig_y_max)
        !! Restore original backend coordinate system
        use fortplot_raster, only: raster_context
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: orig_x_min, orig_x_max, orig_y_min, orig_y_max
        
        select type (ctx => self%backend)
        class is (raster_context)
            ctx%x_min = orig_x_min
            ctx%x_max = orig_x_max
            ctx%y_min = orig_y_min
            ctx%y_max = orig_y_max
        end select
    end subroutine restore_original_coordinate_system

    subroutine render_3d_markers(self, plot_idx)
        !! Render markers for 3D plot points
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        
        real(wp), allocatable :: x2d(:), y2d(:)
        real(wp), allocatable :: x_norm(:), y_norm(:), z_norm(:)
        real(wp) :: orig_x_min, orig_x_max, orig_y_min, orig_y_max
        character(len=:), allocatable :: marker
        integer :: n
        
        if (.not. validate_marker_requirements(self, plot_idx, marker)) return
        
        n = size(self%plots(plot_idx)%x)
        allocate(x2d(n), y2d(n))
        allocate(x_norm(n), y_norm(n), z_norm(n))
        
        call normalize_3d_data_for_projection(self, plot_idx, x_norm, y_norm, z_norm)
        call project_normalized_3d_data(x_norm, y_norm, z_norm, x2d, y2d)
        call setup_3d_coordinate_system(self, x2d, y2d, orig_x_min, orig_x_max, &
                                       orig_y_min, orig_y_max)
        call draw_projected_3d_markers(self, x2d, y2d, marker)
        call restore_original_coordinate_system(self, orig_x_min, orig_x_max, &
                                               orig_y_min, orig_y_max)
    end subroutine render_3d_markers
    
    function validate_marker_requirements(self, plot_idx, marker) result(is_valid)
        !! Validate that markers should be rendered for this plot
        class(figure_t), intent(in) :: self
        integer, intent(in) :: plot_idx
        character(len=:), allocatable, intent(out) :: marker
        logical :: is_valid
        
        is_valid = .false.
        
        if (.not. allocated(self%plots(plot_idx)%marker)) return
        
        marker = self%plots(plot_idx)%marker
        if (marker == 'None' .or. marker == '') return
        
        is_valid = .true.
    end function validate_marker_requirements
    
    subroutine draw_projected_3d_markers(self, x2d, y2d, marker)
        !! Draw markers at projected 2D positions
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x2d(:), y2d(:)
        character(len=*), intent(in) :: marker
        
        integer :: i, n
        
        n = size(x2d)
        do i = 1, n
            call self%backend%draw_marker(x2d(i), y2d(i), marker)
        end do
    end subroutine draw_projected_3d_markers

    subroutine calculate_3d_plot_ranges(self, plot_idx, x_min, x_max, y_min, y_max, first_plot)
        !! Calculate data ranges for 3D plot by projecting to 2D
        class(figure_t), intent(in) :: self
        integer, intent(in) :: plot_idx
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        logical, intent(inout) :: first_plot
        
        real(wp), allocatable :: x2d(:), y2d(:)
        real(wp) :: azim, elev, dist
        integer :: n
        
        n = size(self%plots(plot_idx)%x)
        allocate(x2d(n), y2d(n))
        
        ! Get default viewing angles
        call get_default_view_angles(azim, elev, dist)
        
        ! Project 3D data to 2D
        call project_3d_to_2d(self%plots(plot_idx)%x, &
                              self%plots(plot_idx)%y, &
                              self%plots(plot_idx)%z, &
                              azim, elev, dist, x2d, y2d)
        
        ! Calculate ranges from projected data
        if (first_plot) then
            x_min = minval(x2d)
            x_max = maxval(x2d)
            y_min = minval(y2d)
            y_max = maxval(y2d)
            first_plot = .false.
        else
            x_min = min(x_min, minval(x2d))
            x_max = max(x_max, maxval(x2d))
            y_min = min(y_min, minval(y2d))
            y_max = max(y_max, maxval(y2d))
        end if
    end subroutine calculate_3d_plot_ranges

    subroutine render_contour_plot(self, plot_idx)
        !! Render a single contour plot using proper marching squares algorithm
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        integer :: level_idx
        real(wp) :: contour_level
        real(wp) :: z_min, z_max
        real(wp), dimension(3) :: level_color
        
        if (plot_idx > self%plot_count) return
        if (.not. allocated(self%plots(plot_idx)%z_grid)) return
        
        ! Get data range for filtering valid levels
        z_min = minval(self%plots(plot_idx)%z_grid)
        z_max = maxval(self%plots(plot_idx)%z_grid)
        
        ! For ASCII backend with colored contours, render as heatmap
        ! Polymorphic call - only ASCII backend implements this meaningfully
        if (self%plots(plot_idx)%use_color_levels) then
            call self%backend%fill_heatmap(self%plots(plot_idx)%x_grid, &
                                         self%plots(plot_idx)%y_grid, &
                                         self%plots(plot_idx)%z_grid, &
                                         z_min, z_max)
            return
        end if
        
        ! Render each contour level that falls within data range
        if (allocated(self%plots(plot_idx)%contour_levels)) then
            do level_idx = 1, size(self%plots(plot_idx)%contour_levels)
                contour_level = self%plots(plot_idx)%contour_levels(level_idx)
                
                ! Only render levels within the data range
                if (contour_level > z_min .and. contour_level < z_max) then
                    ! Set color based on contour level
                    if (self%plots(plot_idx)%use_color_levels) then
                        call colormap_value_to_color(contour_level, z_min, z_max, &
                                                   self%plots(plot_idx)%colormap, level_color)
                        call self%backend%color(level_color(1), level_color(2), level_color(3))
                    end if
                    
                    call trace_contour_level(self, plot_idx, contour_level)
                end if
            end do
        else
            ! Draw a few default contour levels with colors
            call render_default_contour_levels(self, plot_idx, z_min, z_max)
        end if
    end subroutine render_contour_plot

    subroutine render_pcolormesh_plot(self, plot_idx)
        !! Render pcolormesh plot as colored quadrilaterals
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        
        integer :: i, j
        real(wp) :: x_quad(4), y_quad(4)
        real(wp) :: x_screen(4), y_screen(4)
        real(wp) :: color(3), c_value, c_min, c_max
        
        ! Get colormap range from pcolormesh data
        c_min = self%plots(plot_idx)%pcolormesh_data%vmin
        c_max = self%plots(plot_idx)%pcolormesh_data%vmax
        
        ! Try polymorphic heatmap rendering - ASCII implements this, others ignore
        block
            real(wp), allocatable :: x_centers(:), y_centers(:)
            integer :: nx, ny, i, j
            
            nx = self%plots(plot_idx)%pcolormesh_data%nx
            ny = self%plots(plot_idx)%pcolormesh_data%ny
            
            allocate(x_centers(nx), y_centers(ny))
            
            ! Calculate cell centers from vertices
            do i = 1, nx
                x_centers(i) = 0.5_wp * (self%plots(plot_idx)%pcolormesh_data%x_vertices(1, i) + &
                                       self%plots(plot_idx)%pcolormesh_data%x_vertices(1, i+1))
            end do
            
            do j = 1, ny
                y_centers(j) = 0.5_wp * (self%plots(plot_idx)%pcolormesh_data%y_vertices(j, 1) + &
                                       self%plots(plot_idx)%pcolormesh_data%y_vertices(j+1, 1))
            end do
            
            ! Polymorphic heatmap call - only ASCII implements meaningfully
            call self%backend%fill_heatmap(x_centers, y_centers, &
                                         self%plots(plot_idx)%pcolormesh_data%c_values, &
                                         c_min, c_max)
        end block
        
        ! Render each quadrilateral
        do i = 1, self%plots(plot_idx)%pcolormesh_data%ny
            do j = 1, self%plots(plot_idx)%pcolormesh_data%nx
                ! Get quad vertices in world coordinates
                call self%plots(plot_idx)%pcolormesh_data%get_quad_vertices(i, j, x_quad, y_quad)
                
                ! Transform to screen coordinates
                call transform_quad_to_screen(self, x_quad, y_quad, x_screen, y_screen)
                
                ! Get color for this quad
                c_value = self%plots(plot_idx)%pcolormesh_data%c_values(i, j)
                call colormap_value_to_color(c_value, c_min, c_max, &
                                            self%plots(plot_idx)%pcolormesh_data%colormap_name, color)
                
                ! Draw filled quadrilateral
                call self%backend%color(color(1), color(2), color(3))
                call draw_filled_quad(self%backend, x_screen, y_screen)
                
                ! Draw edges if requested
                if (self%plots(plot_idx)%pcolormesh_data%show_edges) then
                    call self%backend%color(self%plots(plot_idx)%pcolormesh_data%edge_color(1), &
                                          self%plots(plot_idx)%pcolormesh_data%edge_color(2), &
                                          self%plots(plot_idx)%pcolormesh_data%edge_color(3))
                    call draw_quad_edges(self%backend, x_screen, y_screen, &
                                        self%plots(plot_idx)%pcolormesh_data%edge_width)
                end if
            end do
        end do
    end subroutine render_pcolormesh_plot

    subroutine render_bar_plot(self, plot_idx)
        !! Render bar chart as filled rectangles
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        
        integer :: i, n_bars
        real(wp) :: x1, y1, x2, y2
        real(wp) :: x_screen(4), y_screen(4)
        real(wp) :: bar_left, bar_right, bar_bottom, bar_top
        
        if (plot_idx > self%plot_count) return
        if (.not. allocated(self%plots(plot_idx)%bar_x)) return
        if (.not. allocated(self%plots(plot_idx)%bar_heights)) return
        
        n_bars = size(self%plots(plot_idx)%bar_x)
        
        ! Render each bar as a filled rectangle
        do i = 1, n_bars
            if (self%plots(plot_idx)%bar_horizontal) then
                ! Horizontal bars
                bar_left = 0.0_wp
                bar_right = self%plots(plot_idx)%bar_heights(i)
                bar_bottom = self%plots(plot_idx)%bar_x(i) - self%plots(plot_idx)%bar_width * 0.5_wp
                bar_top = self%plots(plot_idx)%bar_x(i) + self%plots(plot_idx)%bar_width * 0.5_wp
            else
                ! Vertical bars
                bar_left = self%plots(plot_idx)%bar_x(i) - self%plots(plot_idx)%bar_width * 0.5_wp
                bar_right = self%plots(plot_idx)%bar_x(i) + self%plots(plot_idx)%bar_width * 0.5_wp
                bar_bottom = 0.0_wp
                bar_top = self%plots(plot_idx)%bar_heights(i)
            end if
            
            ! Skip bars with zero or negative height/width
            if (self%plots(plot_idx)%bar_horizontal) then
                if (abs(bar_right - bar_left) < 1e-10_wp) cycle
            else
                if (abs(bar_top - bar_bottom) < 1e-10_wp) cycle
            end if
            
            ! Transform coordinates for rendering
            x_screen(1) = apply_scale_transform(bar_left, self%xscale, self%symlog_threshold)
            y_screen(1) = apply_scale_transform(bar_bottom, self%yscale, self%symlog_threshold)
            x_screen(2) = apply_scale_transform(bar_right, self%xscale, self%symlog_threshold)
            y_screen(2) = apply_scale_transform(bar_bottom, self%yscale, self%symlog_threshold)
            x_screen(3) = apply_scale_transform(bar_right, self%xscale, self%symlog_threshold)
            y_screen(3) = apply_scale_transform(bar_top, self%yscale, self%symlog_threshold)
            x_screen(4) = apply_scale_transform(bar_left, self%xscale, self%symlog_threshold)
            y_screen(4) = apply_scale_transform(bar_top, self%yscale, self%symlog_threshold)
            
            ! Draw filled rectangle
            call draw_filled_quad(self%backend, x_screen, y_screen)
            
            ! Draw outline
            call self%backend%line(x_screen(1), y_screen(1), x_screen(2), y_screen(2))
            call self%backend%line(x_screen(2), y_screen(2), x_screen(3), y_screen(3))
            call self%backend%line(x_screen(3), y_screen(3), x_screen(4), y_screen(4))
            call self%backend%line(x_screen(4), y_screen(4), x_screen(1), y_screen(1))
        end do
    end subroutine render_bar_plot

    subroutine render_scatter_plot(self, plot_idx)
        !! Render scatter plot with enhanced size and color mapping
        use fortplot_colormap, only: colormap_value_to_color
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        
        integer :: i
        real(wp) :: x_screen, y_screen, marker_size
        real(wp) :: color_value
        real(wp), dimension(3) :: rgb_color
        logical :: has_color_mapping, has_size_mapping
        
        type(plot_data_t) :: plot
        plot = self%plots(plot_idx)
        
        ! Validate scatter plot data
        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) return
        if (size(plot%x) == 0 .or. size(plot%y) == 0) return
        
        ! Check for enhanced mapping features
        has_size_mapping = allocated(plot%scatter_sizes)
        has_color_mapping = allocated(plot%scatter_colors)
        
        ! Render each scatter point
        do i = 1, size(plot%x)
            ! Transform coordinates to screen space
            x_screen = transform_x_coordinate(plot%x(i), self%x_min, self%x_max, self%width)
            y_screen = transform_y_coordinate(plot%y(i), self%y_min, self%y_max, self%height)
            
            ! Determine marker size
            if (has_size_mapping) then
                marker_size = plot%scatter_sizes(i)
            else
                marker_size = plot%scatter_size_default
            end if
            
            ! Set marker color
            if (has_color_mapping) then
                ! Map color value to RGB using colormap
                color_value = plot%scatter_colors(i)
                call colormap_value_to_color(color_value, plot%scatter_vmin, plot%scatter_vmax, &
                                            plot%scatter_colormap, rgb_color)
                call self%backend%color(rgb_color(1), rgb_color(2), rgb_color(3))
            end if
            
            ! Draw marker with appropriate size
            call draw_enhanced_scatter_marker(self, x_screen, y_screen, &
                                             plot%marker, marker_size)
        end do
        
        ! Render colorbar if requested
        if (has_color_mapping .and. plot%scatter_colorbar) then
            call render_scatter_colorbar(self, plot_idx)
        end if
    end subroutine render_scatter_plot

    subroutine draw_enhanced_scatter_marker(self, x, y, style, size)
        !! Draw scatter marker with enhanced size handling
        use fortplot_markers, only: get_default_marker
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x, y, size
        character(len=*), intent(in) :: style
        
        character(len=10) :: marker_style
        
        ! Validate marker style
        if (len_trim(style) == 0 .or. trim(style) == 'None') then
            marker_style = get_default_marker()
        else
            marker_style = style
        end if
        
        ! TODO: Backend-specific size handling will be implemented later
        ! For now, use the standard marker interface which handles basic drawing
        ! The size parameter will be used for future enhancement of the marker interface
        
        ! Draw marker using backend's marker interface
        call self%backend%draw_marker(x, y, marker_style)
    end subroutine draw_enhanced_scatter_marker

    subroutine render_scatter_colorbar(self, plot_idx)
        !! Render colorbar for scatter plot color mapping
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        
        ! TODO: Implement colorbar rendering
        ! This is a placeholder for colorbar integration
        ! Colorbar rendering will be enhanced in a separate task
    end subroutine render_scatter_colorbar

    subroutine render_default_contour_levels(self, plot_idx, z_min, z_max)
        !! Render default contour levels with optional coloring
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: z_min, z_max
        real(wp), dimension(3) :: level_color
        real(wp) :: level_values(3)
        integer :: i
        
        level_values = [z_min + 0.2_wp * (z_max - z_min), &
                       z_min + 0.5_wp * (z_max - z_min), &
                       z_min + 0.8_wp * (z_max - z_min)]
        
        do i = 1, 3
            ! Set color based on contour level
            if (self%plots(plot_idx)%use_color_levels) then
                call colormap_value_to_color(level_values(i), z_min, z_max, &
                                           self%plots(plot_idx)%colormap, level_color)
                call self%backend%color(level_color(1), level_color(2), level_color(3))
            end if
            
            call trace_contour_level(self, plot_idx, level_values(i))
        end do
    end subroutine render_default_contour_levels

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

    subroutine figure_legend(self, location)
        !! Add legend to figure following SOLID principles
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in), optional :: location
        integer :: i
        
        ! Initialize legend if not already done
        if (.not. allocated(self%legend_data%entries)) then
            allocate(self%legend_data%entries(0))
            self%legend_data%num_entries = 0
        end if
        
        ! Set legend position if specified  
        if (present(location)) then
            call self%legend_data%set_position(location)
        end if
        
        ! Populate legend with labeled plots (DRY principle)
        do i = 1, self%plot_count
            if (allocated(self%plots(i)%label)) then
                if (len_trim(self%plots(i)%label) > 0) then
                    call self%legend_data%add_entry(self%plots(i)%label, &
                                             self%plots(i)%color, &
                                             self%plots(i)%linestyle, &
                                             self%plots(i)%marker)
                end if
            end if
        end do
        
        self%show_legend = .true.
    end subroutine figure_legend
    
    subroutine clear_streamlines(self)
        !! Clear streamline data
        class(figure_t), intent(inout) :: self
        
        if (allocated(self%streamlines)) then
            deallocate(self%streamlines)
        end if
    end subroutine clear_streamlines

    subroutine transform_quad_to_screen(self, x_quad, y_quad, x_screen, y_screen)
        !! Transform quadrilateral vertices from world to screen coordinates
        class(figure_t), intent(in) :: self
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp), intent(out) :: x_screen(4), y_screen(4)
        
        integer :: i
        
        ! Apply scale transformations only (backend handles screen mapping)
        do i = 1, 4
            x_screen(i) = apply_scale_transform(x_quad(i), self%xscale, self%symlog_threshold)
            y_screen(i) = apply_scale_transform(y_quad(i), self%yscale, self%symlog_threshold)
        end do
    end subroutine transform_quad_to_screen

    subroutine draw_filled_quad(backend, x_screen, y_screen)
        !! Draw filled quadrilateral
        use fortplot_raster, only: raster_context
        use fortplot_png, only: png_context
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_screen(4), y_screen(4)
        
        ! Use polymorphic filled quad rendering - eliminates SELECT TYPE
        call backend%fill_quad(x_screen, y_screen)
    end subroutine draw_filled_quad

    subroutine draw_quad_edges(backend, x_screen, y_screen, line_width)
        !! Draw quadrilateral edges
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_screen(4), y_screen(4)
        real(wp), intent(in) :: line_width
        
        ! Draw quad outline
        call backend%line(x_screen(1), y_screen(1), x_screen(2), y_screen(2))
        call backend%line(x_screen(2), y_screen(2), x_screen(3), y_screen(3))
        call backend%line(x_screen(3), y_screen(3), x_screen(4), y_screen(4))
        call backend%line(x_screen(4), y_screen(4), x_screen(1), y_screen(1))
    end subroutine draw_quad_edges

    subroutine set_ydata(self, plot_index, y_new)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        real(wp), intent(in) :: y_new(:)
        
        if (plot_index < 1 .or. plot_index > self%plot_count) then
            call log_warning("Invalid plot index for set_ydata")
            return
        end if
        
        if (self%plots(plot_index)%plot_type /= PLOT_TYPE_LINE) then
            call log_warning("set_ydata only supported for line plots")
            return
        end if
        
        if (.not. allocated(self%plots(plot_index)%y)) then
            call log_warning("Plot has no y data to update")
            return
        end if
        
        if (size(y_new) /= size(self%plots(plot_index)%y)) then
            call log_warning("New y data size does not match existing size")
            return
        end if
        
        self%plots(plot_index)%y = y_new
    end subroutine set_ydata
    
    logical function has_3d_plots(self) result(has_3d)
        !! Check if figure contains any 3D plots
        !! Following KISS - simple loop check
        class(figure_t), intent(in) :: self
        integer :: i
        
        has_3d = .false.
        
        do i = 1, self%plot_count
            if (self%plots(i)%is_3d()) then
                has_3d = .true.
                return
            end if
        end do
        
    end function has_3d_plots
    
    subroutine prepare_gltf_data(backend, plots)
        !! Prepare GLTF data from plot data
        !! Following SRP - handles data conversion
        type(gltf_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plots(:)
        
        integer :: i
        
        ! Process each plot
        do i = 1, size(plots)
            if (plots(i)%is_3d()) then
                select case (plots(i)%plot_type)
                case (PLOT_TYPE_LINE)
                    ! Add 3D line data
                    if (allocated(plots(i)%z)) then
                        call backend%add_3d_line_data(plots(i)%x, plots(i)%y, plots(i)%z)
                    end if
                case (PLOT_TYPE_CONTOUR)
                    ! Add surface data (surface uses contour type)
                    if (allocated(plots(i)%z_grid)) then
                        call backend%add_3d_surface_data(plots(i)%x_grid, plots(i)%y_grid, &
                                                       plots(i)%z_grid)
                    end if
                end select
            end if
        end do
        
    end subroutine prepare_gltf_data
    
    subroutine ensure_directory_exists(filename)
        !! Create directory path for output file if it doesn't exist
        use fortplot_security, only: safe_create_directory
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dir_path
        integer :: last_slash
        logical :: success
        
        ! Find the last directory separator
        last_slash = 0
        do last_slash = len_trim(filename), 1, -1
            if (filename(last_slash:last_slash) == '/') exit
        end do
        
        ! If there's a directory path, create it safely
        if (last_slash > 1) then
            dir_path = filename(1:last_slash-1)
            ! Use secure directory creation
            call safe_create_directory(dir_path, success)
            if (.not. success) then
                call log_warning("Could not create directory: " // trim(dir_path))
            end if
        end if
    end subroutine ensure_directory_exists

    subroutine create_bar_xy_data(positions, values, bar_width, horizontal, x, y)
        !! Convert bar chart data to x,y coordinates for rendering
        real(wp), intent(in) :: positions(:), values(:), bar_width
        logical, intent(in) :: horizontal
        real(wp), allocatable, intent(out) :: x(:), y(:)
        
        integer :: n_bars, i, point_idx
        real(wp) :: bar_left, bar_right, bar_bottom, bar_top
        
        n_bars = size(positions)
        
        ! Create bar outline: 4 points per bar (corners of rectangle)
        allocate(x(4 * n_bars + 1), y(4 * n_bars + 1))
        
        point_idx = 1
        do i = 1, n_bars
            if (horizontal) then
                ! Horizontal bars: bar extends from 0 to value in x-direction
                bar_left = 0.0_wp
                bar_right = values(i)
                bar_bottom = positions(i) - bar_width * 0.5_wp
                bar_top = positions(i) + bar_width * 0.5_wp
            else
                ! Vertical bars: bar extends from 0 to value in y-direction
                bar_left = positions(i) - bar_width * 0.5_wp
                bar_right = positions(i) + bar_width * 0.5_wp
                bar_bottom = 0.0_wp
                bar_top = values(i)
            end if
            
            ! Rectangle corners (bottom-left, bottom-right, top-right, top-left)
            x(point_idx) = bar_left
            y(point_idx) = bar_bottom
            point_idx = point_idx + 1
            
            x(point_idx) = bar_right
            y(point_idx) = bar_bottom
            point_idx = point_idx + 1
            
            x(point_idx) = bar_right
            y(point_idx) = bar_top
            point_idx = point_idx + 1
            
            x(point_idx) = bar_left
            y(point_idx) = bar_top
            point_idx = point_idx + 1
        end do
        
        ! Close the shape to first point
        x(point_idx) = x(1)
        y(point_idx) = y(1)
    end subroutine create_bar_xy_data

    subroutine errorbar(self, x, y, xerr, yerr, xerr_lower, xerr_upper, &
                       yerr_lower, yerr_upper, capsize, elinewidth, &
                       label, linestyle, marker, color)
        !! Add error bar plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        real(wp), intent(in), optional :: xerr_lower(:), xerr_upper(:)
        real(wp), intent(in), optional :: yerr_lower(:), yerr_upper(:)
        real(wp), intent(in), optional :: capsize, elinewidth
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in), optional :: color(3)
        
        type(plot_data_t) :: plot_data
        integer :: n_points
        
        n_points = size(x)
        
        ! Validate input sizes
        if (size(y) /= n_points) then
            write(*,*) 'Error: x and y arrays must have the same size'
            return
        end if
        
        ! Initialize plot data
        plot_data%plot_type = PLOT_TYPE_ERRORBAR
        allocate(plot_data%x(n_points), plot_data%y(n_points))
        plot_data%x = x
        plot_data%y = y
        
        ! Handle error bar parameters (minimal implementation)
        if (present(yerr)) then
            allocate(plot_data%yerr(n_points))
            plot_data%yerr = yerr
            plot_data%has_yerr = .true.
        end if
        
        if (present(xerr)) then
            allocate(plot_data%xerr(n_points))
            plot_data%xerr = xerr
            plot_data%has_xerr = .true.
        end if
        
        ! Handle optional styling parameters
        if (present(capsize)) plot_data%capsize = capsize
        if (present(elinewidth)) plot_data%elinewidth = elinewidth
        if (present(label)) plot_data%label = label
        if (present(linestyle)) plot_data%linestyle = linestyle
        if (present(marker)) plot_data%marker = marker
        if (present(color)) plot_data%color = color
        
        ! Add to plots array
        self%plot_count = self%plot_count + 1
        if (self%plot_count > self%max_plots) then
            write(*,*) 'Warning: Maximum number of plots exceeded'
            return
        end if
        
        self%plots(self%plot_count) = plot_data
    end subroutine errorbar

    ! Histogram helper functions - minimal implementations for compilation
    function validate_histogram_input(self, data, bins) result(is_valid)
        !! Validate histogram input parameters
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical :: is_valid
        
        is_valid = .true.
        
        if (self%plot_count >= self%max_plots) then
            is_valid = .false.
            return
        end if
        
        if (size(data) == 0) then
            is_valid = .false.
            return
        end if
        
        if (present(bins)) then
            if (bins <= 0 .or. bins > MAX_SAFE_BINS) then
                is_valid = .false.
                return
            end if
        end if
    end function validate_histogram_input

    subroutine add_histogram_plot_data(self, data, bins, density, label, color)
        !! Add histogram data to internal storage - minimal implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        integer :: plot_idx, n_bins
        real(wp) :: data_min, data_max, bin_width
        integer :: i
        
        plot_idx = self%plot_count
        self%plots(plot_idx)%plot_type = PLOT_TYPE_HISTOGRAM
        
        ! Simple histogram implementation
        n_bins = 10
        if (present(bins)) n_bins = bins
        
        data_min = minval(data)
        data_max = maxval(data)
        
        ! Handle case where all data points are identical
        if (data_max == data_min) then
            ! Add small padding to create valid bins
            data_min = data_min - 0.5_wp
            data_max = data_max + 0.5_wp
        end if
        
        bin_width = (data_max - data_min) / real(n_bins, wp)
        
        ! Initialize histogram arrays (Fortran automatically reallocates)
        self%plots(plot_idx)%hist_bin_edges = [(data_min + real(i-1, wp) * bin_width, i = 1, n_bins + 1)]
        self%plots(plot_idx)%hist_counts = [(0.0_wp, i = 1, n_bins)]
        do i = 1, size(data)
            if (data(i) >= data_min .and. data(i) <= data_max) then
                associate(bin_idx => min(n_bins, max(1, int((data(i) - data_min) / bin_width) + 1)))
                    self%plots(plot_idx)%hist_counts(bin_idx) = self%plots(plot_idx)%hist_counts(bin_idx) + 1.0_wp
                end associate
            end if
        end do
        
        ! Set density flag
        if (present(density)) then
            self%plots(plot_idx)%hist_density = density
        end if
        
        ! Set plot properties
        if (present(label)) then
            self%plots(plot_idx)%label = label
        else
            self%plots(plot_idx)%label = ''
        end if
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else
            self%plots(plot_idx)%color = [0.0_wp, 0.5_wp, 1.0_wp]  ! Default blue
        end if
    end subroutine add_histogram_plot_data

    subroutine add_boxplot_data(self, data, position, width, label, show_outliers, horizontal, color)
        !! Add box plot data - minimal implementation
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        real(wp), intent(in), optional :: color(3)
        
        integer :: plot_idx
        
        plot_idx = self%plot_count
        self%plots(plot_idx)%plot_type = PLOT_TYPE_BOXPLOT
        
        ! Copy data
        if (allocated(self%plots(plot_idx)%box_data)) deallocate(self%plots(plot_idx)%box_data)
        allocate(self%plots(plot_idx)%box_data(size(data)))
        self%plots(plot_idx)%box_data = data
        
        ! Set optional parameters with defaults
        if (present(position)) then
            self%plots(plot_idx)%position = position
        else
            self%plots(plot_idx)%position = 1.0_wp
        end if
        
        if (present(width)) then
            self%plots(plot_idx)%width = width
        else
            self%plots(plot_idx)%width = 0.6_wp
        end if
        
        if (present(show_outliers)) then
            self%plots(plot_idx)%show_outliers = show_outliers
        else
            self%plots(plot_idx)%show_outliers = .true.
        end if
        
        if (present(horizontal)) then
            self%plots(plot_idx)%horizontal = horizontal
        else
            self%plots(plot_idx)%horizontal = .false.
        end if
        
        if (present(label)) then
            self%plots(plot_idx)%label = label
        else
            self%plots(plot_idx)%label = ''
        end if
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else
            self%plots(plot_idx)%color = [0.5_wp, 0.5_wp, 0.5_wp]  ! Default gray
        end if
        
        ! Calculate basic statistics (simplified)
        associate(sorted_data => data)  ! TODO: implement proper sorting
            if (size(sorted_data) > 0) then
                self%plots(plot_idx)%q1 = minval(sorted_data)
                self%plots(plot_idx)%q2 = (minval(sorted_data) + maxval(sorted_data)) * 0.5_wp
                self%plots(plot_idx)%q3 = maxval(sorted_data)
                self%plots(plot_idx)%whisker_low = minval(sorted_data)
                self%plots(plot_idx)%whisker_high = maxval(sorted_data)
            end if
        end associate
    end subroutine add_boxplot_data

    subroutine update_data_ranges_boxplot(self)
        !! Update figure data ranges after adding box plot - minimal implementation
        class(figure_t), intent(inout) :: self
        
        integer :: plot_idx
        real(wp) :: x_min_plot, x_max_plot, y_min_plot, y_max_plot
        
        plot_idx = self%plot_count
        
        if (self%plots(plot_idx)%horizontal) then
            ! Horizontal box plot
            x_min_plot = self%plots(plot_idx)%whisker_low
            x_max_plot = self%plots(plot_idx)%whisker_high
            y_min_plot = self%plots(plot_idx)%position - self%plots(plot_idx)%width * 0.5_wp
            y_max_plot = self%plots(plot_idx)%position + self%plots(plot_idx)%width * 0.5_wp
        else
            ! Vertical box plot
            y_min_plot = self%plots(plot_idx)%whisker_low
            y_max_plot = self%plots(plot_idx)%whisker_high
            x_min_plot = self%plots(plot_idx)%position - self%plots(plot_idx)%width * 0.5_wp
            x_max_plot = self%plots(plot_idx)%position + self%plots(plot_idx)%width * 0.5_wp
        end if
        
        ! Update figure ranges
        if (self%plot_count == 1) then
            self%x_min = x_min_plot
            self%x_max = x_max_plot
            self%y_min = y_min_plot
            self%y_max = y_max_plot
        else
            self%x_min = min(self%x_min, x_min_plot)
            self%x_max = max(self%x_max, x_max_plot)
            self%y_min = min(self%y_min, y_min_plot)
            self%y_max = max(self%y_max, y_max_plot)
        end if
    end subroutine update_data_ranges_boxplot

    subroutine add_scatter_plot_data(self, x, y, z, s, c, label, marker, markersize, color, &
                                    colormap, vmin, vmax, show_colorbar)
        !! Helper subroutine to add enhanced scatter plot data with size and color mapping
        use fortplot_markers, only: get_default_marker, validate_marker_style
        use fortplot_colormap, only: validate_colormap_name
        use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
        
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: z(:)
        real(wp), intent(in), optional :: s(:)     ! Size mapping array
        real(wp), intent(in), optional :: c(:)     ! Color mapping array
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        logical, intent(in), optional :: show_colorbar
        
        type(plot_data_t) :: plot_data
        
        if (.not. validate_scatter_input_arrays(x, y, z, s, c)) return
        
        call initialize_scatter_plot_data(plot_data)
        call filter_valid_scatter_data(x, y, z, s, c, plot_data)
        call configure_scatter_marker(plot_data, marker, markersize)
        call configure_scatter_color_mapping(plot_data, c, colormap, vmin, vmax, show_colorbar)
        call set_scatter_common_properties(plot_data, label, color)
        call add_plot_to_figure(self, plot_data)
    end subroutine add_scatter_plot_data
    
    function validate_scatter_input_arrays(x, y, z, s, c) result(is_valid)
        !! Validate scatter plot input arrays for size consistency
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: z(:), s(:), c(:)
        logical :: is_valid
        
        integer :: n_points
        
        is_valid = .false.
        n_points = size(x)
        
        if (size(y) /= n_points) then
            write(*, '(A)') 'Error: x and y arrays must have same size'
            return
        end if
        
        if (present(z) .and. size(z) /= n_points) then
            write(*, '(A)') 'Error: z array must have same size as x and y'
            return
        end if
        
        if (present(s) .and. size(s) /= n_points) then
            write(*, '(A)') 'Error: size array must have same size as x and y'
            return
        end if
        
        if (present(c) .and. size(c) /= n_points) then
            write(*, '(A)') 'Error: color array must have same size as x and y'
            return
        end if
        
        is_valid = .true.
    end function validate_scatter_input_arrays
    
    subroutine initialize_scatter_plot_data(plot_data)
        !! Initialize scatter plot data structure
        type(plot_data_t), intent(out) :: plot_data
        
        plot_data%plot_type = PLOT_TYPE_SCATTER
    end subroutine initialize_scatter_plot_data
    
    subroutine configure_scatter_marker(plot_data, marker, markersize)
        !! Configure marker style and size for scatter plot
        use fortplot_markers, only: get_default_marker, validate_marker_style
        type(plot_data_t), intent(inout) :: plot_data
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        
        if (present(marker)) then
            call set_validated_marker_style(plot_data, marker)
        else
            plot_data%marker = get_default_marker()
        end if
        
        if (present(markersize)) then
            plot_data%scatter_size_default = markersize
        end if
    end subroutine configure_scatter_marker
    
    subroutine set_validated_marker_style(plot_data, marker)
        !! Set marker style with validation
        use fortplot_markers, only: get_default_marker, validate_marker_style
        type(plot_data_t), intent(inout) :: plot_data
        character(len=*), intent(in) :: marker
        
        if (validate_marker_style(marker)) then
            plot_data%marker = marker
        else
            plot_data%marker = get_default_marker()
            write(*, '(A,A,A)') 'Warning: Invalid marker "', trim(marker), &
                              '", using default'
        end if
    end subroutine set_validated_marker_style
    
    subroutine configure_scatter_color_mapping(plot_data, c, colormap, vmin, vmax, show_colorbar)
        !! Configure color mapping for scatter plot
        use fortplot_colormap, only: validate_colormap_name
        type(plot_data_t), intent(inout) :: plot_data
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        logical, intent(in), optional :: show_colorbar
        
        if (.not. present(c)) return
        
        call configure_color_range(plot_data, vmin, vmax)
        call configure_colormap_name(plot_data, colormap)
        call configure_colorbar_display(plot_data, show_colorbar)
    end subroutine configure_scatter_color_mapping
    
    subroutine configure_color_range(plot_data, vmin, vmax)
        !! Configure color range for scatter plot
        type(plot_data_t), intent(inout) :: plot_data
        real(wp), intent(in), optional :: vmin, vmax
        
        real(wp) :: c_min, c_max
        
        if (present(vmin) .and. present(vmax)) then
            plot_data%scatter_vmin = vmin
            plot_data%scatter_vmax = vmax
            plot_data%scatter_vrange_set = .true.
        else
            call auto_scale_color_range(plot_data)
        end if
    end subroutine configure_color_range
    
    subroutine auto_scale_color_range(plot_data)
        !! Auto-scale color range from data
        type(plot_data_t), intent(inout) :: plot_data
        
        real(wp) :: c_min, c_max
        
        if (allocated(plot_data%scatter_colors) .and. size(plot_data%scatter_colors) > 0) then
            c_min = minval(plot_data%scatter_colors)
            c_max = maxval(plot_data%scatter_colors)
            plot_data%scatter_vmin = c_min
            plot_data%scatter_vmax = c_max
            plot_data%scatter_vrange_set = .false.
        end if
    end subroutine auto_scale_color_range
    
    subroutine configure_colormap_name(plot_data, colormap)
        !! Configure colormap name with validation
        use fortplot_colormap, only: validate_colormap_name
        type(plot_data_t), intent(inout) :: plot_data
        character(len=*), intent(in), optional :: colormap
        
        if (.not. present(colormap)) return
        
        if (validate_colormap_name(colormap)) then
            plot_data%scatter_colormap = colormap
        else
            plot_data%scatter_colormap = 'viridis'
            write(*, '(A,A,A)') 'Warning: Invalid colormap "', trim(colormap), &
                              '", using viridis'
        end if
    end subroutine configure_colormap_name
    
    subroutine configure_colorbar_display(plot_data, show_colorbar)
        !! Configure colorbar display options
        type(plot_data_t), intent(inout) :: plot_data
        logical, intent(in), optional :: show_colorbar
        
        plot_data%scatter_colorbar = .true.
        if (present(show_colorbar)) plot_data%scatter_colorbar = show_colorbar
    end subroutine configure_colorbar_display
    
    subroutine set_scatter_common_properties(plot_data, label, color)
        !! Set common scatter plot properties
        type(plot_data_t), intent(inout) :: plot_data
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        if (present(label)) plot_data%label = label
        if (present(color)) plot_data%color = color
    end subroutine set_scatter_common_properties
    
    subroutine add_plot_to_figure(self, plot_data)
        !! Add plot data to figure's plot collection
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(in) :: plot_data
        
        self%plot_count = self%plot_count + 1
        self%plots(self%plot_count) = plot_data
    end subroutine add_plot_to_figure
    
    subroutine safe_minmax_arrays(arr, min_val, max_val)
        !! Safely calculate min/max values ignoring NaN and infinite values
        real(wp), intent(in) :: arr(:)
        real(wp), intent(out) :: min_val, max_val
        
        real(wp), allocatable :: valid_values(:)
        logical, allocatable :: valid_mask(:)
        integer :: i, n_valid
        
        allocate(valid_mask(size(arr)))
        
        ! Create mask for finite values
        do i = 1, size(arr)
            valid_mask(i) = ieee_is_finite(arr(i))
        end do
        
        n_valid = count(valid_mask)
        
        if (n_valid == 0) then
            ! No valid values - set reasonable defaults
            min_val = 0.0_wp
            max_val = 1.0_wp
            write(*, '(A)') 'Warning: No finite values in data array, using default range [0,1]'
        else
            ! Allocate and copy valid values
            allocate(valid_values(n_valid))
            valid_values = pack(arr, valid_mask)
            
            ! Calculate min/max of valid values
            min_val = minval(valid_values)
            max_val = maxval(valid_values)
            
            deallocate(valid_values)
        end if
        
        deallocate(valid_mask)
    end subroutine safe_minmax_arrays

    subroutine filter_valid_scatter_data(x_in, y_in, z_in, s_in, c_in, plot_data)
        !! Filter out NaN and infinite values from scatter data
        use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
        
        real(wp), intent(in) :: x_in(:), y_in(:)
        real(wp), intent(in), optional :: z_in(:)
        real(wp), intent(in), optional :: s_in(:)
        real(wp), intent(in), optional :: c_in(:)
        type(plot_data_t), intent(inout) :: plot_data
        
        logical, allocatable :: valid_mask(:)
        integer :: i, j, n_valid, n_total
        
        n_total = size(x_in)
        allocate(valid_mask(n_total))
        
        ! Create mask for valid data points
        do i = 1, n_total
            valid_mask(i) = ieee_is_finite(x_in(i)) .and. ieee_is_finite(y_in(i))
            if (present(z_in)) valid_mask(i) = valid_mask(i) .and. ieee_is_finite(z_in(i))
        end do
        
        n_valid = count(valid_mask)
        
        if (n_valid == 0) then
            write(*, '(A)') 'Warning: No valid data points for scatter plot'
            return
        end if
        
        ! Allocate arrays for valid data
        allocate(plot_data%x(n_valid))
        allocate(plot_data%y(n_valid))
        if (present(z_in)) allocate(plot_data%z(n_valid))
        if (present(s_in)) allocate(plot_data%scatter_sizes(n_valid))
        if (present(c_in)) allocate(plot_data%scatter_colors(n_valid))
        
        ! Copy valid data points
        j = 1
        do i = 1, n_total
            if (valid_mask(i)) then
                plot_data%x(j) = x_in(i)
                plot_data%y(j) = y_in(i)
                if (present(z_in)) plot_data%z(j) = z_in(i)
                
                ! Handle size mapping with validation and clamping
                if (present(s_in)) then
                    plot_data%scatter_sizes(j) = max(1.0_wp, min(200.0_wp, s_in(i)))
                end if
                
                ! Handle color mapping
                if (present(c_in)) then
                    plot_data%scatter_colors(j) = c_in(i)
                end if
                
                j = j + 1
            end if
        end do
        
        deallocate(valid_mask)
    end subroutine filter_valid_scatter_data
    
    ! Helper methods for calculate_figure_data_ranges refactoring
    subroutine initialize_range_calculation(first_plot)
        !! Initialize range calculation state
        logical, intent(out) :: first_plot
        
        first_plot = .true.
    end subroutine initialize_range_calculation

    subroutine add_text_annotation(self, x, y, text, coord_type, font_size, rotation, &
                                  alignment, has_bbox, font_family)
        !! Add text annotation to the figure
        !! 
        !! Arguments:
        !!   x, y: Position coordinates
        !!   text: Text content to display
        !!   coord_type: Optional coordinate system (COORD_DATA, COORD_FIGURE, COORD_AXIS)
        !!   font_size: Optional font size in points
        !!   rotation: Optional rotation angle in degrees
        !!   alignment: Optional text alignment ('left', 'center', 'right')
        !!   has_bbox: Optional background box flag
        !!   font_family: Optional font family name
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        integer, intent(in), optional :: coord_type
        real(wp), intent(in), optional :: font_size, rotation
        character(len=*), intent(in), optional :: alignment
        logical, intent(in), optional :: has_bbox
        character(len=*), intent(in), optional :: font_family
        
        ! Check if we have space for more annotations
        if (self%annotation_count >= self%max_annotations) then
            call log_warning("Maximum number of annotations reached")
            return
        end if
        
        ! Increment annotation count and add annotation
        self%annotation_count = self%annotation_count + 1
        
        ! Set annotation properties
        self%annotations(self%annotation_count)%text = text
        self%annotations(self%annotation_count)%x = x
        self%annotations(self%annotation_count)%y = y
        
        ! Set optional properties with defaults
        self%annotations(self%annotation_count)%coord_type = COORD_DATA
        if (present(coord_type)) self%annotations(self%annotation_count)%coord_type = coord_type
        
        self%annotations(self%annotation_count)%font_size = 12.0_wp
        if (present(font_size)) self%annotations(self%annotation_count)%font_size = font_size
        
        self%annotations(self%annotation_count)%rotation = 0.0_wp
        if (present(rotation)) self%annotations(self%annotation_count)%rotation = rotation
        
        self%annotations(self%annotation_count)%alignment = 'left'
        if (present(alignment)) self%annotations(self%annotation_count)%alignment = alignment
        
        self%annotations(self%annotation_count)%has_bbox = .false.
        if (present(has_bbox)) self%annotations(self%annotation_count)%has_bbox = has_bbox
        
        ! Mark figure as needing re-rendering
        self%rendered = .false.
    end subroutine add_text_annotation

    subroutine add_arrow_annotation(self, text, xy, xytext, xy_coord_type, xytext_coord_type, &
                                   font_size, alignment, has_bbox)
        !! Add arrow annotation to the figure
        !! 
        !! Arguments:
        !!   text: Text content to display
        !!   xy: Position coordinates of arrow tip [x, y]
        !!   xytext: Position coordinates of text [x, y]
        !!   xy_coord_type: Optional coordinate system for arrow tip
        !!   xytext_coord_type: Optional coordinate system for text
        !!   font_size: Optional font size in points
        !!   alignment: Optional text alignment
        !!   has_bbox: Optional background box flag
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: xy(2), xytext(2)
        integer, intent(in), optional :: xy_coord_type, xytext_coord_type
        real(wp), intent(in), optional :: font_size
        character(len=*), intent(in), optional :: alignment
        logical, intent(in), optional :: has_bbox
        
        ! Check if we have space for more annotations
        if (self%annotation_count >= self%max_annotations) then
            call log_warning("Maximum number of annotations reached")
            return
        end if
        
        ! Increment annotation count and add annotation
        self%annotation_count = self%annotation_count + 1
        
        ! Set annotation properties
        self%annotations(self%annotation_count)%text = text
        self%annotations(self%annotation_count)%x = xy(1)
        self%annotations(self%annotation_count)%y = xy(2)
        
        ! Set arrow properties
        self%annotations(self%annotation_count)%has_arrow = .true.
        self%annotations(self%annotation_count)%xytext_x = xytext(1)
        self%annotations(self%annotation_count)%xytext_y = xytext(2)
        
        ! Set coordinate systems with defaults
        self%annotations(self%annotation_count)%coord_type = COORD_DATA
        if (present(xy_coord_type)) self%annotations(self%annotation_count)%coord_type = xy_coord_type
        
        self%annotations(self%annotation_count)%xytext_coord_type = COORD_DATA
        if (present(xytext_coord_type)) &
            self%annotations(self%annotation_count)%xytext_coord_type = xytext_coord_type
        
        ! Set optional properties with defaults
        self%annotations(self%annotation_count)%font_size = 12.0_wp
        if (present(font_size)) self%annotations(self%annotation_count)%font_size = font_size
        
        self%annotations(self%annotation_count)%alignment = 'left'
        if (present(alignment)) self%annotations(self%annotation_count)%alignment = alignment
        
        self%annotations(self%annotation_count)%has_bbox = .false.
        if (present(has_bbox)) self%annotations(self%annotation_count)%has_bbox = has_bbox
        
        ! Mark figure as needing re-rendering
        self%rendered = .false.
    end subroutine add_arrow_annotation

end module fortplot_figure_core