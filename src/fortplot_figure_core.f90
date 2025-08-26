module fortplot_figure_core
    !! Core figure management module (refactored for SOLID principles)
    !! 
    !! This module provides the main user interface for creating scientific plots
    !! with support for line plots, contour plots, and mixed plotting across
    !! PNG, PDF, and ASCII backends. Uses deferred rendering for efficiency.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales
    use fortplot_utils, only: initialize_backend, get_backend_from_filename
    use fortplot_axes
    use fortplot_colormap
    use fortplot_pcolormesh
    use fortplot_format_parser, only: parse_format_string, contains_format_chars
    use fortplot_legend, only: legend_t
    use fortplot_png, only: png_context
    use fortplot_pdf, only: pdf_context
    use fortplot_ascii, only: ascii_context
    ! Import refactored modules
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH
    use fortplot_rendering
    use fortplot_contour_algorithms
    use fortplot_logging, only: log_error
    implicit none

    private
    public :: figure_t, plot_data_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH

    type :: figure_t
        !! Main figure class - coordinates plotting operations
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
        
        ! Legend support
        type(legend_t) :: legend_data
        logical :: show_legend = .false.
        integer :: max_plots = 500
        
        ! Line drawing properties
        real(wp) :: current_line_width = 1.0_wp
        
        ! Streamline data
        type(plot_data_t), allocatable :: streamlines(:)
        logical :: has_error = .false.

    contains
        procedure :: initialize
        procedure :: add_plot
        procedure :: add_contour
        procedure :: add_contour_filled
        procedure :: add_pcolormesh
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
        procedure, private :: add_line_plot_data
        procedure, private :: add_contour_plot_data
        procedure, private :: add_colored_contour_plot_data
        procedure, private :: add_pcolormesh_plot_data
        procedure, private :: update_data_ranges
        procedure, private :: update_data_ranges_pcolormesh
        procedure, private :: render_figure
        procedure, private :: calculate_figure_data_ranges
        procedure, private :: setup_coordinate_system
        procedure, private :: render_figure_background
        procedure, private :: render_figure_axes
        procedure, private :: render_all_plots
        procedure, private :: generate_default_contour_levels
        final :: destroy
    end type figure_t

contains

    subroutine initialize(self, width, height, backend)
        !! Initialize the figure with specified dimensions and backend
        !! Now ensures backend is properly initialized to prevent Issue #355
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        
        if (present(width)) self%width = width
        if (present(height)) self%height = height
        
        ! Initialize backend - default to PNG if not specified
        if (present(backend)) then
            call initialize_backend(self%backend, backend, self%width, self%height)
        else
            ! Default to PNG backend to prevent uninitialized backend (Issue #355 fix)
            if (.not. allocated(self%backend)) then
                call initialize_backend(self%backend, 'png', self%width, self%height)
            end if
        end if
        
        ! Reset plot counter
        self%plot_count = 0
        self%rendered = .false.
        
        ! Allocate plots array
        if (allocated(self%plots)) deallocate(self%plots)
        allocate(self%plots(self%max_plots))
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        !! Add a line plot to the figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        real(wp) :: plot_color(3)
        character(len=:), allocatable :: ls
        
        ! Determine color
        if (present(color)) then
            plot_color = color
        else
            plot_color = self%colors(:, mod(self%plot_count, 6) + 1)
        end if
        
        ! Determine linestyle
        if (present(linestyle)) then
            ls = linestyle
        else
            ls = '-'
        end if
        
        ! Add the plot data
        call self%add_line_plot_data(x, y, label, ls, plot_color, marker='')
    end subroutine add_plot

    subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        !! Add a contour plot to the figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call self%add_contour_plot_data(x_grid, y_grid, z_grid, levels, label)
    end subroutine add_contour

    subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Add a filled contour plot with color mapping
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        call self%add_colored_contour_plot_data(x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        !! Add a pcolormesh plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        
        call self%add_pcolormesh_plot_data(x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
    end subroutine add_pcolormesh

    subroutine streamplot(self, x, y, u, v, density, color, linewidth, rtol, atol, max_time)
        !! Streamplot functionality not available on figure instances
        !! Use the pyplot-style streamplot interface instead:
        !!   use fortplot_matplotlib, only: streamplot
        !!   call streamplot(x, y, u, v)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth
        real(wp), intent(in), optional :: rtol, atol, max_time
        
        ! Set error state and provide guidance
        self%has_error = .true.
        call log_error('streamplot not implemented for figure instances. ' // &
                      'Use pyplot-style streamplot from fortplot_matplotlib module.')
    end subroutine streamplot

    subroutine savefig(self, filename, blocking)
        !! Save figure to file
        !! Automatically switches backend based on file extension (Issue #323 fix)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        
        character(len=20) :: required_backend, current_backend
        logical :: block
        logical :: need_backend_switch
        
        block = .true.
        if (present(blocking)) block = blocking
        
        ! Determine required backend from filename extension
        required_backend = get_backend_from_filename(filename)
        
        ! Determine current backend type
        select type (backend => self%backend)
        type is (png_context)
            current_backend = 'png'
        type is (pdf_context)
            current_backend = 'pdf'
        type is (ascii_context)
            current_backend = 'ascii'
        class default
            current_backend = 'unknown'
        end select
        
        ! Check if we need to switch backends
        need_backend_switch = (trim(required_backend) /= trim(current_backend))
        
        if (need_backend_switch) then
            ! Deallocate current backend and initialize correct one
            if (allocated(self%backend)) deallocate(self%backend)
            call initialize_backend(self%backend, required_backend, self%width, self%height)
            
            ! Force re-rendering with new backend
            self%rendered = .false.
        end if
        
        ! Render if not already rendered (or if we switched backends)
        if (.not. self%rendered) then
            call self%render_figure()
        end if
        
        ! Save the figure with correct backend
        call self%backend%save(filename)
    end subroutine savefig

    subroutine show(self, blocking)
        !! Display the figure
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: blocking
        
        logical :: block
        
        block = .true.
        if (present(blocking)) block = blocking
        
        ! Render if not already rendered
        if (.not. self%rendered) then
            call self%render_figure()
        end if
        
        ! Display the figure
        call self%backend%save("terminal")
    end subroutine show

    subroutine set_xlabel(self, label)
        !! Set x-axis label
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        self%xlabel = label
    end subroutine set_xlabel

    subroutine set_ylabel(self, label)
        !! Set y-axis label
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        self%ylabel = label
    end subroutine set_ylabel

    subroutine set_title(self, title)
        !! Set figure title
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: title
        self%title = title
    end subroutine set_title

    subroutine set_xscale(self, scale, threshold)
        !! Set x-axis scale type
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        self%xscale = scale
        if (present(threshold)) self%symlog_threshold = threshold
    end subroutine set_xscale

    subroutine set_yscale(self, scale, threshold)
        !! Set y-axis scale type
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        self%yscale = scale
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

    subroutine set_line_width(self, width)
        !! Set line width for subsequent plots
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: width
        self%current_line_width = width
    end subroutine set_line_width

    subroutine set_ydata(self, plot_index, y_new)
        !! Update y data for an existing plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        real(wp), intent(in) :: y_new(:)
        
        if (plot_index < 1 .or. plot_index > self%plot_count) then
            print *, "Warning: Invalid plot index", plot_index
            return
        end if
        
        if (.not. allocated(self%plots(plot_index)%y)) then
            print *, "Warning: Plot", plot_index, "has no y data to update"
            return
        end if
        
        if (size(y_new) /= size(self%plots(plot_index)%y)) then
            print *, "Warning: New y data size", size(y_new), &
                     "does not match existing size", size(self%plots(plot_index)%y)
            return
        end if
        
        self%plots(plot_index)%y = y_new
    end subroutine set_ydata

    subroutine figure_legend(self, location)
        !! Add legend to figure
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in), optional :: location
        
        character(len=:), allocatable :: loc
        integer :: i
        
        loc = 'upper right'
        if (present(location)) loc = location
        
        self%show_legend = .true.
        
        ! Set legend position based on location string
        call self%legend_data%set_position(loc)
        
        ! Add legend entries from plots
        do i = 1, self%plot_count
            ! Only add legend entry if label is allocated and not empty
            if (allocated(self%plots(i)%label)) then
                if (len_trim(self%plots(i)%label) > 0) then
                    if (allocated(self%plots(i)%linestyle)) then
                        if (allocated(self%plots(i)%marker)) then
                            call self%legend_data%add_entry(self%plots(i)%label, &
                                                           self%plots(i)%color, &
                                                           self%plots(i)%linestyle, &
                                                           self%plots(i)%marker)
                        else
                            call self%legend_data%add_entry(self%plots(i)%label, &
                                                           self%plots(i)%color, &
                                                           self%plots(i)%linestyle)
                        end if
                    else
                        call self%legend_data%add_entry(self%plots(i)%label, &
                                                       self%plots(i)%color)
                    end if
                end if
            end if
        end do
    end subroutine figure_legend

    subroutine clear_streamlines(self)
        !! Clear streamline data
        class(figure_t), intent(inout) :: self
        if (allocated(self%streamlines)) then
            deallocate(self%streamlines)
        end if
    end subroutine clear_streamlines

    subroutine destroy(self)
        !! Finalize and clean up figure
        type(figure_t), intent(inout) :: self
        
        if (allocated(self%backend)) then
            deallocate(self%backend)
        end if
        
        if (allocated(self%plots)) deallocate(self%plots)
        if (allocated(self%streamlines)) deallocate(self%streamlines)
        if (allocated(self%title)) deallocate(self%title)
        if (allocated(self%xlabel)) deallocate(self%xlabel)
        if (allocated(self%ylabel)) deallocate(self%ylabel)
    end subroutine destroy

    ! Private implementation procedures

    subroutine add_line_plot_data(self, x, y, label, linestyle, color, marker)
        !! Add line plot data to internal storage
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in) :: color(3)
        
        if (self%plot_count >= self%max_plots) then
            print *, "Warning: Maximum number of plots reached"
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        ! Store plot data
        self%plots(self%plot_count)%plot_type = PLOT_TYPE_LINE
        allocate(self%plots(self%plot_count)%x(size(x)))
        allocate(self%plots(self%plot_count)%y(size(y)))
        self%plots(self%plot_count)%x = x
        self%plots(self%plot_count)%y = y
        self%plots(self%plot_count)%color = color
        
        ! Process optional arguments
        if (present(label)) then
            self%plots(self%plot_count)%label = label
        end if
        
        if (present(linestyle)) then
            self%plots(self%plot_count)%linestyle = linestyle
        else
            self%plots(self%plot_count)%linestyle = '-'
        end if
        
        if (present(marker)) then
            if (len_trim(marker) > 0) then
                self%plots(self%plot_count)%marker = marker
            end if
        end if
        
        ! Update data ranges
        call self%update_data_ranges()
    end subroutine add_line_plot_data

    subroutine add_contour_plot_data(self, x_grid, y_grid, z_grid, levels, label)
        !! Add contour plot data to internal storage
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        if (self%plot_count >= self%max_plots) then
            print *, "Warning: Maximum number of plots reached"
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        ! Store plot data
        self%plots(self%plot_count)%plot_type = PLOT_TYPE_CONTOUR
        allocate(self%plots(self%plot_count)%x_grid(size(x_grid)))
        allocate(self%plots(self%plot_count)%y_grid(size(y_grid)))
        allocate(self%plots(self%plot_count)%z_grid(size(z_grid,1), size(z_grid,2)))
        self%plots(self%plot_count)%x_grid = x_grid
        self%plots(self%plot_count)%y_grid = y_grid
        self%plots(self%plot_count)%z_grid = z_grid
        
        if (present(levels)) then
            allocate(self%plots(self%plot_count)%contour_levels(size(levels)))
            self%plots(self%plot_count)%contour_levels = levels
        else
            call self%generate_default_contour_levels(self%plots(self%plot_count))
        end if
        
        if (present(label)) then
            self%plots(self%plot_count)%label = label
        end if
        
        ! Set default color
        self%plots(self%plot_count)%color = self%colors(:, mod(self%plot_count-1, 6) + 1)
        self%plots(self%plot_count)%use_color_levels = .false.
        
        ! Update data ranges
        call self%update_data_ranges()
    end subroutine add_contour_plot_data

    subroutine add_colored_contour_plot_data(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Add colored contour plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        if (self%plot_count >= self%max_plots) then
            print *, "Warning: Maximum number of plots reached"
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        ! Store plot data
        self%plots(self%plot_count)%plot_type = PLOT_TYPE_CONTOUR
        allocate(self%plots(self%plot_count)%x_grid(size(x_grid)))
        allocate(self%plots(self%plot_count)%y_grid(size(y_grid)))
        allocate(self%plots(self%plot_count)%z_grid(size(z_grid,1), size(z_grid,2)))
        self%plots(self%plot_count)%x_grid = x_grid
        self%plots(self%plot_count)%y_grid = y_grid
        self%plots(self%plot_count)%z_grid = z_grid
        
        if (present(levels)) then
            allocate(self%plots(self%plot_count)%contour_levels(size(levels)))
            self%plots(self%plot_count)%contour_levels = levels
        else
            call self%generate_default_contour_levels(self%plots(self%plot_count))
        end if
        
        if (present(colormap)) then
            self%plots(self%plot_count)%colormap = colormap
        else
            self%plots(self%plot_count)%colormap = 'crest'
        end if
        
        if (present(show_colorbar)) then
            self%plots(self%plot_count)%show_colorbar = show_colorbar
        else
            self%plots(self%plot_count)%show_colorbar = .true.
        end if
        
        if (present(label)) then
            self%plots(self%plot_count)%label = label
        end if
        
        self%plots(self%plot_count)%use_color_levels = .true.
        
        ! Update data ranges
        call self%update_data_ranges()
    end subroutine add_colored_contour_plot_data

    subroutine add_pcolormesh_plot_data(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        !! Add pcolormesh plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        
        if (self%plot_count >= self%max_plots) then
            print *, "Warning: Maximum number of plots reached"
            return
        end if
        
        self%plot_count = self%plot_count + 1
        
        ! Store plot data
        self%plots(self%plot_count)%plot_type = PLOT_TYPE_PCOLORMESH
        
        ! Initialize pcolormesh data using proper method
        call self%plots(self%plot_count)%pcolormesh_data%initialize_regular_grid(x, y, c, colormap)
        
        if (present(vmin)) then
            self%plots(self%plot_count)%pcolormesh_data%vmin = vmin
            self%plots(self%plot_count)%pcolormesh_data%vmin_set = .true.
        end if
        
        if (present(vmax)) then
            self%plots(self%plot_count)%pcolormesh_data%vmax = vmax
            self%plots(self%plot_count)%pcolormesh_data%vmax_set = .true.
        end if
        
        if (present(edgecolors)) then
            self%plots(self%plot_count)%pcolormesh_data%show_edges = .true.
            self%plots(self%plot_count)%pcolormesh_data%edge_color = edgecolors
        end if
        
        if (present(linewidths)) then
            self%plots(self%plot_count)%pcolormesh_data%edge_width = linewidths
        end if
        
        ! Update data ranges
        call self%update_data_ranges_pcolormesh()
    end subroutine add_pcolormesh_plot_data

    subroutine update_data_ranges_pcolormesh(self)
        !! Update data ranges after adding pcolormesh plot
        class(figure_t), intent(inout) :: self
        real(wp) :: x_min_new, x_max_new, y_min_new, y_max_new
        
        x_min_new = minval(self%plots(self%plot_count)%pcolormesh_data%x_vertices)
        x_max_new = maxval(self%plots(self%plot_count)%pcolormesh_data%x_vertices)
        y_min_new = minval(self%plots(self%plot_count)%pcolormesh_data%y_vertices)
        y_max_new = maxval(self%plots(self%plot_count)%pcolormesh_data%y_vertices)
        
        if (.not. self%xlim_set) then
            if (self%plot_count == 1) then
                self%x_min = x_min_new
                self%x_max = x_max_new
            else
                self%x_min = min(self%x_min, x_min_new)
                self%x_max = max(self%x_max, x_max_new)
            end if
        end if
        
        if (.not. self%ylim_set) then
            if (self%plot_count == 1) then
                self%y_min = y_min_new
                self%y_max = y_max_new
            else
                self%y_min = min(self%y_min, y_min_new)
                self%y_max = max(self%y_max, y_max_new)
            end if
        end if
    end subroutine update_data_ranges_pcolormesh

    subroutine update_data_ranges(self)
        !! Update data ranges based on current plot
        class(figure_t), intent(inout) :: self
        call self%calculate_figure_data_ranges()
    end subroutine update_data_ranges

    subroutine render_figure(self)
        !! Main rendering pipeline
        class(figure_t), intent(inout) :: self
        
        if (self%plot_count == 0) return
        
        ! Calculate final data ranges
        call self%calculate_figure_data_ranges()
        
        ! Setup coordinate system
        call self%setup_coordinate_system()
        
        ! Render background
        call self%render_figure_background()
        
        ! Render axes
        call self%render_figure_axes()
        
        ! Render all plots
        call self%render_all_plots()
        
        ! Render legend if requested
        if (self%show_legend .and. self%legend_data%num_entries > 0) then
            call self%legend_data%render(self%backend)
        end if
        
        self%rendered = .true.
    end subroutine render_figure

    subroutine generate_default_contour_levels(self, plot_data)
        !! Generate default contour levels
        class(figure_t), intent(inout) :: self
        type(plot_data_t), intent(inout) :: plot_data
        
        real(wp) :: z_min, z_max
        integer :: num_levels
        integer :: i
        
        z_min = minval(plot_data%z_grid)
        z_max = maxval(plot_data%z_grid)
        
        num_levels = 7
        allocate(plot_data%contour_levels(num_levels))
        
        do i = 1, num_levels
            plot_data%contour_levels(i) = z_min + (i-1) * (z_max - z_min) / (num_levels - 1)
        end do
    end subroutine generate_default_contour_levels

    subroutine calculate_figure_data_ranges(self)
        !! Calculate overall data ranges for the figure
        class(figure_t), intent(inout) :: self
        
        real(wp) :: x_min_data, x_max_data, y_min_data, y_max_data
        integer :: i
        logical :: first_plot
        
        if (self%xlim_set .and. self%ylim_set) then
            self%x_min_transformed = apply_scale_transform(self%x_min, self%xscale, self%symlog_threshold)
            self%x_max_transformed = apply_scale_transform(self%x_max, self%xscale, self%symlog_threshold)
            self%y_min_transformed = apply_scale_transform(self%y_min, self%yscale, self%symlog_threshold)
            self%y_max_transformed = apply_scale_transform(self%y_max, self%yscale, self%symlog_threshold)
            return
        end if
        
        first_plot = .true.
        
        do i = 1, self%plot_count
            select case (self%plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                if (allocated(self%plots(i)%x) .and. allocated(self%plots(i)%y)) then
                    if (first_plot) then
                        x_min_data = minval(self%plots(i)%x)
                        x_max_data = maxval(self%plots(i)%x)
                        y_min_data = minval(self%plots(i)%y)
                        y_max_data = maxval(self%plots(i)%y)
                        first_plot = .false.
                    else
                        x_min_data = min(x_min_data, minval(self%plots(i)%x))
                        x_max_data = max(x_max_data, maxval(self%plots(i)%x))
                        y_min_data = min(y_min_data, minval(self%plots(i)%y))
                        y_max_data = max(y_max_data, maxval(self%plots(i)%y))
                    end if
                end if
                
            case (PLOT_TYPE_CONTOUR)
                if (allocated(self%plots(i)%x_grid) .and. allocated(self%plots(i)%y_grid)) then
                    if (first_plot) then
                        x_min_data = minval(self%plots(i)%x_grid)
                        x_max_data = maxval(self%plots(i)%x_grid)
                        y_min_data = minval(self%plots(i)%y_grid)
                        y_max_data = maxval(self%plots(i)%y_grid)
                        first_plot = .false.
                    else
                        x_min_data = min(x_min_data, minval(self%plots(i)%x_grid))
                        x_max_data = max(x_max_data, maxval(self%plots(i)%x_grid))
                        y_min_data = min(y_min_data, minval(self%plots(i)%y_grid))
                        y_max_data = max(y_max_data, maxval(self%plots(i)%y_grid))
                    end if
                end if
                
            case (PLOT_TYPE_PCOLORMESH)
                if (allocated(self%plots(i)%pcolormesh_data%x_vertices) .and. &
                    allocated(self%plots(i)%pcolormesh_data%y_vertices)) then
                    if (first_plot) then
                        x_min_data = minval(self%plots(i)%pcolormesh_data%x_vertices)
                        x_max_data = maxval(self%plots(i)%pcolormesh_data%x_vertices)
                        y_min_data = minval(self%plots(i)%pcolormesh_data%y_vertices)
                        y_max_data = maxval(self%plots(i)%pcolormesh_data%y_vertices)
                        first_plot = .false.
                    else
                        x_min_data = min(x_min_data, minval(self%plots(i)%pcolormesh_data%x_vertices))
                        x_max_data = max(x_max_data, maxval(self%plots(i)%pcolormesh_data%x_vertices))
                        y_min_data = min(y_min_data, minval(self%plots(i)%pcolormesh_data%y_vertices))
                        y_max_data = max(y_max_data, maxval(self%plots(i)%pcolormesh_data%y_vertices))
                    end if
                end if
            end select
        end do
        
        ! Apply user-specified limits or use data ranges
        if (.not. self%xlim_set) then
            self%x_min = x_min_data
            self%x_max = x_max_data
        end if
        
        if (.not. self%ylim_set) then
            self%y_min = y_min_data
            self%y_max = y_max_data
        end if
        
        ! Apply scale transformations
        self%x_min_transformed = apply_scale_transform(self%x_min, self%xscale, self%symlog_threshold)
        self%x_max_transformed = apply_scale_transform(self%x_max, self%xscale, self%symlog_threshold)
        self%y_min_transformed = apply_scale_transform(self%y_min, self%yscale, self%symlog_threshold)
        self%y_max_transformed = apply_scale_transform(self%y_max, self%yscale, self%symlog_threshold)
    end subroutine calculate_figure_data_ranges

    subroutine setup_coordinate_system(self)
        !! Setup the coordinate system for rendering
        class(figure_t), intent(inout) :: self
        
        ! Set data ranges directly on backend
        self%backend%x_min = self%x_min_transformed
        self%backend%x_max = self%x_max_transformed
        self%backend%y_min = self%y_min_transformed
        self%backend%y_max = self%y_max_transformed
    end subroutine setup_coordinate_system

    subroutine render_figure_background(self)
        !! Render figure background
        class(figure_t), intent(inout) :: self
        ! Background clearing is handled by backend-specific rendering
    end subroutine render_figure_background

    subroutine render_figure_axes(self)
        !! Render figure axes and labels
        class(figure_t), intent(inout) :: self
        
        ! Draw axes using backend's polymorphic method
        call self%backend%draw_axes_and_labels_backend(self%xscale, self%yscale, &
                                                      self%symlog_threshold, &
                                                      self%x_min, self%x_max, &
                                                      self%y_min, self%y_max, &
                                                      self%title, self%xlabel, self%ylabel, &
                                                      z_min=0.0_wp, z_max=1.0_wp, &
                                                      has_3d_plots=.false.)
    end subroutine render_figure_axes

    subroutine render_all_plots(self)
        !! Render all plots in the figure
        class(figure_t), intent(inout) :: self
        integer :: i
        
        do i = 1, self%plot_count
            select case (self%plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                call render_line_plot(self%backend, self%plots(i), i, &
                                    self%x_min_transformed, self%x_max_transformed, &
                                    self%y_min_transformed, self%y_max_transformed, &
                                    self%xscale, self%yscale, self%symlog_threshold)
                
                if (allocated(self%plots(i)%marker)) then
                    call render_markers(self%backend, self%plots(i), &
                                      self%x_min_transformed, self%x_max_transformed, &
                                      self%y_min_transformed, self%y_max_transformed, &
                                      self%xscale, self%yscale, self%symlog_threshold)
                end if
                
            case (PLOT_TYPE_CONTOUR)
                call render_contour_plot(self%backend, self%plots(i), &
                                       self%x_min_transformed, self%x_max_transformed, &
                                       self%y_min_transformed, self%y_max_transformed, &
                                       self%xscale, self%yscale, self%symlog_threshold, &
                                       self%width, self%height, &
                                       self%margin_left, self%margin_right, &
                                       self%margin_bottom, self%margin_top)
                
            case (PLOT_TYPE_PCOLORMESH)
                call render_pcolormesh_plot(self%backend, self%plots(i), &
                                          self%x_min_transformed, self%x_max_transformed, &
                                          self%y_min_transformed, self%y_max_transformed, &
                                          self%xscale, self%yscale, self%symlog_threshold, &
                                          self%width, self%height, self%margin_right)
            end select
        end do
    end subroutine render_all_plots

    function get_file_extension(filename) result(ext)
        !! Extract file extension from filename
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: ext
        
        integer :: dot_pos
        
        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0 .and. dot_pos < len_trim(filename)) then
            ext = filename(dot_pos+1:)
        else
            ext = ''
        end if
    end function get_file_extension

end module fortplot_figure_core