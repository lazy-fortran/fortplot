module fortplot_figure
    !! High-level plotting interface with unified multi-backend support
    !! 
    !! This module provides the main user interface for creating scientific plots
    !! with support for line plots, contour plots, and mixed plotting across
    !! PNG, PDF, and ASCII backends. Uses deferred rendering for efficiency.
    !!
    !! Author: fortplotlib contributors
    !! License: [Add appropriate license]
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_png
    use fortplot_pdf
    use fortplot_ascii
    implicit none

    private
    public :: figure_t

    integer, parameter :: PLOT_TYPE_LINE = 1
    integer, parameter :: PLOT_TYPE_CONTOUR = 2

    type :: plot_data_t
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

        ! Colors: seaborn colorblind palette
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
        procedure :: show
        final :: destroy
    end type figure_t

contains

    subroutine initialize(self, width, height, backend)
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        integer :: w, h
        character(len=16) :: backend_type

        w = 640
        h = 480
        if (present(width)) w = width
        if (present(height)) h = height

        self%width = w
        self%height = h
        
        if (present(backend)) then
            call initialize_backend_old(self, trim(backend), w, h)
        end if

        self%margin_left = 0.15_wp
        self%margin_right = 0.05_wp  
        self%margin_bottom = 0.15_wp
        self%margin_top = 0.05_wp

        self%plot_count = 0
        self%rendered = .false.

        self%colors = reshape([ &
            0.0_wp,   0.447_wp, 0.698_wp,  &
            0.0_wp,   0.619_wp, 0.451_wp,  &
            0.835_wp, 0.369_wp, 0.0_wp,    &
            0.8_wp,   0.475_wp, 0.655_wp,  &
            0.941_wp, 0.894_wp, 0.259_wp,  &
            0.337_wp, 0.702_wp, 0.914_wp], &
            [3,6])

        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%max_plots))
        end if
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        !! Add a line plot to the figure
        !!
        !! Arguments:
        !!   x, y: Data arrays for the line plot
        !!   label: Optional label for the plot legend
        !!   linestyle: Optional line style ('-', '--', '-.', ':', 'None')
        !!   color: Optional RGB color array [0,1] for the line
        class(figure_t), intent(inout) :: self
        real(wp), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), dimension(3), intent(in), optional :: color

        integer :: color_idx
        real(wp), dimension(3) :: plot_color

        if (self%plot_count >= self%max_plots) then
            print *, "Warning: Maximum number of plots exceeded"
            return
        end if

        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%max_plots))
        end if

        self%plot_count = self%plot_count + 1

        if (allocated(self%plots(self%plot_count)%x)) deallocate(self%plots(self%plot_count)%x)
        if (allocated(self%plots(self%plot_count)%y)) deallocate(self%plots(self%plot_count)%y)
        allocate(self%plots(self%plot_count)%x(size(x)))
        allocate(self%plots(self%plot_count)%y(size(y)))
        self%plots(self%plot_count)%x = x
        self%plots(self%plot_count)%y = y

        if (present(color)) then
            plot_color = color
        else
            color_idx = mod(self%plot_count-1, 6) + 1
            plot_color = self%colors(:, color_idx)
        end if
        self%plots(self%plot_count)%color = plot_color

        if (present(label)) then
            self%plots(self%plot_count)%label = label
        end if

        if (present(linestyle)) then
            self%plots(self%plot_count)%linestyle = linestyle
        else
            self%plots(self%plot_count)%linestyle = '-'  ! Default solid line
        end if

    end subroutine add_plot

    subroutine add_contour(self, x, y, z, levels, label, color)
        !! Add a contour plot to the figure using marching squares algorithm
        !!
        !! Arguments:
        !!   x, y: Grid coordinate arrays for the contour plot
        !!   z: 2D data array for contouring (z(i,j) corresponds to (x(i), y(j)))
        !!   levels: Optional array of contour levels (auto-generated if not provided)
        !!   label: Optional label for the plot legend
        !!   color: Optional RGB color array [0,1] for the contour lines
        class(figure_t), intent(inout) :: self
        real(wp), dimension(:), intent(in) :: x, y
        real(wp), dimension(:,:), intent(in) :: z
        real(wp), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label
        real(wp), dimension(3), intent(in), optional :: color

        integer :: color_idx
        real(wp), dimension(3) :: plot_color
        integer :: n_levels
        real(wp), allocatable :: default_levels(:)
        integer :: i

        ! Check if we have space for more plots
        if (self%plot_count >= self%max_plots) then
            print *, "Warning: Maximum number of plots exceeded"
            return
        end if

        ! Ensure plots storage is initialized
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%max_plots))
        end if

        ! Increment plot count
        self%plot_count = self%plot_count + 1

        ! Set plot type
        self%plots(self%plot_count)%plot_type = PLOT_TYPE_CONTOUR

        ! Store the contour data
        if (allocated(self%plots(self%plot_count)%x_grid)) deallocate(self%plots(self%plot_count)%x_grid)
        if (allocated(self%plots(self%plot_count)%y_grid)) deallocate(self%plots(self%plot_count)%y_grid)
        if (allocated(self%plots(self%plot_count)%z_grid)) deallocate(self%plots(self%plot_count)%z_grid)
        if (allocated(self%plots(self%plot_count)%contour_levels)) deallocate(self%plots(self%plot_count)%contour_levels)
        
        allocate(self%plots(self%plot_count)%x_grid(size(x)))
        allocate(self%plots(self%plot_count)%y_grid(size(y)))
        allocate(self%plots(self%plot_count)%z_grid(size(z,1), size(z,2)))
        
        self%plots(self%plot_count)%x_grid = x
        self%plots(self%plot_count)%y_grid = y
        self%plots(self%plot_count)%z_grid = z

        ! Set contour levels
        if (present(levels)) then
            allocate(self%plots(self%plot_count)%contour_levels(size(levels)))
            self%plots(self%plot_count)%contour_levels = levels
        else
            call generate_default_contour_levels(self, z, n_levels, default_levels)
            allocate(self%plots(self%plot_count)%contour_levels(n_levels))
            self%plots(self%plot_count)%contour_levels = default_levels
            deallocate(default_levels)
        end if

        ! Set plot color
        if (present(color)) then
            plot_color = color
        else
            color_idx = mod(self%plot_count-1, 6) + 1
            plot_color = self%colors(:, color_idx)
        end if
        self%plots(self%plot_count)%color = plot_color

        ! Store label if provided
        if (present(label)) then
            self%plots(self%plot_count)%label = label
        end if

    end subroutine add_contour

    subroutine draw_plot_background(self)
        class(figure_t), intent(inout) :: self
        integer :: i
        real(wp) :: plot_x0, plot_y0, plot_x1, plot_y1
        real(wp) :: grid_x, grid_y

        ! Calculate plot area in normalized coordinates
        plot_x0 = self%margin_left
        plot_y0 = self%margin_bottom
        plot_x1 = 1.0_wp - self%margin_right
        plot_y1 = 1.0_wp - self%margin_top

        ! Temporarily set coordinate system to screen coordinates
        self%backend%x_min = 0.0_wp
        self%backend%x_max = 1.0_wp
        self%backend%y_min = 0.0_wp
        self%backend%y_max = 1.0_wp

        ! Draw light gray grid
        call self%backend%color(0.9_wp, 0.9_wp, 0.9_wp)

        ! Draw vertical grid lines
        do i = 1, 9
            grid_x = plot_x0 + real(i-1, wp) / 8.0_wp * (plot_x1 - plot_x0)
            call self%backend%line(real(grid_x, wp), real(plot_y0, wp), real(grid_x, wp), real(plot_y1, wp))
        end do

        ! Draw horizontal grid lines
        do i = 1, 7
            grid_y = plot_y0 + real(i-1, wp) / 6.0_wp * (plot_y1 - plot_y0)
            call self%backend%line(real(plot_x0, wp), real(grid_y, wp), real(plot_x1, wp), real(grid_y, wp))
        end do

    end subroutine draw_plot_background


    subroutine draw_plot_axes_with_range(self, xmin_axis, xmax_axis, ymin_axis, ymax_axis)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: xmin_axis, xmax_axis, ymin_axis, ymax_axis
        real(wp) :: plot_x0, plot_y0, plot_x1, plot_y1
        integer :: nx, ny
        real(wp) :: xticks(20), yticks(20)
        real(wp) :: xstep, ystep, xstart, ystart, xend, yend
        integer :: i
        real(wp) :: tick_x, tick_y
        character(len=32) :: label_text

        ! Calculate plot area in normalized coordinates
        plot_x0 = self%margin_left
        plot_y0 = self%margin_bottom
        plot_x1 = 1.0_wp - self%margin_right
        plot_y1 = 1.0_wp - self%margin_top

        ! Use screen coordinates for drawing axes
        self%backend%x_min = 0.0_wp
        self%backend%x_max = 1.0_wp
        self%backend%y_min = 0.0_wp
        self%backend%y_max = 1.0_wp

        ! Draw border
        call self%backend%color(0.2_wp, 0.2_wp, 0.2_wp)
        call self%backend%line(real(plot_x0, wp), real(plot_y0, wp), real(plot_x1, wp), real(plot_y0, wp))  ! bottom
        call self%backend%line(real(plot_x1, wp), real(plot_y0, wp), real(plot_x1, wp), real(plot_y1, wp))  ! right
        call self%backend%line(real(plot_x1, wp), real(plot_y1, wp), real(plot_x0, wp), real(plot_y1, wp))  ! top
        call self%backend%line(real(plot_x0, wp), real(plot_y1, wp), real(plot_x0, wp), real(plot_y0, wp))  ! left

        ! Calculate scale-appropriate tick positions
        call compute_scale_ticks(self, xmin_axis, xmax_axis, self%xscale, self%symlog_threshold, nx, xticks)
        call compute_scale_ticks(self, ymin_axis, ymax_axis, self%yscale, self%symlog_threshold, ny, yticks)

        ! Draw x-axis ticks and labels
        call self%backend%color(0.1_wp, 0.1_wp, 0.1_wp)
        call draw_axis_ticks(self, plot_x0, plot_y0, plot_x1, plot_y1, &
                            xmin_axis, xmax_axis, xticks(1:nx), self%xscale, self%symlog_threshold, &
                            'x', .true.)

        ! Draw y-axis ticks and labels  
        call draw_axis_ticks(self, plot_x0, plot_y0, plot_x1, plot_y1, &
                            ymin_axis, ymax_axis, yticks(1:ny), self%yscale, self%symlog_threshold, &
                            'y', .true.)

        ! Draw axis labels
        if (allocated(self%xlabel)) then
            call self%backend%text(real((plot_x0 + plot_x1) / 2.0_wp - 0.05_wp, wp), real(plot_y0 - 0.08_wp, wp), self%xlabel)
        end if

        if (allocated(self%ylabel)) then
            call self%backend%text(real(plot_x0 - 0.12_wp, wp), real((plot_y0 + plot_y1) / 2.0_wp, wp), self%ylabel)
        end if

        if (allocated(self%title)) then
            call self%backend%text(real((plot_x0 + plot_x1) / 2.0_wp - 0.05_wp, wp), real(plot_y1 + 0.02_wp, wp), self%title)
        end if

    end subroutine draw_plot_axes_with_range

    subroutine savefig(self, filename)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        character(len=20) :: backend_type
        logical :: backend_changed

        ! Auto-detect backend from filename extension
        call get_backend_from_filename(filename, backend_type)
        
        ! Check if we need to switch backends
        backend_changed = .false.
        if (.not. allocated(self%backend)) then
            call initialize_backend(self, backend_type)
            backend_changed = .true.
        else
            ! Check if current backend matches required backend
            if (.not. backend_matches_type(self%backend, backend_type)) then
                deallocate(self%backend)
                call initialize_backend(self, backend_type)
                backend_changed = .true.
                self%rendered = .false.  ! Need to re-render with new backend
            end if
        end if

        ! Render all plots if not already done or backend changed
        if (.not. self%rendered .or. backend_changed) then
            call render_all_plots(self)
            self%rendered = .true.
        end if

        call self%backend%save(filename)
    end subroutine savefig

    subroutine set_xlabel(self, label)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        if (allocated(self%xlabel)) deallocate(self%xlabel)
        self%xlabel = label
    end subroutine set_xlabel

    subroutine set_ylabel(self, label)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        if (allocated(self%ylabel)) deallocate(self%ylabel)
        self%ylabel = label
    end subroutine set_ylabel

    subroutine set_title(self, title)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: title
        if (allocated(self%title)) deallocate(self%title)
        self%title = title
    end subroutine set_title

    subroutine set_xscale(self, scale, threshold)
        !! Set x-axis scale: 'linear', 'log', 'symlog'
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        self%xscale = trim(scale)
        if (present(threshold)) then
            self%symlog_threshold = threshold
        end if
    end subroutine set_xscale

    subroutine set_yscale(self, scale, threshold)
        !! Set y-axis scale: 'linear', 'log', 'symlog'
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        self%yscale = trim(scale)
        if (present(threshold)) then
            self%symlog_threshold = threshold
        end if
    end subroutine set_yscale

    subroutine show(self)
        class(figure_t), intent(inout) :: self
        
        ! Default to ASCII terminal output for show
        if (.not. allocated(self%backend)) then
            call initialize_backend(self, "ascii")
        else
            ! Switch to ASCII backend for show regardless of current backend
            if (.not. backend_matches_type(self%backend, "ascii")) then
                deallocate(self%backend)
                call initialize_backend(self, "ascii")
                self%rendered = .false.
            end if
        end if
        
        ! Minimize margins for ASCII display
        call set_ascii_margins(self)
        
        ! Render and display
        if (.not. self%rendered) then
            call render_all_plots(self)
            self%rendered = .true.
        end if
        
        call self%backend%save("terminal")
    end subroutine show


    subroutine render_all_plots(self)
        class(figure_t), intent(inout) :: self
        real(wp) :: xmin_global, xmax_global, ymin_global, ymax_global
        real(wp) :: xmin_axis, xmax_axis, ymin_axis, ymax_axis

        if (self%plot_count == 0) return

        call get_global_data_range(self, xmin_global, xmax_global, ymin_global, ymax_global)
        call setup_nice_coordinate_system_with_range(self, xmin_global, xmax_global, ymin_global, ymax_global, &
                                                    xmin_axis, xmax_axis, ymin_axis, ymax_axis)
        call draw_plot_background(self)
        call draw_plot_axes_with_range(self, xmin_axis, xmax_axis, ymin_axis, ymax_axis)
        call map_to_plot_area(self, xmin_axis, xmax_axis, ymin_axis, ymax_axis)
        call render_individual_plots(self)
    end subroutine render_all_plots

    subroutine render_individual_plots(self)
        class(figure_t), intent(inout) :: self
        integer :: i

        do i = 1, self%plot_count
            call self%backend%color(self%plots(i)%color(1), self%plots(i)%color(2), self%plots(i)%color(3))

            select case (self%plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                call render_line_plot(self, i)
            case (PLOT_TYPE_CONTOUR)
                call render_contour_plot(self, i)
            end select
        end do
    end subroutine render_individual_plots

    subroutine render_line_plot(self, plot_index)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        character(len=:), allocatable :: style

        if (allocated(self%plots(plot_index)%linestyle)) then
            style = self%plots(plot_index)%linestyle
        else
            style = '-'
        end if

        if (style == 'None' .or. style == '') then
            return  ! No line to draw
        end if

        if (style == '-') then
            ! Solid line - draw all segments normally
            call render_solid_line(self, plot_index)
        else
            ! Patterned line - render with continuous pattern
            call render_patterned_line(self, plot_index, style)
        end if
    end subroutine render_line_plot

    subroutine render_solid_line(self, plot_index)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        integer :: j
        real(wp) :: x1, y1, x2, y2, x1_trans, y1_trans, x2_trans, y2_trans

        do j = 1, size(self%plots(plot_index)%x) - 1
            x1 = real(self%plots(plot_index)%x(j), wp)
            y1 = real(self%plots(plot_index)%y(j), wp)
            x2 = real(self%plots(plot_index)%x(j+1), wp)
            y2 = real(self%plots(plot_index)%y(j+1), wp)
            
            ! Apply scale transformations
            call transform_data_coordinates(self, x1, y1, x1_trans, y1_trans)
            call transform_data_coordinates(self, x2, y2, x2_trans, y2_trans)
            
            call self%backend%line(x1_trans, y1_trans, x2_trans, y2_trans)
        end do
    end subroutine render_solid_line

    subroutine render_patterned_line(self, plot_index, linestyle)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        character(len=*), intent(in) :: linestyle
        
        real(wp) :: total_length, current_distance, segment_length
        real(wp) :: dash_len, dot_len, gap_len
        real(wp) :: pattern(20), pattern_length
        integer :: pattern_size, pattern_index
        logical :: drawing
        integer :: j, k, num_points
        real(wp) :: x1, y1, x2, y2, dx, dy, t, step_size
        real(wp) :: x1_trans, y1_trans, x2_trans, y2_trans
        
        ! Get transformed data range for scaling
        real(wp) :: x_range, y_range, plot_scale
        real(wp), allocatable :: x_trans(:), y_trans(:)
        integer :: i
        
        ! Transform all data points to get proper scaling
        allocate(x_trans(size(self%plots(plot_index)%x)))
        allocate(y_trans(size(self%plots(plot_index)%y)))
        
        do i = 1, size(self%plots(plot_index)%x)
            call transform_data_coordinates(self, self%plots(plot_index)%x(i), self%plots(plot_index)%y(i), &
                                          x_trans(i), y_trans(i))
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
            call render_solid_line(self, plot_index)
            return
        end select
        
        ! Calculate total pattern length
        pattern_length = sum(pattern(1:pattern_size))
        
        ! Render with continuous pattern
        current_distance = 0.0_wp
        pattern_index = 1
        drawing = .true.  ! Start drawing
        
        do j = 1, size(self%plots(plot_index)%x) - 1
            x1 = real(self%plots(plot_index)%x(j), wp)
            y1 = real(self%plots(plot_index)%y(j), wp)
            x2 = real(self%plots(plot_index)%x(j+1), wp)
            y2 = real(self%plots(plot_index)%y(j+1), wp)
            
            ! Apply scale transformations
            call transform_data_coordinates(self, x1, y1, x1_trans, y1_trans)
            call transform_data_coordinates(self, x2, y2, x2_trans, y2_trans)
            
            dx = x2_trans - x1_trans
            dy = y2_trans - y1_trans
            segment_length = sqrt(dx*dx + dy*dy)
            
            if (segment_length < 1e-10_wp) cycle
            
            call render_segment_with_pattern(self, x1_trans, y1_trans, x2_trans, y2_trans, segment_length, &
                                            pattern, pattern_size, pattern_length, &
                                            current_distance, pattern_index, drawing)
        end do
        
        ! Clean up
        deallocate(x_trans, y_trans)
    end subroutine render_patterned_line

    subroutine render_segment_with_pattern(self, x1, y1, x2, y2, segment_length, &
                                          pattern, pattern_size, pattern_length, &
                                          current_distance, pattern_index, drawing)
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

    subroutine render_line_segment_with_style(self, plot_index, i1, i2, linestyle)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index, i1, i2
        character(len=*), intent(in) :: linestyle
        
        real(wp) :: x1, y1, x2, y2, dx, dy, length
        real(wp) :: x_range, y_range, plot_scale
        real(wp) :: dash_len, dot_len, gap_len
        
        x1 = real(self%plots(plot_index)%x(i1), wp)
        y1 = real(self%plots(plot_index)%y(i1), wp)
        x2 = real(self%plots(plot_index)%x(i2), wp)
        y2 = real(self%plots(plot_index)%y(i2), wp)
        
        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)
        
        ! Calculate scale based on data range for visible patterns
        x_range = maxval(self%plots(plot_index)%x) - minval(self%plots(plot_index)%x)
        y_range = maxval(self%plots(plot_index)%y) - minval(self%plots(plot_index)%y)
        plot_scale = max(x_range, y_range)
        
        ! Scale pattern lengths to be visible and properly spaced
        dash_len = plot_scale * 0.05_wp   ! 5% of plot range - longer dashes
        dot_len = plot_scale * 0.005_wp   ! 0.5% of plot range - visible dots
        gap_len = plot_scale * 0.01_wp    ! 1% of plot range - tighter spacing
        
        select case (trim(linestyle))
        case ('-')
            ! Solid line
            call self%backend%line(x1, y1, x2, y2)
            
        case ('--')
            ! Dashed line
            call render_dashed_line(self, x1, y1, x2, y2, dash_len, gap_len)
            
        case ('-.')
            ! Dash-dot line  
            call render_dash_dot_line(self, x1, y1, x2, y2, dash_len, dot_len, gap_len)
            
        case (':')
            ! Dotted line
            call render_dotted_line(self, x1, y1, x2, y2, dot_len, gap_len)
            
        case default
            ! Unknown style, default to solid
            call self%backend%line(x1, y1, x2, y2)
        end select
    end subroutine render_line_segment_with_style

    subroutine map_to_plot_area(self, xmin_axis, xmax_axis, ymin_axis, ymax_axis)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: xmin_axis, xmax_axis, ymin_axis, ymax_axis
        real(wp) :: plot_x0, plot_y0, plot_x1, plot_y1
        real(wp) :: axis_width, axis_height

        ! Calculate plot area in normalized coordinates
        plot_x0 = self%margin_left
        plot_y0 = self%margin_bottom
        plot_x1 = 1.0_wp - self%margin_right
        plot_y1 = 1.0_wp - self%margin_top

        ! Set coordinate system to map axis range to plot area within normalized coordinates
        axis_width = xmax_axis - xmin_axis
        axis_height = ymax_axis - ymin_axis

        self%backend%x_min = real(xmin_axis - axis_width * plot_x0 / (plot_x1 - plot_x0), wp)
        self%backend%x_max = real(xmax_axis + axis_width * (1.0_wp - plot_x1) / (plot_x1 - plot_x0), wp)
        self%backend%y_min = real(ymin_axis - axis_height * plot_y0 / (plot_y1 - plot_y0), wp)
        self%backend%y_max = real(ymax_axis + axis_height * (1.0_wp - plot_y1) / (plot_y1 - plot_y0), wp)
    end subroutine map_to_plot_area

    subroutine destroy(self)
        type(figure_t), intent(inout) :: self
        integer :: i

        if (allocated(self%backend)) deallocate(self%backend)
        if (allocated(self%xlabel)) deallocate(self%xlabel)
        if (allocated(self%ylabel)) deallocate(self%ylabel)
        if (allocated(self%title)) deallocate(self%title)

        ! Clean up plot data
        if (allocated(self%plots)) then
            do i = 1, self%plot_count
                ! Clean up line plot data
                if (allocated(self%plots(i)%x)) deallocate(self%plots(i)%x)
                if (allocated(self%plots(i)%y)) deallocate(self%plots(i)%y)
                ! Clean up contour plot data
                if (allocated(self%plots(i)%x_grid)) deallocate(self%plots(i)%x_grid)
                if (allocated(self%plots(i)%y_grid)) deallocate(self%plots(i)%y_grid)
                if (allocated(self%plots(i)%z_grid)) deallocate(self%plots(i)%z_grid)
                if (allocated(self%plots(i)%contour_levels)) deallocate(self%plots(i)%contour_levels)
                ! Clean up common data
                if (allocated(self%plots(i)%label)) deallocate(self%plots(i)%label)
            end do
            deallocate(self%plots)
        end if
    end subroutine destroy

    ! Utility subroutines adapted from old fortplotlib
    subroutine setup_nice_coordinate_system_with_range(self, xmin_data, xmax_data, ymin_data, ymax_data, &
                                                      xmin_axis_out, xmax_axis_out, ymin_axis_out, ymax_axis_out)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: xmin_data, xmax_data, ymin_data, ymax_data
        real(wp), intent(out) :: xmin_axis_out, xmax_axis_out, ymin_axis_out, ymax_axis_out
        real(wp) :: plot_x0, plot_y0, plot_x1, plot_y1
        real(wp) :: axis_width, axis_height
        integer :: nx, ny
        real(wp) :: xticks(20), yticks(20)
        real(wp) :: xstep, ystep, xstart, ystart, xend, yend
        real(wp) :: xmin_axis, xmax_axis, ymin_axis, ymax_axis
        real(wp) :: xmin_expanded, xmax_expanded, ymin_expanded, ymax_expanded

        ! Add some padding to the data range first
        xmin_expanded = xmin_data
        xmax_expanded = xmax_data
        ymin_expanded = ymin_data
        ymax_expanded = ymax_data
        call expand_range(xmin_expanded, xmax_expanded)
        call expand_range(ymin_expanded, ymax_expanded)

        ! Calculate nice tick intervals to determine actual axis ranges
        call compute_ticks(xmin_expanded, xmax_expanded, nx, xstart, xend, xstep, xticks)
        call compute_ticks(ymin_expanded, ymax_expanded, ny, ystart, yend, ystep, yticks)

        ! Use the tick ranges as our axis ranges for better alignment
        if (nx > 0) then
            xmin_axis = xticks(1)
            xmax_axis = xticks(nx)
        else
            xmin_axis = xmin_expanded
            xmax_axis = xmax_expanded
        end if

        if (ny > 0) then
            ymin_axis = yticks(1)
            ymax_axis = yticks(ny)
        else
            ymin_axis = ymin_expanded
            ymax_axis = ymax_expanded
        end if

        ! Return the axis ranges
        xmin_axis_out = xmin_axis
        xmax_axis_out = xmax_axis
        ymin_axis_out = ymin_axis
        ymax_axis_out = ymax_axis

        ! Set coordinate system to directly map axis range to full canvas
        ! The drawing functions will handle the plot area mapping
        self%backend%x_min = real(xmin_axis, wp)
        self%backend%x_max = real(xmax_axis, wp)
        self%backend%y_min = real(ymin_axis, wp)
        self%backend%y_max = real(ymax_axis, wp)
    end subroutine setup_nice_coordinate_system_with_range

    subroutine expand_range(dmin, dmax)
        real(wp), intent(inout) :: dmin, dmax
        real(wp) :: range, center, margin

        range = dmax - dmin
        if (range == 0.0_wp) then
            range = 1.0_wp
            center = dmin
        else
            center = (dmin + dmax) / 2.0_wp
        end if

        margin = range * 0.05_wp  ! 5% margin
        dmin = center - range / 2.0_wp - margin
        dmax = center + range / 2.0_wp + margin
    end subroutine expand_range

    subroutine compute_ticks(dmin, dmax, n, tstart, tend, tstep, ticks)
        real(wp), intent(in) :: dmin, dmax
        integer, intent(out) :: n
        real(wp), intent(out) :: tstart, tend, tstep, ticks(:)
        real(wp) :: range, rawstep, mag, frac
        integer :: i

        range = dmax - dmin
        if (range == 0.0_wp) then
            tstep = 1.0_wp
            tstart = dmin
            tend = dmax
            n = 1
            ticks(1) = dmin
            return
        end if

        rawstep = range / 8.0_wp  ! Target ~8 ticks
        mag = 10.0_wp ** floor(log10(rawstep))
        frac = rawstep / mag

        if (frac < 1.5_wp) then
            tstep = 1.0_wp * mag
        else if (frac < 3.0_wp) then
            tstep = 2.0_wp * mag
        else if (frac < 7.0_wp) then
            tstep = 5.0_wp * mag
        else
            tstep = 10.0_wp * mag
        end if

        tstart = tstep * ceiling(dmin / tstep)
        tend = tstep * floor(dmax / tstep)
        n = int((tend - tstart) / tstep) + 1
        n = min(n, size(ticks))  ! Safety check

        do i = 1, n
            ticks(i) = tstart + (i-1)*tstep
        end do
    end subroutine compute_ticks

    ! Helper functions for unified backend management
    
    subroutine get_backend_from_filename(filename, backend_type)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: backend_type
        integer :: dot_pos
        character(len=10) :: extension
        
        ! Find the last dot in filename
        dot_pos = index(filename, '.', back=.true.)
        
        if (dot_pos > 0) then
            extension = filename(dot_pos+1:)
            call to_lowercase(extension)
            
            select case (trim(extension))
            case ('png')
                backend_type = 'png'
            case ('pdf')
                backend_type = 'pdf'
            case ('txt', 'ascii')
                backend_type = 'ascii'
            case default
                if (trim(filename) == 'terminal') then
                    backend_type = 'ascii'
                else
                    backend_type = 'png'
                end if
            end select
        else
            if (trim(filename) == 'terminal') then
                backend_type = 'ascii'
            else
                backend_type = 'png'
            end if
        end if
    end subroutine get_backend_from_filename
    
    logical function backend_matches_type(backend, type_name)
        class(plot_context), intent(in) :: backend
        character(len=*), intent(in) :: type_name
        
        select type (backend)
        type is (png_context)
            backend_matches_type = (trim(type_name) == 'png')
        type is (pdf_context)
            backend_matches_type = (trim(type_name) == 'pdf')
        type is (ascii_context)
            backend_matches_type = (trim(type_name) == 'ascii')
        class default
            backend_matches_type = .false.
        end select
    end function backend_matches_type
    
    subroutine initialize_backend(self, backend_type)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: backend_type
        
        select case (trim(backend_type))
        case ('png')
            allocate(png_context :: self%backend)
            self%backend = create_png_canvas(self%width, self%height)
        case ('pdf')
            allocate(pdf_context :: self%backend)
            self%backend = create_pdf_canvas(self%width, self%height)
        case ('ascii')
            allocate(ascii_context :: self%backend)
            self%backend = create_ascii_canvas(80, 40)
        case default
            print *, "Error: Unsupported backend type: ", trim(backend_type)
            ! Fallback to PNG
            allocate(png_context :: self%backend)
            self%backend = create_png_canvas(self%width, self%height)
        end select
    end subroutine initialize_backend
    
    subroutine set_ascii_margins(self)
        class(figure_t), intent(inout) :: self
        
        ! Set minimal margins for ASCII display
        self%margin_left = 0.02_wp
        self%margin_right = 0.02_wp  
        self%margin_bottom = 0.05_wp
        self%margin_top = 0.05_wp
    end subroutine set_ascii_margins
    
    subroutine to_lowercase(str)
        character(len=*), intent(inout) :: str
        integer :: i, ascii_val
        
        do i = 1, len_trim(str)
            ascii_val = iachar(str(i:i))
            if (ascii_val >= 65 .and. ascii_val <= 90) then  ! A-Z
                str(i:i) = achar(ascii_val + 32)  ! Convert to lowercase
            end if
        end do
    end subroutine to_lowercase
    
    subroutine initialize_backend_old(self, backend_type, w, h)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: backend_type
        integer, intent(in) :: w, h
        
        select case (trim(backend_type))
        case ("png")
            allocate(png_context :: self%backend)
            select type(ctx => self%backend)
            type is (png_context)
                ctx = create_png_canvas(w, h)
            end select
        case ("pdf")
            allocate(pdf_context :: self%backend)
            select type(ctx => self%backend)
            type is (pdf_context)
                ctx = create_pdf_canvas(w, h)
            end select
        case ("ascii", "unicode", "terminal")
            allocate(ascii_context :: self%backend)
            select type(ctx => self%backend)
            type is (ascii_context)
                ctx = create_ascii_canvas(w, h)
            end select
        case default
            print *, "Unknown backend: ", trim(backend_type), ", using PNG"
            allocate(png_context :: self%backend)
            select type(ctx => self%backend)
            type is (png_context)
                ctx = create_png_canvas(w, h)
            end select
        end select

        ! Set default coordinate system (will be updated with first plot)
        call setup_canvas(self%backend, w, h)
    end subroutine initialize_backend_old

    subroutine get_global_data_range(self, xmin_global, xmax_global, ymin_global, ymax_global)
        class(figure_t), intent(inout) :: self
        real(wp), intent(out) :: xmin_global, xmax_global, ymin_global, ymax_global
        integer :: i, j
        logical :: first_plot = .true.
        real(wp) :: x_trans, y_trans

        ! Initialize with extreme values
        xmin_global = huge(1.0_wp)
        xmax_global = -huge(1.0_wp)
        ymin_global = huge(1.0_wp)
        ymax_global = -huge(1.0_wp)

        do i = 1, self%plot_count
            select case (self%plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                if (allocated(self%plots(i)%x) .and. allocated(self%plots(i)%y)) then
                    ! Transform each point and find range in transformed space
                    do j = 1, size(self%plots(i)%x)
                        call transform_data_coordinates(self, self%plots(i)%x(j), self%plots(i)%y(j), x_trans, y_trans)
                        
                        if (first_plot) then
                            xmin_global = x_trans
                            xmax_global = x_trans
                            ymin_global = y_trans
                            ymax_global = y_trans
                            first_plot = .false.
                        else
                            xmin_global = min(xmin_global, x_trans)
                            xmax_global = max(xmax_global, x_trans)
                            ymin_global = min(ymin_global, y_trans)
                            ymax_global = max(ymax_global, y_trans)
                        end if
                    end do
                end if
            case (PLOT_TYPE_CONTOUR)
                if (allocated(self%plots(i)%x_grid) .and. allocated(self%plots(i)%y_grid)) then
                    ! For contour plots, use original coordinates (scale transformations for contours need different handling)
                    if (first_plot) then
                        xmin_global = minval(self%plots(i)%x_grid)
                        xmax_global = maxval(self%plots(i)%x_grid)
                        ymin_global = minval(self%plots(i)%y_grid)
                        ymax_global = maxval(self%plots(i)%y_grid)
                        first_plot = .false.
                    else
                        xmin_global = min(xmin_global, minval(self%plots(i)%x_grid))
                        xmax_global = max(xmax_global, maxval(self%plots(i)%x_grid))
                        ymin_global = min(ymin_global, minval(self%plots(i)%y_grid))
                        ymax_global = max(ymax_global, maxval(self%plots(i)%y_grid))
                    end if
                end if
            end select
        end do

        ! Fallback if no valid data found
        if (first_plot) then
            xmin_global = 0.0_wp
            xmax_global = 1.0_wp
            ymin_global = 0.0_wp
            ymax_global = 1.0_wp
        end if
    end subroutine get_global_data_range

    subroutine render_contour_plot(self, plot_index)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        integer :: level_idx
        real(wp) :: contour_level
        real(wp) :: z_min, z_max
        
        ! Get data range for filtering valid levels
        z_min = minval(self%plots(plot_index)%z_grid)
        z_max = maxval(self%plots(plot_index)%z_grid)
        
        ! Render each contour level that falls within data range
        do level_idx = 1, size(self%plots(plot_index)%contour_levels)
            contour_level = self%plots(plot_index)%contour_levels(level_idx)
            
            ! Only render levels within the data range
            if (contour_level > z_min .and. contour_level < z_max) then
                call trace_contour_level(self, plot_index, contour_level)
            end if
        end do
    end subroutine render_contour_plot

    subroutine trace_contour_level(self, plot_index, level)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        real(wp), intent(in) :: level
        integer :: nx, ny, i, j
        real(wp) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp) :: z1, z2, z3, z4
        integer :: config
        real(wp), dimension(8) :: line_points
        integer :: num_lines
        
        nx = size(self%plots(plot_index)%x_grid)
        ny = size(self%plots(plot_index)%y_grid)
        
        do i = 1, nx-1
            do j = 1, ny-1
                call process_contour_cell(self, plot_index, i, j, level)
            end do
        end do
    end subroutine trace_contour_level

    subroutine process_contour_cell(self, plot_index, i, j, level)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index, i, j
        real(wp), intent(in) :: level
        real(wp) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp) :: z1, z2, z3, z4
        integer :: config
        real(wp), dimension(8) :: line_points
        integer :: num_lines

        call get_cell_coordinates(self, plot_index, i, j, x1, y1, x2, y2, x3, y3, x4, y4)
        call get_cell_values(self, plot_index, i, j, z1, z2, z3, z4)
        call calculate_marching_squares_config(z1, z2, z3, z4, level, config)
        call get_contour_lines(config, x1, y1, x2, y2, x3, y3, x4, y4, &
                             z1, z2, z3, z4, level, line_points, num_lines)
        call draw_contour_lines(self, line_points, num_lines)
    end subroutine process_contour_cell

    subroutine get_cell_coordinates(self, plot_index, i, j, x1, y1, x2, y2, x3, y3, x4, y4)
        class(figure_t), intent(in) :: self
        integer, intent(in) :: plot_index, i, j
        real(wp), intent(out) :: x1, y1, x2, y2, x3, y3, x4, y4

        x1 = self%plots(plot_index)%x_grid(i)
        y1 = self%plots(plot_index)%y_grid(j)
        x2 = self%plots(plot_index)%x_grid(i+1)
        y2 = self%plots(plot_index)%y_grid(j)
        x3 = self%plots(plot_index)%x_grid(i+1)
        y3 = self%plots(plot_index)%y_grid(j+1)
        x4 = self%plots(plot_index)%x_grid(i)
        y4 = self%plots(plot_index)%y_grid(j+1)
    end subroutine get_cell_coordinates

    subroutine get_cell_values(self, plot_index, i, j, z1, z2, z3, z4)
        class(figure_t), intent(in) :: self
        integer, intent(in) :: plot_index, i, j
        real(wp), intent(out) :: z1, z2, z3, z4

        z1 = self%plots(plot_index)%z_grid(i, j)
        z2 = self%plots(plot_index)%z_grid(i+1, j)
        z3 = self%plots(plot_index)%z_grid(i+1, j+1)
        z4 = self%plots(plot_index)%z_grid(i, j+1)
    end subroutine get_cell_values

    subroutine calculate_marching_squares_config(z1, z2, z3, z4, level, config)
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

    subroutine draw_contour_lines(self, line_points, num_lines)
        class(figure_t), intent(inout) :: self
        real(wp), dimension(8), intent(in) :: line_points
        integer, intent(in) :: num_lines
        integer :: i
        
        do i = 1, num_lines
            call self%backend%line(real(line_points(4*i-3), wp), real(line_points(4*i-2), wp), &
                                  real(line_points(4*i-1), wp), real(line_points(4*i), wp))
        end do
    end subroutine draw_contour_lines

    subroutine generate_default_contour_levels(self, z_data, n_levels, levels)
        !! Generate default contour levels with boundary margins
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: z_data(:,:)
        integer, intent(out) :: n_levels
        real(wp), allocatable, intent(out) :: levels(:)
        
        real(wp), parameter :: BOUNDARY_MARGIN = 0.05_wp  ! 5% margin
        real(wp) :: z_min, z_max, z_range
        real(wp) :: level_min, level_max
        integer :: i
        
        n_levels = 10
        allocate(levels(n_levels))
        
        z_min = minval(z_data)
        z_max = maxval(z_data)
        z_range = z_max - z_min
        
        ! Apply boundary margins to avoid edge effects
        level_min = z_min + BOUNDARY_MARGIN * z_range
        level_max = z_max - BOUNDARY_MARGIN * z_range
        
        ! Generate evenly spaced levels
        do i = 1, n_levels
            levels(i) = level_min + real(i-1, wp) * (level_max - level_min) / real(n_levels-1, wp)
        end do
    end subroutine generate_default_contour_levels

    subroutine interpolate_edge_crossings(x1, y1, x2, y2, x3, y3, x4, y4, &
                                         z1, z2, z3, z4, level, xa, ya, xb, yb, xc, yc, xd, yd)
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp), intent(in) :: z1, z2, z3, z4, level
        real(wp), intent(out) :: xa, ya, xb, yb, xc, yc, xd, yd

        if (abs(z2 - z1) > 1e-10_wp) then
            xa = x1 + (level - z1) / (z2 - z1) * (x2 - x1)
            ya = y1 + (level - z1) / (z2 - z1) * (y2 - y1)
        else
            xa = (x1 + x2) * 0.5_wp
            ya = (y1 + y2) * 0.5_wp
        end if
        
        if (abs(z3 - z2) > 1e-10_wp) then
            xb = x2 + (level - z2) / (z3 - z2) * (x3 - x2)
            yb = y2 + (level - z2) / (z3 - z2) * (y3 - y2)
        else
            xb = (x2 + x3) * 0.5_wp
            yb = (y2 + y3) * 0.5_wp
        end if
        
        if (abs(z4 - z3) > 1e-10_wp) then
            xc = x3 + (level - z3) / (z4 - z3) * (x4 - x3)
            yc = y3 + (level - z3) / (z4 - z3) * (y4 - y3)
        else
            xc = (x3 + x4) * 0.5_wp
            yc = (y3 + y4) * 0.5_wp
        end if
        
        if (abs(z1 - z4) > 1e-10_wp) then
            xd = x4 + (level - z4) / (z1 - z4) * (x1 - x4)
            yd = y4 + (level - z4) / (z1 - z4) * (y1 - y4)
        else
            xd = (x4 + x1) * 0.5_wp
            yd = (y4 + y1) * 0.5_wp
        end if
    end subroutine interpolate_edge_crossings

    subroutine apply_marching_squares_lookup(config, xa, ya, xb, yb, xc, yc, xd, yd, line_points, num_lines)
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

    subroutine render_dashed_line(self, x1, y1, x2, y2, dash_len, gap_len)
        !! Render a dashed line between two points
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x1, y1, x2, y2, dash_len, gap_len
        
        real(wp) :: dx, dy, length, pattern_len, t, t_end
        real(wp) :: seg_x1, seg_y1, seg_x2, seg_y2
        
        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)
        pattern_len = dash_len + gap_len
        
        if (length < 1e-10_wp) return
        
        t = 0.0_wp
        do while (t < length)
            t_end = min(t + dash_len, length)
            
            seg_x1 = x1 + (t / length) * dx
            seg_y1 = y1 + (t / length) * dy
            seg_x2 = x1 + (t_end / length) * dx
            seg_y2 = y1 + (t_end / length) * dy
            
            call self%backend%line(seg_x1, seg_y1, seg_x2, seg_y2)
            
            t = t + pattern_len
        end do
    end subroutine render_dashed_line

    subroutine render_dotted_line(self, x1, y1, x2, y2, dot_len, gap_len)
        !! Render a dotted line between two points
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x1, y1, x2, y2, dot_len, gap_len
        
        real(wp) :: dx, dy, length, pattern_len, t, t_end
        real(wp) :: seg_x1, seg_y1, seg_x2, seg_y2
        
        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)
        pattern_len = dot_len + gap_len
        
        if (length < 1e-10_wp) return
        
        t = 0.0_wp
        do while (t < length)
            t_end = min(t + dot_len, length)
            
            seg_x1 = x1 + (t / length) * dx
            seg_y1 = y1 + (t / length) * dy
            seg_x2 = x1 + (t_end / length) * dx
            seg_y2 = y1 + (t_end / length) * dy
            
            call self%backend%line(seg_x1, seg_y1, seg_x2, seg_y2)
            
            t = t + pattern_len
        end do
    end subroutine render_dotted_line

    subroutine render_dash_dot_line(self, x1, y1, x2, y2, dash_len, dot_len, gap_len)
        !! Render a dash-dot line between two points
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x1, y1, x2, y2, dash_len, dot_len, gap_len
        
        real(wp) :: dx, dy, length, pattern_len, t, t_end
        real(wp) :: seg_x1, seg_y1, seg_x2, seg_y2
        logical :: is_dash
        
        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)
        pattern_len = dash_len + gap_len + dot_len + gap_len
        
        if (length < 1e-10_wp) return
        
        t = 0.0_wp
        is_dash = .true.
        
        do while (t < length)
            if (is_dash) then
                ! Draw dash
                t_end = min(t + dash_len, length)
            else
                ! Draw dot
                t_end = min(t + dot_len, length)
            end if
            
            seg_x1 = x1 + (t / length) * dx
            seg_y1 = y1 + (t / length) * dy
            seg_x2 = x1 + (t_end / length) * dx
            seg_y2 = y1 + (t_end / length) * dy
            
            call self%backend%line(seg_x1, seg_y1, seg_x2, seg_y2)
            
            if (is_dash) then
                t = t + dash_len + gap_len
                is_dash = .false.
            else
                t = t + dot_len + gap_len
                is_dash = .true.
            end if
        end do
    end subroutine render_dash_dot_line

    function apply_scale_transform(value, scale_type, threshold) result(transformed)
        !! Apply scale transformation to a value
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: threshold
        real(wp) :: transformed
        
        select case (trim(scale_type))
        case ('log')
            if (value > 0.0_wp) then
                transformed = log10(value)
            else
                transformed = -huge(1.0_wp)  ! Invalid for log scale
            end if
        case ('symlog')
            transformed = apply_symlog_transform(value, threshold)
        case default  ! 'linear'
            transformed = value
        end select
    end function apply_scale_transform

    function apply_symlog_transform(value, threshold) result(transformed)
        !! Apply symmetric log transformation
        real(wp), intent(in) :: value, threshold
        real(wp) :: transformed
        
        if (abs(value) <= threshold) then
            ! Linear region around zero
            transformed = value / threshold
        else if (value > threshold) then
            ! Positive logarithmic region
            transformed = 1.0_wp + log10(value / threshold)
        else
            ! Negative logarithmic region
            transformed = -1.0_wp - log10(-value / threshold)
        end if
    end function apply_symlog_transform

    subroutine transform_data_coordinates(self, x_in, y_in, x_out, y_out)
        !! Transform data coordinates according to axis scales
        class(figure_t), intent(in) :: self
        real(wp), intent(in) :: x_in, y_in
        real(wp), intent(out) :: x_out, y_out
        
        x_out = apply_scale_transform(x_in, self%xscale, self%symlog_threshold)
        y_out = apply_scale_transform(y_in, self%yscale, self%symlog_threshold)
    end subroutine transform_data_coordinates

    function apply_inverse_scale_transform(transformed_value, scale_type, threshold) result(original)
        !! Convert transformed coordinate back to original value for tick labeling
        real(wp), intent(in) :: transformed_value
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: threshold
        real(wp) :: original
        
        select case (trim(scale_type))
        case ('log')
            original = 10.0_wp**transformed_value
        case ('symlog')
            original = apply_inverse_symlog_transform(transformed_value, threshold)
        case default  ! 'linear'
            original = transformed_value
        end select
    end function apply_inverse_scale_transform

    function apply_inverse_symlog_transform(transformed, threshold) result(original)
        !! Convert symlog transformed value back to original
        real(wp), intent(in) :: transformed, threshold
        real(wp) :: original
        
        if (abs(transformed) <= 1.0_wp) then
            ! Linear region
            original = transformed * threshold
        else if (transformed > 1.0_wp) then
            ! Positive logarithmic region
            original = threshold * 10.0_wp**(transformed - 1.0_wp)
        else
            ! Negative logarithmic region
            original = -threshold * 10.0_wp**(-transformed - 1.0_wp)
        end if
    end function apply_inverse_symlog_transform

    subroutine format_tick_label(tick_value, scale_type, threshold, label_text)
        !! Format tick label according to scale type
        real(wp), intent(in) :: tick_value
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: threshold
        character(len=*), intent(out) :: label_text
        
        real(wp) :: original_value
        
        original_value = apply_inverse_scale_transform(tick_value, scale_type, threshold)
        
        select case (trim(scale_type))
        case ('log')
            ! For log scale, use scientific notation or clean powers of 10
            if (abs(original_value) >= 1000.0_wp .or. abs(original_value) <= 0.01_wp) then
                write(label_text, '(ES8.1)') original_value
            else
                write(label_text, '(F0.1)') original_value
            end if
        case ('symlog')
            ! For symlog, format based on the region
            if (abs(original_value) <= threshold * 10.0_wp) then
                write(label_text, '(F0.1)') original_value
            else
                write(label_text, '(ES8.1)') original_value
            end if
        case default  ! 'linear'
            write(label_text, '(F0.2)') original_value
        end select
    end subroutine format_tick_label

    subroutine compute_scale_ticks(self, axis_min, axis_max, scale_type, threshold, n_ticks, tick_positions)
        !! Compute tick positions appropriate for the given scale type
        class(figure_t), intent(in) :: self
        real(wp), intent(in) :: axis_min, axis_max
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: threshold
        integer, intent(out) :: n_ticks
        real(wp), intent(out) :: tick_positions(20)
        
        real(wp) :: tstart, tend, tstep
        
        select case (trim(scale_type))
        case ('log')
            call compute_log_ticks(axis_min, axis_max, n_ticks, tick_positions)
        case ('symlog')
            call compute_symlog_ticks(axis_min, axis_max, threshold, n_ticks, tick_positions)
        case default  ! 'linear'
            ! For linear scale, use the original compute_ticks function
            call compute_ticks(axis_min, axis_max, n_ticks, tstart, tend, tstep, tick_positions)
        end select
    end subroutine compute_scale_ticks

    subroutine compute_log_ticks(axis_min, axis_max, n_ticks, tick_positions)
        !! Compute logarithmically spaced major ticks
        real(wp), intent(in) :: axis_min, axis_max
        integer, intent(out) :: n_ticks
        real(wp), intent(out) :: tick_positions(20)
        
        integer :: decade_min, decade_max, i
        real(wp) :: log_min, log_max
        
        ! Find the range in log space
        log_min = axis_min
        log_max = axis_max
        
        ! Find decades (integer powers of 10)
        decade_min = floor(log_min)
        decade_max = ceiling(log_max)
        
        n_ticks = 0
        do i = decade_min, decade_max
            if (real(i, wp) >= log_min .and. real(i, wp) <= log_max .and. n_ticks < 20) then
                n_ticks = n_ticks + 1
                tick_positions(n_ticks) = real(i, wp)
            end if
        end do
        
        ! Ensure we have at least 2 ticks
        if (n_ticks < 2) then
            n_ticks = 2
            tick_positions(1) = log_min
            tick_positions(2) = log_max
        end if
    end subroutine compute_log_ticks

    subroutine compute_symlog_ticks(axis_min, axis_max, threshold, n_ticks, tick_positions)
        !! Compute ticks for symmetric log scale
        real(wp), intent(in) :: axis_min, axis_max, threshold
        integer, intent(out) :: n_ticks
        real(wp), intent(out) :: tick_positions(20)
        
        ! For symlog, use a combination approach
        ! This is simplified - could be more sophisticated
        integer :: i
        real(wp) :: range, step
        
        range = axis_max - axis_min
        step = range / 6.0_wp  ! Approximately 6 ticks
        
        n_ticks = 0
        do i = 0, 7
            if (n_ticks >= 20) exit
            tick_positions(i+1) = axis_min + i * step
            if (tick_positions(i+1) <= axis_max) then
                n_ticks = n_ticks + 1
            end if
        end do
        
        if (n_ticks == 0) then
            n_ticks = 2
            tick_positions(1) = axis_min
            tick_positions(2) = axis_max
        end if
    end subroutine compute_symlog_ticks

    subroutine draw_axis_ticks(self, plot_x0, plot_y0, plot_x1, plot_y1, &
                              axis_min, axis_max, major_ticks, scale_type, threshold, &
                              axis_direction, draw_labels)
        !! Draw major and minor ticks for an axis
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: plot_x0, plot_y0, plot_x1, plot_y1
        real(wp), intent(in) :: axis_min, axis_max
        real(wp), intent(in) :: major_ticks(:)
        character(len=*), intent(in) :: scale_type, axis_direction
        real(wp), intent(in) :: threshold
        logical, intent(in) :: draw_labels
        
        integer :: i, j
        real(wp) :: tick_pos, minor_tick_pos
        real(wp) :: tick_x, tick_y
        character(len=32) :: label_text
        real(wp) :: minor_ticks(9)
        logical :: is_x_axis
        
        is_x_axis = (trim(axis_direction) == 'x')
        
        ! Draw major ticks
        do i = 1, size(major_ticks)
            if (is_x_axis) then
                tick_x = plot_x0 + (major_ticks(i) - axis_min) / (axis_max - axis_min) * &
                         (plot_x1 - plot_x0)
                ! Major tick mark
                call self%backend%line(real(tick_x, wp), real(plot_y0, wp), &
                                      real(tick_x, wp), real(plot_y0 - 0.02_wp, wp))
                
                if (draw_labels) then
                    call format_tick_label(major_ticks(i), scale_type, threshold, label_text)
                    call self%backend%text(real(tick_x - 0.02_wp, wp), real(plot_y0 - 0.04_wp, wp), &
                                          trim(adjustl(label_text)))
                end if
            else
                tick_y = plot_y0 + (major_ticks(i) - axis_min) / (axis_max - axis_min) * &
                         (plot_y1 - plot_y0)
                ! Major tick mark
                call self%backend%line(real(plot_x0, wp), real(tick_y, wp), &
                                      real(plot_x0 - 0.02_wp, wp), real(tick_y, wp))
                
                if (draw_labels) then
                    call format_tick_label(major_ticks(i), scale_type, threshold, label_text)
                    call self%backend%text(real(plot_x0 - 0.08_wp, wp), real(tick_y - 0.01_wp, wp), &
                                          trim(adjustl(label_text)))
                end if
            end if
        end do
        
        ! Draw minor ticks for log scale
        if (trim(scale_type) == 'log') then
            call self%backend%color(0.5_wp, 0.5_wp, 0.5_wp)  ! Lighter color for minor ticks
            
            do i = 1, size(major_ticks) - 1
                ! Generate minor ticks between major ticks
                call generate_log_minor_ticks(major_ticks(i), major_ticks(i+1), minor_ticks)
                
                do j = 1, 9
                    if (minor_ticks(j) > major_ticks(i) .and. minor_ticks(j) < major_ticks(i+1)) then
                        if (is_x_axis) then
                            tick_x = plot_x0 + (minor_ticks(j) - axis_min) / (axis_max - axis_min) * &
                                     (plot_x1 - plot_x0)
                            call self%backend%line(real(tick_x, wp), real(plot_y0, wp), &
                                                  real(tick_x, wp), real(plot_y0 - 0.01_wp, wp))
                        else
                            tick_y = plot_y0 + (minor_ticks(j) - axis_min) / (axis_max - axis_min) * &
                                     (plot_y1 - plot_y0)
                            call self%backend%line(real(plot_x0, wp), real(tick_y, wp), &
                                                  real(plot_x0 - 0.01_wp, wp), real(tick_y, wp))
                        end if
                    end if
                end do
            end do
        end if
    end subroutine draw_axis_ticks

    subroutine generate_log_minor_ticks(major_low, major_high, minor_ticks)
        !! Generate minor tick positions between two major log ticks
        real(wp), intent(in) :: major_low, major_high
        real(wp), intent(out) :: minor_ticks(9)
        
        integer :: i
        real(wp) :: decade_start
        
        ! For log scale, minor ticks are at log10(2), log10(3), ..., log10(9) within each decade
        decade_start = major_low
        
        do i = 1, 9
            minor_ticks(i) = decade_start + log10(real(i+1, wp))
        end do
    end subroutine generate_log_minor_ticks

end module fortplot_figure
