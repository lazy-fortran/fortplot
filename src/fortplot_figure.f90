module fortplot_figure
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
        procedure :: show
        procedure :: show_ascii
        final :: destroy
    end type figure_t

contains

    subroutine initialize(self, width, height, backend)
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        integer :: w, h
        character(len=16) :: backend_type

        ! Default size
        w = 640
        h = 480
        if (present(width)) w = width
        if (present(height)) h = height

        ! Store dimensions for later use
        self%width = w
        self%height = h
        
        ! Only initialize backend if explicitly specified
        if (present(backend)) then
            call initialize_backend_old(self, trim(backend), w, h)
        end if
        ! If no backend specified, it will be auto-detected during savefig/show

        ! Set default margins
        self%margin_left = 0.15_wp
        self%margin_right = 0.05_wp  
        self%margin_bottom = 0.15_wp
        self%margin_top = 0.05_wp

        ! Initialize plot counter
        self%plot_count = 0
        self%rendered = .false.

        ! Initialize color palette
        self%colors = reshape([ &
            0.0_wp,   0.447_wp, 0.698_wp,  & ! #0072B2 (blue)
            0.0_wp,   0.619_wp, 0.451_wp,  & ! #009E73 (green)
            0.835_wp, 0.369_wp, 0.0_wp,    & ! #D55E00 (orange)
            0.8_wp,   0.475_wp, 0.655_wp,  & ! #CC79A7 (purple)
            0.941_wp, 0.894_wp, 0.259_wp,  & ! #F0E442 (yellow)
            0.337_wp, 0.702_wp, 0.914_wp], & ! #56B4E9 (cyan)
            [3,6])

        ! Allocate storage for plot data
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%max_plots))
        end if
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        real(wp), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), dimension(3), intent(in), optional :: color

        integer :: color_idx
        real(wp), dimension(3) :: plot_color

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

        ! Store the plot data for deferred rendering  
        if (allocated(self%plots(self%plot_count)%x)) deallocate(self%plots(self%plot_count)%x)
        if (allocated(self%plots(self%plot_count)%y)) deallocate(self%plots(self%plot_count)%y)
        allocate(self%plots(self%plot_count)%x(size(x)))
        allocate(self%plots(self%plot_count)%y(size(y)))
        self%plots(self%plot_count)%x = x
        self%plots(self%plot_count)%y = y

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

    end subroutine add_plot

    subroutine add_contour(self, x, y, z, levels, label, color)
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
            ! Generate default levels
            n_levels = 10
            allocate(self%plots(self%plot_count)%contour_levels(n_levels))
            allocate(default_levels(n_levels))
            do i = 1, n_levels
                default_levels(i) = minval(z) + real(i-1, wp) * (maxval(z) - minval(z)) / real(n_levels-1, wp)
            end do
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

    subroutine draw_plot_axes(self)
        class(figure_t), intent(inout) :: self
        ! This is kept for compatibility but not used in new deferred rendering approach
        print *, "Warning: draw_plot_axes called - this function is deprecated"
    end subroutine draw_plot_axes

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

        ! Calculate nice tick intervals - these should match what was used in coordinate setup
        call compute_ticks(xmin_axis, xmax_axis, nx, xstart, xend, xstep, xticks)
        call compute_ticks(ymin_axis, ymax_axis, ny, ystart, yend, ystep, yticks)

        ! Draw x-axis ticks and labels
        call self%backend%color(0.1_wp, 0.1_wp, 0.1_wp)
        do i = 1, nx
            tick_x = plot_x0 + (xticks(i) - xmin_axis) / (xmax_axis - xmin_axis) * (plot_x1 - plot_x0)
            ! Draw tick mark
            call self%backend%line(real(tick_x, wp), real(plot_y0, wp), real(tick_x, wp), real(plot_y0 - 0.01_wp, wp))
            ! Draw label
            write(label_text, '(F0.2)') xticks(i)
            call self%backend%text(real(tick_x - 0.02_wp, wp), real(plot_y0 - 0.04_wp, wp), trim(adjustl(label_text)))
        end do

        ! Draw y-axis ticks and labels
        do i = 1, ny
            tick_y = plot_y0 + (yticks(i) - ymin_axis) / (ymax_axis - ymin_axis) * (plot_y1 - plot_y0)
            ! Draw tick mark
            call self%backend%line(real(plot_x0, wp), real(tick_y, wp), real(plot_x0 - 0.01_wp, wp), real(tick_y, wp))
            ! Draw label
            write(label_text, '(F0.2)') yticks(i)
            call self%backend%text(real(plot_x0 - 0.08_wp, wp), real(tick_y - 0.01_wp, wp), trim(adjustl(label_text)))
        end do

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

    subroutine show_ascii(self, x, y, n, title)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        integer, intent(in) :: n
        character(len=*), intent(in), optional :: title

        ! DEPRECATED: Use show() instead for unified ASCII display
        print *, "Warning: show_ascii is deprecated. Use show() for ASCII terminal output."
        
        ! Set title if provided
        if (present(title)) call self%set_title(title)
        
        ! Use the new unified show method
        call self%show()
    end subroutine show_ascii

    subroutine render_all_plots(self)
        class(figure_t), intent(inout) :: self
        real(wp) :: xmin_global, xmax_global, ymin_global, ymax_global
        real(wp) :: xmin_axis, xmax_axis, ymin_axis, ymax_axis
        integer :: i, j

        if (self%plot_count == 0) return

        ! Calculate global data range from all plots
        call get_global_data_range(self, xmin_global, xmax_global, ymin_global, ymax_global)

        ! Set up coordinate system and get the actual axis ranges used
        call setup_nice_coordinate_system_with_range(self, xmin_global, xmax_global, ymin_global, ymax_global, &
                                                    xmin_axis, xmax_axis, ymin_axis, ymax_axis)

        ! Draw background and axes using the actual axis ranges
        call draw_plot_background(self)
        call draw_plot_axes_with_range(self, xmin_axis, xmax_axis, ymin_axis, ymax_axis)

        ! Render all plots - map data coordinates to plot area
        call map_to_plot_area(self, xmin_axis, xmax_axis, ymin_axis, ymax_axis)

        do i = 1, self%plot_count
            call self%backend%color(self%plots(i)%color(1), self%plots(i)%color(2), self%plots(i)%color(3))

            select case (self%plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                ! Draw line segments
                do j = 1, size(self%plots(i)%x) - 1
                    call self%backend%line(real(self%plots(i)%x(j), wp), real(self%plots(i)%y(j), wp), &
                                          real(self%plots(i)%x(j+1), wp), real(self%plots(i)%y(j+1), wp))
                end do
            case (PLOT_TYPE_CONTOUR)
                ! Render contour plot (placeholder for now)
                call render_contour_plot(self, i)
            end select
        end do
    end subroutine render_all_plots

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
                    backend_type = 'png'  ! Default fallback
                end if
            end select
        else
            if (trim(filename) == 'terminal') then
                backend_type = 'ascii'
            else
                backend_type = 'png'  ! Default fallback
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
        integer :: i
        logical :: first_plot = .true.

        ! Initialize with extreme values
        xmin_global = huge(1.0_wp)
        xmax_global = -huge(1.0_wp)
        ymin_global = huge(1.0_wp)
        ymax_global = -huge(1.0_wp)

        do i = 1, self%plot_count
            select case (self%plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                if (allocated(self%plots(i)%x) .and. allocated(self%plots(i)%y)) then
                    if (first_plot) then
                        xmin_global = minval(self%plots(i)%x)
                        xmax_global = maxval(self%plots(i)%x)
                        ymin_global = minval(self%plots(i)%y)
                        ymax_global = maxval(self%plots(i)%y)
                        first_plot = .false.
                    else
                        xmin_global = min(xmin_global, minval(self%plots(i)%x))
                        xmax_global = max(xmax_global, maxval(self%plots(i)%x))
                        ymin_global = min(ymin_global, minval(self%plots(i)%y))
                        ymax_global = max(ymax_global, maxval(self%plots(i)%y))
                    end if
                end if
            case (PLOT_TYPE_CONTOUR)
                if (allocated(self%plots(i)%x_grid) .and. allocated(self%plots(i)%y_grid)) then
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
        
        ! Placeholder for contour rendering implementation
        ! For now, just print a message indicating contour plot would be rendered
        print *, "INFO: Contour plot rendering not yet implemented for plot", plot_index
        print *, "      Grid size:", size(self%plots(plot_index)%x_grid), "x", size(self%plots(plot_index)%y_grid)
        print *, "      Z range:", minval(self%plots(plot_index)%z_grid), "to", maxval(self%plots(plot_index)%z_grid)
        print *, "      Contour levels:", size(self%plots(plot_index)%contour_levels)
    end subroutine render_contour_plot

end module fortplot_figure
