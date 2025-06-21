module figure_module
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use plot_context_module
    use png_context_module
    use pdf_module
    implicit none
    
    private
    public :: figure_t
    
    type :: figure_t
        class(plot_context), allocatable :: backend
        integer :: plot_count = 0
        
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
            
        ! Data range tracking
        real(wp) :: xmin_global, xmax_global, ymin_global, ymax_global
        logical :: first_plot = .true.
        
        ! Labels
        character(len=:), allocatable :: xlabel, ylabel, title
        
    contains
        procedure :: initialize
        procedure :: add_plot
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
        
        ! Default backend
        backend_type = "png"
        if (present(backend)) backend_type = trim(backend)
        
        ! Initialize appropriate backend
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
        
        self%plot_count = 0
        self%first_plot = .true.
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        real(wp), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), dimension(3), intent(in), optional :: color
        
        real(wp) :: xmin, xmax, ymin, ymax
        integer :: i, color_idx
        real(wp), dimension(3) :: plot_color
        
        if (.not. allocated(self%backend)) then
            call self%initialize()
        end if
        
        ! Find data range
        xmin = minval(x)
        xmax = maxval(x)
        ymin = minval(y)
        ymax = maxval(y)
        
        ! Update global range or set it for first plot
        if (self%first_plot) then
            self%xmin_global = xmin
            self%xmax_global = xmax
            self%ymin_global = ymin
            self%ymax_global = ymax
            self%first_plot = .false.
        else
            self%xmin_global = min(self%xmin_global, xmin)
            self%xmax_global = max(self%xmax_global, xmax)
            self%ymin_global = min(self%ymin_global, ymin)
            self%ymax_global = max(self%ymax_global, ymax)
        end if
        
        ! Add some padding to the data range
        call expand_range(self%xmin_global, self%xmax_global)
        call expand_range(self%ymin_global, self%ymax_global)
        
        ! Calculate plot area in normalized coordinates for data plotting
        call setup_data_coordinate_system(self)
        
        ! Draw axes and labels only for the first plot
        if (self%plot_count == 0) then
            call draw_plot_background(self)
            call draw_plot_axes(self)
        end if
        
        ! Get plot color
        if (present(color)) then
            plot_color = color
        else
            self%plot_count = self%plot_count + 1
            color_idx = mod(self%plot_count-1, 6) + 1
            plot_color = self%colors(:, color_idx)
        end if
        
        ! Set color and draw the line
        call self%backend%color(plot_color(1), plot_color(2), plot_color(3))
        
        ! Draw line segments
        do i = 1, size(x) - 1
            call self%backend%line(real(x(i), wp), real(y(i), wp), real(x(i+1), wp), real(y(i+1), wp))
        end do
        
    end subroutine add_plot
    
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
        
        ! Calculate nice tick intervals
        call compute_ticks(self%xmin_global, self%xmax_global, nx, xstart, xend, xstep, xticks)
        call compute_ticks(self%ymin_global, self%ymax_global, ny, ystart, yend, ystep, yticks)
        
        ! Draw x-axis ticks and labels
        call self%backend%color(0.1_wp, 0.1_wp, 0.1_wp)
        do i = 1, nx
            tick_x = plot_x0 + (xticks(i) - self%xmin_global) / (self%xmax_global - self%xmin_global) * (plot_x1 - plot_x0)
            ! Draw tick mark
            call self%backend%line(real(tick_x, wp), real(plot_y0, wp), real(tick_x, wp), real(plot_y0 - 0.01_wp, wp))
            ! Draw label
            write(label_text, '(F0.2)') xticks(i)
            call self%backend%text(real(tick_x - 0.02_wp, wp), real(plot_y0 - 0.04_wp, wp), trim(adjustl(label_text)))
        end do
        
        ! Draw y-axis ticks and labels
        do i = 1, ny
            tick_y = plot_y0 + (yticks(i) - self%ymin_global) / (self%ymax_global - self%ymin_global) * (plot_y1 - plot_y0)
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
        
    end subroutine draw_plot_axes

    subroutine savefig(self, filename)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        
        if (.not. allocated(self%backend)) then
            print *, "Error: Figure not initialized"
            return
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
        print *, "Show functionality not implemented in this version"
    end subroutine show

    subroutine show_ascii(self, x, y, n, title)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        integer, intent(in) :: n
        character(len=*), intent(in), optional :: title
        
        ! Simple ASCII plot implementation
        call show_ascii_plot(x, y, n, title)
    end subroutine show_ascii

    subroutine destroy(self)
        type(figure_t), intent(inout) :: self
        if (allocated(self%backend)) deallocate(self%backend)
        if (allocated(self%xlabel)) deallocate(self%xlabel)
        if (allocated(self%ylabel)) deallocate(self%ylabel)
        if (allocated(self%title)) deallocate(self%title)
    end subroutine destroy

    ! Utility subroutines adapted from old fortplotlib
    subroutine setup_data_coordinate_system(self)
        class(figure_t), intent(inout) :: self
        real(wp) :: plot_x0, plot_y0, plot_x1, plot_y1
        real(wp) :: data_width, data_height
        
        ! Calculate plot area in normalized coordinates
        plot_x0 = self%margin_left
        plot_y0 = self%margin_bottom
        plot_x1 = 1.0_wp - self%margin_right
        plot_y1 = 1.0_wp - self%margin_top
        
        ! Set coordinate system to map data range to plot area
        ! We need to map data coordinates to the plot area (not full screen)
        data_width = self%xmax_global - self%xmin_global
        data_height = self%ymax_global - self%ymin_global
        
        ! Transform data coordinates to plot area coordinates
        self%backend%x_min = real(self%xmin_global - data_width * plot_x0 / (plot_x1 - plot_x0), wp)
        self%backend%x_max = real(self%xmax_global + data_width * (1.0_wp - plot_x1) / (plot_x1 - plot_x0), wp)
        self%backend%y_min = real(self%ymin_global - data_height * plot_y0 / (plot_y1 - plot_y0), wp)
        self%backend%y_max = real(self%ymax_global + data_height * (1.0_wp - plot_y1) / (plot_y1 - plot_y0), wp)
    end subroutine setup_data_coordinate_system

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

    subroutine show_ascii_plot(x, y, n, title)
        real(wp), intent(in) :: x(:), y(:)
        integer, intent(in) :: n
        character(len=*), intent(in), optional :: title
        
        integer, parameter :: plot_width = 60, plot_height = 20
        character(len=plot_width) :: lines(plot_height)
        real(wp) :: xmin, xmax, ymin, ymax, xrange, yrange
        integer :: i, xi, yi
        
        if (present(title)) then
            print '(A)', title
            print '(A)', repeat('=', len_trim(title))
        end if
        
        xmin = minval(x(1:n))
        xmax = maxval(x(1:n))
        ymin = minval(y(1:n))
        ymax = maxval(y(1:n))
        
        xrange = xmax - xmin
        yrange = ymax - ymin
        
        if (xrange == 0.0_wp) xrange = 1.0_wp
        if (yrange == 0.0_wp) yrange = 1.0_wp
        
        ! Initialize with spaces
        do i = 1, plot_height
            lines(i) = repeat(' ', plot_width)
        end do
        
        ! Plot points
        do i = 1, min(n, size(x), size(y))
            xi = int((x(i) - xmin) / xrange * (plot_width - 1)) + 1
            yi = plot_height - int((y(i) - ymin) / yrange * (plot_height - 1))
            
            if (xi >= 1 .and. xi <= plot_width .and. yi >= 1 .and. yi <= plot_height) then
                lines(yi)(xi:xi) = '*'
            end if
        end do
        
        ! Print the plot
        do i = 1, plot_height
            print '(A)', lines(i)
        end do
        
        print '(A,F8.3,A,F8.3)', 'X range: ', xmin, ' to ', xmax
        print '(A,F8.3,A,F8.3)', 'Y range: ', ymin, ' to ', ymax
    end subroutine show_ascii_plot

end module figure_module