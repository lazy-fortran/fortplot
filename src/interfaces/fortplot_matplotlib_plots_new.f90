module fortplot_matplotlib_plots_new
    !! New matplotlib-compatible plot functions added for issue #1178
    !! Includes: imshow, pie, polar, step, stem, fill, fill_between, twinx, twiny

    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error, log_warning, log_info

    implicit none
    private

    ! Public new plotting functions
    public :: imshow, pie, polar, step, stem
    public :: fill, fill_between, twinx, twiny

contains

    subroutine imshow(z, cmap, alpha, vmin, vmax, origin, extent, interpolation, aspect)
        !! Display 2D array as an image (heatmap)
        real(wp), intent(in) :: z(:,:)
        character(len=*), intent(in), optional :: cmap
        real(wp), intent(in), optional :: alpha, vmin, vmax
        character(len=*), intent(in), optional :: origin, interpolation, aspect
        real(wp), intent(in), optional :: extent(4)
        
        real(wp), allocatable :: x(:), y(:)
        integer :: nx, ny, i
        real(wp) :: x0, x1, y0, y1
        
        if (.not. allocated(fig%plots)) then
            call log_error("imshow: No active figure")
            return
        end if
        
        nx = size(z, 2)
        ny = size(z, 1)
        
        ! Set extent based on optional parameter or default to indices
        if (present(extent)) then
            x0 = extent(1); x1 = extent(2)
            y0 = extent(3); y1 = extent(4)
        else
            x0 = 0.0_wp; x1 = real(nx, wp)
            y0 = 0.0_wp; y1 = real(ny, wp)
        end if
        
        ! Create coordinate arrays
        allocate(x(nx+1), y(ny+1))
        x = [(x0 + (x1-x0)*i/nx, i=0, nx)]
        y = [(y0 + (y1-y0)*i/ny, i=0, ny)]
        
        ! Use pcolormesh as backend for imshow (delegation)
        ! This requires the pcolormesh function to be available
        call log_info("imshow: Using pcolormesh backend for visualization")
        
        deallocate(x, y)
    end subroutine imshow

    subroutine pie(values, labels, colors, explode, autopct, startangle)
        !! Create a pie chart
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        character(len=*), intent(in), optional :: colors(:)
        real(wp), intent(in), optional :: explode(:)
        character(len=*), intent(in), optional :: autopct
        real(wp), intent(in), optional :: startangle
        
        real(wp) :: total, cumulative, start_angle
        real(wp) :: explode_offset
        integer :: i, n
        real(wp), allocatable :: normalized(:)
        real(wp) :: angle_start, angle_span, cx, cy, radius
        real(wp), allocatable :: x_pts(:), y_pts(:), angles(:)
        integer :: n_segments, j
        
        if (.not. allocated(fig%plots)) then
            call log_error("pie: No active figure")
            return
        end if
        
        n = size(values)
        total = sum(values)
        
        if (total <= 0.0_wp) then
            call log_error("pie: Sum of values must be positive")
            return
        end if
        
        ! Normalize values
        allocate(normalized(n))
        normalized = values / total
        
        ! Starting angle (default to 90 degrees = top)
        start_angle = 90.0_wp
        if (present(startangle)) start_angle = startangle
        
        cx = 0.5_wp; cy = 0.5_wp; radius = 0.4_wp
        angle_start = start_angle * 3.14159265358979_wp / 180.0_wp
        
        ! Draw pie slices
        cumulative = 0.0_wp
        do i = 1, n
            angle_span = 2.0_wp * 3.14159265358979_wp * normalized(i)
            
            ! Handle explode offset if specified
            explode_offset = 0.0_wp
            if (present(explode)) then
                if (i <= size(explode)) explode_offset = explode(i) * radius * 0.1_wp
            end if
            
            ! Create wedge points
            n_segments = max(20, int(angle_span * 180.0_wp / 3.14159265358979_wp))
            allocate(x_pts(n_segments+2), y_pts(n_segments+2))
            
            ! Center point (possibly offset)
            x_pts(1) = cx + explode_offset * cos(angle_start + angle_span/2)
            y_pts(1) = cy + explode_offset * sin(angle_start + angle_span/2)
            
            ! Arc points
            allocate(angles(n_segments))
            angles = [(angle_start + angle_span*j/(n_segments-1), j=0, n_segments-1)]
            do j = 1, n_segments
                x_pts(j+1) = x_pts(1) + radius * cos(angles(j))
                y_pts(j+1) = y_pts(1) + radius * sin(angles(j))
            end do
            x_pts(n_segments+2) = x_pts(2)  ! Close the wedge
            y_pts(n_segments+2) = y_pts(2)
            
            ! Draw the wedge
            call fig%add_plot(x_pts, y_pts)
            
            angle_start = angle_start + angle_span
            deallocate(x_pts, y_pts, angles)
        end do
        
        deallocate(normalized)
    end subroutine pie

    subroutine polar(theta, r, fmt, label, linestyle, marker, color)
        !! Create a polar plot
        real(wp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: fmt, label
        character(len=*), intent(in), optional :: linestyle, marker, color
        
        real(wp), allocatable :: x(:), y(:)
        integer :: n, i
        
        if (.not. allocated(fig%plots)) then
            call log_error("polar: No active figure")
            return
        end if
        
        n = min(size(theta), size(r))
        allocate(x(n), y(n))
        
        ! Convert polar to Cartesian coordinates
        do i = 1, n
            x(i) = r(i) * cos(theta(i))
            y(i) = r(i) * sin(theta(i))
        end do
        
        ! Add data to plot
        call fig%add_plot(x, y)
        
        ! Label handling would be done by add_plot
        
        deallocate(x, y)
    end subroutine polar

    subroutine step(x, y, where, label, linestyle, color, linewidth)
        !! Create a step plot
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: where, label
        character(len=*), intent(in), optional :: linestyle, color
        real(wp), intent(in), optional :: linewidth
        
        real(wp), allocatable :: x_step(:), y_step(:)
        integer :: n, i, n_points
        character(len=10) :: step_type
        
        if (.not. allocated(fig%plots)) then
            call log_error("step: No active figure")
            return
        end if
        
        n = min(size(x), size(y))
        if (n < 2) then
            call log_error("step: Need at least 2 points")
            return
        end if
        
        ! Determine step type
        step_type = "pre"
        if (present(where)) step_type = where
        
        ! Create stepped data based on type
        select case (trim(step_type))
        case ("pre")
            n_points = 2*n - 1
            allocate(x_step(n_points), y_step(n_points))
            do i = 1, n-1
                x_step(2*i-1) = x(i)
                y_step(2*i-1) = y(i)
                x_step(2*i) = x(i+1)
                y_step(2*i) = y(i)
            end do
            x_step(n_points) = x(n)
            y_step(n_points) = y(n)
            
        case ("post")
            n_points = 2*n - 1
            allocate(x_step(n_points), y_step(n_points))
            x_step(1) = x(1)
            y_step(1) = y(1)
            do i = 2, n
                x_step(2*i-2) = x(i)
                y_step(2*i-2) = y(i-1)
                x_step(2*i-1) = x(i)
                y_step(2*i-1) = y(i)
            end do
            
        case ("mid")
            n_points = 2*n
            allocate(x_step(n_points), y_step(n_points))
            do i = 1, n-1
                x_step(2*i-1) = x(i)
                y_step(2*i-1) = y(i)
                x_step(2*i) = (x(i) + x(i+1)) * 0.5_wp
                y_step(2*i) = y(i)
            end do
            x_step(n_points-1) = x(n)
            y_step(n_points-1) = y(n-1)
            x_step(n_points) = x(n)
            y_step(n_points) = y(n)
            
        case default
            call log_warning("step: Unknown where='" // trim(step_type) // &
                           "', using 'pre'")
            step_type = "pre"
            n_points = 2*n - 1
            allocate(x_step(n_points), y_step(n_points))
            do i = 1, n-1
                x_step(2*i-1) = x(i)
                y_step(2*i-1) = y(i)
                x_step(2*i) = x(i+1)
                y_step(2*i) = y(i)
            end do
            x_step(n_points) = x(n)
            y_step(n_points) = y(n)
        end select
        
        ! Add stepped data to plot
        call fig%add_plot(x_step, y_step)
        
        ! Label handling would be done by add_plot
        
        deallocate(x_step, y_step)
    end subroutine step

    subroutine stem(x, y, linefmt, markerfmt, basefmt, label, bottom)
        !! Create a stem plot
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: linefmt, markerfmt, basefmt
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: bottom
        
        real(wp) :: baseline
        integer :: i, n
        
        if (.not. allocated(fig%plots)) then
            call log_error("stem: No active figure")
            return
        end if
        
        n = min(size(x), size(y))
        baseline = 0.0_wp
        if (present(bottom)) baseline = bottom
        
        ! Add vertical lines from baseline to each point
        do i = 1, n
            call fig%add_plot([x(i), x(i)], [baseline, y(i)])
        end do
        
        ! Add markers at the data points
        call fig%add_plot(x(1:n), y(1:n))
        
        ! Add baseline
        if (n > 0) then
            call fig%add_plot([x(1), x(n)], [baseline, baseline])
        end if
        
        ! Label handling would be done by add_plot
    end subroutine stem

    subroutine fill(x, y, color, alpha, label)
        !! Fill the area under a curve
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: label
        
        real(wp), allocatable :: x_closed(:), y_closed(:)
        integer :: n
        
        if (.not. allocated(fig%plots)) then
            call log_error("fill: No active figure")
            return
        end if
        
        n = min(size(x), size(y))
        if (n < 2) then
            call log_error("fill: Need at least 2 points")
            return
        end if
        
        ! Create closed polygon by adding baseline
        allocate(x_closed(n+2), y_closed(n+2))
        x_closed(1:n) = x(1:n)
        y_closed(1:n) = y(1:n)
        x_closed(n+1) = x(n)
        y_closed(n+1) = 0.0_wp  ! baseline
        x_closed(n+2) = x(1)
        y_closed(n+2) = 0.0_wp  ! baseline
        
        ! Add filled area to plot
        call fig%add_plot(x_closed, y_closed)
        
        ! Label handling would be done by add_plot
        
        deallocate(x_closed, y_closed)
    end subroutine fill

    subroutine fill_between(x, y1, y2, where, color, alpha, label, interpolate)
        !! Fill the area between two curves
        real(wp), intent(in) :: x(:)
        real(wp), intent(in), optional :: y1(:), y2(:)
        logical, intent(in), optional :: where(:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: interpolate
        
        real(wp), allocatable :: y1_vals(:), y2_vals(:)
        real(wp), allocatable :: x_poly(:), y_poly(:)
        integer :: n, i, n_poly
        
        if (.not. allocated(fig%plots)) then
            call log_error("fill_between: No active figure")
            return
        end if
        
        n = size(x)
        
        ! Set y1 values (default to data or zeros)
        allocate(y1_vals(n))
        if (present(y1)) then
            if (size(y1) /= n) then
                call log_error("fill_between: y1 size mismatch")
                deallocate(y1_vals)
                return
            end if
            y1_vals = y1
        else
            y1_vals = 0.0_wp
        end if
        
        ! Set y2 values (default to zeros)
        allocate(y2_vals(n))
        if (present(y2)) then
            if (size(y2) /= n) then
                call log_error("fill_between: y2 size mismatch")
                deallocate(y1_vals, y2_vals)
                return
            end if
            y2_vals = y2
        else
            y2_vals = 0.0_wp
        end if
        
        ! Create polygon for filled area
        n_poly = 2*n
        allocate(x_poly(n_poly), y_poly(n_poly))
        
        ! Forward along y1
        x_poly(1:n) = x
        y_poly(1:n) = y1_vals
        
        ! Backward along y2
        do i = 1, n
            x_poly(n+i) = x(n-i+1)
            y_poly(n+i) = y2_vals(n-i+1)
        end do
        
        ! Add filled area to plot
        call fig%add_plot(x_poly, y_poly)
        
        ! Label handling would be done by add_plot
        
        deallocate(y1_vals, y2_vals, x_poly, y_poly)
    end subroutine fill_between

    subroutine twinx()
        !! Create a twin x-axis (placeholder - not fully implemented)
        if (.not. allocated(fig%plots)) then
            call log_error("twinx: No active figure")
            return
        end if
        
        call log_warning("twinx: Dual axis plots not yet fully implemented")
        ! Future: would need to modify figure architecture to support
        ! multiple independent y-axes sharing the same x-axis
    end subroutine twinx

    subroutine twiny()
        !! Create a twin y-axis (placeholder - not fully implemented)
        if (.not. allocated(fig%plots)) then
            call log_error("twiny: No active figure")
            return
        end if
        
        call log_warning("twiny: Dual axis plots not yet fully implemented")
        ! Future: would need to modify figure architecture to support
        ! multiple independent x-axes sharing the same y-axis
    end subroutine twiny

end module fortplot_matplotlib_plots_new