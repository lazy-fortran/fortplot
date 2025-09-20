module fortplot_figure_advanced_plots
    !! Advanced plotting functions extracted for file size compliance
    !! Contains specialized plot types: imshow, polar, step, stem, pie
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_logging, only: log_error, log_warning
    implicit none
    
    private
    public :: add_imshow_impl, add_polar_impl, add_step_impl, add_stem_impl, add_pie_impl
    
    real(wp), parameter :: PI = acos(-1.0_wp)
    
contains

    subroutine add_imshow_impl(fig_add_pcolormesh, z, xlim, ylim, cmap, alpha, vmin, vmax, origin, &
                               extent, interpolation, aspect)
        !! Display 2D array as an image using the pcolormesh backend
        interface
            subroutine fig_add_pcolormesh(x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
                import :: wp
                real(wp), intent(in) :: x(:), y(:), c(:,:)
                character(len=*), intent(in), optional :: colormap
                real(wp), intent(in), optional :: vmin, vmax
                real(wp), intent(in), optional :: edgecolors(3)
                real(wp), intent(in), optional :: linewidths
            end subroutine
        end interface
        real(wp), intent(in) :: z(:,:)
        real(wp), intent(in), optional :: xlim(2), ylim(2)
        character(len=*), intent(in), optional :: cmap, origin, interpolation, aspect
        real(wp), intent(in), optional :: alpha, vmin, vmax
        real(wp), intent(in), optional :: extent(4)

        integer :: nx, ny, i
        real(wp) :: x0, x1, y0, y1, tmp_edge
        real(wp), allocatable :: x_edges(:), y_edges(:), z_flip(:,:)
        character(len=8) :: origin_mode
        character(len=32) :: cmap_local

        nx = size(z, 2)
        ny = size(z, 1)
        if (nx == 0 .or. ny == 0) then
            call log_error("imshow: input array must be non-empty")
            return
        end if

        cmap_local = 'viridis'
        if (present(cmap)) cmap_local = cmap

        x0 = 0.0_wp; x1 = real(nx, wp)
        y0 = 0.0_wp; y1 = real(ny, wp)
        if (present(extent)) then
            if (size(extent) /= 4) then
                call log_error("imshow: extent must contain exactly 4 values")
                return
            end if
            x0 = extent(1); x1 = extent(2)
            y0 = extent(3); y1 = extent(4)
            if (present(xlim) .or. present(ylim)) then
                call log_warning('imshow: ignoring xlim/ylim because extent is set')
            end if
        else
            if (present(xlim)) then
                x0 = xlim(1)
                x1 = xlim(2)
            end if
            if (present(ylim)) then
                y0 = ylim(1)
                y1 = ylim(2)
            end if
        end if

        allocate(x_edges(nx+1), y_edges(ny+1))
        do i = 1, nx + 1
            x_edges(i) = x0 + (x1 - x0) * real(i - 1, wp) / real(nx, wp)
        end do
        do i = 1, ny + 1
            y_edges(i) = y0 + (y1 - y0) * real(i - 1, wp) / real(ny, wp)
        end do

        origin_mode = 'lower'
        if (present(origin)) then
            select case (trim(origin))
            case ('upper', 'Upper', 'UPPER')
                origin_mode = 'upper'
            case ('lower', 'Lower', 'LOWER')
                origin_mode = 'lower'
            case default
                call log_warning('imshow: unsupported origin "' // trim(origin) // &
                                 '"; using "lower"')
            end select
        end if

        if (origin_mode == 'upper') then
            do i = 1, ny/2
                tmp_edge = y_edges(i)
                y_edges(i) = y_edges(ny - i + 2)
                y_edges(ny - i + 2) = tmp_edge
            end do
            allocate(z_flip(ny, nx))
            do i = 1, ny
                z_flip(i, :) = z(ny - i + 1, :)
            end do
        end if

        if (present(alpha)) then
            call log_warning('imshow: alpha not yet supported')
        end if
        if (present(interpolation)) then
            call log_warning('imshow: interpolation ignored by current backend')
        end if
        if (present(aspect)) then
            call log_warning('imshow: aspect not configurable on current backend')
        end if

        if (origin_mode == 'upper') then
            call fig_add_pcolormesh(x_edges, y_edges, z_flip, colormap=cmap_local, &
                                    vmin=vmin, vmax=vmax)
            deallocate(z_flip)
        else
            call fig_add_pcolormesh(x_edges, y_edges, z, colormap=cmap_local, &
                                    vmin=vmin, vmax=vmax)
        end if

        deallocate(x_edges, y_edges)
    end subroutine add_imshow_impl

    subroutine add_polar_impl(fig_add_plot, theta, r, label, fmt, linestyle, marker, color)
        !! Plot data provided in polar coordinates by converting to Cartesian
        interface
            subroutine fig_add_plot(x, y, label, linestyle)
                import :: wp
                real(wp), intent(in) :: x(:), y(:)
                character(len=*), intent(in), optional :: label, linestyle
            end subroutine
        end interface
        real(wp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: label, fmt
        character(len=*), intent(in), optional :: linestyle, marker, color

        integer :: n, i
        real(wp), allocatable :: x(:), y(:)

        n = min(size(theta), size(r))
        if (n == 0) then
            call log_error('polar: theta and r must contain values')
            return
        end if

        allocate(x(n), y(n))
        do i = 1, n
            x(i) = r(i) * cos(theta(i))
            y(i) = r(i) * sin(theta(i))
        end do

        if (present(fmt)) then
            call log_warning('polar: fmt ignored; use linestyle/marker arguments')
        end if
        if (present(marker)) then
            call log_warning('polar: marker styling not yet supported')
        end if
        if (present(color)) then
            call log_warning('polar: color strings not mapped to RGB yet')
        end if

        call fig_add_plot(x, y, label=label, linestyle=linestyle)
        deallocate(x, y)
    end subroutine add_polar_impl

    subroutine add_step_impl(fig_add_plot, x, y, label, where, linestyle, color, linewidth)
        !! Create a stepped line plot using repeated x positions
        interface
            subroutine fig_add_plot(x, y, label, linestyle)
                import :: wp
                real(wp), intent(in) :: x(:), y(:)
                character(len=*), intent(in), optional :: label, linestyle
            end subroutine
        end interface
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, where, linestyle, color
        real(wp), intent(in), optional :: linewidth

        integer :: n, i, n_points
        character(len=8) :: step_type
        real(wp), allocatable :: x_step(:), y_step(:)

        n = min(size(x), size(y))
        if (n < 2) then
            call log_error('step: need at least two samples')
            return
        end if

        step_type = 'pre'
        if (present(where)) then
            select case (trim(where))
            case ('post', 'Post', 'POST')
                step_type = 'post'
            case ('mid', 'Mid', 'MID')
                step_type = 'mid'
            case ('pre', 'Pre', 'PRE')
                step_type = 'pre'
            case default
                call log_warning('step: unsupported where value; using "pre"')
            end select
        end if

        select case (step_type)
        case ('pre', 'PRE')
            n_points = 2 * n - 1
            allocate(x_step(n_points), y_step(n_points))
            do i = 1, n - 1
                x_step(2 * i - 1) = x(i)
                y_step(2 * i - 1) = y(i)
                x_step(2 * i) = x(i + 1)
                y_step(2 * i) = y(i)
            end do
            x_step(n_points) = x(n)
            y_step(n_points) = y(n)

        case ('post', 'POST')
            n_points = 2 * n - 1
            allocate(x_step(n_points), y_step(n_points))
            x_step(1) = x(1)
            y_step(1) = y(1)
            do i = 2, n
                x_step(2 * i - 2) = x(i)
                y_step(2 * i - 2) = y(i - 1)
                x_step(2 * i - 1) = x(i)
                y_step(2 * i - 1) = y(i)
            end do

        case ('mid', 'MID')
            n_points = 2 * n
            allocate(x_step(n_points), y_step(n_points))
            do i = 1, n - 1
                x_step(2 * i - 1) = x(i)
                y_step(2 * i - 1) = y(i)
                x_step(2 * i) = 0.5_wp * (x(i) + x(i + 1))
                y_step(2 * i) = y(i)
            end do
            x_step(n_points - 1) = x(n)
            y_step(n_points - 1) = y(n - 1)
            x_step(n_points) = x(n)
            y_step(n_points) = y(n)
        end select

        if (present(color)) then
            call log_warning('step: color strings not yet mapped to RGB values')
        end if
        if (present(linewidth)) then
            call log_warning('step: linewidth not configurable in current backend')
        end if

        call fig_add_plot(x_step, y_step, label=label, linestyle=linestyle)
        deallocate(x_step, y_step)
    end subroutine add_step_impl

    subroutine add_stem_impl(fig_add_plot, x, y, label, linefmt, markerfmt, basefmt, bottom)
        !! Draw vertical stems from a baseline to each data point
        interface
            subroutine fig_add_plot(x, y, label, linestyle)
                import :: wp
                real(wp), intent(in) :: x(:), y(:)
                character(len=*), intent(in), optional :: label, linestyle
            end subroutine
        end interface
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linefmt, markerfmt, basefmt
        real(wp), intent(in), optional :: bottom

        integer :: n, i
        real(wp) :: baseline, xmin, xmax
        real(wp), allocatable :: xs(:), ys(:)

        n = min(size(x), size(y))
        if (n == 0) then
            call log_error('stem: x and y must contain data')
            return
        end if

        baseline = 0.0_wp
        if (present(bottom)) baseline = bottom

        if (present(linefmt)) then
            call log_warning('stem: linefmt styling not fully implemented')
        end if
        if (present(markerfmt)) then
            call log_warning('stem: markerfmt not yet supported')
        end if
        if (present(basefmt)) then
            call log_warning('stem: basefmt styling not yet implemented')
        end if

        allocate(xs(2*n + 2), ys(2*n + 2))

        ! Draw baseline
        xmin = minval(x)
        xmax = maxval(x)
        xs(1) = xmin - 0.05_wp * (xmax - xmin)
        ys(1) = baseline
        xs(2) = xmax + 0.05_wp * (xmax - xmin)
        ys(2) = baseline

        ! Draw stems
        do i = 1, n
            xs(2 + 2*(i-1) + 1) = x(i)
            ys(2 + 2*(i-1) + 1) = baseline
            xs(2 + 2*(i-1) + 2) = x(i)
            ys(2 + 2*(i-1) + 2) = y(i)
        end do

        call fig_add_plot(xs, ys, label=label)
        deallocate(xs, ys)
    end subroutine add_stem_impl

    subroutine add_pie_impl(fig_add_plot, values, labels, autopct, startangle, colors, explode)
        !! Create a pie chart using polygon segments
        interface
            subroutine fig_add_plot(x, y, label)
                import :: wp
                real(wp), intent(in) :: x(:), y(:)
                character(len=*), intent(in), optional :: label
            end subroutine
        end interface
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        character(len=*), intent(in), optional :: autopct
        real(wp), intent(in), optional :: startangle
        real(wp), intent(in), optional :: colors(:,:)
        real(wp), intent(in), optional :: explode(:)

        integer :: n, i, j, seg_count
        real(wp) :: total, angle_start, angle_span, radius, cx, cy
        real(wp) :: offset, base_angle
        real(wp), allocatable :: x_pts(:), y_pts(:)

        n = size(values)
        if (n == 0) then
            call log_error('pie: values array must contain data')
            return
        end if

        total = sum(values)
        if (total <= 0.0_wp) then
            call log_error('pie: sum of values must be positive')
            return
        end if

        angle_start = 90.0_wp
        if (present(startangle)) angle_start = startangle
        angle_start = angle_start * PI / 180.0_wp

        radius = 1.0_wp
        cx = 0.0_wp
        cy = 0.0_wp

        if (present(colors)) then
            call log_warning('pie: custom colors not yet supported; using defaults')
        end if
        if (present(autopct)) then
            call log_warning('pie: autopct formatting is not implemented')
        end if

        do i = 1, n
            angle_span = 2.0_wp * PI * values(i) / total
            seg_count = max(12, int(abs(angle_span) * 180.0_wp / PI) + 1)
            allocate(x_pts(seg_count + 2), y_pts(seg_count + 2))

            offset = 0.0_wp
            if (present(explode)) then
                if (i <= size(explode)) offset = explode(i)
            end if
            offset = offset * radius * 0.1_wp

            base_angle = angle_start + 0.5_wp * angle_span
            x_pts(1) = cx + offset * cos(base_angle)
            y_pts(1) = cy + offset * sin(base_angle)

            do j = 1, seg_count + 1
                x_pts(j + 1) = x_pts(1) + radius * cos(angle_start + &
                                 angle_span * real(j - 1, wp) / real(seg_count, wp))
                y_pts(j + 1) = y_pts(1) + radius * sin(angle_start + &
                                 angle_span * real(j - 1, wp) / real(seg_count, wp))
            end do

            if (present(labels) .and. i <= size(labels)) then
                call fig_add_plot(x_pts, y_pts, label=labels(i))
            else
                call fig_add_plot(x_pts, y_pts)
            end if

            deallocate(x_pts, y_pts)
            angle_start = angle_start + angle_span
        end do
    end subroutine add_pie_impl

end module fortplot_figure_advanced_plots