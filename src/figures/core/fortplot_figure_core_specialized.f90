submodule(fortplot_figure_core) fortplot_figure_core_specialized

    use fortplot_colors, only: parse_color, is_valid_color
    use fortplot_format_parser, only: parse_format_string
    use fortplot_logging, only: log_error, log_warning

    implicit none

contains

    module subroutine add_imshow(self, z, xlim, ylim, cmap, alpha, vmin, vmax, &
                                 origin, extent, interpolation, aspect)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: z(:, :)
        real(wp), intent(in), optional :: xlim(2), ylim(2)
        character(len=*), intent(in), optional :: cmap, origin
        character(len=*), intent(in), optional :: interpolation, aspect
        real(wp), intent(in), optional :: alpha, vmin, vmax
        real(wp), intent(in), optional :: extent(4)

        integer :: nx, ny, i
        real(wp) :: x0, x1, y0, y1, tmp_edge
        real(wp), allocatable :: x_edges(:), y_edges(:), z_flip(:, :)
        character(len=8) :: origin_mode

        nx = size(z, 2)
        ny = size(z, 1)
        if (nx == 0 .or. ny == 0) then
            call log_error('imshow: input array must be non-empty')
            return
        end if

        x0 = 0.0_wp
        x1 = real(nx, wp)
        y0 = 0.0_wp
        y1 = real(ny, wp)
        if (present(extent)) then
            if (size(extent) /= 4) then
                call log_error('imshow: extent must contain exactly 4 values')
                return
            end if
            x0 = extent(1)
            x1 = extent(2)
            y0 = extent(3)
            y1 = extent(4)
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

        allocate (x_edges(nx + 1), y_edges(ny + 1))
        do i = 1, nx + 1
            x_edges(i) = x0 + (x1 - x0)*real(i - 1, wp)/real(nx, wp)
        end do
        do i = 1, ny + 1
            y_edges(i) = y0 + (y1 - y0)*real(i - 1, wp)/real(ny, wp)
        end do

        origin_mode = 'lower'
        if (present(origin)) then
            select case (trim(origin))
            case ('upper', 'Upper', 'UPPER')
                origin_mode = 'upper'
            case ('lower', 'Lower', 'LOWER')
                origin_mode = 'lower'
            case default
                call log_warning('imshow: unsupported origin '//trim(origin)// &
                                 '; using lower')
            end select
        end if

        if (origin_mode == 'upper') then
            do i = 1, ny/2
                tmp_edge = y_edges(i)
                y_edges(i) = y_edges(ny - i + 2)
                y_edges(ny - i + 2) = tmp_edge
            end do
            allocate (z_flip(ny, nx))
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
            call self%add_pcolormesh(x_edges, y_edges, z_flip, colormap=cmap, &
                                     vmin=vmin, vmax=vmax)
            deallocate (z_flip)
        else
            call self%add_pcolormesh(x_edges, y_edges, z, colormap=cmap, &
                                     vmin=vmin, vmax=vmax)
        end if

        deallocate (x_edges, y_edges)
    end subroutine add_imshow

    module subroutine add_polar(self, theta, r, label, fmt, linestyle, marker, color)
        !! Add polar plot data with true polar projection support
        !!
        !! When called, this enables polar projection mode on the figure,
        !! rendering a circular plot area with radial gridlines and angular ticks.
        use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_POLAR
        use fortplot_figure_plot_management, only: next_plot_color
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: label, fmt
        character(len=*), intent(in), optional :: linestyle, marker, color

        integer :: n, i, pos, color_len, idx
        real(wp), allocatable :: x(:), y(:)
        real(wp) :: color_rgb(3), r_max
        character(len=20) :: fmt_marker, fmt_linestyle
        character(len=20) :: final_marker, final_linestyle
        character(len=:), allocatable :: fmt_work, fmt_color
        logical :: have_color, color_ok

        n = min(size(theta), size(r))
        if (n == 0) then
            call log_error('polar: theta and r must contain values')
            return
        end if

        ! Enable polar projection mode
        self%state%polar_projection = .true.
        self%state%aspect_mode = 'equal'  ! Square plot for circular appearance

        ! Calculate r_max for axis limits
        r_max = maxval(abs(r(1:n)))
        if (r_max < 1.0e-10_wp) r_max = 1.0_wp
        self%state%polar_r_max = r_max*1.1_wp  ! 10% padding

        ! Set symmetric Cartesian limits for circular plot area
        if (.not. self%state%xlim_set) then
            self%state%x_min = -self%state%polar_r_max
            self%state%x_max = self%state%polar_r_max
            self%state%xlim_set = .true.
        end if
        if (.not. self%state%ylim_set) then
            self%state%y_min = -self%state%polar_r_max
            self%state%y_max = self%state%polar_r_max
            self%state%ylim_set = .true.
        end if

        ! Also compute Cartesian coordinates for data range calculation
        allocate (x(n), y(n))
        do i = 1, n
            x(i) = r(i)*cos(theta(i))
            y(i) = r(i)*sin(theta(i))
        end do

        ! Parse format string for color, marker, linestyle
        final_marker = ''
        final_linestyle = '-'  ! Default solid line

        if (present(fmt)) then
            fmt_work = trim(adjustl(fmt))
            if (len_trim(fmt_work) > 0) then
                color_len = 0
                do pos = 1, len_trim(fmt_work)
                    if (is_valid_color(fmt_work(1:pos))) then
                        color_len = pos
                    else
                        if (color_len > 0) exit
                    end if
                end do
                if (color_len > 0) then
                    fmt_color = fmt_work(1:color_len)
                    if (color_len + 1 <= len(fmt_work)) then
                        fmt_work = fmt_work(color_len + 1:)
                    else
                        fmt_work = ''
                    end if
                end if
                fmt_work = trim(adjustl(fmt_work))
                call parse_format_string(fmt_work, fmt_marker, fmt_linestyle)
                if (len_trim(fmt_marker) > 0) final_marker = trim(fmt_marker)
                if (len_trim(fmt_linestyle) > 0) final_linestyle = trim(fmt_linestyle)
            end if
        end if

        if (present(marker)) then
            if (len_trim(marker) > 0) final_marker = trim(marker)
        end if
        if (present(linestyle)) then
            if (len_trim(linestyle) > 0) final_linestyle = trim(linestyle)
        end if

        ! Normalize linestyle names
        select case (trim(final_linestyle))
        case ('solid', 'Solid', 'SOLID')
            final_linestyle = '-'
        case ('dashed', 'Dashed', 'DASHED')
            final_linestyle = '--'
        case ('dotted', 'Dotted', 'DOTTED')
            final_linestyle = ':'
        case ('dashdot', 'Dashdot', 'DASHDOT')
            final_linestyle = '-.'
        case ('none', 'None', 'NONE')
            final_linestyle = 'None'
        end select

        ! Parse color
        have_color = .false.
        if (present(color)) then
            call parse_color(color, color_rgb, color_ok)
            if (color_ok) then
                have_color = .true.
            else
                call log_warning('polar: unsupported color; using palette')
            end if
        else if (allocated(fmt_color)) then
            call parse_color(fmt_color, color_rgb, color_ok)
            if (color_ok) have_color = .true.
        end if

        if (.not. have_color) then
            color_rgb = next_plot_color(self%state)
        end if

        ! Check capacity and add polar plot
        if (self%state%plot_count >= self%state%max_plots) then
            call log_warning('polar: maximum number of plots reached')
            return
        end if
        self%state%plot_count = self%state%plot_count + 1
        self%plot_count = self%state%plot_count
        idx = self%state%plot_count

        self%plots(idx)%plot_type = PLOT_TYPE_POLAR
        self%plots(idx)%color = color_rgb

        ! Store both Cartesian (for range) and polar (for rendering) data
        allocate (self%plots(idx)%x(n), source=x)
        allocate (self%plots(idx)%y(n), source=y)
        allocate (self%plots(idx)%polar_theta(n), source=theta(1:n))
        allocate (self%plots(idx)%polar_r(n), source=r(1:n))

        if (len_trim(final_linestyle) > 0) then
            self%plots(idx)%linestyle = trim(final_linestyle)
        end if
        if (len_trim(final_marker) > 0) then
            self%plots(idx)%marker = trim(final_marker)
        end if
        if (present(label)) then
            self%plots(idx)%label = label
        end if

        if (allocated(fmt_work)) deallocate (fmt_work)
        if (allocated(fmt_color)) deallocate (fmt_color)
        deallocate (x, y)
    end subroutine add_polar

    module subroutine add_step(self, x, y, label, where, linestyle, color, linewidth)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, where
        character(len=*), intent(in), optional :: linestyle, color
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
                call log_warning('step: unsupported where value; using pre')
            end select
        end if

        select case (step_type)
        case ('pre', 'PRE')
            n_points = 2*n - 1
            allocate (x_step(n_points), y_step(n_points))
            do i = 1, n - 1
                x_step(2*i - 1) = x(i)
                y_step(2*i - 1) = y(i)
                x_step(2*i) = x(i + 1)
                y_step(2*i) = y(i)
            end do
            x_step(n_points) = x(n)
            y_step(n_points) = y(n)

        case ('post', 'POST')
            n_points = 2*n - 1
            allocate (x_step(n_points), y_step(n_points))
            x_step(1) = x(1)
            y_step(1) = y(1)
            do i = 2, n
                x_step(2*i - 2) = x(i)
                y_step(2*i - 2) = y(i - 1)
                x_step(2*i - 1) = x(i)
                y_step(2*i - 1) = y(i)
            end do

        case ('mid', 'MID')
            n_points = 2*n
            allocate (x_step(n_points), y_step(n_points))
            do i = 1, n - 1
                x_step(2*i - 1) = x(i)
                y_step(2*i - 1) = y(i)
                x_step(2*i) = 0.5_wp*(x(i) + x(i + 1))
                y_step(2*i) = y(i)
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

        call self%add_plot(x_step, y_step, label=label, linestyle=linestyle)
        deallocate (x_step, y_step)
    end subroutine add_step

    module subroutine add_stem(self, x, y, label, linefmt, markerfmt, basefmt, bottom)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linefmt
        character(len=*), intent(in), optional :: markerfmt, basefmt
        real(wp), intent(in), optional :: bottom

        integer :: n, i
        real(wp) :: baseline, xmin, xmax
        real(wp), allocatable :: xs(:), ys(:)
        logical :: label_used

        n = min(size(x), size(y))
        if (n == 0) then
            call log_error('stem: x and y must contain values')
            return
        end if

        baseline = 0.0_wp
        if (present(bottom)) baseline = bottom

        xmin = minval(x(1:n))
        xmax = maxval(x(1:n))
        allocate (xs(2), ys(2))
        label_used = .false.

        if (present(linefmt)) then
            call log_warning('stem: linefmt ignored; use subplot styling instead')
        end if
        if (present(markerfmt)) then
            call log_warning('stem: markerfmt ignored by current backend')
        end if
        if (present(basefmt)) then
            call log_warning('stem: basefmt ignored by current backend')
        end if

        do i = 1, n
            xs(1) = x(i)
            xs(2) = x(i)
            ys(1) = baseline
            ys(2) = y(i)
            if (present(label) .and. .not. label_used) then
                call self%add_plot(xs, ys, label=label)
                label_used = .true.
            else
                call self%add_plot(xs, ys)
            end if
        end do

        xs(1) = xmin
        xs(2) = xmax
        ys(1) = baseline
        ys(2) = baseline
        call self%add_plot(xs, ys)
        deallocate (xs, ys)

        call self%add_plot(x(1:n), y(1:n))
    end subroutine add_stem

    module subroutine add_fill(self, x, y, color, alpha)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha

        if (present(color) .and. present(alpha)) then
            call self%add_fill_between(x, y1=y, color=color, alpha=alpha)
        else if (present(color)) then
            call self%add_fill_between(x, y1=y, color=color)
        else if (present(alpha)) then
            call self%add_fill_between(x, y1=y, alpha=alpha)
        else
            call self%add_fill_between(x, y1=y)
        end if
    end subroutine add_fill

    module subroutine add_fill_between(self, x, y1, y2, where, color, alpha, &
                                       interpolate)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:)
        real(wp), intent(in), optional :: y1(:), y2(:)
        logical, intent(in), optional :: where (:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha
        logical, intent(in), optional :: interpolate

        integer :: n
        real(wp), allocatable :: upper_vals(:), lower_vals(:)
        logical, allocatable :: mask_vals(:)
        logical :: has_mask, has_color, has_alpha
        character(len=:), allocatable :: color_value
        real(wp) :: alpha_value

        n = size(x)
        if (n < 2) then
            call log_error('fill_between: need at least two points to form area')
            return
        end if

        allocate (upper_vals(n), lower_vals(n))
        if (present(y1)) then
            if (size(y1) /= n) then
                call log_error('fill_between: y1 size mismatch')
                deallocate (upper_vals, lower_vals)
                return
            end if
            upper_vals = y1
        else
            upper_vals = 0.0_wp
        end if

        if (present(y2)) then
            if (size(y2) /= n) then
                call log_error('fill_between: y2 size mismatch')
                deallocate (upper_vals, lower_vals)
                return
            end if
            lower_vals = y2
        else
            lower_vals = 0.0_wp
        end if

        has_mask = .false.
        if (present(where)) then
            if (size(where) /= n) then
                call log_error('fill_between: where mask size mismatch')
                deallocate (upper_vals, lower_vals)
                return
            end if
            allocate (mask_vals(n))
            mask_vals = where
            if (.not. any(mask_vals)) then
                call log_warning('fill_between: mask excludes all data points')
                deallocate (upper_vals, lower_vals, mask_vals)
                return
            end if
            has_mask = .true.
        end if

        if (present(interpolate)) then
            call log_warning('fill_between: interpolate option ignored')
        end if

        has_color = present(color)
        if (has_color) color_value = color
        has_alpha = present(alpha)
        if (has_alpha) alpha_value = alpha

        select case (merge(1, 0, has_mask) + merge(2, 0, has_color) + &
                     merge(4, 0, has_alpha))
        case (0)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, plot_count=self%plot_count)
        case (1)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, mask=mask_vals, &
                                       plot_count=self%plot_count)
        case (2)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, color_string=color_value, &
                                       plot_count=self%plot_count)
        case (3)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, mask=mask_vals, &
                                       color_string=color_value, &
                                       plot_count=self%plot_count)
        case (4)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, alpha=alpha_value, &
                                       plot_count=self%plot_count)
        case (5)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, mask=mask_vals, alpha=alpha_value, &
                                       plot_count=self%plot_count)
        case (6)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, color_string=color_value, &
                                       alpha=alpha_value, plot_count=self%plot_count)
        case default
            call core_add_fill_between(self%plots, self%state, x, upper_vals, &
                                       lower_vals, mask=mask_vals, &
                                       color_string=color_value, &
                                       alpha=alpha_value, plot_count=self%plot_count)
        end select

        self%plot_count = self%state%plot_count

        if (has_mask) deallocate (mask_vals)
        deallocate (upper_vals, lower_vals)
    end subroutine add_fill_between

end submodule fortplot_figure_core_specialized
