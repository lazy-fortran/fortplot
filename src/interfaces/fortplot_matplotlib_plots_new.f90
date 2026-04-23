module fortplot_matplotlib_plots_new
    !! Matplotlib-compatible plot functions for imshow/pie/polar/step/stem/fill.
    !!
    !! Color kwargs accept either a named color string or an RGB triple via
    !! generic interfaces. `fill_between` makes `y1` required to match
    !! matplotlib's contract. `fill` accepts an optional `step` stage to
    !! mirror matplotlib's stair-fill mode.

    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_global, only: fig => global_figure
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb
    use fortplot_matplotlib_session, only: ensure_fig_init
    use fortplot_logging, only: log_error, log_warning

    implicit none
    private

    public :: imshow, pie, polar, step, stem
    public :: fill, fill_between, twinx, twiny

    interface polar
        module procedure polar_string
        module procedure polar_rgb
    end interface polar

    interface step
        module procedure step_string
        module procedure step_rgb
    end interface step

    interface fill
        module procedure fill_string
        module procedure fill_rgb
        module procedure fill_default
    end interface fill

    interface fill_between
        module procedure fill_between_string
        module procedure fill_between_rgb
    end interface fill_between

contains

    subroutine imshow(z, cmap, alpha, vmin, vmax, origin, extent, interpolation, aspect)
        !! Display 2D array as an image (heatmap)
        real(wp), intent(in) :: z(:,:)
        character(len=*), intent(in), optional :: cmap
        real(wp), intent(in), optional :: alpha, vmin, vmax
        character(len=*), intent(in), optional :: origin, interpolation, aspect
        real(wp), intent(in), optional :: extent(4)

        call ensure_fig_init()
        call fig%add_imshow(z, cmap=cmap, alpha=alpha, vmin=vmin, vmax=vmax, &
                            origin=origin, extent=extent, interpolation=interpolation, &
                            aspect=aspect)
    end subroutine imshow

    subroutine pie(values, labels, colors, explode, autopct, startangle)
        !! Create a pie chart
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        character(len=*), intent(in), optional :: colors(:)
        real(wp), intent(in), optional :: explode(:)
        character(len=*), intent(in), optional :: autopct
        real(wp), intent(in), optional :: startangle

        call ensure_fig_init()
        call fig%add_pie(values, labels=labels, autopct=autopct, startangle=startangle, &
                         colors=colors, explode=explode)
    end subroutine pie

    subroutine polar_string(theta, r, fmt, label, linestyle, marker, color)
        !! String-color variant of polar.
        real(wp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: fmt, label
        character(len=*), intent(in), optional :: linestyle, marker
        character(len=*), intent(in), optional :: color

        call ensure_fig_init()
        call fig%add_polar(theta, r, label=label, fmt=fmt, linestyle=linestyle, &
                           marker=marker, color=color)
    end subroutine polar_string

    subroutine polar_rgb(theta, r, color, fmt, label, linestyle, marker)
        !! RGB-color variant of polar. Serialises the RGB triple as a hex
        !! string so the underlying implementation remains untouched.
        real(wp), intent(in) :: theta(:), r(:)
        real(wp), intent(in) :: color(3)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker

        character(len=7) :: hex

        call ensure_fig_init()
        hex = rgb_to_hex(color)
        call fig%add_polar(theta, r, label=label, fmt=fmt, linestyle=linestyle, &
                           marker=marker, color=hex)
    end subroutine polar_rgb

    subroutine step_string(x, y, where, label, linestyle, color, linewidth)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: where, label
        character(len=*), intent(in), optional :: linestyle
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%add_step(x, y, label=label, where=where, linestyle=linestyle, &
                          color=color, linewidth=linewidth)
    end subroutine step_string

    subroutine step_rgb(x, y, color, where, label, linestyle, linewidth)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: color(3)
        character(len=*), intent(in), optional :: where, label, linestyle
        real(wp), intent(in), optional :: linewidth

        character(len=7) :: hex

        call ensure_fig_init()
        hex = rgb_to_hex(color)
        call fig%add_step(x, y, label=label, where=where, linestyle=linestyle, &
                          color=hex, linewidth=linewidth)
    end subroutine step_rgb

    subroutine stem(x, y, linefmt, markerfmt, basefmt, label, bottom)
        !! Create a stem plot
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: linefmt, markerfmt, basefmt
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: bottom

        call ensure_fig_init()
        call fig%add_stem(x, y, label=label, linefmt=linefmt, markerfmt=markerfmt, &
                          basefmt=basefmt, bottom=bottom)
    end subroutine stem

    subroutine fill_string(x, y, color, alpha, step)
        !! Fill the area between a curve and zero. `step` activates stair
        !! fill to match matplotlib's `step` argument on `fill_between`.
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: step

        real(wp), allocatable :: x_use(:), y_use(:)

        call ensure_fig_init()
        call apply_step_transform(x, y, step, x_use, y_use)
        call fig%add_fill(x_use, y_use, color=color, alpha=alpha)
    end subroutine fill_string

    subroutine fill_rgb(x, y, color, alpha, step)
        !! RGB-color variant of fill.
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: color(3)
        real(wp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: step

        character(len=7) :: hex
        real(wp), allocatable :: x_use(:), y_use(:)

        call ensure_fig_init()
        hex = rgb_to_hex(color)
        call apply_step_transform(x, y, step, x_use, y_use)
        call fig%add_fill(x_use, y_use, color=hex, alpha=alpha)
    end subroutine fill_rgb

    subroutine fill_default(x, y, alpha, step)
        !! `fill` called without an explicit color uses the figure palette.
        !! Kept as a dedicated overload so matplotlib-style no-color calls
        !! remain legal through the generic interface.
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: step

        real(wp), allocatable :: x_use(:), y_use(:)

        call ensure_fig_init()
        call apply_step_transform(x, y, step, x_use, y_use)
        call fig%add_fill(x_use, y_use, alpha=alpha)
    end subroutine fill_default

    subroutine fill_between_string(x, y1, y2, where, color, alpha, interpolate, step)
        !! Matplotlib-style fill_between with string color. `y1` is required
        !! (matching matplotlib); `y2` defaults to zero.
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: y1(:)
        real(wp), intent(in), optional :: y2(:)
        logical, intent(in), optional :: where(:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha
        logical, intent(in), optional :: interpolate
        character(len=*), intent(in), optional :: step

        real(wp), allocatable :: x_use(:), y1_use(:), y2_use(:)
        logical, allocatable :: where_use(:)

        call ensure_fig_init()
        call apply_step_fill_between(x, y1, y2, where, step, x_use, y1_use, &
                                     y2_use, where_use)
        if (size(x_use) < 2) then
            call log_error('fill_between: need at least two points to form area')
            return
        end if

        if (allocated(where_use)) then
            call fig%add_fill_between(x_use, y1=y1_use, y2=y2_use, color=color, &
                                      alpha=alpha, where=where_use, &
                                      interpolate=interpolate)
        else
            call fig%add_fill_between(x_use, y1=y1_use, y2=y2_use, color=color, &
                                      alpha=alpha, interpolate=interpolate)
        end if
    end subroutine fill_between_string

    subroutine fill_between_rgb(x, y1, y2, where, color, alpha, interpolate, step)
        !! RGB-color variant of fill_between. Same positional layout as the
        !! string variant; `color` keyword type distinguishes the two.
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: y1(:)
        real(wp), intent(in), optional :: y2(:)
        logical, intent(in), optional :: where(:)
        real(wp), intent(in) :: color(3)
        real(wp), intent(in), optional :: alpha
        logical, intent(in), optional :: interpolate
        character(len=*), intent(in), optional :: step

        character(len=7) :: hex

        call ensure_fig_init()
        hex = rgb_to_hex(color)
        call fill_between_string(x, y1=y1, color=hex, y2=y2, where=where, &
                                 alpha=alpha, interpolate=interpolate, step=step)
    end subroutine fill_between_rgb

    subroutine apply_step_transform(x, y, step, x_out, y_out)
        !! Generate a stair-cased (x, y) sequence for fill/fill_between.
        !!
        !! `pre`: each y value anchors the interval to its left.
        !! `post`: each y value anchors the interval to its right.
        !! `mid`: transitions happen at the midpoint between samples.
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: step
        real(wp), allocatable, intent(out) :: x_out(:), y_out(:)

        integer :: n, i, m

        if (.not. present(step)) then
            allocate (x_out(size(x)), y_out(size(y)))
            x_out = x
            y_out = y
            return
        end if

        n = size(x)
        select case (trim(step))
        case ('pre')
            m = 2*n - 1
            allocate (x_out(m), y_out(m))
            do i = 1, n - 1
                x_out(2*i - 1) = x(i)
                y_out(2*i - 1) = y(i)
                x_out(2*i) = x(i + 1)
                y_out(2*i) = y(i)
            end do
            x_out(m) = x(n)
            y_out(m) = y(n)
        case ('post')
            m = 2*n - 1
            allocate (x_out(m), y_out(m))
            x_out(1) = x(1)
            y_out(1) = y(1)
            do i = 2, n
                x_out(2*i - 2) = x(i)
                y_out(2*i - 2) = y(i - 1)
                x_out(2*i - 1) = x(i)
                y_out(2*i - 1) = y(i)
            end do
        case ('mid')
            m = 2*n
            allocate (x_out(m), y_out(m))
            do i = 1, n - 1
                x_out(2*i - 1) = x(i)
                y_out(2*i - 1) = y(i)
                x_out(2*i) = 0.5_wp*(x(i) + x(i + 1))
                y_out(2*i) = y(i)
            end do
            x_out(m - 1) = x(n)
            y_out(m - 1) = y(n - 1)
            x_out(m) = x(n)
            y_out(m) = y(n)
        case default
            call log_warning('fill: unsupported step value ' // trim(step))
            allocate (x_out(size(x)), y_out(size(y)))
            x_out = x
            y_out = y
        end select
    end subroutine apply_step_transform

    subroutine apply_step_fill_between(x, y1, y2, where, step, x_out, y1_out, &
                                       y2_out, where_out)
        !! Apply stair-casing to x, y1, y2, and optional mask for
        !! `fill_between(step=...)`. Mirrors `apply_step_transform` on the
        !! pair of curves and preserves boolean mask alignment.
        real(wp), intent(in) :: x(:), y1(:)
        real(wp), intent(in), optional :: y2(:)
        logical, intent(in), optional :: where(:)
        character(len=*), intent(in), optional :: step
        real(wp), allocatable, intent(out) :: x_out(:), y1_out(:), y2_out(:)
        logical, allocatable, intent(out) :: where_out(:)

        real(wp), allocatable :: y2_work(:)
        logical, allocatable :: where_work(:)
        real(wp), allocatable :: x_stepped(:), y1_stepped(:), y2_stepped(:)
        logical, allocatable :: where_stepped(:)
        integer :: n

        n = size(x)
        allocate (y2_work(n))
        if (present(y2)) then
            if (size(y2) /= n) then
                call log_error('fill_between: y2 size mismatch')
                return
            end if
            y2_work = y2
        else
            y2_work = 0.0_wp
        end if

        if (present(where)) then
            if (size(where) /= n) then
                call log_error('fill_between: where mask size mismatch')
                return
            end if
            allocate (where_work(n), source=where)
        end if

        if (.not. present(step)) then
            allocate (x_out(n), y1_out(n), y2_out(n))
            x_out = x
            y1_out = y1
            y2_out = y2_work
            if (allocated(where_work)) then
                allocate (where_out(n), source=where_work)
            end if
            return
        end if

        call step_pair(x, y1, y2_work, where_work, trim(step), x_stepped, &
                       y1_stepped, y2_stepped, where_stepped)
        call move_alloc(x_stepped, x_out)
        call move_alloc(y1_stepped, y1_out)
        call move_alloc(y2_stepped, y2_out)
        if (allocated(where_stepped)) then
            call move_alloc(where_stepped, where_out)
        end if
    end subroutine apply_step_fill_between

    subroutine step_pair(x, y1, y2, where_in, mode, x_out, y1_out, y2_out, &
                         where_out)
        !! Stair-case both curves and the optional mask in lockstep.
        real(wp), intent(in) :: x(:), y1(:), y2(:)
        logical, intent(in), allocatable :: where_in(:)
        character(len=*), intent(in) :: mode
        real(wp), allocatable, intent(out) :: x_out(:), y1_out(:), y2_out(:)
        logical, allocatable, intent(out) :: where_out(:)

        integer :: n, i, m
        logical :: has_mask

        n = size(x)
        has_mask = allocated(where_in)

        select case (mode)
        case ('pre')
            m = 2*n - 1
            call allocate_step_buffers(m, has_mask, x_out, y1_out, y2_out, where_out)
            do i = 1, n - 1
                x_out(2*i - 1) = x(i)
                y1_out(2*i - 1) = y1(i)
                y2_out(2*i - 1) = y2(i)
                x_out(2*i) = x(i + 1)
                y1_out(2*i) = y1(i)
                y2_out(2*i) = y2(i)
                if (has_mask) then
                    where_out(2*i - 1) = where_in(i)
                    where_out(2*i) = where_in(i)
                end if
            end do
            x_out(m) = x(n)
            y1_out(m) = y1(n)
            y2_out(m) = y2(n)
            if (has_mask) where_out(m) = where_in(n)

        case ('post')
            m = 2*n - 1
            call allocate_step_buffers(m, has_mask, x_out, y1_out, y2_out, where_out)
            x_out(1) = x(1)
            y1_out(1) = y1(1)
            y2_out(1) = y2(1)
            if (has_mask) where_out(1) = where_in(1)
            do i = 2, n
                x_out(2*i - 2) = x(i)
                y1_out(2*i - 2) = y1(i - 1)
                y2_out(2*i - 2) = y2(i - 1)
                x_out(2*i - 1) = x(i)
                y1_out(2*i - 1) = y1(i)
                y2_out(2*i - 1) = y2(i)
                if (has_mask) then
                    where_out(2*i - 2) = where_in(i - 1)
                    where_out(2*i - 1) = where_in(i)
                end if
            end do

        case ('mid')
            m = 2*n
            call allocate_step_buffers(m, has_mask, x_out, y1_out, y2_out, where_out)
            do i = 1, n - 1
                x_out(2*i - 1) = x(i)
                y1_out(2*i - 1) = y1(i)
                y2_out(2*i - 1) = y2(i)
                x_out(2*i) = 0.5_wp*(x(i) + x(i + 1))
                y1_out(2*i) = y1(i)
                y2_out(2*i) = y2(i)
                if (has_mask) then
                    where_out(2*i - 1) = where_in(i)
                    where_out(2*i) = where_in(i)
                end if
            end do
            x_out(m - 1) = x(n)
            y1_out(m - 1) = y1(n - 1)
            y2_out(m - 1) = y2(n - 1)
            x_out(m) = x(n)
            y1_out(m) = y1(n)
            y2_out(m) = y2(n)
            if (has_mask) then
                where_out(m - 1) = where_in(n)
                where_out(m) = where_in(n)
            end if

        case default
            call log_warning('fill_between: unsupported step value ' // mode)
            allocate (x_out(n), y1_out(n), y2_out(n))
            x_out = x
            y1_out = y1
            y2_out = y2
            if (has_mask) allocate (where_out(n), source=where_in)
        end select
    end subroutine step_pair

    subroutine allocate_step_buffers(m, has_mask, x_out, y1_out, y2_out, where_out)
        integer, intent(in) :: m
        logical, intent(in) :: has_mask
        real(wp), allocatable, intent(out) :: x_out(:), y1_out(:), y2_out(:)
        logical, allocatable, intent(out) :: where_out(:)

        allocate (x_out(m), y1_out(m), y2_out(m))
        if (has_mask) allocate (where_out(m))
    end subroutine allocate_step_buffers

    pure function rgb_to_hex(color) result(hex)
        !! Serialise an RGB triple as a matplotlib-style `#RRGGBB` hex
        !! string so the underlying string-only implementations can consume
        !! it without additional overloads. Components are clipped to [0,1].
        real(wp), intent(in) :: color(3)
        character(len=7) :: hex

        integer :: rgb(3), i
        real(wp) :: clipped

        do i = 1, 3
            clipped = max(0.0_wp, min(1.0_wp, color(i)))
            rgb(i) = nint(clipped*255.0_wp)
        end do
        write (hex, '("#",3(Z2.2))') rgb(1), rgb(2), rgb(3)
    end function rgb_to_hex

    subroutine twinx()
        !! Activate a secondary y-axis that shares the x-axis but renders on the right
        call ensure_fig_init()
        call fig%twinx()
    end subroutine twinx

    subroutine twiny()
        !! Activate a secondary x-axis that shares the y-axis but renders on the top
        call ensure_fig_init()
        call fig%twiny()
    end subroutine twiny

end module fortplot_matplotlib_plots_new
