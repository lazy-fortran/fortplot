module fortplot_matplotlib_axes
    !! Axes, scale, and labelling helpers for the matplotlib facade

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_warning
    use fortplot_matplotlib_session, only: ensure_fig_init

    implicit none
    private

    public :: xlabel
    public :: ylabel
    public :: title
    public :: suptitle
    public :: legend
    public :: grid
    public :: xlim
    public :: ylim
    public :: set_xscale
    public :: set_yscale
    public :: xscale
    public :: yscale
    public :: set_line_width
    public :: set_ydata
    public :: use_axis
    public :: get_active_axis
    public :: minorticks_on
    public :: axis
    public :: tight_layout
    public :: axhline
    public :: axvline
    public :: hlines
    public :: vlines
    public :: set_xticks
    public :: set_yticks

    interface axis
        !! Set aspect ratio: axis('equal'), axis('auto'), or axis(2.0)
        module procedure axis_str
        module procedure axis_num
    end interface axis

contains

    subroutine xlabel(label_text)
        character(len=*), intent(in) :: label_text
        integer :: idx, nrows, ncols, row, col
        call ensure_fig_init()
        nrows = fig%subplot_rows
        ncols = fig%subplot_cols
        idx = fig%current_subplot
        if (nrows > 0 .and. ncols > 0 .and. idx >= 1 .and. idx <= nrows*ncols) then
            row = (idx - 1)/ncols + 1
            col = mod(idx - 1, ncols) + 1
            call fig%subplot_set_xlabel(row, col, label_text)
        else
            call fig%set_xlabel(label_text)
        end if
    end subroutine xlabel

    subroutine ylabel(label_text)
        character(len=*), intent(in) :: label_text
        integer :: idx, nrows, ncols, row, col
        call ensure_fig_init()
        nrows = fig%subplot_rows
        ncols = fig%subplot_cols
        idx = fig%current_subplot
        if (nrows > 0 .and. ncols > 0 .and. idx >= 1 .and. idx <= nrows*ncols) then
            row = (idx - 1)/ncols + 1
            col = mod(idx - 1, ncols) + 1
            call fig%subplot_set_ylabel(row, col, label_text)
        else
            call fig%set_ylabel(label_text)
        end if
    end subroutine ylabel

    subroutine title(title_text)
        character(len=*), intent(in) :: title_text
        integer :: idx, nrows, ncols, row, col
        call ensure_fig_init()
        nrows = fig%subplot_rows
        ncols = fig%subplot_cols
        idx = fig%current_subplot
        if (nrows > 0 .and. ncols > 0 .and. idx >= 1 .and. idx <= nrows*ncols) then
            row = (idx - 1)/ncols + 1
            col = mod(idx - 1, ncols) + 1
            call fig%subplot_set_title(row, col, title_text)
        else
            call fig%set_title(title_text)
        end if
    end subroutine title

    subroutine suptitle(title_text, fontsize)
        !! Set a centered figure-level title above all subplots
        character(len=*), intent(in) :: title_text
        real(wp), intent(in), optional :: fontsize

        call ensure_fig_init()
        call fig%suptitle(title_text, fontsize)
    end subroutine suptitle

    subroutine legend(loc, box, fontsize, position)
        !! Display figure legend (matplotlib-compatible)
        !!
        !! Arguments mirror matplotlib.pyplot.legend:
        !!   loc      - legend location string (e.g. 'upper right', 'best')
        !!   box      - accepted for compatibility; styling not yet implemented
        !!   fontsize - accepted for compatibility; styling not yet implemented
        !!   position - deprecated alias for loc, kept for backward compatibility
        character(len=*), intent(in), optional :: loc
        logical, intent(in), optional :: box
        integer, intent(in), optional :: fontsize
        character(len=*), intent(in), optional :: position

        call ensure_fig_init()

        if (present(position) .and. .not. present(loc)) then
            call log_warning( &
                "legend: 'position' is deprecated; use 'loc' for matplotlib parity")
            call fig%legend(location=position)
        else if (present(loc)) then
            call fig%legend(location=loc)
        else
            call fig%legend()
        end if

        ! box and fontsize are accepted silently for matplotlib parity;
        ! styling passthrough is tracked by follow-up work and is not
        ! reported as an error on every call.
        call ignore_unused_legend_kwargs(box, fontsize)
    end subroutine legend

    subroutine ignore_unused_legend_kwargs(box, fontsize)
        !! Explicit no-op to document silent acceptance of matplotlib kwargs
        logical, intent(in), optional :: box
        integer, intent(in), optional :: fontsize

        if (present(box) .or. present(fontsize)) return
    end subroutine ignore_unused_legend_kwargs

    subroutine grid(visible, which, axis, alpha, linestyle, enabled)
        !! Toggle or style grid lines (matplotlib-compatible)
        !!
        !! Arguments:
        !!   visible   - show grid when .true.; canonical matplotlib name
        !!   which     - 'major', 'minor', or 'both'
        !!   axis      - 'x', 'y', or 'both'
        !!   alpha     - grid line transparency
        !!   linestyle - grid line style ('-', '--', ':', '-.')
        !!   enabled   - deprecated alias for visible, kept for backward compatibility
        !!
        !! Default grid state follows the active style: MPL mode disables
        !! grid by default; Vega-Lite mode enables it.
        logical, intent(in), optional :: visible
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        logical, intent(in), optional :: enabled

        logical :: effective_visible
        logical :: has_visible

        has_visible = present(visible) .or. present(enabled)
        effective_visible = .true.

        if (present(visible)) then
            effective_visible = visible
        else if (present(enabled)) then
            call log_warning( &
                "grid: 'enabled' is deprecated; use 'visible' for matplotlib parity")
            effective_visible = enabled
        end if

        call ensure_fig_init()
        if (has_visible) then
            call fig%grid(enabled=effective_visible, which=which, axis=axis, &
                          alpha=alpha, linestyle=linestyle)
        else
            call fig%grid(which=which, axis=axis, alpha=alpha, linestyle=linestyle)
        end if
    end subroutine grid

    subroutine xlim(xmin, xmax)
        real(wp), intent(in) :: xmin, xmax
        call ensure_fig_init()
        call fig%set_xlim(xmin, xmax)
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        real(wp), intent(in) :: ymin, ymax
        call ensure_fig_init()
        call fig%set_ylim(ymin, ymax)
    end subroutine ylim

    subroutine set_xscale(scale, linthresh, threshold)
        !! Set x-axis scale (matplotlib-compatible)
        !!
        !! Arguments:
        !!   scale     - 'linear', 'log', 'symlog', 'logit'
        !!   linthresh - symlog linear range threshold (matplotlib canonical)
        !!   threshold - deprecated alias for linthresh, kept for compatibility
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: linthresh
        real(wp), intent(in), optional :: threshold
        real(wp) :: resolved_threshold
        logical :: has_threshold

        call ensure_fig_init()
        call resolve_scale_threshold(linthresh, threshold, resolved_threshold, &
                                     has_threshold)
        if (has_threshold) then
            call fig%set_xscale(scale, resolved_threshold)
        else
            call fig%set_xscale(scale)
        end if
    end subroutine set_xscale

    subroutine set_yscale(scale, linthresh, threshold)
        !! Set y-axis scale (matplotlib-compatible); see set_xscale for kwargs
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: linthresh
        real(wp), intent(in), optional :: threshold
        real(wp) :: resolved_threshold
        logical :: has_threshold

        call ensure_fig_init()
        call resolve_scale_threshold(linthresh, threshold, resolved_threshold, &
                                     has_threshold)
        if (has_threshold) then
            call fig%set_yscale(scale, resolved_threshold)
        else
            call fig%set_yscale(scale)
        end if
    end subroutine set_yscale

    subroutine xscale(scale, linthresh, threshold)
        !! matplotlib pyplot alias for set_xscale
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: linthresh, threshold
        call set_xscale(scale, linthresh=linthresh, threshold=threshold)
    end subroutine xscale

    subroutine yscale(scale, linthresh, threshold)
        !! matplotlib pyplot alias for set_yscale
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: linthresh, threshold
        call set_yscale(scale, linthresh=linthresh, threshold=threshold)
    end subroutine yscale

    subroutine resolve_scale_threshold(linthresh, threshold, value, present_out)
        real(wp), intent(in), optional :: linthresh, threshold
        real(wp), intent(out) :: value
        logical, intent(out) :: present_out

        value = 1.0_wp
        present_out = .false.
        if (present(linthresh)) then
            value = linthresh
            present_out = .true.
        else if (present(threshold)) then
            call log_warning( &
                "set_xscale/set_yscale: 'threshold' is deprecated; use 'linthresh'")
            value = threshold
            present_out = .true.
        end if
    end subroutine resolve_scale_threshold

    subroutine set_line_width(width)
        real(wp), intent(in) :: width
        call ensure_fig_init()
        call fig%set_line_width(width)
    end subroutine set_line_width

    subroutine set_ydata(ydata)
        real(wp), intent(in) :: ydata(:)
        call ensure_fig_init()
        call fig%set_ydata(1, ydata)
    end subroutine set_ydata

    subroutine use_axis(axis_name)
        character(len=*), intent(in) :: axis_name
        call ensure_fig_init()
        call fig%use_axis(axis_name)
    end subroutine use_axis

    function get_active_axis() result(axis_name)
        character(len=10) :: axis_name
        call ensure_fig_init()
        axis_name = fig%get_active_axis()
    end function get_active_axis

    subroutine minorticks_on()
        !! Enable minor ticks on both axes (matplotlib-compatible)
        call ensure_fig_init()
        call fig%minorticks_on()
    end subroutine minorticks_on

    subroutine axis_str(aspect)
        !! Set axis aspect ratio using string mode: equal or auto
        character(len=*), intent(in) :: aspect
        call ensure_fig_init()
        call fig%set_aspect(aspect)
    end subroutine axis_str

    subroutine axis_num(ratio)
        !! Set axis aspect ratio using numeric value (y-scale = ratio * x-scale)
        real(wp), intent(in) :: ratio
        call ensure_fig_init()
        call fig%set_aspect(ratio)
    end subroutine axis_num

    subroutine tight_layout(pad, w_pad, h_pad)
        !! Automatically adjust subplot parameters to give specified padding
        !!
        !! Enables tight layout mode which optimizes the figure layout to
        !! minimize overlap between subplots, titles, and axis labels.
        real(wp), intent(in), optional :: pad
        real(wp), intent(in), optional :: w_pad
        real(wp), intent(in), optional :: h_pad

        call ensure_fig_init()
        call fig%tight_layout(pad, w_pad, h_pad)
    end subroutine tight_layout

    subroutine axhline(y, xmin, xmax, color, linestyle, linewidth, label)
        !! Draw a horizontal reference line at data value y (matplotlib-compatible)
        real(wp), intent(in) :: y
        real(wp), intent(in), optional :: xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%axhline(y, xmin=xmin, xmax=xmax, color=color, &
                         linestyle=linestyle, linewidth=linewidth, label=label)
    end subroutine axhline

    subroutine axvline(x, ymin, ymax, color, linestyle, linewidth, label)
        !! Draw a vertical reference line at data value x (matplotlib-compatible)
        real(wp), intent(in) :: x
        real(wp), intent(in), optional :: ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%axvline(x, ymin=ymin, ymax=ymax, color=color, &
                         linestyle=linestyle, linewidth=linewidth, label=label)
    end subroutine axvline

    subroutine hlines(y, xmin, xmax, colors, linestyles, linewidth, label)
        !! Draw one or more horizontal lines at y values between xmin and xmax
        real(wp), intent(in) :: y(:)
        real(wp), intent(in) :: xmin, xmax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%hlines(y, xmin=xmin, xmax=xmax, colors=colors, &
                        linestyles=linestyles, linewidth=linewidth, label=label)
    end subroutine hlines

    subroutine vlines(x, ymin, ymax, colors, linestyles, linewidth, label)
        !! Draw one or more vertical lines at x values between ymin and ymax
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: ymin, ymax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%vlines(x, ymin=ymin, ymax=ymax, colors=colors, &
                        linestyles=linestyles, linewidth=linewidth, label=label)
    end subroutine vlines

    subroutine set_xticks(positions, labels)
        !! Set custom x-axis tick positions and optionally labels
        real(wp), intent(in) :: positions(:)
        character(len=*), intent(in), optional :: labels(:)

        call ensure_fig_init()
        if (present(labels)) then
            call fig%set_xticks(positions, labels)
        else
            call fig%set_xticks(positions)
        end if
    end subroutine set_xticks

    subroutine set_yticks(positions, labels)
        !! Set custom y-axis tick positions and optionally labels
        real(wp), intent(in) :: positions(:)
        character(len=*), intent(in), optional :: labels(:)

        call ensure_fig_init()
        if (present(labels)) then
            call fig%set_yticks(positions, labels)
        else
            call fig%set_yticks(positions)
        end if
    end subroutine set_yticks

end module fortplot_matplotlib_axes
