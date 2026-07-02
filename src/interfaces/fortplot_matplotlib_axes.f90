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
    public :: view_init
    public :: set_xscale
    public :: set_yscale
    public :: xscale
    public :: yscale
    public :: set_line_width
    public :: set_text_charset
    public :: set_text_color_mode
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
        !! Set the x-axis label text.
        !!
        !! Parameters
        !! label_text : character(len=*), intent(in)
        !!     X-axis label.
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
        !! Set the y-axis label text.
        !!
        !! Parameters
        !! label_text : character(len=*), intent(in)
        !!     Y-axis label.
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
        !! Set the title for the current axes.
        !!
        !! Parameters
        !! title_text : character(len=*), intent(in)
        !!     Title text.
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
        !! Set a centered figure-level title above all subplots.
        !!
        !! Parameters
        !! title_text : character(len=*), intent(in)
        !!     Figure title.
        !! fontsize : real(wp), optional
        !!     Font size for the title.
        character(len=*), intent(in) :: title_text
        real(wp), intent(in), optional :: fontsize

        call ensure_fig_init()
        call fig%suptitle(title_text, fontsize)
    end subroutine suptitle

    subroutine set_text_charset(charset)
        character(len=*), intent(in) :: charset
        call ensure_fig_init()
        call fig%set_text_charset(charset)
    end subroutine set_text_charset

    subroutine set_text_color_mode(mode)
        character(len=*), intent(in) :: mode
        call ensure_fig_init()
        call fig%set_text_color_mode(mode)
    end subroutine set_text_color_mode

    subroutine legend(loc, box, fontsize, position)
        !! Display the figure legend.
        !!
        !! Parameters
        !! loc : character(len=*), optional
        !!     Legend location string such as 'upper right' or 'best'.
        !! box : logical, optional
        !!     Accepted for matplotlib parity.
        !! fontsize : integer, optional
        !!     Accepted for matplotlib parity.
        !! position : character(len=*), optional
        !!     Deprecated alias for loc.
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
        !! Toggle or style grid lines.
        !!
        !! Parameters
        !! visible : logical, optional
        !!     Show the grid when .true.
        !! which : character(len=*), optional
        !!     Grid selection: 'major', 'minor', or 'both'.
        !! axis : character(len=*), optional
        !!     Axis selection: 'x', 'y', or 'both'.
        !! alpha : real(wp), optional
        !!     Grid line transparency.
        !! linestyle : character(len=*), optional
        !!     Grid line style.
        !! enabled : logical, optional
        !!     Deprecated alias for visible.
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
        !! Set the x-axis display limits.
        !!
        !! Parameters
        !! xmin : real(wp), intent(in)
        !!     Lower x bound.
        !! xmax : real(wp), intent(in)
        !!     Upper x bound.
        real(wp), intent(in) :: xmin, xmax
        call ensure_fig_init()
        call fig%set_xlim(xmin, xmax)
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        !! Set the y-axis display limits.
        !!
        !! Parameters
        !! ymin : real(wp), intent(in)
        !!     Lower y bound.
        !! ymax : real(wp), intent(in)
        !!     Upper y bound.
        real(wp), intent(in) :: ymin, ymax
        call ensure_fig_init()
        call fig%set_ylim(ymin, ymax)
    end subroutine ylim

    subroutine view_init(elev, azim, dist)
        !! Set the 3D view angles in degrees.
        !!
        !! Parameters
        !! elev : real(wp), optional
        !!     Elevation angle.
        !! azim : real(wp), optional
        !!     Azimuth angle.
        !! dist : real(wp), optional
        !!     Camera distance.
        real(wp), intent(in), optional :: elev, azim, dist
        call ensure_fig_init()
        call fig%set_view(elev=elev, azim=azim, dist=dist)
    end subroutine view_init

    subroutine set_xscale(scale, linthresh, threshold, base, linscale)
        !! Set the x-axis scale.
        !!
        !! Parameters
        !! scale : character(len=*), intent(in)
        !!     'linear', 'log', 'symlog', or 'logit'.
        !! linthresh : real(wp), optional
        !!     Symlog linear range threshold.
        !! threshold : real(wp), optional
        !!     Deprecated alias for linthresh.
        !! base : real(wp), optional
        !!     Symlog logarithm base.
        !! linscale : real(wp), optional
        !!     Symlog linear-region scaling factor.
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: linthresh, threshold, base, linscale
        real(wp) :: resolved_threshold
        logical :: has_threshold

        call ensure_fig_init()
        call resolve_scale_threshold(linthresh, threshold, resolved_threshold, &
                                     has_threshold)
        if (has_threshold) then
            call fig%set_xscale(scale, resolved_threshold, base=base, linscale=linscale)
        else
            call fig%set_xscale(scale, base=base, linscale=linscale)
        end if
    end subroutine set_xscale

    subroutine set_yscale(scale, linthresh, threshold, base, linscale)
        !! Set the y-axis scale.
        !!
        !! Parameters
        !! scale : character(len=*), intent(in)
        !!     'linear', 'log', 'symlog', or 'logit'.
        !! linthresh : real(wp), optional
        !!     Symlog linear range threshold.
        !! threshold : real(wp), optional
        !!     Deprecated alias for linthresh.
        !! base : real(wp), optional
        !!     Symlog logarithm base.
        !! linscale : real(wp), optional
        !!     Symlog linear-region scaling factor.
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: linthresh, threshold, base, linscale
        real(wp) :: resolved_threshold
        logical :: has_threshold

        call ensure_fig_init()
        call resolve_scale_threshold(linthresh, threshold, resolved_threshold, &
                                     has_threshold)
        if (has_threshold) then
            call fig%set_yscale(scale, resolved_threshold, base=base, linscale=linscale)
        else
            call fig%set_yscale(scale, base=base, linscale=linscale)
        end if
    end subroutine set_yscale

    subroutine xscale(scale, linthresh, threshold, base, linscale)
        !! Alias for set_xscale.
        !!
        !! Parameters
        !! scale : character(len=*), intent(in)
        !!     'linear', 'log', 'symlog', or 'logit'.
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: linthresh, threshold, base, linscale
        call set_xscale(scale, linthresh=linthresh, threshold=threshold, &
                        base=base, linscale=linscale)
    end subroutine xscale

    subroutine yscale(scale, linthresh, threshold, base, linscale)
        !! Alias for set_yscale.
        !!
        !! Parameters
        !! scale : character(len=*), intent(in)
        !!     'linear', 'log', 'symlog', or 'logit'.
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: linthresh, threshold, base, linscale
        call set_yscale(scale, linthresh=linthresh, threshold=threshold, &
                        base=base, linscale=linscale)
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
        !! Set the default line width for subsequent plots.
        !!
        !! Parameters
        !! width : real(wp), intent(in)
        !!     Line width.
        real(wp), intent(in) :: width
        call ensure_fig_init()
        call fig%set_line_width(width)
    end subroutine set_line_width

    subroutine set_ydata(ydata)
        !! Replace the y-data of the first plot line.
        !!
        !! Parameters
        !! ydata : real(wp), contiguous, intent(in)
        !!     New y coordinates.
        real(wp), contiguous, intent(in) :: ydata(:)
        call ensure_fig_init()
        call fig%set_ydata(1, ydata)
    end subroutine set_ydata

    subroutine use_axis(axis_name)
        !! Switch the active axes by name.
        !!
        !! Parameters
        !! axis_name : character(len=*), intent(in)
        !!     Target axes name.
        character(len=*), intent(in) :: axis_name
        call ensure_fig_init()
        call fig%use_axis(axis_name)
    end subroutine use_axis

    function get_active_axis() result(axis_name)
        !! Return the name of the currently active axes.
        !!
        !! Returns
        !! axis_name : character(len=10)
        !!     Active axes name.
        character(len=10) :: axis_name
        call ensure_fig_init()
        axis_name = fig%get_active_axis()
    end function get_active_axis

    subroutine minorticks_on()
        !! Enable minor ticks on both axes.
        call ensure_fig_init()
        call fig%minorticks_on()
    end subroutine minorticks_on

    subroutine axis_str(aspect)
        !! Set axis aspect ratio using string mode.
        !!
        !! Parameters
        !! aspect : character(len=*), intent(in)
        !!     'equal' or 'auto'.
        character(len=*), intent(in) :: aspect
        call ensure_fig_init()
        call fig%set_aspect(aspect)
    end subroutine axis_str

    subroutine axis_num(ratio)
        !! Set axis aspect ratio using a numeric value.
        !!
        !! Parameters
        !! ratio : real(wp), intent(in)
        !!     y-scale = ratio * x-scale.
        real(wp), intent(in) :: ratio
        call ensure_fig_init()
        call fig%set_aspect(ratio)
    end subroutine axis_num

    subroutine tight_layout(pad, w_pad, h_pad)
        !! Automatically adjust subplot parameters to give specified padding.
        !!
        !! Parameters
        !! pad : real(wp), optional
        !!     Padding between figure edge and subplot edge.
        !! w_pad : real(wp), optional
        !!     Padding between subplots in width.
        !! h_pad : real(wp), optional
        !!     Padding between subplots in height.
        real(wp), intent(in), optional :: pad
        real(wp), intent(in), optional :: w_pad
        real(wp), intent(in), optional :: h_pad

        call ensure_fig_init()
        call fig%tight_layout(pad, w_pad, h_pad)
    end subroutine tight_layout

    subroutine axhline(y, xmin, xmax, color, linestyle, linewidth, label)
        !! Draw a horizontal reference line.
        !!
        !! Parameters
        !! y : real(wp), intent(in)
        !!     Data value for the line.
        real(wp), intent(in) :: y
        real(wp), intent(in), optional :: xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%axhline(y, xmin=xmin, xmax=xmax, color=color, &
                         linestyle=linestyle, linewidth=linewidth, label=label)
    end subroutine axhline

    subroutine axvline(x, ymin, ymax, color, linestyle, linewidth, label)
        !! Draw a vertical reference line.
        !!
        !! Parameters
        !! x : real(wp), intent(in)
        !!     Data value for the line.
        real(wp), intent(in) :: x
        real(wp), intent(in), optional :: ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%axvline(x, ymin=ymin, ymax=ymax, color=color, &
                         linestyle=linestyle, linewidth=linewidth, label=label)
    end subroutine axvline

    subroutine hlines(y, xmin, xmax, colors, linestyles, linewidth, label)
        !! Draw one or more horizontal lines.
        !!
        !! Parameters
        !! y : real(wp), contiguous, intent(in)
        !!     Line positions.
        !! xmin : real(wp), intent(in)
        !!     Line start value.
        !! xmax : real(wp), intent(in)
        !!     Line end value.
        real(wp), contiguous, intent(in) :: y(:)
        real(wp), intent(in) :: xmin, xmax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%hlines(y, xmin=xmin, xmax=xmax, colors=colors, &
                        linestyles=linestyles, linewidth=linewidth, label=label)
    end subroutine hlines

    subroutine vlines(x, ymin, ymax, colors, linestyles, linewidth, label)
        !! Draw one or more vertical lines.
        !!
        !! Parameters
        !! x : real(wp), contiguous, intent(in)
        !!     Line positions.
        !! ymin : real(wp), intent(in)
        !!     Line start value.
        !! ymax : real(wp), intent(in)
        !!     Line end value.
        real(wp), contiguous, intent(in) :: x(:)
        real(wp), intent(in) :: ymin, ymax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%vlines(x, ymin=ymin, ymax=ymax, colors=colors, &
                        linestyles=linestyles, linewidth=linewidth, label=label)
    end subroutine vlines

    subroutine set_xticks(positions, labels)
        !! Set custom x-axis tick positions and optional labels.
        !!
        !! Parameters
        !! positions : real(wp), contiguous, intent(in)
        !!     Tick locations.
        !! labels : character(len=*), optional
        !!     Tick labels.
        real(wp), contiguous, intent(in) :: positions(:)
        character(len=*), intent(in), optional :: labels(:)

        call ensure_fig_init()
        if (present(labels)) then
            call fig%set_xticks(positions, labels)
        else
            call fig%set_xticks(positions)
        end if
    end subroutine set_xticks

    subroutine set_yticks(positions, labels)
        !! Set custom y-axis tick positions and optional labels.
        !!
        !! Parameters
        !! positions : real(wp), contiguous, intent(in)
        !!     Tick locations.
        !! labels : character(len=*), optional
        !!     Tick labels.
        real(wp), contiguous, intent(in) :: positions(:)
        character(len=*), intent(in), optional :: labels(:)

        call ensure_fig_init()
        if (present(labels)) then
            call fig%set_yticks(positions, labels)
        else
            call fig%set_yticks(positions)
        end if
    end subroutine set_yticks

end module fortplot_matplotlib_axes
