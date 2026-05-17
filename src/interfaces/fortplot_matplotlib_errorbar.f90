module fortplot_matplotlib_errorbar
    !! Errorbar plot wrappers (matplotlib-compatible).
    !!
    !! Extracted from fortplot_matplotlib_plot_wrappers to respect module
    !! size limits. Re-exported by that module for backward compatibility.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_errorbar_plots, only: errorbar_impl => errorbar
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_warning
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb
    use fortplot_matplotlib_session, only: ensure_fig_init

    implicit none
    private

    public :: errorbar
    public :: add_errorbar

    interface errorbar
        module procedure errorbar_rgb
        module procedure errorbar_string
    end interface errorbar

    interface add_errorbar
        module procedure add_errorbar_rgb
        module procedure add_errorbar_string
    end interface add_errorbar

contains

    subroutine errorbar_rgb(x, y, xerr, yerr, fmt, label, capsize, linestyle, &
                            marker, color, ecolor, elinewidth, capthick, &
                            barsabove, errorevery, lolims, uplims, xlolims, &
                            xuplims)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: ecolor(3)
        real(wp), intent(in), optional :: elinewidth, capthick
        logical, intent(in), optional :: barsabove
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: lolims, uplims, xlolims, xuplims

        real(wp), allocatable :: xerr_use(:), yerr_use(:)

        call ensure_fig_init()
        call build_errorbar_arrays(x, y, xerr, yerr, errorevery, xlolims, &
                                   xuplims, lolims, uplims, xerr_use, yerr_use)

        call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, label=label, &
                           capsize=capsize, marker=marker, color=color, &
                           ecolor=ecolor, elinewidth=elinewidth, capthick=capthick)
        call note_unsupported_barsabove(barsabove)
    end subroutine errorbar_rgb

    subroutine errorbar_string(x, y, color, xerr, yerr, fmt, label, capsize, &
                                linestyle, marker, ecolor, elinewidth, capthick, &
                                barsabove, errorevery, lolims, uplims, xlolims, &
                                xuplims)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        character(len=*), intent(in), optional :: ecolor
        real(wp), intent(in), optional :: elinewidth, capthick
        logical, intent(in), optional :: barsabove
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: lolims, uplims, xlolims, xuplims

        real(wp) :: color_rgb(3), ecolor_rgb(3)
        logical :: has_color, has_ecolor
        real(wp), allocatable :: xerr_use(:), yerr_use(:)

        call ensure_fig_init()
        call resolve_color_string_or_rgb(color_str=color, context='errorbar', &
                                         rgb_out=color_rgb, has_color=has_color)
        call resolve_color_string_or_rgb(color_str=ecolor, context='errorbar', &
                                         rgb_out=ecolor_rgb, has_color=has_ecolor)
        call build_errorbar_arrays(x, y, xerr, yerr, errorevery, xlolims, &
                                   xuplims, lolims, uplims, xerr_use, yerr_use)

        if (has_color .and. has_ecolor) then
            call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, &
                               label=label, capsize=capsize, marker=marker, &
                               color=color_rgb, ecolor=ecolor_rgb, &
                               elinewidth=elinewidth, capthick=capthick)
        else if (has_color) then
            call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, &
                               label=label, capsize=capsize, marker=marker, &
                               color=color_rgb, elinewidth=elinewidth, &
                               capthick=capthick)
        else if (has_ecolor) then
            call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, &
                               label=label, capsize=capsize, marker=marker, &
                               ecolor=ecolor_rgb, elinewidth=elinewidth, &
                               capthick=capthick)
        else
            call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, &
                               label=label, capsize=capsize, marker=marker, &
                               elinewidth=elinewidth, capthick=capthick)
        end if
        call note_unsupported_barsabove(barsabove)
    end subroutine errorbar_string

    subroutine add_errorbar_rgb(x, y, xerr, yerr, fmt, label, capsize, &
                                 linestyle, marker, color, ecolor, elinewidth, &
                                 capthick, barsabove, errorevery, lolims, uplims, &
                                 xlolims, xuplims)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        real(wp), intent(in), optional :: color(3), ecolor(3)
        real(wp), intent(in), optional :: elinewidth, capthick
        logical, intent(in), optional :: barsabove
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: lolims, uplims, xlolims, xuplims

        call errorbar_rgb(x, y, xerr=xerr, yerr=yerr, fmt=fmt, label=label, &
                          capsize=capsize, linestyle=linestyle, marker=marker, &
                          color=color, ecolor=ecolor, elinewidth=elinewidth, &
                          capthick=capthick, barsabove=barsabove, &
                          errorevery=errorevery, lolims=lolims, uplims=uplims, &
                          xlolims=xlolims, xuplims=xuplims)
    end subroutine add_errorbar_rgb

    subroutine add_errorbar_string(x, y, color, xerr, yerr, fmt, label, capsize, &
                                    linestyle, marker, ecolor, elinewidth, capthick, &
                                    barsabove, errorevery, lolims, uplims, xlolims, &
                                    xuplims)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        character(len=*), intent(in), optional :: ecolor
        real(wp), intent(in), optional :: elinewidth, capthick
        logical, intent(in), optional :: barsabove
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: lolims, uplims, xlolims, xuplims

        call errorbar_string(x, y, color=color, xerr=xerr, yerr=yerr, fmt=fmt, &
                             label=label, capsize=capsize, linestyle=linestyle, &
                             marker=marker, ecolor=ecolor, elinewidth=elinewidth, &
                             capthick=capthick, barsabove=barsabove, &
                             errorevery=errorevery, lolims=lolims, uplims=uplims, &
                             xlolims=xlolims, xuplims=xuplims)
    end subroutine add_errorbar_string

    subroutine build_errorbar_arrays(x, y, xerr, yerr, errorevery, xlolims, &
                                     xuplims, lolims, uplims, xerr_out, yerr_out)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: xlolims, xuplims, lolims, uplims
        real(wp), allocatable, intent(out) :: xerr_out(:), yerr_out(:)

        integer :: i, n, stride
        logical :: hide_x, hide_y

        n = size(x)
        stride = 1
        if (present(errorevery)) stride = max(errorevery, 1)

        hide_x = limit_flag(xlolims) .or. limit_flag(xuplims)
        hide_y = limit_flag(lolims) .or. limit_flag(uplims)

        if (present(xerr) .and. .not. hide_x) then
            allocate (xerr_out(size(xerr)))
            xerr_out = xerr
            if (stride > 1 .and. size(xerr_out) == n) then
                do i = 1, n
                    if (mod(i - 1, stride) /= 0) xerr_out(i) = 0.0_wp
                end do
            end if
        end if

        if (present(yerr) .and. .not. hide_y) then
            allocate (yerr_out(size(yerr)))
            yerr_out = yerr
            if (stride > 1 .and. size(yerr_out) == n) then
                do i = 1, n
                    if (mod(i - 1, stride) /= 0) yerr_out(i) = 0.0_wp
                end do
            end if
        end if
    end subroutine build_errorbar_arrays

    pure function limit_flag(flag) result(is_set)
        logical, intent(in), optional :: flag
        logical :: is_set
        is_set = .false.
        if (present(flag)) is_set = flag
    end function limit_flag

    subroutine note_unsupported_barsabove(barsabove)
        logical, intent(in), optional :: barsabove
        if (present(barsabove)) then
            if (barsabove) then
                call log_warning( &
                    'errorbar: barsabove=.true. is not supported; error bars always drawn below data')
            end if
        end if
    end subroutine note_unsupported_barsabove

end module fortplot_matplotlib_errorbar
