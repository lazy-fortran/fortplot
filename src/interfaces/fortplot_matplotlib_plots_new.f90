module fortplot_matplotlib_plots_new
    !! New matplotlib-compatible plot functions added for issue #1178
    !! Includes: imshow, pie, polar, step, stem, fill, fill_between, twinx, twiny

    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_global, only: fig => global_figure
    use fortplot_matplotlib_session, only: ensure_fig_init

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

    subroutine polar(theta, r, fmt, label, linestyle, marker, color)
        !! Create a polar plot
        real(wp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: fmt, label
        character(len=*), intent(in), optional :: linestyle, marker, color

        call ensure_fig_init()
        call fig%add_polar(theta, r, label=label, fmt=fmt, linestyle=linestyle, &
                           marker=marker, color=color)
    end subroutine polar

    subroutine step(x, y, where, label, linestyle, color, linewidth)
        !! Create a step plot
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: where, label
        character(len=*), intent(in), optional :: linestyle, color
        real(wp), intent(in), optional :: linewidth

        call ensure_fig_init()
        call fig%add_step(x, y, label=label, where=where, linestyle=linestyle, &
                          color=color, linewidth=linewidth)
    end subroutine step

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

    subroutine fill(x, y, color, alpha)
        !! Fill the area under a curve
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha

        call ensure_fig_init()
        call fig%add_fill(x, y, color=color, alpha=alpha)
    end subroutine fill

    subroutine fill_between(x, y1, y2, where, color, alpha, interpolate)
        !! Fill the area between two curves
        real(wp), intent(in) :: x(:)
        real(wp), intent(in), optional :: y1(:), y2(:)
        logical, intent(in), optional :: where(:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha
        logical, intent(in), optional :: interpolate

        call ensure_fig_init()
        call fig%add_fill_between(x, y1=y1, y2=y2, color=color, alpha=alpha, &
                                  where=where, interpolate=interpolate)
    end subroutine fill_between

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
