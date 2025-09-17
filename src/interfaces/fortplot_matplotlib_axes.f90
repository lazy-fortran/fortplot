module fortplot_matplotlib_axes
    !! Axes, scale, and labelling helpers for the matplotlib facade

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_session, only: ensure_fig_init

    implicit none
    private

    public :: xlabel
    public :: ylabel
    public :: title
    public :: legend
    public :: grid
    public :: xlim
    public :: ylim
    public :: set_xscale
    public :: set_yscale
    public :: set_line_width
    public :: set_ydata

contains

    subroutine xlabel(label_text)
        character(len=*), intent(in) :: label_text
        call ensure_fig_init()
        call fig%set_xlabel(label_text)
    end subroutine xlabel

    subroutine ylabel(label_text)
        character(len=*), intent(in) :: label_text
        call ensure_fig_init()
        call fig%set_ylabel(label_text)
    end subroutine ylabel

    subroutine title(title_text)
        character(len=*), intent(in) :: title_text
        call ensure_fig_init()
        call fig%set_title(title_text)
    end subroutine title

    subroutine legend(position, box, fontsize)
        character(len=*), intent(in), optional :: position
        logical, intent(in), optional :: box
        integer, intent(in), optional :: fontsize

        call ensure_fig_init()

        if (present(position)) then
            call fig%legend(location=position)
        else
            call fig%legend()
        end if
        if (present(box)) call log_error("legend: legend box option not implemented")
        if (present(fontsize)) call log_error("legend: fontsize option not implemented")
    end subroutine legend

    subroutine grid(enabled, which, axis, alpha, linestyle)
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha

        call ensure_fig_init()
        call fig%grid(enabled=enabled, which=which, axis=axis, alpha=alpha, &
                     linestyle=linestyle)
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

    subroutine set_xscale(scale, threshold)
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call ensure_fig_init()
        call fig%set_xscale(scale, threshold)
    end subroutine set_xscale

    subroutine set_yscale(scale, threshold)
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call ensure_fig_init()
        call fig%set_yscale(scale, threshold)
    end subroutine set_yscale

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

end module fortplot_matplotlib_axes
