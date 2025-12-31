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
    public :: suptitle
    public :: legend
    public :: grid
    public :: xlim
    public :: ylim
    public :: set_xscale
    public :: set_yscale
    public :: set_line_width
    public :: set_ydata
    public :: use_axis
    public :: get_active_axis
    public :: minorticks_on
    public :: axis

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
            row = (idx - 1) / ncols + 1
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
            row = (idx - 1) / ncols + 1
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
            row = (idx - 1) / ncols + 1
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

end module fortplot_matplotlib_axes
