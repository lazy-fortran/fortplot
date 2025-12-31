module fortplot_matplotlib_colorbar
    !! Matplotlib-style stateful colorbar wrapper.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_matplotlib_session, only: ensure_fig_init
    implicit none

    private
    public :: colorbar

contains

    subroutine colorbar(label, location, fraction, pad, shrink, plot_index, &
                        ticks, ticklabels, label_fontsize)
        character(len=*), intent(in), optional :: label, location
        real(wp), intent(in), optional :: fraction, pad, shrink
        integer, intent(in), optional :: plot_index
        real(wp), intent(in), optional :: ticks(:)
        character(len=*), intent(in), optional :: ticklabels(:)
        real(wp), intent(in), optional :: label_fontsize

        call ensure_fig_init()

        call fig%colorbar(plot_index=plot_index, label=label, location=location, &
                          fraction=fraction, pad=pad, shrink=shrink, ticks=ticks, &
                          ticklabels=ticklabels, label_fontsize=label_fontsize)
    end subroutine colorbar

end module fortplot_matplotlib_colorbar

