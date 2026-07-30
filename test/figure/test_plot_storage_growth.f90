program test_plot_storage_growth
    use fortplot, only: figure_t, wp
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(2), y(2)
    integer :: artist, old_limit

    x = [0.0_wp, 1.0_wp]
    call fig%initialize()
    old_limit = fig%state%max_plots

    do artist = 1, old_limit + 1
        y = real(artist, wp)
        call fig%add_plot(x, y)
    end do

    if (fig%plot_count /= old_limit + 1) then
        error stop "valid line artists must not be silently truncated"
    end if
    if (size(fig%plots) < fig%plot_count) then
        error stop "plot storage must grow with the artist count"
    end if
    if (any(fig%plots(fig%plot_count)%y /= real(old_limit + 1, wp))) then
        error stop "the first artist beyond the old limit must be retained"
    end if
end program test_plot_storage_growth
