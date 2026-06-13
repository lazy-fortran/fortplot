program test_errorbar_legend_entry
    !! Regression: errorbar() + legend() must register a legend entry.
    !! errorbar_impl increments the figure plot count but historically did not
    !! mirror it into the figure state, so legend setup (which reads the state
    !! plot count) saw zero plots and drew no legend box for errorbar figures.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_errorbar_plots, only: errorbar
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(5), y(5), yerr(5)
    integer :: i, failures

    failures = 0
    do i = 1, 5
        x(i) = real(i, wp)
        y(i) = sin(real(i, wp) * 0.5_wp)
        yerr(i) = 0.2_wp
    end do

    call fig%initialize(640, 480)
    call errorbar(fig, x, y, yerr=yerr, label='measured')
    call fig%legend()

    if (fig%state%legend_data%num_entries == 1) then
        print *, "PASS: errorbar legend entry registered"
    else
        print *, "FAIL: expected 1 legend entry, got", &
            fig%state%legend_data%num_entries
        failures = failures + 1
    end if

    if (failures == 0) then
        print *, "ALL ERRORBAR LEGEND TESTS PASSED"
    else
        stop 1
    end if

end program test_errorbar_legend_entry
