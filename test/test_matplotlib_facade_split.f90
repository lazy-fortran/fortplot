program test_matplotlib_facade_split
    !! Regression tests for the slimmed matplotlib advanced facade
    !! Ensures delegation keeps default styling aligned with plots_new helpers

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_matplotlib, only: figure, plot, bar, fill
    use fortplot_matplotlib_plots_new, only: fill_new => fill

    implicit none

    real(wp), dimension(2) :: xs = [0.0_wp, 1.0_wp]
    real(wp), dimension(2) :: ys = [0.0_wp, 0.5_wp]
    real(wp), dimension(3) :: color_adv, color_new
    logical :: mismatch

    call figure(figsize=[2.0_wp, 2.0_wp])
    call fill(xs, ys)

    if (.not. allocated(fig)) then
        print *, 'FAIL: global figure not initialised'
        stop 1
    end if

    if (.not. allocated(fig%plots)) then
        print *, 'FAIL: fill via facade did not allocate plot storage'
        stop 1
    end if

    color_adv = fig%plots(fig%plot_count)%color

    call figure(figsize=[2.0_wp, 2.0_wp])
    call fill_new(xs, ys)

    if (.not. allocated(fig%plots)) then
        print *, 'FAIL: fill via plots_new did not allocate plot storage'
        stop 1
    end if

    color_new = fig%plots(fig%plot_count)%color

    mismatch = any(abs(color_adv - color_new) > 1.0e-12_wp)
    if (mismatch) then
        print *, 'FAIL: default colors diverge between advanced and plots_new'
        stop 1
    end if

    call figure(figsize=[3.0_wp, 2.0_wp])
    call plot(xs, ys)
    call bar(xs, [1.0_wp, 2.0_wp])

    print *, 'Matplotlib facade split regression tests passed.'
end program test_matplotlib_facade_split
