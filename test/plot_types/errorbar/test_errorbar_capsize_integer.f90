program test_errorbar_capsize_integer
    !! Regression test for issue #2020: matplotlib-style errorbar(..., capsize=4)
    !! with an integer capsize must compile and store the same cap length as the
    !! real-literal form. capsize was real-only, so the integer literal matched
    !! no specific procedure of the errorbar generic and failed to compile.
    use fortplot, only: wp, figure_t, figure, errorbar, get_global_figure
    use fortplot_plot_data, only: plot_data_t
    implicit none

    real(wp), dimension(5) :: x, y, yerr
    type(figure_t), pointer :: f
    type(plot_data_t), pointer :: plots(:)
    real(wp) :: cap_from_int, cap_from_real

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 2.5_wp, 3.0_wp, 3.5_wp, 4.0_wp]
    yerr = [0.2_wp, 0.3_wp, 0.25_wp, 0.35_wp, 0.15_wp]

    ! Integer capsize (the matplotlib idiom the user reported).
    call figure()
    call errorbar(x, y, yerr=yerr, capsize=4)
    f => get_global_figure()
    plots => f%get_plots()
    cap_from_int = plots(1)%capsize

    ! Real capsize for the same value, as a reference.
    call figure()
    call errorbar(x, y, yerr=yerr, capsize=4.0_wp)
    f => get_global_figure()
    plots => f%get_plots()
    cap_from_real = plots(1)%capsize

    if (abs(cap_from_int - 4.0_wp) > 1.0e-12_wp) then
        print *, 'FAIL: integer capsize=4 stored as', cap_from_int
        stop 1
    end if

    if (abs(cap_from_int - cap_from_real) > 1.0e-12_wp) then
        print *, 'FAIL: integer capsize differs from real capsize:', &
            cap_from_int, cap_from_real
        stop 1
    end if

    print *, 'PASS: integer capsize accepted and matches real capsize'

end program test_errorbar_capsize_integer
