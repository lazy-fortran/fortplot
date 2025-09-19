program test_boxplot_stats
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    implicit none

    type(figure_t) :: fig
    real(wp), parameter :: data(10) = [1.0_wp,2.0_wp,3.0_wp,4.0_wp,5.0_wp,6.0_wp,7.0_wp,8.0_wp,9.0_wp,10.0_wp]

    call fig%initialize(400,300)
    call fig%boxplot(data, position=1.0_wp)

    if (fig%plot_count /= 1) then
        print *, 'FAIL: plot_count expected 1, got', fig%plot_count
        stop 1
    end if

    if (.not. allocated(fig%plots(1)%box_data)) then
        print *, 'FAIL: box_data not allocated'
        stop 1
    end if

    if (abs(fig%plots(1)%q2 - 5.0_wp) > 1.0e-6_wp .and. abs(fig%plots(1)%q2 - 6.0_wp) > 1.0e-6_wp) then
        print *, 'FAIL: median (q2) not set as expected: ', fig%plots(1)%q2
        stop 1
    end if

    if (fig%plots(1)%whisker_low < 1.0_wp - 1e-6_wp .or. fig%plots(1)%whisker_high > 10.0_wp + 1e-6_wp) then
        print *, 'FAIL: whiskers not within data range'
        stop 1
    end if

    print *, 'Boxplot stats test PASSED'
end program test_boxplot_stats

