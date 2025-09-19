program test_boxplot_outliers
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    implicit none

    type(figure_t) :: fig
    real(wp), parameter :: data(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 20.0_wp]

    call fig%initialize(400,300)
    call fig%boxplot(data, position=1.0_wp, show_outliers=.true.)

    if (fig%plot_count /= 1) then
        print *, 'FAIL: plot_count expected 1, got', fig%plot_count
        stop 1
    end if

    if (.not. allocated(fig%plots(1)%outliers)) then
        print *, 'FAIL: outliers not allocated'
        stop 1
    end if

    if (size(fig%plots(1)%outliers) /= 1) then
        print *, 'FAIL: expected 1 outlier, got', size(fig%plots(1)%outliers)
        stop 1
    end if

    if (abs(fig%plots(1)%outliers(1) - 20.0_wp) > 1.0e-6_wp) then
        print *, 'FAIL: outlier value unexpected:', fig%plots(1)%outliers(1)
        stop 1
    end if

    if (fig%plots(1)%whisker_high > 5.0_wp + 1.0e-6_wp) then
        print *, 'FAIL: whisker_high exceeds inlier max:', fig%plots(1)%whisker_high
        stop 1
    end if

    ! Horizontal flag should not affect statistics computation
    block
        type(figure_t) :: fig2
        call fig2%initialize(400,300)
        call fig2%boxplot(data, position=1.0_wp, show_outliers=.true., horizontal=.true.)

        if (.not. allocated(fig2%plots(1)%outliers)) then
            print *, 'FAIL: outliers not allocated (horizontal)'
            stop 1
        end if

        if (size(fig2%plots(1)%outliers) /= 1) then
            print *, 'FAIL: expected 1 outlier (horizontal), got', size(fig2%plots(1)%outliers)
            stop 1
        end if

        if (abs(fig2%plots(1)%outliers(1) - 20.0_wp) > 1.0e-6_wp) then
            print *, 'FAIL: outlier value unexpected (horizontal):', fig2%plots(1)%outliers(1)
            stop 1
        end if
    end block

    print *, 'Boxplot outliers test PASSED'
end program test_boxplot_outliers
