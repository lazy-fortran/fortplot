program test_boxplot_comprehensive
    !! Comprehensive test suite for box plot functionality
    !! Consolidates: test_boxplot, test_boxplot_outliers, test_boxplot_rendering_regression_1327,
    !! test_boxplot_stats
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_validation, only: validation_result_t, validate_file_exists, validate_file_size
    implicit none

    call test_basic_boxplot()
    call test_outliers()
    call test_stats()
    call test_rendering_regression_1327()

    print *, 'All boxplot tests PASSED!'

contains

    subroutine test_basic_boxplot()
        !! Basic test for box plot functionality
        type(figure_t) :: fig
        real(wp), parameter :: test_data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                               6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]

        call fig%initialize(800, 600)
        call fig%boxplot(test_data, label='Test Data')

        if (fig%plot_count /= 1) then
            print *, 'FAIL: Expected 1 plot, got', fig%plot_count
            error stop 1
        end if

        if (.not. allocated(fig%plots(1)%box_data)) then
            print *, 'FAIL: Box data not allocated'
            error stop 1
        end if

        print *, '  PASS: test_basic_boxplot'
    end subroutine test_basic_boxplot

    subroutine test_outliers()
        !! Test outlier detection in boxplots
        type(figure_t) :: fig
        real(wp), parameter :: data(6) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 20.0_wp]

        call fig%initialize(400, 300)
        call fig%boxplot(data, position=1.0_wp, show_outliers=.true.)

        if (fig%plot_count /= 1) then
            print *, 'FAIL: plot_count expected 1, got', fig%plot_count
            error stop 1
        end if

        if (.not. allocated(fig%plots(1)%outliers)) then
            print *, 'FAIL: outliers not allocated'
            error stop 1
        end if

        if (size(fig%plots(1)%outliers) /= 1) then
            print *, 'FAIL: expected 1 outlier, got', size(fig%plots(1)%outliers)
            error stop 1
        end if

        if (abs(fig%plots(1)%outliers(1) - 20.0_wp) > 1.0e-6_wp) then
            print *, 'FAIL: outlier value unexpected:', fig%plots(1)%outliers(1)
            error stop 1
        end if

        if (fig%plots(1)%whisker_high > 5.0_wp + 1.0e-6_wp) then
            print *, 'FAIL: whisker_high exceeds inlier max:', fig%plots(1)%whisker_high
            error stop 1
        end if

        block
            type(figure_t) :: fig2
            call fig2%initialize(400, 300)
            call fig2%boxplot(data, position=1.0_wp, show_outliers=.true., horizontal=.true.)

            if (.not. allocated(fig2%plots(1)%outliers)) then
                print *, 'FAIL: outliers not allocated (horizontal)'
                error stop 1
            end if

            if (size(fig2%plots(1)%outliers) /= 1) then
                print *, 'FAIL: expected 1 outlier (horizontal), got', size(fig2%plots(1)%outliers)
                error stop 1
            end if

            if (abs(fig2%plots(1)%outliers(1) - 20.0_wp) > 1.0e-6_wp) then
                print *, 'FAIL: outlier value unexpected (horizontal):', fig2%plots(1)%outliers(1)
                error stop 1
            end if
        end block

        print *, '  PASS: test_outliers'
    end subroutine test_outliers

    subroutine test_stats()
        !! Test boxplot statistics computation
        type(figure_t) :: fig
        real(wp), parameter :: data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                          6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]

        call fig%initialize(400, 300)
        call fig%boxplot(data, position=1.0_wp)

        if (fig%plot_count /= 1) then
            print *, 'FAIL: plot_count expected 1, got', fig%plot_count
            error stop 1
        end if

        if (.not. allocated(fig%plots(1)%box_data)) then
            print *, 'FAIL: box_data not allocated'
            error stop 1
        end if

        if (abs(fig%plots(1)%q2 - 5.0_wp) > 1.0e-6_wp .and. &
            abs(fig%plots(1)%q2 - 6.0_wp) > 1.0e-6_wp) then
            print *, 'FAIL: median (q2) not set as expected: ', fig%plots(1)%q2
            error stop 1
        end if

        if (fig%plots(1)%whisker_low < 1.0_wp - 1e-6_wp .or. &
            fig%plots(1)%whisker_high > 10.0_wp + 1e-6_wp) then
            print *, 'FAIL: whiskers not within data range'
            error stop 1
        end if

        print *, '  PASS: test_stats'
    end subroutine test_stats

    subroutine test_rendering_regression_1327()
        !! Verify boxplot rendering produces non-empty output (Issue #1327)
        type(figure_t) :: fig
        real(wp), dimension(10) :: data
        character(len=*), parameter :: out_png = 'test/output/test_boxplot_1327.png'
        type(validation_result_t) :: val
        integer :: i

        do i = 1, 9
            data(i) = real(i, wp)
        end do
        data(10) = 30.0_wp

        call fig%initialize(400, 300)
        call fig%boxplot(data)
        call fig%savefig(out_png)

        val = validate_file_exists(out_png)
        if (.not. val%passed) then
            print *, 'FAIL: expected output PNG not created: ', trim(out_png)
            error stop 1
        end if

        val = validate_file_size(out_png, min_size=2000)
        if (.not. val%passed) then
            print *, 'FAIL: output PNG too small, likely empty plot. Size=', int(val%metric_value)
            error stop 1
        end if

        print *, '  PASS: test_rendering_regression_1327'
    end subroutine test_rendering_regression_1327

end program test_boxplot_comprehensive
