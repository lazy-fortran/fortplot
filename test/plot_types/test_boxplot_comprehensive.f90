program test_boxplot_comprehensive
    !! Comprehensive test suite for box plot functionality
    !! Consolidates: test_boxplot, test_boxplot_outliers, test_boxplot_rendering_regression_1327,
    !! test_boxplot_stats
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_validation, only: validation_result_t, validate_file_exists, validate_file_size
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_matplotlib, only: figure, boxplot, savefig
    use fortplot_matplotlib_session, only: get_global_figure
    use fortplot_boxplot_rendering, only: boxplot_cap_half_width
    implicit none
    logical :: dir_ok

    call create_directory_runtime('build/test/output', dir_ok)
    call test_basic_boxplot()
    call test_outliers()
    call test_stats()
    call test_rendering_regression_1327()
    call test_boxplot_rgb_color()
    call test_boxplot_palette_progression()
    call test_boxplot_string_color()
    call test_cap_width()

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
        character(len=*), parameter :: out_png = 'build/test/output/test_boxplot_1327.png'
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

    subroutine test_boxplot_rgb_color()
        !! Verify boxplot accepts RGB triple color and stores it in plot_data
        type(figure_t) :: fig
        real(wp), parameter :: test_data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                               6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]
        real(wp), parameter :: red_color(3) = [1.0_wp, 0.0_wp, 0.0_wp]

        call fig%initialize(400, 300)
        call fig%boxplot(test_data, color=red_color)

        if (fig%plot_count /= 1) then
            print *, 'FAIL: expected plot_count=1, got', fig%plot_count
            error stop 1
        end if

        if (fig%plots(1)%color(1) /= 1.0_wp .or. &
            fig%plots(1)%color(2) /= 0.0_wp .or. &
            fig%plots(1)%color(3) /= 0.0_wp) then
            print *, 'FAIL: RGB color not stored correctly: ', &
                     fig%plots(1)%color(1), fig%plots(1)%color(2), fig%plots(1)%color(3)
            error stop 1
        end if

        print *, '  PASS: test_boxplot_rgb_color'
    end subroutine test_boxplot_rgb_color

    subroutine test_boxplot_palette_progression()
        !! Verify that multiple boxplots without explicit color cycle through palette
        type(figure_t) :: fig
        real(wp), parameter :: test_data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                               6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]

        call fig%initialize(400, 300)
        call fig%boxplot(test_data)
        call fig%boxplot(test_data)
        call fig%boxplot(test_data)

        if (fig%plot_count /= 3) then
            print *, 'FAIL: expected plot_count=3, got', fig%plot_count
            error stop 1
        end if

        ! First boxplot should get palette color 0
        ! Second boxplot should get palette color 1 (different from first)
        ! Third boxplot should get palette color 2 (different from second)
        if (fig%plots(1)%color(1) == fig%plots(2)%color(1) .and. &
            fig%plots(1)%color(2) == fig%plots(2)%color(2) .and. &
            fig%plots(1)%color(3) == fig%plots(2)%color(3)) then
            print *, 'FAIL: second boxplot has same color as first (no palette progression)'
            error stop 1
        end if

        if (fig%plots(2)%color(1) == fig%plots(3)%color(1) .and. &
            fig%plots(2)%color(2) == fig%plots(3)%color(2) .and. &
            fig%plots(2)%color(3) == fig%plots(3)%color(3)) then
            print *, 'FAIL: third boxplot has same color as second (no palette progression)'
            error stop 1
        end if

        print *, '  PASS: test_boxplot_palette_progression'
    end subroutine test_boxplot_palette_progression

    subroutine test_boxplot_string_color()
        !! Verify pyplot boxplot facade accepts string color and resolves it to RGB
        character(len=*), parameter :: out_png = 'build/test/output/test_boxplot_string_color.png'
        real(wp), parameter :: test_data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                               6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]
        real(wp), parameter :: expected_orange(3) = [1.0_wp, 0.647_wp, 0.0_wp]
        type(validation_result_t) :: val
        type(figure_t), pointer :: fig_ptr => null()

        call figure()
        call boxplot(test_data, color='orange')
        call savefig(out_png)

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

        ! Verify the string color was resolved to RGB and stored in plot_data
        fig_ptr => get_global_figure()
        if (fig_ptr%plot_count < 1) then
            print *, 'FAIL: no plots in figure after boxplot with string color'
            error stop 1
        end if

        if (abs(fig_ptr%plots(1)%color(1) - expected_orange(1)) > 0.001_wp .or. &
            abs(fig_ptr%plots(1)%color(2) - expected_orange(2)) > 0.001_wp .or. &
            abs(fig_ptr%plots(1)%color(3) - expected_orange(3)) > 0.001_wp) then
            print *, 'FAIL: string color not resolved correctly: ', &
                     fig_ptr%plots(1)%color(1), fig_ptr%plots(1)%color(2), fig_ptr%plots(1)%color(3), &
                     ' expected: ', expected_orange(1), expected_orange(2), expected_orange(3)
            error stop 1
        end if

        print *, '  PASS: test_boxplot_string_color'
    end subroutine test_boxplot_string_color

    subroutine test_cap_width()
        !! matplotlib 3.10 default: whisker cap total width is half the box width.
        !! The renderer draws caps from pos-capw to pos+capw, so the cap
        !! half-width must equal a quarter of the box width.
        real(wp), parameter :: box_width = 0.6_wp
        real(wp), parameter :: expected_cap_half_width = 0.25_wp * box_width
        real(wp) :: capw

        capw = boxplot_cap_half_width(box_width)

        if (abs(capw - expected_cap_half_width) > 1.0e-12_wp) then
            print *, 'FAIL: cap half-width', capw, 'expected', expected_cap_half_width
            error stop 1
        end if

        ! Total cap width (2*capw) must be half the box width.
        if (abs(2.0_wp * capw - 0.5_wp * box_width) > 1.0e-12_wp) then
            print *, 'FAIL: cap total width', 2.0_wp * capw, &
                     'expected', 0.5_wp * box_width
            error stop 1
        end if

        print *, '  PASS: test_cap_width'
    end subroutine test_cap_width

end program test_boxplot_comprehensive
