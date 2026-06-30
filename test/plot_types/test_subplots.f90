program test_subplots
    !! Comprehensive test suite for subplot functionality
    !! Consolidates: test_subplot, test_subplots_edge_cases,
    !! test_subplots_returns, test_subplots_stateful
    use iso_fortran_env, only: real64, wp => real64
    use fortplot
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    call test_subplot_creation()
    call test_subplot_layouts()
    call test_subplot_plotting()
    call test_subplot_marker()
    call test_subplot_other_plot_types()
    call test_subplot_alpha()
    call test_subplot_independence()
    call test_edge_cases()
    call test_grid_returns()
    call test_stateful_api()

    print *, ''
    print *, '=== Subplot Test Summary ==='
    print *, 'Tests passed:', passed_tests, '/', total_tests

    if (passed_tests == total_tests) then
        print *, 'All subplot tests PASSED!'
        stop 0
    else
        print *, 'FAIL: Some subplot tests failed'
        stop 1
    end if

contains

    subroutine test_subplot_creation()
        !! Test basic subplot creation and grid setup
        type(figure_t) :: fig

        total_tests = total_tests + 1

        call fig%initialize(800, 600)
        call fig%subplots(2, 2)

        if (fig%subplot_rows /= 2 .or. fig%subplot_cols /= 2) then
            print *, 'FAIL: test_subplot_creation - grid not created'
            return
        end if

        if (.not. allocated(fig%subplots_array)) then
            print *, 'FAIL: test_subplot_creation - array not allocated'
            return
        end if

        print *, '  PASS: test_subplot_creation'
        passed_tests = passed_tests + 1
    end subroutine test_subplot_creation

    subroutine test_subplot_layouts()
        !! Test different subplot grid configurations
        type(figure_t) :: fig

        total_tests = total_tests + 1

        call fig%initialize(800, 600)
        call fig%subplots(1, 2)

        if (fig%subplot_rows /= 1 .or. fig%subplot_cols /= 2) then
            print *, 'FAIL: test_subplot_layouts - 1x2 layout'
            return
        end if

        call fig%initialize(800, 600)
        call fig%subplots(3, 2)

        if (fig%subplot_rows /= 3 .or. fig%subplot_cols /= 2) then
            print *, 'FAIL: test_subplot_layouts - 3x2 layout'
            return
        end if

        print *, '  PASS: test_subplot_layouts'
        passed_tests = passed_tests + 1
    end subroutine test_subplot_layouts

    subroutine test_subplot_plotting()
        !! Test plotting on individual subplots
        type(figure_t) :: fig
        real(wp), parameter :: x_data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y_data(5) = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]

        total_tests = total_tests + 1

        call fig%initialize(800, 600)
        call fig%subplots(2, 2)

        call fig%subplot_plot(1, 1, x_data, y_data, label='Plot 1')
        call fig%subplot_plot(1, 2, x_data, y_data*2.0_wp, label='Plot 2')

        if (fig%subplot_plot_count(1, 1) /= 1) then
            print *, 'FAIL: test_subplot_plotting - plot not added to (1,1)'
            return
        end if

        if (fig%subplot_plot_count(1, 2) /= 1) then
            print *, 'FAIL: test_subplot_plotting - plot not added to (1,2)'
            return
        end if

        print *, '  PASS: test_subplot_plotting'
        passed_tests = passed_tests + 1
    end subroutine test_subplot_plotting

    subroutine test_subplot_marker()
        type(figure_t) :: fig
        real(wp), parameter :: x_data(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp), parameter :: y_data(4) = [2.0_wp, 3.0_wp, 5.0_wp, 8.0_wp]
        real(wp), parameter :: color(3) = [0.2_wp, 0.4_wp, 0.6_wp]

        total_tests = total_tests + 1

        call fig%initialize(800, 600)
        call fig%subplots(1, 1)

        call fig%subplot_plot(1, 1, x_data, y_data, label='Line', &
                              linestyle='--', marker='s', color=color, alpha=0.35_wp)

        if (fig%subplots_array(1, 1)%plots(1)%marker /= 's') then
            print *, 'FAIL: test_subplot_marker - marker not stored'
            return
        end if

        if (trim(fig%subplots_array(1, 1)%plots(1)%label) /= 'Line') then
            print *, 'FAIL: test_subplot_marker - label not stored'
            return
        end if

        if (trim(fig%subplots_array(1, 1)%plots(1)%linestyle) /= '--') then
            print *, 'FAIL: test_subplot_marker - linestyle not stored'
            return
        end if

        if (any(abs(fig%subplots_array(1, 1)%plots(1)%color - color) > 1.0e-12_wp)) then
            print *, 'FAIL: test_subplot_marker - color not stored'
            return
        end if

        if (abs(fig%subplots_array(1, 1)%plots(1)%fill_alpha - 0.35_wp) > 1.0e-12_wp) then
            print *, 'FAIL: test_subplot_marker - alpha not stored'
            return
        end if

        print *, '  PASS: test_subplot_marker'
        passed_tests = passed_tests + 1
    end subroutine test_subplot_marker

    subroutine test_subplot_other_plot_types()
        !! Test subplot helpers for non-line plot types.
        use fortplot_plot_data, only: PLOT_TYPE_BAR, PLOT_TYPE_BOXPLOT, &
                                      PLOT_TYPE_HISTOGRAM, PLOT_TYPE_SCATTER
        type(figure_t) :: fig
        real(wp), parameter :: x_data(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp), parameter :: y_data(4) = [2.0_wp, 1.0_wp, 3.0_wp, 4.0_wp]
        real(wp), parameter :: hist_data(6) = [1.0_wp, 1.5_wp, 2.0_wp, 2.5_wp, &
                                               3.0_wp, 3.5_wp]
        real(wp), parameter :: box_data(5) = [5.0_wp, 6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp]

        total_tests = total_tests + 1

        call fig%initialize(800, 600)
        call fig%subplots(3, 2)

        call fig%subplot_hist(1, 1, hist_data, bins=3)
        call fig%subplot_boxplot(1, 2, box_data)
        call fig%subplot_scatter(2, 1, x_data, y_data)
        call fig%subplot_bar(2, 2, x_data, y_data)
        call fig%subplot_barh(3, 1, x_data, y_data)

        if (fig%subplot_plot_count(1, 1) /= 1) then
            print *, 'FAIL: test_subplot_other_plot_types - histogram not routed'
            return
        end if

        if (fig%subplots_array(1, 1)%plots(1)%plot_type /= PLOT_TYPE_HISTOGRAM) then
            print *, 'FAIL: test_subplot_other_plot_types - histogram type mismatch'
            return
        end if

        if (fig%subplot_plot_count(1, 2) /= 1) then
            print *, 'FAIL: test_subplot_other_plot_types - boxplot not routed'
            return
        end if

        if (fig%subplots_array(1, 2)%plots(1)%plot_type /= PLOT_TYPE_BOXPLOT) then
            print *, 'FAIL: test_subplot_other_plot_types - boxplot type mismatch'
            return
        end if

        if (fig%subplot_plot_count(2, 1) /= 1) then
            print *, 'FAIL: test_subplot_other_plot_types - scatter not routed'
            return
        end if

        if (fig%subplots_array(2, 1)%plots(1)%plot_type /= PLOT_TYPE_SCATTER) then
            print *, 'FAIL: test_subplot_other_plot_types - scatter type mismatch'
            return
        end if

        if (fig%subplot_plot_count(2, 2) /= 1) then
            print *, 'FAIL: test_subplot_other_plot_types - bar not routed'
            return
        end if

        if (fig%subplots_array(2, 2)%plots(1)%plot_type /= PLOT_TYPE_BAR) then
            print *, 'FAIL: test_subplot_other_plot_types - bar type mismatch'
            return
        end if

        if (fig%subplot_plot_count(3, 1) /= 1) then
            print *, 'FAIL: test_subplot_other_plot_types - barh not routed'
            return
        end if

        if (fig%subplots_array(3, 1)%plots(1)%plot_type /= PLOT_TYPE_BAR) then
            print *, 'FAIL: test_subplot_other_plot_types - barh type mismatch'
            return
        end if

        if (.not. fig%subplots_array(3, 1)%plots(1)%bar_horizontal) then
            print *, 'FAIL: test_subplot_other_plot_types - barh orientation mismatch'
            return
        end if

        if (fig%subplot_plot_count(3, 2) /= 0) then
            print *, 'FAIL: test_subplot_other_plot_types - barh polluted other subplot'
            return
        end if

        print *, '  PASS: test_subplot_other_plot_types'
        passed_tests = passed_tests + 1
    end subroutine test_subplot_other_plot_types

    subroutine test_subplot_alpha()
        type(figure_t) :: fig
        real(wp), parameter :: x_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y_data(3) = [2.0_wp, 3.0_wp, 4.0_wp]

        total_tests = total_tests + 1

        call fig%initialize(800, 600)
        call fig%subplots(2, 2)
        call fig%subplot_plot(1, 2, x_data, y_data, alpha=1.2_wp)

        if (abs(fig%subplots_array(1, 2)%plots(1)%fill_alpha - 1.0_wp) > 1.0e-12_wp) then
            print *, 'FAIL: test_subplot_alpha - fill alpha not clamped'
            return
        end if

        if (abs(fig%subplots_array(1, 2)%plots(1)%marker_face_alpha - 1.0_wp) > &
            1.0e-12_wp) then
            print *, 'FAIL: test_subplot_alpha - marker face alpha not clamped'
            return
        end if

        if (abs(fig%subplots_array(1, 2)%plots(1)%marker_edge_alpha - 1.0_wp) > &
            1.0e-12_wp) then
            print *, 'FAIL: test_subplot_alpha - marker edge alpha not clamped'
            return
        end if

        print *, '  PASS: test_subplot_alpha'
        passed_tests = passed_tests + 1
    end subroutine test_subplot_alpha

    subroutine test_subplot_independence()
        !! Test that subplots maintain independent properties
        type(figure_t) :: fig
        real(wp), parameter :: x_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y1_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y2_data(3) = [10.0_wp, 20.0_wp, 30.0_wp]

        total_tests = total_tests + 1

        call fig%initialize(800, 600)
        call fig%subplots(1, 2)

        call fig%subplot_set_title(1, 1, 'First Plot')
        call fig%subplot_set_title(1, 2, 'Second Plot')
        call fig%subplot_set_xlabel(1, 1, 'X1')
        call fig%subplot_set_xlabel(1, 2, 'X2')

        call fig%subplot_plot(1, 1, x_data, y1_data)
        call fig%subplot_plot(1, 2, x_data, y2_data)

        if (fig%subplot_title(1, 1) == fig%subplot_title(1, 2)) then
            print *, 'FAIL: test_subplot_independence - titles not independent'
            return
        end if

        print *, '  PASS: test_subplot_independence'
        passed_tests = passed_tests + 1
    end subroutine test_subplot_independence

    subroutine test_edge_cases()
        !! Test edge cases for the stateful API subplots function
        real(8) :: x(5), y(5)
        character(len=:), allocatable :: output_dir
        integer :: i

        total_tests = total_tests + 1

        call ensure_test_output_dir('subplots_edge_cases', output_dir)

        do i = 1, 5
            x(i) = real(i, 8)
            y(i) = real(i*i, 8)
        end do

        call subplots(1, 1)
        call subplots(3, 4)
        call subplots(0, 1)
        call subplots(1, 0)
        call subplots(-1, 2)
        call subplots(10, 10)

        call subplots(2, 2)
        call plot(x, y, label='test')
        call savefig(trim(output_dir)//'test_subplots_edge_cases.png')

        print *, '  PASS: test_edge_cases'
        passed_tests = passed_tests + 1
    end subroutine test_edge_cases

    subroutine test_grid_returns()
        !! Test that subplots_grid() returns subplot indices
        integer :: i, j, idx
        real(8) :: x(100), y(100)
        integer :: nrows, ncols
        integer, allocatable :: axes(:,:)
        character(len=:), allocatable :: output_dir

        total_tests = total_tests + 1

        call ensure_test_output_dir('subplots_returns', output_dir)

        do i = 1, 100
            x(i) = real(i-1, 8) * 0.1
            y(i) = sin(x(i))
        end do

        nrows = 2
        ncols = 2
        axes = subplots_grid(nrows, ncols)

        do i = 1, nrows
            do j = 1, ncols
                idx = axes(i,j)
                call subplot(nrows, ncols, idx)
                call plot(x, y*real(i*j, 8))
                call title('Subplot ' // char(48+i) // ',' // char(48+j))
            end do
        end do

        call savefig(trim(output_dir)//'test_subplots_grid_returns.png')

        print *, '  PASS: test_grid_returns'
        passed_tests = passed_tests + 1
    end subroutine test_grid_returns

    subroutine test_stateful_api()
        !! Test the stateful API subplots function
        real(8) :: x(10), y1(10)
        character(len=:), allocatable :: output_dir
        integer :: i

        total_tests = total_tests + 1

        call ensure_test_output_dir('subplots_stateful', output_dir)

        do i = 1, 10
            x(i) = real(i-1, 8)
            y1(i) = x(i)**2
        end do

        call subplots(2, 2)
        call plot(x, y1, label='y = x^2')
        call savefig(trim(output_dir)//'test_subplots_stateful.png')

        print *, '  PASS: test_stateful_api'
        passed_tests = passed_tests + 1
    end subroutine test_stateful_api

end program test_subplots
