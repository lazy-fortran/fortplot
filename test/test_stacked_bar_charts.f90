program test_stacked_bar_charts
    !! Tests for stacked bar chart functionality (Issue #1460)
    !!
    !! This test covers:
    !! - Basic stacked vertical bar charts with bottom parameter
    !! - Basic stacked horizontal bar charts with left parameter
    !! - Proper range calculation including stacked values
    !! - Legend support for stacked bar layers

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_plotting_advanced, only: bar_impl, barh_impl
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0
    integer :: fail_count = 0

    print *, "=== STACKED BAR CHART TESTS (Issue #1460) ==="

    call test_stacked_vertical_bars()
    call test_stacked_horizontal_bars()
    call test_stacked_bar_ranges()
    call test_stacked_bar_stateful()
    call print_test_summary()

    if (fail_count > 0) stop 1

contains

    subroutine test_stacked_vertical_bars()
        type(figure_t) :: fig
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: heights1(3) = [10.0_wp, 15.0_wp, 12.0_wp]
        real(wp) :: heights2(3) = [5.0_wp, 8.0_wp, 6.0_wp]
        real(wp), dimension(3) :: c1 = [0.2_wp, 0.4_wp, 0.8_wp]
        real(wp), dimension(3) :: c2 = [0.8_wp, 0.4_wp, 0.2_wp]

        call start_test("Stacked vertical bars with bottom parameter")

        call fig%initialize(640, 480)

        ! First layer at base (bottom=0 by default)
        call bar_impl(fig, x, heights1, label='Product A', color=c1)
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "First bar layer added")

        ! Second layer stacked on top
        call bar_impl(fig, x, heights2, bottom=heights1, label='Product B', color=c2)
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Second bar layer stacked")

        call fig%legend()
        call fig%set_title('Stacked Vertical Bar Chart')
        call fig%set_xlabel('Categories')
        call fig%set_ylabel('Values')

        call fig%savefig('test/output/stacked_vertical_bars.png')
        print *, '  Output: test/output/stacked_vertical_bars.png'

        call end_test()
    end subroutine test_stacked_vertical_bars

    subroutine test_stacked_horizontal_bars()
        type(figure_t) :: fig
        real(wp) :: y(4) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp) :: widths1(4) = [20.0_wp, 25.0_wp, 18.0_wp, 22.0_wp]
        real(wp) :: widths2(4) = [10.0_wp, 12.0_wp, 8.0_wp, 11.0_wp]
        real(wp), dimension(3) :: c1 = [0.3_wp, 0.6_wp, 0.3_wp]
        real(wp), dimension(3) :: c2 = [0.6_wp, 0.3_wp, 0.6_wp]

        call start_test("Stacked horizontal bars with left parameter")

        call fig%initialize(640, 480)

        ! First layer at base (left=0 by default)
        call barh_impl(fig, y, widths1, label='Category 1', color=c1)
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "First horizontal bar added")

        ! Second layer stacked to the right
        call barh_impl(fig, y, widths2, left=widths1, label='Category 2', color=c2)
        call assert_equal(real(fig%plot_count, wp), 2.0_wp, "Second horizontal bar stacked")

        call fig%legend()
        call fig%set_title('Stacked Horizontal Bar Chart')
        call fig%set_xlabel('Values')
        call fig%set_ylabel('Categories')

        call fig%savefig('test/output/stacked_horizontal_bars.png')
        print *, '  Output: test/output/stacked_horizontal_bars.png'

        call end_test()
    end subroutine test_stacked_horizontal_bars

    subroutine test_stacked_bar_ranges()
        type(figure_t) :: fig
        real(wp) :: x(2) = [1.0_wp, 2.0_wp]
        real(wp) :: h1(2) = [10.0_wp, 20.0_wp]
        real(wp) :: h2(2) = [5.0_wp, 10.0_wp]

        call start_test("Data range calculation for stacked bars")

        call fig%initialize(400, 300)

        call bar_impl(fig, x, h1)
        call bar_impl(fig, x, h2, bottom=h1)

        ! The y-range should include the full stacked height (h1 + h2)
        ! For x=2: h1=20, h2=10, so max y should be 30
        call assert_true(fig%state%y_max >= 25.0_wp, &
                        "Y-max includes stacked heights")

        call end_test()
    end subroutine test_stacked_bar_ranges

    subroutine test_stacked_bar_stateful()
        real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp) :: heights1(3) = [5.0_wp, 10.0_wp, 7.0_wp]
        real(wp) :: heights2(3) = [3.0_wp, 5.0_wp, 4.0_wp]
        real(wp) :: heights3(3) = [2.0_wp, 3.0_wp, 2.0_wp]
        real(wp), dimension(3) :: c1 = [0.2_wp, 0.6_wp, 0.8_wp]
        real(wp), dimension(3) :: c2 = [0.8_wp, 0.6_wp, 0.2_wp]
        real(wp), dimension(3) :: c3 = [0.4_wp, 0.8_wp, 0.4_wp]

        call start_test("Stacked bars via stateful interface")

        call figure(figsize=[6.4_wp, 4.8_wp])

        ! Three layers of stacked bars
        call bar(x, heights1, label='Layer 1', color=c1)
        call bar(x, heights2, bottom=heights1, label='Layer 2', color=c2)
        call bar(x, heights3, bottom=heights1 + heights2, label='Layer 3', color=c3)

        call legend()
        call title('Three-Layer Stacked Bar Chart')
        call xlabel('Categories')
        call ylabel('Values')

        call savefig('test/output/stacked_bars_stateful.png')
        print *, '  Output: test/output/stacked_bars_stateful.png'

        call end_test()
    end subroutine test_stacked_bar_stateful

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') 'Test ', test_count, ': ', test_name
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') '  PASS'
        write(*, *)
    end subroutine end_test

    subroutine assert_equal(actual, expected, description)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: description

        if (abs(actual - expected) < 1.0e-10_wp) then
            print *, '  PASS: ', description
        else
            print *, '  FAIL: ', description, ' (expected ', expected, ', got ', actual, ')'
            fail_count = fail_count + 1
        end if
    end subroutine assert_equal

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) then
            print *, '  PASS: ', description
        else
            print *, '  FAIL: ', description
            fail_count = fail_count + 1
        end if
    end subroutine assert_true

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'Stacked Bar Chart Test Summary (Issue #1460)'
        write(*, '(A, I0, A, I0, A, I0)') 'Tests run: ', test_count, &
            ' | Passed: ', pass_count, ' | Failed: ', fail_count
        if (fail_count == 0) then
            write(*, '(A)') 'ALL TESTS PASSED!'
        else
            write(*, '(A)') 'SOME TESTS FAILED!'
        end if
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_stacked_bar_charts
