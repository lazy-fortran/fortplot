program test_grouped_bar_ticks
    !! Tests for grouped bar chart tick positioning (Issue #1708)
    !!
    !! This test covers:
    !! - Stateful set_xticks() sets custom tick positions and labels
    !! - OO set_xticks() sets custom tick positions and labels
    !! - Custom tick positions are stored correctly in figure state
    !! - Grouped bar chart renders with correct tick positions
    !! - set_yticks() stateful wrapper

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_plotting_advanced, only: bar_impl
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0
    integer :: fail_count = 0
    integer :: test_fail_count = 0
    logical :: dir_ok

    call create_directory_runtime('build/test/output', dir_ok)
    print *, "=== GROUPED BAR TICK POSITION TESTS (Issue #1708) ==="

    call test_stateful_set_xticks()
    call test_oo_set_xticks()
    call test_grouped_bar_with_ticks()
    call test_set_yticks()
    call print_test_summary()

    if (fail_count > 0) stop 1

contains

    subroutine test_stateful_set_xticks()
        !! Test stateful set_xticks via global figure pointer
        real(wp) :: centers(4)
        real(wp) :: positions_a(4)
        real(wp) :: positions_b(4)
        real(wp) :: values_a(4)
        real(wp) :: values_b(4)
        character(len=4) :: labels(4)
        class(figure_t), pointer :: fig_ptr

        call start_test("Stateful set_xticks stores positions and labels")

        centers = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        positions_a = centers - 0.2_wp
        positions_b = centers + 0.2_wp
        values_a = [4.5_wp, 5.8_wp, 6.1_wp, 6.7_wp]
        values_b = [3.2_wp, 4.6_wp, 5.4_wp, 6.0_wp]
        labels = ['Q1  ', 'Q2  ', 'Q3  ', 'Q4  ']

        call figure(figsize=[7.5_wp, 5.0_wp])
        call bar(positions_a, values_a, width=0.4_wp, label='Product A')
        call bar(positions_b, values_b, width=0.4_wp, label='Product B')
        call set_xticks(centers, labels)

        fig_ptr => get_global_figure()

        call assert_true(fig_ptr%state%custom_xticks_set, &
            "Custom x-ticks flag is set")
        call assert_true(allocated(fig_ptr%state%custom_xtick_positions), &
            "Custom x-tick positions allocated")
        call assert_equal(real(size(fig_ptr%state%custom_xtick_positions), wp), &
            4.0_wp, "Four tick positions stored")
        call assert_equal(fig_ptr%state%custom_xtick_positions(1), &
            1.0_wp, "First tick at center position 1")
        call assert_equal(fig_ptr%state%custom_xtick_positions(4), &
            4.0_wp, "Last tick at center position 4")
        call assert_str_equal(trim(fig_ptr%state%custom_xtick_labels(1)), &
            'Q1', "First label is Q1")
        call assert_str_equal(trim(fig_ptr%state%custom_xtick_labels(4)), &
            'Q4', "Last label is Q4")

        call savefig('build/test/output/grouped_bar_stateful_ticks.png')

        call end_test()
    end subroutine test_stateful_set_xticks

    subroutine test_oo_set_xticks()
        !! Test OO set_xticks positions and labels are stored
        type(figure_t) :: fig
        real(wp) :: centers(3)
        real(wp) :: positions_a(3)
        real(wp) :: positions_b(3)
        real(wp) :: values_a(3)
        real(wp) :: values_b(3)
        character(len=6) :: labels(3)

        call start_test("OO set_xticks stores positions and labels")

        centers = [1.0_wp, 2.0_wp, 3.0_wp]
        positions_a = centers - 0.18_wp
        positions_b = centers + 0.18_wp
        values_a = [2.8_wp, 3.4_wp, 3.9_wp]
        values_b = [3.5_wp, 3.8_wp, 4.6_wp]
        labels = ['Team 1', 'Team 2', 'Team 3']

        call fig%initialize()
        call bar_impl(fig, positions_a, values_a, width=0.32_wp, label='A')
        call bar_impl(fig, positions_b, values_b, width=0.32_wp, label='B')
        call fig%set_xticks(centers, labels)

        call assert_true(fig%state%custom_xticks_set, &
            "Custom x-ticks flag is set")
        call assert_true(allocated(fig%state%custom_xtick_positions), &
            "Custom x-tick positions allocated")
        call assert_equal(real(size(fig%state%custom_xtick_positions), wp), &
            3.0_wp, "Three tick positions stored")
        call assert_equal(fig%state%custom_xtick_positions(1), &
            1.0_wp, "First tick at center position 1")
        call assert_equal(fig%state%custom_xtick_positions(3), &
            3.0_wp, "Last tick at center position 3")
        call assert_str_equal(trim(fig%state%custom_xtick_labels(1)), &
            'Team 1', "First label is Team 1")
        call assert_str_equal(trim(fig%state%custom_xtick_labels(3)), &
            'Team 3', "Last label is Team 3")

        call fig%savefig('build/test/output/grouped_bar_oo_ticks.png')

        call end_test()
    end subroutine test_oo_set_xticks

    subroutine test_grouped_bar_with_ticks()
        !! End-to-end: grouped bar chart with explicit tick positions
        !! Verifies bars render at offset positions but ticks at centers
        type(figure_t) :: fig
        real(wp) :: centers(4)
        real(wp) :: positions_a(4)
        real(wp) :: positions_b(4)
        real(wp) :: values_a(4)
        real(wp) :: values_b(4)
        character(len=4) :: labels(4)

        call start_test("Grouped bar chart with explicit tick positions")

        centers = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        positions_a = centers - 0.2_wp
        positions_b = centers + 0.2_wp
        values_a = [4.5_wp, 5.8_wp, 6.1_wp, 6.7_wp]
        values_b = [3.2_wp, 4.6_wp, 5.4_wp, 6.0_wp]
        labels = ['Q1  ', 'Q2  ', 'Q3  ', 'Q4  ']

        call fig%initialize(800, 600)
        call bar_impl(fig, positions_a, values_a, width=0.4_wp, label='A')
        call bar_impl(fig, positions_b, values_b, width=0.4_wp, label='B')
        call fig%set_xticks(centers, labels)

        call assert_true(fig%plot_count >= 2, "Two bar plots added")
        call assert_true(fig%state%custom_xticks_set, "Ticks set to centers")
        call assert_equal(fig%state%custom_xtick_positions(1), &
            1.0_wp, "Tick 1 at center (not offset)")
        call assert_equal(fig%state%custom_xtick_positions(2), &
            2.0_wp, "Tick 2 at center (not offset)")

        call fig%savefig('build/test/output/grouped_bar_explicit_ticks.png')

        call end_test()
    end subroutine test_grouped_bar_with_ticks

    subroutine test_set_yticks()
        !! Test stateful set_yticks stores positions and labels
        class(figure_t), pointer :: fig_ptr
        real(wp) :: yticks(3)
        character(len=5) :: ylabels(3)

        call start_test("Stateful set_yticks stores positions and labels")

        call figure(figsize=[6.0_wp, 4.0_wp])
        yticks = [0.0_wp, 5.0_wp, 10.0_wp]
        ylabels = ['Low ', 'Mid ', 'High']

        call bar([1.0_wp, 2.0_wp], [3.0_wp, 7.0_wp])
        call set_yticks(yticks, ylabels)

        fig_ptr => get_global_figure()

        call assert_true(fig_ptr%state%custom_yticks_set, &
            "Custom y-ticks flag set")
        call assert_true(allocated(fig_ptr%state%custom_ytick_positions), &
            "Custom y-tick positions allocated")
        call assert_equal(real(size(fig_ptr%state%custom_ytick_positions), wp), &
            3.0_wp, "Three y-tick positions stored")
        call assert_str_equal(trim(fig_ptr%state%custom_ytick_labels(1)), &
            'Low', "First y-label is Low")

        call savefig('build/test/output/set_yticks.png')

        call end_test()
    end subroutine test_set_yticks

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        test_fail_count = fail_count
        write(*, '(A, I0, A, A)') 'Test ', test_count, ': ', test_name
    end subroutine start_test

    subroutine end_test()
        if (fail_count == test_fail_count) then
            pass_count = pass_count + 1
            write(*, '(A)') '  PASS'
        else
            write(*, '(A)') '  FAIL'
        end if
        write(*, *)
    end subroutine end_test

    subroutine assert_equal(actual, expected, description)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: description

        if (abs(actual - expected) < 1.0e-10_wp) then
            print *, '  PASS: ', description
        else
            print *, '  FAIL: ', description, &
                ' (expected ', expected, ', got ', actual, ')'
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

    subroutine assert_str_equal(actual, expected, description)
        character(len=*), intent(in) :: actual, expected, description

        if (trim(actual) == trim(expected)) then
            print *, '  PASS: ', description
        else
            print *, '  FAIL: ', description, &
                ' (expected "', trim(expected), '", got "', trim(actual), '")'
            fail_count = fail_count + 1
        end if
    end subroutine assert_str_equal

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'Grouped Bar Tick Position Test Summary'
        write(*, '(A, I0, A, I0, A, I0)') &
            'Tests run: ', test_count, &
            ' | Passed: ', pass_count, ' | Failed: ', fail_count
        if (fail_count == 0) then
            write(*, '(A)') 'ALL TESTS PASSED!'
        else
            write(*, '(A)') 'SOME TESTS FAILED!'
        end if
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_grouped_bar_ticks
