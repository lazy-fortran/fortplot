program test_no_error_stop
    !! Test that stub functions no longer call error_stop (issue #444)
    !! Also verifies each function stores data (plot_count increments) and
    !! the final render produces a non-empty file.
    use fortplot_matplotlib
    use fortplot_matplotlib_session, only: get_global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(8) :: x(5), y(5), data(20)
    integer :: i
    logical :: all_passed, dir_ok
    integer :: expected_count
    integer :: expected_annotations
    character(len=*), parameter :: outfile = 'build/test/output/test_no_error_stop.png'
    logical :: file_exists
    integer :: file_size

    call create_directory_runtime('build/test/output', dir_ok)

    ! Generate test data
    do i = 1, 5
        x(i) = real(i, 8)
        y(i) = real(i*i, 8)
    end do

    do i = 1, 20
        data(i) = real(i, 8) + sin(real(i, 8))
    end do

    all_passed = .true.
    expected_count = 0
    expected_annotations = 0

    ! Start fresh figure for this test
    call figure()

    ! bar() - should add a plot
    print *, "Testing bar()..."
    call bar(x, y)
    print *, "  bar() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('bar()', expected_count, all_passed)

    ! barh()
    print *, "Testing barh()..."
    call barh(x, y)
    print *, "  barh() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('barh()', expected_count, all_passed)

    ! scatter()
    print *, "Testing scatter()..."
    call scatter(x, y)
    print *, "  scatter() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('scatter()', expected_count, all_passed)

    ! These should also store visible state, not just return.
    print *, "Testing hist()..."
    call hist(data)
    print *, "  hist() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('hist()', expected_count, all_passed)

    print *, "Testing histogram()..."
    call histogram(data)
    print *, "  histogram() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('histogram()', expected_count, all_passed)

    print *, "Testing boxplot()..."
    call boxplot(data)
    print *, "  boxplot() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('boxplot()', expected_count, all_passed)

    print *, "Testing text()..."
    call text(2.5d0, 6.0d0, "Test")
    print *, "  text() passed (no error_stop)"
    expected_annotations = expected_annotations + 1
    call assert_annotation_count('text()', expected_annotations, all_passed)

    print *, "Testing annotate()..."
    call annotate("Arrow", [3.0d0, 9.0d0], [2.0d0, 10.0d0])
    print *, "  annotate() passed (no error_stop)"
    expected_annotations = expected_annotations + 1
    call assert_annotation_count('annotate()', expected_annotations, all_passed)

    print *, "Testing errorbar()..."
    call errorbar(x, y)
    print *, "  errorbar() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('errorbar()', expected_count, all_passed)

    ! Save and verify rendering pipeline accepts the accumulated data
    call savefig(outfile)
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: savefig did not create output file"
        all_passed = .false.
    else if (file_size <= 0) then
        print *, "FAIL: output file is empty"
        all_passed = .false.
    else
        print *, "PASS: rendering pipeline accepted data (", file_size, " bytes)"
    end if

    if (.not. all_passed) then
        error stop "test_no_error_stop: assertions failed"
    end if

    print *, ""
    print *, "SUCCESS: All previously stubbed functions work without error_stop."
    print *, "Issue #444 is resolved and data is stored correctly."

contains

    subroutine assert_plot_count(operation, expected, passed)
        character(len=*), intent(in) :: operation
        integer, intent(in) :: expected
        logical, intent(inout) :: passed
        class(figure_t), pointer :: current_fig

        current_fig => get_global_figure()
        if (.not. associated(current_fig)) then
            print *, "FAIL: global figure not associated after ", operation
            passed = .false.
            return
        end if

        if (current_fig%plot_count /= expected) then
            print *, "FAIL: plot_count after ", operation, " =", &
                current_fig%plot_count, "; expected", expected
            passed = .false.
        else
            print *, "PASS: plot_count =", current_fig%plot_count, &
                " after ", operation
        end if
    end subroutine assert_plot_count

    subroutine assert_annotation_count(operation, expected, passed)
        character(len=*), intent(in) :: operation
        integer, intent(in) :: expected
        logical, intent(inout) :: passed
        class(figure_t), pointer :: current_fig

        current_fig => get_global_figure()
        if (.not. associated(current_fig)) then
            print *, "FAIL: global figure not associated after ", operation
            passed = .false.
            return
        end if

        if (current_fig%annotation_count /= expected) then
            print *, "FAIL: annotation_count after ", operation, " =", &
                current_fig%annotation_count, "; expected", expected
            passed = .false.
        else
            print *, "PASS: annotation_count =", current_fig%annotation_count, &
                " after ", operation
        end if
    end subroutine assert_annotation_count

end program test_no_error_stop
