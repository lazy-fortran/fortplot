program test_no_error_stop
    !! Test that stub functions no longer call error_stop (issue #444)
    !! Also verifies each function stores data (plot_count increments) and
    !! the final render produces a non-empty file (issue #1715).
    use fortplot_matplotlib
    use fortplot_matplotlib_session, only: get_global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: PLOT_TYPE_BAR, PLOT_TYPE_BOXPLOT, &
                                  PLOT_TYPE_ERRORBAR, PLOT_TYPE_LINE, &
                                  PLOT_TYPE_SCATTER
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
    call assert_bar_data('bar()', expected_count, x, y, .false., all_passed)

    ! barh()
    print *, "Testing barh()..."
    call barh(x, y)
    print *, "  barh() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('barh()', expected_count, all_passed)
    call assert_bar_data('barh()', expected_count, x, y, .true., all_passed)

    ! scatter()
    print *, "Testing scatter()..."
    call scatter(x, y)
    print *, "  scatter() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('scatter()', expected_count, all_passed)
    call assert_xy_data('scatter()', expected_count, PLOT_TYPE_SCATTER, x, y, &
                        all_passed)

    ! These should also store visible state, not just return.
    print *, "Testing hist()..."
    call hist(data)
    print *, "  hist() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('hist()', expected_count, all_passed)
    call assert_histogram_data('hist()', expected_count, data, all_passed)

    print *, "Testing histogram()..."
    call histogram(data)
    print *, "  histogram() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('histogram()', expected_count, all_passed)
    call assert_histogram_data('histogram()', expected_count, data, all_passed)

    print *, "Testing boxplot()..."
    call boxplot(data)
    print *, "  boxplot() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('boxplot()', expected_count, all_passed)
    call assert_boxplot_data('boxplot()', expected_count, data, all_passed)

    print *, "Testing text()..."
    call text(2.5d0, 6.0d0, "Test")
    print *, "  text() passed (no error_stop)"
    expected_annotations = expected_annotations + 1
    call assert_annotation_count('text()', expected_annotations, all_passed)
    call assert_annotation_data('text()', expected_annotations, 'Test', &
                                2.5d0, 6.0d0, .false., all_passed)

    print *, "Testing annotate()..."
    call annotate("Arrow", [3.0d0, 9.0d0], [2.0d0, 10.0d0])
    print *, "  annotate() passed (no error_stop)"
    expected_annotations = expected_annotations + 1
    call assert_annotation_count('annotate()', expected_annotations, all_passed)
    call assert_annotation_data('annotate()', expected_annotations, 'Arrow', &
                                2.0d0, 10.0d0, .true., all_passed, &
                                arrow_x=3.0d0, arrow_y=9.0d0)

    print *, "Testing errorbar()..."
    call errorbar(x, y)
    print *, "  errorbar() passed (no error_stop)"
    expected_count = expected_count + 1
    call assert_plot_count('errorbar()', expected_count, all_passed)
    call assert_xy_data('errorbar()', expected_count, PLOT_TYPE_ERRORBAR, x, y, &
                        all_passed)

    ! Save and verify rendering pipeline accepts the accumulated data
    call savefig(outfile)
    inquire (file=outfile, exist=file_exists, size=file_size)
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

    subroutine assert_bar_data(operation, index, expected_x, expected_y, &
                               expected_horizontal, passed)
        character(len=*), intent(in) :: operation
        integer, intent(in) :: index
        real(8), intent(in) :: expected_x(:), expected_y(:)
        logical, intent(in) :: expected_horizontal
        logical, intent(inout) :: passed
        class(figure_t), pointer :: current_fig

        current_fig => get_global_figure()
        if (.not. has_plot(current_fig, index, operation, passed)) return

        if (current_fig%plots(index)%plot_type /= PLOT_TYPE_BAR) then
            call fail_plot(operation, 'plot_type is not bar', passed)
        end if
        if (.not. allocated(current_fig%plots(index)%bar_x) .or. &
            .not. allocated(current_fig%plots(index)%bar_heights)) then
            call fail_plot(operation, 'bar arrays are not allocated', passed)
            return
        end if
        if (.not. same_values(current_fig%plots(index)%bar_x, expected_x) .or. &
            .not. same_values(current_fig%plots(index)%bar_heights, expected_y) .or. &
            current_fig%plots(index)%bar_horizontal .neqv. expected_horizontal) then
            call fail_plot(operation, 'stored bar data mismatch', passed)
        else
            print *, "PASS: stored bar data after ", operation
        end if
    end subroutine assert_bar_data

    subroutine assert_xy_data(operation, index, expected_type, expected_x, &
                              expected_y, passed)
        character(len=*), intent(in) :: operation
        integer, intent(in) :: index, expected_type
        real(8), intent(in) :: expected_x(:), expected_y(:)
        logical, intent(inout) :: passed
        class(figure_t), pointer :: current_fig

        current_fig => get_global_figure()
        if (.not. has_plot(current_fig, index, operation, passed)) return

        if (current_fig%plots(index)%plot_type /= expected_type) then
            call fail_plot(operation, 'plot_type mismatch', passed)
        end if
        if (.not. allocated(current_fig%plots(index)%x) .or. &
            .not. allocated(current_fig%plots(index)%y)) then
            call fail_plot(operation, 'x/y arrays are not allocated', passed)
            return
        end if
        if (.not. same_values(current_fig%plots(index)%x, expected_x) .or. &
            .not. same_values(current_fig%plots(index)%y, expected_y)) then
            call fail_plot(operation, 'stored x/y data mismatch', passed)
        else
            print *, "PASS: stored x/y data after ", operation
        end if
    end subroutine assert_xy_data

    subroutine assert_histogram_data(operation, index, source_data, passed)
        character(len=*), intent(in) :: operation
        integer, intent(in) :: index
        real(8), intent(in) :: source_data(:)
        logical, intent(inout) :: passed
        real(8), allocatable :: expected_x(:), expected_y(:)

        call expected_histogram_path(source_data, expected_x, expected_y)
        call assert_xy_data(operation, index, PLOT_TYPE_LINE, expected_x, &
                            expected_y, passed)
    end subroutine assert_histogram_data

    subroutine assert_boxplot_data(operation, index, expected_data, passed)
        character(len=*), intent(in) :: operation
        integer, intent(in) :: index
        real(8), intent(in) :: expected_data(:)
        logical, intent(inout) :: passed
        class(figure_t), pointer :: current_fig

        current_fig => get_global_figure()
        if (.not. has_plot(current_fig, index, operation, passed)) return

        if (current_fig%plots(index)%plot_type /= PLOT_TYPE_BOXPLOT) then
            call fail_plot(operation, 'plot_type is not boxplot', passed)
        end if
        if (.not. allocated(current_fig%plots(index)%box_data)) then
            call fail_plot(operation, 'box_data is not allocated', passed)
            return
        end if
        if (.not. same_values(current_fig%plots(index)%box_data, expected_data)) then
            call fail_plot(operation, 'stored boxplot data mismatch', passed)
        else
            print *, "PASS: stored boxplot data after ", operation
        end if
    end subroutine assert_boxplot_data

    subroutine assert_annotation_data(operation, index, expected_text, expected_x, &
                                      expected_y, expect_arrow, passed, arrow_x, &
                                      arrow_y)
        character(len=*), intent(in) :: operation, expected_text
        integer, intent(in) :: index
        real(8), intent(in) :: expected_x, expected_y
        logical, intent(in) :: expect_arrow
        logical, intent(inout) :: passed
        real(8), intent(in), optional :: arrow_x, arrow_y
        class(figure_t), pointer :: current_fig

        current_fig => get_global_figure()
        if (.not. associated(current_fig)) return
        if (index < 1 .or. index > current_fig%annotation_count) return

        if (trim(current_fig%annotations(index)%text) /= expected_text .or. &
            abs(current_fig%annotations(index)%x - expected_x) > 1.0d-10 .or. &
            abs(current_fig%annotations(index)%y - expected_y) > 1.0d-10 .or. &
            current_fig%annotations(index)%has_arrow .neqv. expect_arrow) then
            call fail_plot(operation, 'stored annotation data mismatch', passed)
            return
        end if
        if (expect_arrow .and. present(arrow_x) .and. present(arrow_y)) then
            if (abs(current_fig%annotations(index)%arrow_x - arrow_x) > 1.0d-10 .or. &
                abs(current_fig%annotations(index)%arrow_y - arrow_y) > 1.0d-10) then
                call fail_plot(operation, 'stored arrow data mismatch', passed)
                return
            end if
        end if
        print *, "PASS: stored annotation data after ", operation
    end subroutine assert_annotation_data

    logical function has_plot(current_fig, index, operation, passed)
        class(figure_t), pointer, intent(in) :: current_fig
        integer, intent(in) :: index
        character(len=*), intent(in) :: operation
        logical, intent(inout) :: passed

        has_plot = .false.
        if (.not. associated(current_fig)) then
            call fail_plot(operation, 'global figure not associated', passed)
        else if (index < 1 .or. index > current_fig%plot_count) then
            call fail_plot(operation, 'plot index out of range', passed)
        else
            has_plot = .true.
        end if
    end function has_plot

    subroutine fail_plot(operation, reason, passed)
        character(len=*), intent(in) :: operation, reason
        logical, intent(inout) :: passed

        print *, "FAIL: ", trim(reason), " after ", operation
        passed = .false.
    end subroutine fail_plot

    logical function same_values(actual, expected)
        real(8), intent(in) :: actual(:), expected(:)

        same_values = size(actual) == size(expected)
        if (same_values .and. size(actual) > 0) then
            same_values = all(abs(actual - expected) <= 1.0d-10)
        end if
    end function same_values

    subroutine expected_histogram_path(source_data, expected_x, expected_y)
        real(8), intent(in) :: source_data(:)
        real(8), allocatable, intent(out) :: expected_x(:), expected_y(:)
        real(8) :: bin_edges(11), bin_counts(10), data_min, data_max, bin_width
        integer :: i, bin_index

        data_min = minval(source_data)
        data_max = maxval(source_data)
        if (abs(data_max - data_min) < epsilon(1.0d0)) then
            data_min = data_min - 0.5d0
            data_max = data_max + 0.5d0
        end if
        bin_width = (data_max - data_min)/10.0d0
        do i = 1, 11
            bin_edges(i) = data_min + real(i - 1, 8)*bin_width
        end do
        bin_counts = 0.0d0
        do i = 1, size(source_data)
            bin_index = min(10, max(1, int((source_data(i) - data_min)/bin_width) + 1))
            bin_counts(bin_index) = bin_counts(bin_index) + 1.0d0
        end do

        allocate (expected_x(41), expected_y(41))
        do i = 1, 10
            expected_x(4*(i - 1) + 1) = bin_edges(i)
            expected_y(4*(i - 1) + 1) = 0.0d0
            expected_x(4*(i - 1) + 2) = bin_edges(i)
            expected_y(4*(i - 1) + 2) = bin_counts(i)
            expected_x(4*(i - 1) + 3) = bin_edges(i + 1)
            expected_y(4*(i - 1) + 3) = bin_counts(i)
            expected_x(4*(i - 1) + 4) = bin_edges(i + 1)
            expected_y(4*(i - 1) + 4) = 0.0d0
        end do
        expected_x(41) = bin_edges(1)
        expected_y(41) = 0.0d0
    end subroutine expected_histogram_path

end program test_no_error_stop
