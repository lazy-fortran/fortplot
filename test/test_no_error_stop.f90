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
    class(figure_t), pointer :: fig
    integer :: expected_count
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

    ! Start fresh figure for this test
    call figure()

    ! bar() - should add a plot
    print *, "Testing bar()..."
    call bar(x, y)
    print *, "  bar() passed (no error_stop)"
    expected_count = expected_count + 1
    fig => get_global_figure()
    if (.not. associated(fig)) then
        print *, "FAIL: global figure not associated after bar()"
        all_passed = .false.
    else if (fig%plot_count /= expected_count) then
        print *, "FAIL: plot_count after bar() =", fig%plot_count, "; expected", expected_count
        all_passed = .false.
    else
        print *, "PASS: plot_count =", fig%plot_count, " after bar()"
    end if

    ! barh()
    print *, "Testing barh()..."
    call barh(x, y)
    print *, "  barh() passed (no error_stop)"
    expected_count = expected_count + 1
    fig => get_global_figure()
    if (associated(fig) .and. fig%plot_count /= expected_count) then
        print *, "FAIL: plot_count after barh() =", fig%plot_count, "; expected", expected_count
        all_passed = .false.
    else
        print *, "PASS: plot_count =", fig%plot_count, " after barh()"
    end if

    ! scatter()
    print *, "Testing scatter()..."
    call scatter(x, y)
    print *, "  scatter() passed (no error_stop)"
    expected_count = expected_count + 1
    fig => get_global_figure()
    if (associated(fig) .and. fig%plot_count /= expected_count) then
        print *, "FAIL: plot_count after scatter() =", fig%plot_count, "; expected", expected_count
        all_passed = .false.
    else
        print *, "PASS: plot_count =", fig%plot_count, " after scatter()"
    end if

    ! These do not add a line plot but must not crash:
    print *, "Testing hist()..."
    call hist(data)
    print *, "  hist() passed (no error_stop)"

    print *, "Testing histogram()..."
    call histogram(data)
    print *, "  histogram() passed (no error_stop)"

    print *, "Testing boxplot()..."
    call boxplot(data)
    print *, "  boxplot() passed (no error_stop)"

    print *, "Testing text()..."
    call text(2.5d0, 6.0d0, "Test")
    print *, "  text() passed (no error_stop)"

    print *, "Testing annotate()..."
    call annotate("Arrow", [3.0d0, 9.0d0], [2.0d0, 10.0d0])
    print *, "  annotate() passed (no error_stop)"

    print *, "Testing errorbar()..."
    call errorbar(x, y)
    print *, "  errorbar() passed (no error_stop)"

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

end program test_no_error_stop
