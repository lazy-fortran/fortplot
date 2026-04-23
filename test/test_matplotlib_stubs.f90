program test_matplotlib_stubs
    !! Test that matplotlib stub implementations produce real output files
    use fortplot_matplotlib
    use fortplot_logging, only: log_info
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(8) :: x(10), y(10), data(100)
    integer :: i
    logical :: all_passed, dir_ok

    ! Generate test data
    do i = 1, 10
        x(i) = real(i-1, 8)
        y(i) = real((i-1)**2, 8)
    end do

    do i = 1, 100
        data(i) = real(i, 8) + 0.5d0 * sin(real(i, 8))
    end do

    call create_directory_runtime('build/test/output', dir_ok)
    all_passed = .true.
    call log_info("Testing matplotlib stub implementations...")

    call test_scatter(all_passed)
    call test_bar(all_passed)
    call test_barh(all_passed)
    call test_hist(all_passed)
    call test_text(all_passed)
    call test_annotate(all_passed)

    if (all_passed) then
        call log_info("All matplotlib stub tests passed!")
    else
        call log_info("Some matplotlib stub tests FAILED")
        stop 1
    end if

contains

    subroutine assert_file_written(path, label, passed)
        character(len=*), intent(in) :: path, label
        logical, intent(inout) :: passed
        logical :: exists
        integer :: sz

        inquire(file=path, exist=exists, size=sz)
        if (.not. exists) then
            print *, "  FAIL: ", label, " - file not created: ", path
            passed = .false.
        else if (sz <= 0) then
            print *, "  FAIL: ", label, " - file is empty: ", path
            passed = .false.
        else
            print *, "  PASS: ", label, " (", sz, " bytes)"
        end if
    end subroutine assert_file_written

    subroutine test_scatter(passed)
        logical, intent(inout) :: passed
        call log_info("Testing scatter plot...")
        call figure()
        call scatter(x, y, label="Test scatter")
        call title("Scatter Plot Test")
        call xlabel("X values")
        call ylabel("Y values")
        call savefig("build/test/output/test_scatter_stubs.png")
        call assert_file_written("build/test/output/test_scatter_stubs.png", &
                                 "scatter", passed)
    end subroutine test_scatter

    subroutine test_bar(passed)
        logical, intent(inout) :: passed
        call log_info("Testing bar plot...")
        call figure()
        call bar(x, y, label="Test bars")
        call title("Bar Plot Test")
        call xlabel("Categories")
        call ylabel("Values")
        call savefig("build/test/output/test_bar_stubs.png")
        call assert_file_written("build/test/output/test_bar_stubs.png", &
                                 "bar", passed)
    end subroutine test_bar

    subroutine test_barh(passed)
        logical, intent(inout) :: passed
        call log_info("Testing horizontal bar plot...")
        call figure()
        call barh(x, y, label="Test horizontal bars")
        call title("Horizontal Bar Plot Test")
        call xlabel("Values")
        call ylabel("Categories")
        call savefig("build/test/output/test_barh_stubs.png")
        call assert_file_written("build/test/output/test_barh_stubs.png", &
                                 "barh", passed)
    end subroutine test_barh

    subroutine test_hist(passed)
        logical, intent(inout) :: passed
        call log_info("Testing histogram...")
        call figure()
        call hist(data, bins=20, label="Test histogram")
        call title("Histogram Test")
        call xlabel("Values")
        call ylabel("Frequency")
        call savefig("build/test/output/test_hist_stubs.png")
        call assert_file_written("build/test/output/test_hist_stubs.png", &
                                 "hist", passed)
    end subroutine test_hist

    subroutine test_text(passed)
        logical, intent(inout) :: passed
        call log_info("Testing text annotation...")
        call figure()
        call plot(x, y)
        call text(5.0d0, 25.0d0, "Test annotation")
        call title("Text Annotation Test")
        call savefig("build/test/output/test_text_stubs.png")
        call assert_file_written("build/test/output/test_text_stubs.png", &
                                 "text", passed)
    end subroutine test_text

    subroutine test_annotate(passed)
        logical, intent(inout) :: passed
        call log_info("Testing arrow annotation...")
        call figure()
        call plot(x, y)
        call annotate("Peak", [5.0d0, 25.0d0], [3.0d0, 30.0d0])
        call title("Arrow Annotation Test")
        call savefig("build/test/output/test_annotate_stubs.png")
        call assert_file_written("build/test/output/test_annotate_stubs.png", &
                                 "annotate", passed)
    end subroutine test_annotate

end program test_matplotlib_stubs
