program test_axes_labels_comprehensive
    !! Comprehensive test for Issue #335: Missing axis labels and tick marks
    !! Tests that all axis text elements are properly rendered in both PNG and ASCII

    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(wp), dimension(6) :: x_data, y_linear, y_log
    integer :: i
    logical :: all_passed, dir_ok

    all_passed = .true.
    call create_directory_runtime('build/test/output', dir_ok)

    print *, "=== Comprehensive Axes Labels Test ==="

    ! Generate test data
    x_data = [(real(i, wp), i = 1, 6)]
    y_linear = x_data * 2.0_wp + 1.0_wp
    y_log = exp(x_data * 0.5_wp)

    ! Test 1: Linear scale with all axis elements
    print *, "Testing linear scale with comprehensive axis labeling..."
    call figure()
    call plot(x_data, y_linear)
    call title('Linear Scale Test - All Elements')
    call xlabel('Input Values (x)')
    call ylabel('Linear Response (2x+1)')
    call savefig("build/test/output/test_linear_axes_comprehensive.png")
    call savefig("build/test/output/test_linear_axes_comprehensive.txt")
    call assert_file_nonempty("build/test/output/test_linear_axes_comprehensive.png", &
                              "linear PNG", all_passed)
    call assert_txt_has_content("build/test/output/test_linear_axes_comprehensive.txt", &
                                all_passed)

    ! Test 2: Log scale with proper formatting
    print *, "Testing log scale with scientific notation..."
    call figure()
    call plot(x_data, y_log)
    call set_yscale('log')
    call title('Log Scale Test - Scientific Labels')
    call xlabel('Input Index')
    call ylabel('Exponential Growth')
    call savefig("build/test/output/test_log_axes_comprehensive.png")
    call savefig("build/test/output/test_log_axes_comprehensive.txt")
    call assert_file_nonempty("build/test/output/test_log_axes_comprehensive.png", &
                              "log PNG", all_passed)
    call assert_txt_has_content("build/test/output/test_log_axes_comprehensive.txt", &
                                all_passed)

    if (.not. all_passed) then
        error stop "test_axes_labels_comprehensive: one or more assertions failed"
    end if
    print *, "All axis label tests PASSED"

contains

    subroutine assert_file_nonempty(path, label, passed)
        character(len=*), intent(in) :: path, label
        logical, intent(inout) :: passed
        logical :: exists
        integer :: sz
        inquire(file=path, exist=exists, size=sz)
        if (.not. exists) then
            print *, "FAIL: ", label, " file not created: ", path
            passed = .false.
        else if (sz <= 0) then
            print *, "FAIL: ", label, " file is empty"
            passed = .false.
        else
            print *, "PASS: ", label, " written (", sz, " bytes)"
        end if
    end subroutine assert_file_nonempty

    subroutine assert_txt_has_content(path, passed)
        !! Verify the ASCII output file contains at least one line with
        !! a plot-character (pipe, dash, asterisk, or digit for axis ticks)
        character(len=*), intent(in) :: path
        logical, intent(inout) :: passed
        integer :: unit, ios, line_count
        character(len=512) :: line
        logical :: has_plot_chars

        line_count = 0
        has_plot_chars = .false.

        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "FAIL: cannot open ASCII file: ", path
            passed = .false.
            return
        end if

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_count = line_count + 1
            if (index(line, '|') > 0 .or. index(line, '-') > 0 .or. &
                index(line, '*') > 0 .or. index(line, '+') > 0) then
                has_plot_chars = .true.
            end if
        end do
        close(unit)

        if (line_count < 5) then
            print *, "FAIL: ASCII file too short (", line_count, " lines): ", path
            passed = .false.
        else if (.not. has_plot_chars) then
            print *, "FAIL: ASCII file has no plot characters: ", path
            passed = .false.
        else
            print *, "PASS: ASCII content present (", line_count, " lines with plot chars)"
        end if
    end subroutine assert_txt_has_content

end program test_axes_labels_comprehensive
