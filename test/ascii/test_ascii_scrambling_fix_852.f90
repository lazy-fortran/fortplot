program test_ascii_scrambling_fix_852
    !! Test case for ASCII text scrambling fix (Issue #852)
    !! Verifies that text elements appear in the output without character corruption
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    logical :: all_passed, dir_ok

    all_passed = .true.
    call create_directory_runtime('build/test/output', dir_ok)

    print *, "=== ISSUE #852 ASCII TEXT SCRAMBLING FIX TEST ==="

    ! Test 1: Title text must appear in output (regression guard)
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 4.0_wp, 9.0_wp])
    call title("ScrambleTest852")
    call savefig("build/test/output/test_852_original_scenario.txt")
    call assert_txt_contains("build/test/output/test_852_original_scenario.txt", &
                             "ScrambleTest852", "title appears in output", all_passed)
    call assert_txt_has_plot_chars("build/test/output/test_852_original_scenario.txt", &
                                   all_passed)

    ! Test 2: Multiple text elements - file must be non-trivial
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 2.0_wp, 3.0_wp])
    call title("MultiLabelTest")
    call text(1.2_wp, 1.8_wp, "LabelA")
    call text(2.0_wp, 1.9_wp, "LabelB")
    call text(2.8_wp, 1.7_wp, "LabelC")
    call savefig("build/test/output/test_852_multiple_text.txt")
    call assert_txt_contains("build/test/output/test_852_multiple_text.txt", &
                             "MultiLabelTest", "multi-label title visible", all_passed)
    call assert_txt_has_plot_chars("build/test/output/test_852_multiple_text.txt", &
                                   all_passed)

    if (.not. all_passed) then
        error stop "test_ascii_scrambling_fix_852: assertions failed"
    end if
    print *, "PASS: Issue #852 regression guards passed"

contains

    subroutine assert_txt_contains(path, needle, label, passed)
        character(len=*), intent(in) :: path, needle, label
        logical, intent(inout) :: passed
        integer :: unit, ios
        character(len=2048) :: line
        logical :: found

        found = .false.
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "FAIL (", label, "): cannot open ", path
            passed = .false.
            return
        end if
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, needle) > 0) then
                found = .true.
                exit
            end if
        end do
        close(unit)
        if (.not. found) then
            print *, "FAIL (", label, "): '", needle, "' not found in ", path
            passed = .false.
        else
            print *, "PASS (", label, "): '", needle, "' found"
        end if
    end subroutine assert_txt_contains

    subroutine assert_txt_has_plot_chars(path, passed)
        character(len=*), intent(in) :: path
        logical, intent(inout) :: passed
        integer :: unit, ios, nlines
        character(len=512) :: line
        logical :: has_chars

        nlines = 0
        has_chars = .false.
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "FAIL: cannot open for plot-char check: ", path
            passed = .false.
            return
        end if
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            if (index(line, '|') > 0 .or. index(line, '-') > 0 .or. &
                index(line, '*') > 0 .or. index(line, '+') > 0) then
                has_chars = .true.
            end if
        end do
        close(unit)
        if (nlines < 5 .or. .not. has_chars) then
            print *, "FAIL: ASCII canvas lacks plot content in ", path
            passed = .false.
        else
            print *, "PASS: ASCII canvas has plot content (", nlines, " lines)"
        end if
    end subroutine assert_txt_has_plot_chars

end program test_ascii_scrambling_fix_852
