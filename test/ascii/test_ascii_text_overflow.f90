program test_ascii_text_overflow
    !! Regression test for issue #1706: ASCII text annotation overflow
    !!
    !! Verifies that text elements (legend entries, annotations, tick labels)
    !! never overflow past the right frame border character.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(wp), dimension(100) :: x, y
    integer :: i
    character(len=*), parameter :: outfile = 'build/test/output/ascii_overflow_test.txt'
    character(len=*), parameter :: footer_outfile = &
        'build/test/output/ascii_footer_test.txt'
    logical :: dir_ok, file_exists
    integer :: file_size, unit, ios, nlines, overflow_count
    character(len=512) :: line
    character(len=1) :: char_before_border

    call create_directory_runtime('build/test/output', dir_ok)

    ! Generate data that produces long legend labels
    x = [(real(i, wp), i=0, size(x) - 1)] * 0.06_wp
    y = sin(x) * exp(-x / 4.0_wp)

    ! Create figure with long legend labels that could overflow
    call figure(figsize=[8.0_wp, 6.0_wp])
    call title("Overflow Test")
    call plot(x, y, label="Damped sine: sin(x)e^-x/4", linestyle="-")
    call plot(x, -y, label="Quadratic: 0.1(x-3)^2 - 0.3", linestyle=":")
    call plot(x, y * 0.5_wp, label="Exponential: e^-x - 0.5", linestyle="--")

    ! Add text near right edge to test clamping
    call text(5.5_wp, 0.4_wp, "Right Edge", &
              coord_type=COORD_DATA, font_size=10.0_wp)

    call legend("upper right")
    call savefig(outfile)

    ! Verify file exists
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: ASCII output file not created"
        stop 1
    end if
    if (file_size <= 0) then
        print *, "FAIL: ASCII output file is empty"
        stop 1
    end if
    print *, "PASS: ASCII file created (", file_size, " bytes)"

    ! Check every line for overflow: no line should exceed frame width
    ! Frame width = plot_width + 2 (left/right border)
    nlines = 0
    overflow_count = 0
    open(newunit=unit, file=outfile, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "FAIL: Cannot open ASCII output file"
        stop 1
    end if

    do
        read(unit, '(A)', iostat=ios) line
        if (ios > 0) exit
        nlines = nlines + 1
        if (ios == -1 .and. len_trim(line) == 0) cycle

        ! Frame lines start with +
        if (index(line, '+') == 1) then
            if (len_trim(line) > 82) then
                overflow_count = overflow_count + 1
                print *, "FAIL: Frame line", nlines, "overflows (", &
                       len_trim(line), "chars)"
            end if
        else if (index(line, '|') == 1) then
            ! Content lines: check for overflow
            if (len_trim(line) > 82) then
                overflow_count = overflow_count + 1
                print *, "FAIL: Content line", nlines, "overflows (", &
                       len_trim(line), "chars)"
            end if
            ! Check for arrow characters touching the right border
            if (len_trim(line) >= 81) then
                char_before_border = line(81:81)
                if (char_before_border == '>' .or. char_before_border == '<') then
                    overflow_count = overflow_count + 1
                    print *, "FAIL: Arrow char at right border, line", nlines
                end if
            end if
        end if
    end do
    close(unit)

    if (overflow_count > 0) then
        print *, "FAIL:", overflow_count, "overflow violations detected"
        stop 1
    end if

    print *, "PASS: No text overflow detected (", nlines, " lines checked)"
    print *, "PASS: Issue #1706 regression guard passed"

    call test_figure_footer_avoids_spine()

contains

    subroutine test_figure_footer_avoids_spine()
        real(wp) :: x_small(40), y_small(40)
        integer :: k

        do k = 1, size(x_small)
            x_small(k) = real(k - 1, wp)/real(size(x_small) - 1, wp)
            y_small(k) = x_small(k)**2
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call plot(x_small, y_small)
        call xlabel('x')
        call ylabel('y')
        call text(0.02_wp, 0.02_wp, 'Footer note', coord_type=COORD_FIGURE)
        call savefig(footer_outfile)

        call assert_footer_clear_of_spine(footer_outfile)
        print *, 'PASS: Figure-coordinate footer avoids the axis spine'
    end subroutine test_figure_footer_avoids_spine

    subroutine assert_footer_clear_of_spine(path)
        character(len=*), intent(in) :: path
        integer :: footer_unit, footer_ios
        character(len=512) :: footer_line
        logical :: found

        found = .false.
        open(newunit=footer_unit, file=path, status='old', action='read', &
             iostat=footer_ios)
        if (footer_ios /= 0) then
            print *, 'FAIL: cannot open ', path
            stop 1
        end if

        do
            read(footer_unit, '(A)', iostat=footer_ios) footer_line
            if (footer_ios /= 0) exit
            if (index(footer_line, 'Footer note') > 0) then
                found = .true.
                if (index(footer_line, '--------') > 0 .or. &
                    index(footer_line, '+---') > 0) then
                    print *, 'FAIL: footer overlapped the axis spine'
                    stop 1
                end if
            end if
        end do
        close(footer_unit)

        if (.not. found) then
            print *, 'FAIL: footer annotation was not rendered'
            stop 1
        end if
    end subroutine assert_footer_clear_of_spine

end program test_ascii_text_overflow
