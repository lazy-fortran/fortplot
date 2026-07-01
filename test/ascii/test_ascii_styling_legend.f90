program test_ascii_styling_legend
    !! Regression test for issue #2068: text/ASCII legend placement and readable
    !! handles/labels for the styling_demo line_styles output.
    !!
    !! Verifies the six line-style legend entries render with exact labels,
    !! style-distinct handles, an invisible-line entry with no misleading line
    !! handle, and a clean legend block that plot glyphs do not overwrite.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    character(len=*), parameter :: outfile = 'build/test/output/line_styles.txt'
    integer, parameter :: max_lines = 200
    character(len=256) :: lines(max_lines)
    integer :: nlines
    integer :: failures

    failures = 0

    call render_styling_line_styles_text(outfile)
    call read_lines(outfile, lines, nlines)

    call assert_contains_line('Solid (-)')
    call assert_contains_line('Dashed (--)')
    call assert_contains_line('Dotted (:)')
    call assert_contains_line('Dash-dot (-.)')
    call assert_contains_line('None (invisible)')
    call assert_contains_line('Markers only')

    call assert_label_once('Solid (-)')
    call assert_label_once('Dash-dot (-.)')
    call assert_label_once('None (invisible)')

    call assert_not_contains('MDashrd')

    call assert_handle('--- Solid (-)')
    call assert_handle('- - Dashed (--)')
    call assert_handle('... Dotted (:)')
    call assert_handle('-.- Dash-dot (-.)')
    call assert_handle('o  Markers only')

    call assert_no_line_handle('None (invisible)')
    call assert_dashdot_distinct()
    call assert_clean_left_margin('--- Solid (-)')
    call assert_clean_left_margin('-.- Dash-dot (-.)')
    call assert_legend_block_inside_frame()
    call assert_label_not_on_axis('Solid (-)')
    call assert_label_not_on_axis('Markers only')

    if (failures > 0) then
        print *, "FAIL:", failures, "assertion(s) failed"
        stop 1
    end if
    print *, "PASS: styling legend handles and placement verified"

contains

    subroutine render_styling_line_styles_text(path)
        character(len=*), intent(in) :: path
        real(wp), dimension(50) :: x, y1, y2, y3, y4, y5, y6
        integer :: i
        logical :: dir_ok

        call create_directory_runtime('build/test/output', dir_ok)

        do i = 1, 50
            x(i) = real(i - 1, wp) * 0.2_wp
            y1(i) = sin(x(i)) + 2.5_wp
            y2(i) = cos(x(i)) + 1.5_wp
            y3(i) = sin(x(i) * 2.0_wp) + 0.5_wp
            y4(i) = cos(x(i) * 3.0_wp) - 0.5_wp
            y5(i) = sin(x(i) * 0.5_wp) - 1.5_wp
            y6(i) = cos(x(i) * 0.5_wp) - 2.5_wp
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call plot(x, y1, label='Solid (-)', linestyle=LINESTYLE_SOLID)
        call plot(x, y2, label='Dashed (--)', linestyle=LINESTYLE_DASHED)
        call plot(x, y3, label='Dotted (:)', linestyle=LINESTYLE_DOTTED)
        call plot(x, y4, label='Dash-dot (-.)', linestyle=LINESTYLE_DASHDOT)
        call plot(x, y5, label='None (invisible)', linestyle=LINESTYLE_NONE)
        call plot(x, y6, label='Markers only', linestyle='o')
        call title('Line Style Reference')
        call xlabel('X values')
        call ylabel('Y values')
        call legend()
        ! Mirror styling_demo: PNG/PDF are saved first, so the shared 'best'
        ! placement is resolved before the text save. This is the path that
        ! corrupted labels (issue #2068), so exercise it here.
        call savefig('build/test/output/line_styles_styling.png')
        call savefig('build/test/output/line_styles_styling.pdf')
        call savefig(path)
    end subroutine render_styling_line_styles_text

    subroutine read_lines(path, buf, n)
        character(len=*), intent(in) :: path
        character(len=256), intent(out) :: buf(:)
        integer, intent(out) :: n
        integer :: unit, ios

        n = 0
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "FAIL: cannot read ", path
            stop 1
        end if
        do
            if (n >= size(buf)) exit
            read(unit, '(A)', iostat=ios) buf(n + 1)
            if (ios /= 0) exit
            n = n + 1
        end do
        close(unit)
    end subroutine read_lines

    integer function count_lines_with(substr) result(cnt)
        character(len=*), intent(in) :: substr
        integer :: i

        cnt = 0
        do i = 1, nlines
            if (index(lines(i), substr) > 0) cnt = cnt + 1
        end do
    end function count_lines_with

    integer function first_line_with(substr) result(idx)
        character(len=*), intent(in) :: substr
        integer :: i

        idx = 0
        do i = 1, nlines
            if (index(lines(i), substr) > 0) then
                idx = i
                return
            end if
        end do
    end function first_line_with

    subroutine assert_contains_line(substr)
        character(len=*), intent(in) :: substr

        if (count_lines_with(substr) < 1) then
            print *, "FAIL: missing legend text '", substr, "'"
            failures = failures + 1
        end if
    end subroutine assert_contains_line

    subroutine assert_label_once(substr)
        character(len=*), intent(in) :: substr
        integer :: cnt

        cnt = count_lines_with(substr)
        if (cnt /= 1) then
            print *, "FAIL: label '", substr, "' expected once, found", cnt
            failures = failures + 1
        end if
    end subroutine assert_label_once

    subroutine assert_not_contains(substr)
        character(len=*), intent(in) :: substr

        if (count_lines_with(substr) > 0) then
            print *, "FAIL: corrupted text '", substr, "' present"
            failures = failures + 1
        end if
    end subroutine assert_not_contains

    subroutine assert_handle(handle_label)
        !! The exact "<handle> <label>" must appear intact, proving the style
        !! handle is correct and no plot glyph broke the label.
        character(len=*), intent(in) :: handle_label

        if (count_lines_with(handle_label) < 1) then
            print *, "FAIL: missing handle+label '", handle_label, "'"
            failures = failures + 1
        end if
    end subroutine assert_handle

    subroutine assert_no_line_handle(label)
        !! LINESTYLE_NONE must not draw a line handle: the handle slot ahead of
        !! the label is blank, so no dash/dot glyph precedes it.
        character(len=*), intent(in) :: label
        integer :: idx, col

        idx = first_line_with(label)
        if (idx == 0) then
            print *, "FAIL: label '", label, "' not found for handle check"
            failures = failures + 1
            return
        end if
        col = index(lines(idx), label)
        if (col >= 5) then
            if (index(lines(idx)(col - 4:col - 1), '-') > 0 .or. &
                index(lines(idx)(col - 4:col - 1), '.') > 0) then
                print *, "FAIL: invisible-line entry has misleading line handle"
                failures = failures + 1
            end if
        end if
    end subroutine assert_no_line_handle

    subroutine assert_dashdot_distinct()
        !! The dash-dot handle must differ from solid, dashed, and dotted.
        if (index('-.-', '---') > 0) then
            failures = failures + 1
        end if
        if (count_lines_with('-.- Dash-dot') < 1) then
            print *, "FAIL: dash-dot handle not distinct/present"
            failures = failures + 1
        end if
        if (count_lines_with('--- Dash-dot') > 0 .or. &
            count_lines_with('- - Dash-dot') > 0 .or. &
            count_lines_with('... Dash-dot') > 0) then
            print *, "FAIL: dash-dot rendered with a non-dashdot handle"
            failures = failures + 1
        end if
    end subroutine assert_dashdot_distinct

    subroutine assert_clean_left_margin(handle_label)
        !! The cell immediately left of a legend handle must be blank so plot
        !! glyphs do not run into the legend block.
        character(len=*), intent(in) :: handle_label
        integer :: idx, col

        idx = first_line_with(handle_label)
        if (idx == 0) then
            print *, "FAIL: '", handle_label, "' not found for margin check"
            failures = failures + 1
            return
        end if
        col = index(lines(idx), handle_label)
        if (col >= 2) then
            if (lines(idx)(col - 1:col - 1) /= ' ') then
                print *, "FAIL: plot glyph overwrites legend left margin near '", &
                    handle_label, "'"
                failures = failures + 1
            end if
        end if
    end subroutine assert_clean_left_margin

    subroutine assert_legend_block_inside_frame()
        !! Every legend row sits between the frame borders and never touches the
        !! right border character.
        character(len=*), parameter :: labels(6) = [ character(len=16) :: &
            'Solid (-)', 'Dashed (--)', 'Dotted (:)', 'Dash-dot (-.)', &
            'None (invisible)', 'Markers only']
        integer :: k, idx, last

        do k = 1, size(labels)
            idx = first_line_with(trim(labels(k)))
            if (idx == 0) cycle
            if (lines(idx)(1:1) /= '|') then
                print *, "FAIL: legend row not inside left frame: ", trim(labels(k))
                failures = failures + 1
            end if
            last = len_trim(lines(idx))
            if (lines(idx)(last:last) /= '|') then
                print *, "FAIL: legend row not inside right frame: ", trim(labels(k))
                failures = failures + 1
            end if
            if (last >= 2) then
                if (lines(idx)(last - 1:last - 1) /= ' ') then
                    print *, "FAIL: legend text touches right border: ", trim(labels(k))
                    failures = failures + 1
                end if
            end if
        end do
    end subroutine assert_legend_block_inside_frame

    subroutine assert_label_not_on_axis(label)
        !! Legend labels must not land on a frame/border row.
        character(len=*), intent(in) :: label
        integer :: idx

        idx = first_line_with(label)
        if (idx == 0) return
        if (index(lines(idx), '+---') > 0) then
            print *, "FAIL: legend label '", label, "' overlaps the axis border"
            failures = failures + 1
        end if
    end subroutine assert_label_not_on_axis

end program test_ascii_styling_legend
