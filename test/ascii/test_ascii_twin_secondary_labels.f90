program test_ascii_twin_secondary_labels
    !! Regression test for issue #2066: ASCII twin-axis (twinx) secondary y tick
    !! labels must sit in the reserved right-hand band, never inside the primary
    !! data glyph area and never interrupting the bottom axis spine.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: n = 60
    integer, parameter :: MAX_LINES = 400
    real(wp) :: x(n), y(n), y2(n)
    type(figure_t) :: fig
    integer :: i
    character(len=*), parameter :: outfile = &
        'build/test/output/test_ascii_twin_secondary_labels.txt'
    logical :: dir_ok

    character(len=512) :: lines(MAX_LINES)
    integer :: nlines, b1, b2, width, ncanvas
    character(len=512) :: canvas(MAX_LINES)
    integer :: right_data_col
    integer :: alpha_in_data, alpha_in_band, digit_in_data
    integer :: spine_row, spine_alpha

    call create_directory_runtime('build/test/output', dir_ok)

    do i = 1, n
        x(i) = real(i, wp)
        y(i) = 2.0_wp * real(i, wp)
        y2(i) = 5.0_wp * exp(0.12_wp * real(i, wp))
    end do

    call fig%initialize(80, 24)
    call fig%add_plot(x, y, label='primary')
    call fig%set_ylabel('Primary')
    call fig%twinx()
    call fig%add_plot(x, y2, label='secondary')
    call fig%set_yscale('log')
    call fig%set_ylabel('Secondary')
    call fig%savefig(outfile)

    call read_lines(outfile, lines, nlines)
    call find_canvas(lines, nlines, b1, b2, width)
    if (b1 <= 0 .or. b2 <= 0 .or. width <= 0) then
        print *, 'FAIL: could not locate ASCII canvas borders'
        stop 1
    end if

    ncanvas = 0
    do i = b1 + 1, b2 - 1
        ncanvas = ncanvas + 1
        canvas(ncanvas) = lines(i)(2:1 + width)
    end do
    if (ncanvas <= 0) then
        print *, 'FAIL: empty ASCII canvas'
        stop 1
    end if

    right_data_col = rightmost_data_col(canvas, ncanvas, width)
    if (right_data_col <= 0) then
        print *, 'FAIL: no plotted data found on canvas'
        stop 1
    end if
    if (right_data_col >= width) then
        print *, 'FAIL: no reserved band; data fills the full canvas width'
        stop 1
    end if

    call count_alpha(canvas, ncanvas, width, right_data_col, &
        alpha_in_data, alpha_in_band, digit_in_data)

    ! Negative fixture: any secondary tick-label letter inside the primary data
    ! bounds (columns <= right_data_col) is the bug and must fail.
    if (alpha_in_data > 0) then
        print *, 'FAIL: secondary label characters found inside the data area', &
            ' (count=', alpha_in_data, ')'
        stop 1
    end if

    ! Positive fixture: secondary labels are still rendered, in the reserved band.
    if (alpha_in_band <= 0) then
        print *, 'FAIL: secondary tick labels missing from the reserved band'
        stop 1
    end if

    ! Positive fixture: primary numeric labels still render inside the plot.
    if (digit_in_data <= 0) then
        print *, 'FAIL: primary tick labels missing from the plot'
        stop 1
    end if

    ! Positive fixture: bottom axis spine (the dash row) is not interrupted by
    ! right-axis tick-label letters within the data columns.
    spine_row = bottom_spine_row(canvas, ncanvas, width)
    spine_alpha = row_alpha_count(canvas(spine_row), right_data_col)
    if (spine_alpha > 0) then
        print *, 'FAIL: bottom axis spine interrupted by tick-label text'
        stop 1
    end if

    print *, 'PASS: ASCII twin secondary labels stay in the reserved band'

contains

    subroutine read_lines(fname, buf, count)
        character(len=*), intent(in) :: fname
        character(len=512), intent(out) :: buf(:)
        integer, intent(out) :: count
        integer :: unit, ios

        count = 0
        open (newunit=unit, file=fname, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', fname
            stop 1
        end if
        do
            if (count >= size(buf)) exit
            read (unit, '(A)', iostat=ios) buf(count + 1)
            if (ios /= 0) exit
            count = count + 1
        end do
        close (unit)
    end subroutine read_lines

    subroutine find_canvas(buf, count, first_border, second_border, w)
        character(len=512), intent(in) :: buf(:)
        integer, intent(in) :: count
        integer, intent(out) :: first_border, second_border, w
        integer :: i

        first_border = 0
        second_border = 0
        w = 0
        do i = 1, count
            if (is_border(buf(i))) then
                if (first_border == 0) then
                    first_border = i
                    w = len_trim(buf(i)) - 2
                else
                    second_border = i
                    exit
                end if
            end if
        end do
    end subroutine find_canvas

    logical function is_border(line)
        character(len=*), intent(in) :: line
        integer :: i, ln
        character(len=1) :: c

        is_border = .false.
        ln = len_trim(line)
        if (ln < 3) return
        if (line(1:1) /= '+') return
        if (line(ln:ln) /= '+') return
        do i = 2, ln - 1
            c = line(i:i)
            if (c /= '-') return
        end do
        is_border = .true.
    end function is_border

    integer function rightmost_data_col(rows, nrows, w) result(col)
        character(len=512), intent(in) :: rows(:)
        integer, intent(in) :: nrows, w
        integer :: r, c

        col = 0
        do r = 1, nrows
            do c = 1, w
                if (is_data_glyph(rows(r)(c:c))) col = max(col, c)
            end do
        end do
    end function rightmost_data_col

    logical function is_data_glyph(c)
        character(len=1), intent(in) :: c

        select case (c)
        case ('#', '*', 'o', '.', ':', '=', '@', '%', '-', '+', '|')
            is_data_glyph = .true.
        case default
            is_data_glyph = .false.
        end select
    end function is_data_glyph

    subroutine count_alpha(rows, nrows, w, split_col, in_data, in_band, digits_data)
        character(len=512), intent(in) :: rows(:)
        integer, intent(in) :: nrows, w, split_col
        integer, intent(out) :: in_data, in_band, digits_data
        integer :: r, c
        character(len=1) :: ch

        in_data = 0
        in_band = 0
        digits_data = 0
        do r = 1, nrows
            do c = 1, w
                ch = rows(r)(c:c)
                if (is_alpha(ch)) then
                    if (c <= split_col) then
                        in_data = in_data + 1
                    else
                        in_band = in_band + 1
                    end if
                else if (is_digit(ch) .and. c <= split_col) then
                    digits_data = digits_data + 1
                end if
            end do
        end do
    end subroutine count_alpha

    integer function bottom_spine_row(rows, nrows, w) result(row)
        character(len=512), intent(in) :: rows(:)
        integer, intent(in) :: nrows, w
        integer :: r, c, dashes, best

        row = 1
        best = -1
        do r = 1, nrows
            dashes = 0
            do c = 1, w
                if (rows(r)(c:c) == '-') dashes = dashes + 1
            end do
            if (dashes > best) then
                best = dashes
                row = r
            end if
        end do
    end function bottom_spine_row

    integer function row_alpha_count(line, up_to_col) result(cnt)
        character(len=*), intent(in) :: line
        integer, intent(in) :: up_to_col
        integer :: c

        cnt = 0
        do c = 1, up_to_col
            if (is_alpha(line(c:c))) cnt = cnt + 1
        end do
    end function row_alpha_count

    logical function is_alpha(c)
        character(len=1), intent(in) :: c
        integer :: k

        k = iachar(c)
        is_alpha = (k >= iachar('A') .and. k <= iachar('Z')) .or. &
            (k >= iachar('a') .and. k <= iachar('z'))
    end function is_alpha

    logical function is_digit(c)
        character(len=1), intent(in) :: c
        integer :: k

        k = iachar(c)
        is_digit = k >= iachar('0') .and. k <= iachar('9')
    end function is_digit

end program test_ascii_twin_secondary_labels
