program test_ascii_3d_axes
    !! Regression tests for the ASCII 3D axes (refs #2053, #2054).
    !!
    !!   1. #2053: ASCII 3D tick labels are deduplicated - no adjacent duplicate
    !!      y tick-label rows and only a single bottom x tick-label row.
    !!   2. #2054: the projected 3D axes rotate with view_init; two frames at
    !!      different azimuths produce different axis/tick layouts.
    !!
    !! The detectors are exercised against a synthetic bad fixture as well, so a
    !! frame with the old duplication defect fails the assertions.
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: N = 200
    real(wp) :: xs(N), ys(N), zs(N), t
    integer :: i
    logical :: dir_ok
    character(len=*), parameter :: out_dir = 'build/test/output'
    character(len=*), parameter :: frame_a = out_dir//'/ascii_3d_view_a.txt'
    character(len=*), parameter :: frame_b = out_dir//'/ascii_3d_view_b.txt'
    character(len=*), parameter :: bad = out_dir//'/ascii_3d_bad_fixture.txt'

    call create_directory_runtime(out_dir, dir_ok)

    do i = 1, N
        t = real(i - 1, wp)*2.0_wp*3.141592653589793_wp/real(N - 1, wp)
        xs(i) = sin(3.0_wp*t)
        ys(i) = cos(2.0_wp*t)
        zs(i) = sin(t)*cos(t)
    end do

    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_3d_plot(xs, ys, zs, label='Lissajous 3D', linestyle='-')
    call title('Rotating 3D Lissajous')
    call xlim(-1.2_wp, 1.2_wp)
    call ylim(-1.2_wp, 1.2_wp)

    call view_init(elev=30.0_wp, azim=-60.0_wp)
    call savefig(frame_a)
    call view_init(elev=30.0_wp, azim=60.0_wp)
    call savefig(frame_b)

    ! #2053 positive fixtures on a real frame.
    call assert_no_adjacent_duplicate_tick_labels(frame_a, 'frame_a')
    call assert_single_bottom_tick_label_row(frame_a, 'frame_a')

    ! #2054: the axis/tick layout must change with the azimuth.
    call assert_axis_tick_layout_differs(frame_a, frame_b)

    ! #2053 negative fixture: the old-style duplicated frame must be rejected.
    call write_bad_fixture(bad)
    if (.not. has_adjacent_duplicate_tick_labels(bad)) then
        print *, 'FAIL: detector missed adjacent duplicate y tick labels'
        error stop 'dedup detector too weak'
    end if
    if (count_bottom_tick_sequence_rows(bad) < 2) then
        print *, 'FAIL: detector missed duplicate bottom x tick rows'
        error stop 'bottom-row detector too weak'
    end if
    print *, 'PASS: negative fixture correctly flagged'

    print *, 'All ASCII 3D axes regression tests PASSED'

contains

    subroutine assert_no_adjacent_duplicate_tick_labels(path, label)
        character(len=*), intent(in) :: path, label

        if (has_adjacent_duplicate_tick_labels(path)) then
            print *, 'FAIL: adjacent duplicate tick labels in ', label
            error stop 'adjacent duplicate tick labels'
        end if
        print *, 'PASS: no adjacent duplicate tick labels in ', label
    end subroutine assert_no_adjacent_duplicate_tick_labels

    subroutine assert_single_bottom_tick_label_row(path, label)
        character(len=*), intent(in) :: path, label

        if (count_bottom_tick_sequence_rows(path) >= 2) then
            print *, 'FAIL: multiple bottom x tick-label rows in ', label
            error stop 'duplicate bottom tick row'
        end if
        print *, 'PASS: single bottom x tick-label row in ', label
    end subroutine assert_single_bottom_tick_label_row

    subroutine assert_axis_tick_layout_differs(path_a, path_b)
        character(len=*), intent(in) :: path_a, path_b
        character(len=256), allocatable :: la(:), lb(:)
        integer :: na, nb, k, n, diff
        character(len=256) :: sa, sb

        call read_lines(path_a, la, na)
        call read_lines(path_b, lb, nb)
        n = min(na, nb)
        diff = 0
        do k = 1, n
            sa = mask_data_glyphs(la(k))
            sb = mask_data_glyphs(lb(k))
            if (line_has_axis_content(sa) .or. line_has_axis_content(sb)) then
                if (sa /= sb) diff = diff + 1
            end if
        end do
        if (diff == 0) then
            print *, 'FAIL: axis/tick layout identical across azimuths'
            error stop 'axes do not rotate with view_init'
        end if
        print *, 'PASS: axis/tick layout differs across azimuths (', diff, ' rows)'
    end subroutine assert_axis_tick_layout_differs

    logical function has_adjacent_duplicate_tick_labels(path) result(found)
        character(len=*), intent(in) :: path
        character(len=256), allocatable :: lines(:)
        integer :: nlines, k
        character(len=32) :: prev_tok, tok

        found = .false.
        call read_lines(path, lines, nlines)
        prev_tok = ''
        do k = 1, nlines
            tok = left_margin_numeric_token(lines(k))
            if (len_trim(tok) > 0 .and. tok == prev_tok) then
                found = .true.
                return
            end if
            prev_tok = tok
        end do
    end function has_adjacent_duplicate_tick_labels

    integer function count_bottom_tick_sequence_rows(path) result(cnt)
        !! Number of adjacent rows in the bottom band that each carry a full
        !! x-tick sequence (>= 3 numeric tokens). The healthy frame has one.
        character(len=*), intent(in) :: path
        character(len=256), allocatable :: lines(:)
        integer :: nlines, k, lo

        cnt = 0
        call read_lines(path, lines, nlines)
        lo = max(1, nlines - 5)
        do k = lo, nlines
            if (count_numeric_tokens(lines(k)) >= 3) cnt = cnt + 1
        end do
    end function count_bottom_tick_sequence_rows

    subroutine write_bad_fixture(path)
        character(len=*), intent(in) :: path
        integer :: u

        open (newunit=u, file=path, status='replace', action='write')
        write (u, '(A)') '+----------------------------------------+'
        write (u, '(A)') '| 1.0                                    |'
        write (u, '(A)') '| 1.0                                    |'
        write (u, '(A)') '| 0.0                                    |'
        write (u, '(A)') '| -1.0    -0.5    0.0    0.5    1.0       |'
        write (u, '(A)') '|  -1.0    -0.5    0.0    0.5    1.0      |'
        write (u, '(A)') '+----------------------------------------+'
        close (u)
    end subroutine write_bad_fixture

    subroutine read_lines(path, lines, nlines)
        character(len=*), intent(in) :: path
        character(len=256), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: nlines
        integer :: u, ios, cap
        character(len=256) :: buf
        character(len=256), allocatable :: tmp(:)

        cap = 64
        allocate (lines(cap))
        nlines = 0
        open (newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', trim(path)
            error stop 'missing ASCII frame'
        end if
        do
            read (u, '(A)', iostat=ios) buf
            if (ios /= 0) exit
            if (nlines == cap) then
                allocate (tmp(2*cap))
                tmp(1:cap) = lines
                call move_alloc(tmp, lines)
                cap = 2*cap
            end if
            nlines = nlines + 1
            lines(nlines) = buf
        end do
        close (u)
    end subroutine read_lines

    character(len=32) function left_margin_numeric_token(line) result(tok)
        !! First numeric token within the left margin (columns 1..10), where the
        !! 2D y-axis labels used to sit. Empty when the left margin has no label.
        character(len=*), intent(in) :: line
        character(len=32) :: cand
        integer :: j, endc

        tok = ''
        endc = min(len(line), 10)
        j = 1
        do while (j <= endc)
            if (is_number_char(line(j:j)) .and. line(j:j) /= '.') then
                cand = ''
                do while (j <= len(line))
                    if (.not. is_number_char(line(j:j))) exit
                    cand = trim(cand)//line(j:j)
                    j = j + 1
                end do
                if (token_is_numeric(cand)) then
                    tok = cand
                    return
                end if
            else
                j = j + 1
            end if
        end do
    end function left_margin_numeric_token

    integer function count_numeric_tokens(line) result(cnt)
        character(len=*), intent(in) :: line
        integer :: j
        character(len=32) :: cand

        cnt = 0
        j = 1
        do while (j <= len(line))
            if (is_number_char(line(j:j)) .and. line(j:j) /= '.') then
                cand = ''
                do while (j <= len(line))
                    if (.not. is_number_char(line(j:j))) exit
                    cand = trim(cand)//line(j:j)
                    j = j + 1
                end do
                if (token_is_numeric(cand)) cnt = cnt + 1
            else
                j = j + 1
            end if
        end do
    end function count_numeric_tokens

    logical function is_number_char(c) result(res)
        character(len=1), intent(in) :: c

        res = (c >= '0' .and. c <= '9')
        if (c == '.') res = .true.
        if (c == '-') res = .true.
    end function is_number_char

    logical function token_is_numeric(tok) result(res)
        character(len=*), intent(in) :: tok
        integer :: j
        logical :: has_digit

        res = .false.
        has_digit = .false.
        if (len_trim(tok) == 0) return
        do j = 1, len_trim(tok)
            if (tok(j:j) >= '0' .and. tok(j:j) <= '9') has_digit = .true.
        end do
        res = has_digit
    end function token_is_numeric

    character(len=256) function mask_data_glyphs(line) result(out)
        !! Replace curve/data glyphs with spaces so only the projected axis
        !! frame (spine dots) and tick digits remain for the layout comparison.
        character(len=*), intent(in) :: line
        integer :: j
        character(len=1) :: c

        out = ''
        do j = 1, min(len(line), len(out))
            c = line(j:j)
            if (c == '#' .or. c == '@' .or. c == '-' .or. c == '*') then
                out(j:j) = ' '
            else
                out(j:j) = c
            end if
        end do
    end function mask_data_glyphs

    logical function line_has_axis_content(line) result(res)
        character(len=*), intent(in) :: line
        integer :: j
        character(len=1) :: c

        res = .false.
        do j = 1, len(line)
            c = line(j:j)
            if (c == '.') res = .true.
            if (c >= '0' .and. c <= '9') res = .true.
        end do
    end function line_has_axis_content

end program test_ascii_3d_axes
