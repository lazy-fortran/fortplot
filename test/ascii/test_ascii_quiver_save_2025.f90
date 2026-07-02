program test_ascii_quiver_save_2025
    !! Text-backend quiver rendering (issue #2071).
    !!
    !! Proves that quiver arrows project into the interior plot area as eight
    !! compass ASCII glyphs, spread across multiple rows and columns, never land
    !! on the frame border, never corrupt tick/axis labels, and that a smaller
    !! user scale lowers the visible glyph occupancy.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, quiver, xlabel, ylabel, title, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: ng = 7
    integer, parameter :: n = ng*ng
    real(wp) :: x(n), y(n), u(n), v(n)
    integer :: i, j, k
    real(wp) :: xi, yj
    logical :: dir_ok
    character(len=*), parameter :: outfile = &
        'build/test/output/ascii_quiver_save_2025.txt'
    character(len=*), parameter :: scaledfile = &
        'build/test/output/ascii_quiver_scaled_2025.txt'
    integer :: default_glyphs, scaled_glyphs

    call create_directory_runtime('build/test/output', dir_ok)

    ! Circular flow field over [-2, 2], same shape as the quiver demo.
    k = 0
    do j = 1, ng
        do i = 1, ng
            k = k + 1
            xi = -2.0_wp + 4.0_wp*real(i - 1, wp)/real(ng - 1, wp)
            yj = -2.0_wp + 4.0_wp*real(j - 1, wp)/real(ng - 1, wp)
            x(k) = xi
            y(k) = yj
            u(k) = -yj
            v(k) = xi
        end do
    end do

    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v)
    call xlabel('X')
    call ylabel('Y')
    call title('Quiver Plot Demo - Circular Flow')
    call savefig(outfile)

    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, scale=0.5_wp)
    call xlabel('X')
    call ylabel('Y')
    call title('Quiver Plot Demo - Smaller Arrows')
    call savefig(scaledfile)

    call check_field(outfile, default_glyphs)
    call check_field(scaledfile, scaled_glyphs)

    if (scaled_glyphs < 1) then
        print *, 'FAIL: scaled quiver produced no arrows'
        stop 1
    end if
    if (scaled_glyphs >= default_glyphs) then
        print *, 'FAIL: scale=0.5 did not lower quiver occupancy: ', &
            scaled_glyphs, ' >= ', default_glyphs
        stop 1
    end if

    print *, 'PASS: ASCII quiver renders a clipped compass field inside the frame'

contains

    subroutine check_field(fname, glyph_count)
        !! Load a saved quiver .txt and assert the interior placement policy.
        character(len=*), intent(in) :: fname
        integer, intent(out) :: glyph_count

        integer, parameter :: max_lines = 200
        character(len=512) :: lines(max_lines)
        integer :: nlines, unit, ios, li, col, width
        integer :: top_frame, bottom_frame
        integer :: rows_seen(max_lines), cols_seen(512)
        integer :: n_rows, n_cols, has_right
        logical :: file_exists
        character(len=1) :: ch

        inquire (file=fname, exist=file_exists)
        if (.not. file_exists) then
            print *, 'FAIL: ASCII quiver file missing: ', trim(fname)
            stop 1
        end if

        open (newunit=unit, file=fname, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', trim(fname)
            stop 1
        end if
        nlines = 0
        do
            if (nlines >= max_lines) exit
            read (unit, '(A)', iostat=ios) lines(nlines + 1)
            if (ios /= 0) exit
            nlines = nlines + 1
        end do
        close (unit)

        top_frame = 0
        bottom_frame = 0
        do li = 1, nlines
            if (is_frame_line(lines(li))) then
                if (top_frame == 0) then
                    top_frame = li
                else
                    bottom_frame = li
                end if
            end if
        end do
        if (top_frame == 0 .or. bottom_frame == 0) then
            print *, 'FAIL: could not locate the ASCII frame in ', trim(fname)
            stop 1
        end if

        ! No arrow glyph may sit inside a numeric label anywhere.
        do li = 1, nlines
            call assert_no_label_corruption(lines(li), fname)
        end do

        ! Axis tick labels must survive the vector field intact.
        call assert_label_present(lines, nlines, '-2.0', fname)
        call assert_label_present(lines, nlines, '2.0', fname)
        call assert_label_present(lines, nlines, '0.0', fname)

        width = len_trim(lines(top_frame))
        glyph_count = 0
        n_rows = 0
        n_cols = 0
        has_right = 0
        do li = top_frame + 1, bottom_frame - 1
            do col = 2, width - 1
                ch = lines(li)(col:col)
                if (is_arrow_glyph(ch)) then
                    if (col <= 1 .or. col >= width) then
                        print *, 'FAIL: quiver glyph on the frame border in ', &
                            trim(fname)
                        stop 1
                    end if
                    glyph_count = glyph_count + 1
                    if (ch == '>') has_right = 1
                    call record_unique(rows_seen, n_rows, li)
                    call record_unique(cols_seen, n_cols, col)
                end if
            end do
        end do

        if (glyph_count < 8) then
            print *, 'FAIL: too few interior quiver glyphs in ', trim(fname), &
                ' count=', glyph_count
            stop 1
        end if
        if (n_rows < 3 .or. n_cols < 3) then
            print *, 'FAIL: quiver glyphs not spread across the grid in ', &
                trim(fname), ' rows=', n_rows, ' cols=', n_cols
            stop 1
        end if
        if (has_right == 0) then
            print *, 'FAIL: expected a > glyph in ', trim(fname)
            stop 1
        end if
    end subroutine check_field

    subroutine record_unique(seen, count, value)
        integer, intent(inout) :: seen(:)
        integer, intent(inout) :: count
        integer, intent(in) :: value
        integer :: m

        do m = 1, count
            if (seen(m) == value) return
        end do
        if (count < size(seen)) then
            count = count + 1
            seen(count) = value
        end if
    end subroutine record_unique

    subroutine assert_no_label_corruption(line, fname)
        !! An arrow glyph embedded between two digits means it was stamped into a
        !! tick or axis label (e.g. -2.0 rendered as -2\0). Adjacency on one side
        !! only is a legal arrow abutting a label and is allowed.
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: fname
        integer :: c, ln
        character(len=1) :: prev, this, next

        ln = len(line)
        do c = 2, ln - 1
            this = line(c:c)
            if (.not. is_arrow_glyph(this)) cycle
            prev = line(c - 1:c - 1)
            next = line(c + 1:c + 1)
            if (is_digit(prev) .and. is_digit(next)) then
                print *, 'FAIL: quiver glyph corrupts a label in ', trim(fname)
                print *, '  line: ', trim(line)
                stop 1
            end if
        end do
    end subroutine assert_no_label_corruption

    subroutine assert_label_present(lines, nlines, label, fname)
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: nlines
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: fname
        integer :: li

        do li = 1, nlines
            if (index(lines(li), label) > 0) return
        end do
        print *, 'FAIL: axis label ', label, ' missing from ', trim(fname)
        stop 1
    end subroutine assert_label_present

    pure logical function is_arrow_glyph(ch) result(found)
        character(len=1), intent(in) :: ch

        select case (ch)
        case ('>', '<', '^', 'v', '/', '\')
            found = .true.
        case default
            found = .false.
        end select
    end function is_arrow_glyph

    pure logical function is_digit(ch) result(found)
        character(len=1), intent(in) :: ch

        found = ch >= '0' .and. ch <= '9'
    end function is_digit

    pure logical function is_frame_line(line) result(found)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: t

        t = trim(adjustl(line))
        found = .false.
        if (len(t) < 3) return
        found = t(1:1) == '+' .and. index(t, '---') > 0
    end function is_frame_line

end program test_ascii_quiver_save_2025
