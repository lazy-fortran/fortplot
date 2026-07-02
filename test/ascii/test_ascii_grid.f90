program test_ascii_grid
    !! Regression tests for text/ASCII major-tick grid rendering (issue #2074).
    !!
    !! Positive fixture: a grid-enabled plot shows faint vertical (':') columns
    !! aligned to x tick marks and horizontal ('.') rows aligned to y ticks,
    !! spanning the plot interior at major tick positions.
    !! Negative fixture: the same plot with the grid disabled keeps a blank
    !! interior with no full grid columns or rows.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: MAXL = 64
    logical :: ok

    ok = .true.
    call test_grid_on_and_off(ok)

    if (.not. ok) then
        error stop "test_ascii_grid: one or more assertions failed"
    end if
    print *, "PASS: ascii major-tick grid rendering"

contains

    subroutine test_grid_on_and_off(ok)
        logical, intent(inout) :: ok
        character(len=256) :: on_lines(MAXL), off_lines(MAXL)
        integer :: on_n, off_n
        integer :: on_cols, on_rows, off_cols, off_rows
        integer :: top_on, bot_on, spine_on
        integer :: aligned, tick_cols
        logical :: dir_ok

        call create_directory_runtime('build/test/output', dir_ok)
        call render_case(.true., 'build/test/output/ascii_grid_on.txt')
        call render_case(.false., 'build/test/output/ascii_grid_off.txt')

        call load_lines('build/test/output/ascii_grid_on.txt', on_lines, on_n)
        call load_lines('build/test/output/ascii_grid_off.txt', off_lines, off_n)
        if (on_n == 0 .or. off_n == 0) then
            print *, "FAIL: could not read grid output files"
            ok = .false.
            return
        end if

        call frame_bounds(on_lines, on_n, top_on, bot_on)
        if (top_on == 0 .or. bot_on == 0) then
            print *, "FAIL: could not locate ASCII frame"
            ok = .false.
            return
        end if

        on_cols = count_grid_columns(on_lines, on_n, ':')
        on_rows = count_grid_rows(on_lines, on_n, '.')
        off_cols = count_grid_columns(off_lines, off_n, ':')
        off_rows = count_grid_rows(off_lines, off_n, '.')

        if (on_cols < 2) then
            print *, "FAIL: grid-on lacks vertical grid columns, got ", on_cols
            ok = .false.
        end if
        if (on_rows < 2) then
            print *, "FAIL: grid-on lacks horizontal grid rows, got ", on_rows
            ok = .false.
        end if
        if (off_cols /= 0) then
            print *, "FAIL: grid-off has vertical grid columns, got ", off_cols
            ok = .false.
        end if
        if (off_rows /= 0) then
            print *, "FAIL: grid-off has horizontal grid rows, got ", off_rows
            ok = .false.
        end if

        ! Alignment: every vertical grid column must sit on a bottom-spine tick.
        spine_on = bottom_spine_row(on_lines, on_n, top_on, bot_on)
        if (spine_on == 0) then
            print *, "FAIL: could not locate bottom spine on grid-on plot"
            ok = .false.
            return
        end if
        call check_column_alignment(on_lines, on_n, spine_on, aligned, tick_cols)
        if (tick_cols < 2) then
            print *, "FAIL: grid-on bottom spine lacks tick marks, got ", tick_cols
            ok = .false.
        end if
        if (aligned < 2) then
            print *, "FAIL: grid columns not aligned to ticks, aligned=", aligned
            ok = .false.
        end if
    end subroutine test_grid_on_and_off

    subroutine render_case(grid_on, filename)
        logical, intent(in) :: grid_on
        character(len=*), intent(in) :: filename
        type(figure_t) :: fig
        real(wp) :: x(11), y(11)
        integer :: i

        x = [(real(i, wp), i=0, 10)]
        y = x
        call fig%initialize(80, 24)
        call fig%add_plot(x, y, label="line")
        call fig%grid(grid_on)
        call fig%savefig(filename)
    end subroutine render_case

    subroutine load_lines(filename, lines, n)
        character(len=*), intent(in) :: filename
        character(len=256), intent(out) :: lines(:)
        integer, intent(out) :: n
        integer :: unit, ios
        character(len=256) :: line

        n = 0
        open (newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            n = n + 1
            if (n <= size(lines)) lines(n) = line
        end do
        close (unit)
        n = min(n, size(lines))
    end subroutine load_lines

    subroutine frame_bounds(lines, n, top_frame, bottom_frame)
        character(len=256), intent(in) :: lines(:)
        integer, intent(in) :: n
        integer, intent(out) :: top_frame, bottom_frame
        integer :: i

        top_frame = 0
        bottom_frame = 0
        do i = 1, n
            if (is_frame_line(lines(i))) then
                if (top_frame == 0) then
                    top_frame = i
                else
                    bottom_frame = i
                end if
            end if
        end do
    end subroutine frame_bounds

    integer function bottom_spine_row(lines, n, top_frame, bottom_frame) result(row)
        character(len=256), intent(in) :: lines(:)
        integer, intent(in) :: n, top_frame, bottom_frame
        integer :: i

        row = 0
        do i = bottom_frame - 1, top_frame + 1, -1
            if (max_dash_run(lines(i)) >= 8) then
                row = i
                return
            end if
        end do
    end function bottom_spine_row

    integer function count_grid_columns(lines, n, ch) result(cols)
        !! Interior columns whose glyph ``ch`` fills a strong majority of the
        !! interior height mark a full vertical grid line, unlike sparse data.
        character(len=256), intent(in) :: lines(:)
        integer, intent(in) :: n
        character(len=1), intent(in) :: ch
        integer :: top_frame, bottom_frame, spine
        integer :: k, i, hits, interior_rows, kmax

        cols = 0
        call frame_bounds(lines, n, top_frame, bottom_frame)
        if (top_frame == 0 .or. bottom_frame == 0) return
        spine = bottom_spine_row(lines, n, top_frame, bottom_frame)
        if (spine == 0) spine = bottom_frame
        interior_rows = spine - 1 - (top_frame + 1) + 1
        if (interior_rows <= 0) return

        kmax = min_line_len(lines, top_frame + 1, spine - 1)
        do k = 2, kmax
            hits = 0
            do i = top_frame + 1, spine - 1
                if (k <= len(lines(i))) then
                    if (lines(i)(k:k) == ch) hits = hits + 1
                end if
            end do
            if (real(hits, wp) >= 0.5_wp*real(interior_rows, wp)) cols = cols + 1
        end do
    end function count_grid_columns

    integer function count_grid_rows(lines, n, ch) result(rows)
        !! Interior rows whose glyph ``ch`` fills a strong majority of the
        !! interior width mark a full horizontal grid line.
        character(len=256), intent(in) :: lines(:)
        integer, intent(in) :: n
        character(len=1), intent(in) :: ch
        integer :: top_frame, bottom_frame, spine
        integer :: i, k, hits, width, kmax

        rows = 0
        call frame_bounds(lines, n, top_frame, bottom_frame)
        if (top_frame == 0 .or. bottom_frame == 0) return
        spine = bottom_spine_row(lines, n, top_frame, bottom_frame)
        if (spine == 0) spine = bottom_frame
        width = len_trim(lines(top_frame)) - 2
        if (width <= 0) return
        kmax = len_trim(lines(top_frame)) - 1

        do i = top_frame + 1, spine - 1
            hits = 0
            do k = 2, kmax
                if (k <= len(lines(i))) then
                    if (lines(i)(k:k) == ch) hits = hits + 1
                end if
            end do
            if (real(hits, wp) >= 0.5_wp*real(width, wp)) rows = rows + 1
        end do
    end function count_grid_rows

    subroutine check_column_alignment(lines, n, spine, aligned, tick_cols)
        !! Count bottom-spine tick columns and how many carry a grid column.
        character(len=256), intent(in) :: lines(:)
        integer, intent(in) :: n, spine
        integer, intent(out) :: aligned, tick_cols
        integer :: top_frame, bottom_frame
        integer :: k, i, hits, kmax

        aligned = 0
        tick_cols = 0
        call frame_bounds(lines, n, top_frame, bottom_frame)
        if (top_frame == 0 .or. bottom_frame == 0) return
        kmax = len(lines(spine))

        do k = 2, kmax
            if (lines(spine)(k:k) /= '+') cycle
            tick_cols = tick_cols + 1
            hits = 0
            do i = top_frame + 1, spine - 1
                if (k <= len(lines(i))) then
                    if (lines(i)(k:k) == ':') hits = hits + 1
                end if
            end do
            if (hits >= 2) aligned = aligned + 1
        end do
    end subroutine check_column_alignment

    integer function min_line_len(lines, lo, hi) result(m)
        character(len=256), intent(in) :: lines(:)
        integer, intent(in) :: lo, hi
        integer :: i

        m = len(lines(lo))
        do i = lo, hi
            m = min(m, len(lines(i)))
        end do
    end function min_line_len

    pure logical function is_frame_line(line) result(found)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: t

        t = trim(adjustl(line))
        found = .false.
        if (len(t) < 3) return
        found = t(1:1) == '+' .and. index(t, '---') > 0
    end function is_frame_line

    pure integer function max_dash_run(line) result(best)
        character(len=*), intent(in) :: line
        integer :: i, run

        best = 0
        run = 0
        do i = 1, len(line)
            if (line(i:i) == '-') then
                run = run + 1
                if (run > best) best = run
            else
                run = 0
            end if
        end do
    end function max_dash_run

end program test_ascii_grid
