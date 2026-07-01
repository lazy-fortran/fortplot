program test_axhline_log_span
    !! Regression test for issue 2047: axhline/axvline must span the full axis on
    !! log-scaled axes. The refline endpoints are interpolated in transformed
    !! space, so applying the scale transform a second time truncated the line.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(50), y(50)
    integer :: i, status
    character(len=:), allocatable :: output_dir, path

    call ensure_test_output_dir('reflines', output_dir)
    do i = 1, 50
        x(i) = 10.0_wp**(-2.0_wp + 3.3_wp*real(i - 1, wp)/49.0_wp)
        y(i) = 1.0e9_wp*x(i)
    end do

    call test_axhline_spans_full_log_x()
    call test_axvline_spans_full_log_y()

    print '(A)', 'All axhline/axvline log-span tests passed'

contains

    subroutine test_axhline_spans_full_log_x()
        call fig%initialize(width=640, height=480)
        call fig%add_plot(x, y)
        call fig%axhline(8.5e7_wp, linestyle='--')
        call fig%set_xscale('log')
        call fig%set_yscale('log')
        call fig%set_xlim(1.0e-2_wp, 2.0e1_wp)
        call fig%set_ylim(1.0e7_wp, 1.0e11_wp)
        path = trim(output_dir)//'test_axhline_log_span.txt'
        call fig%savefig_with_status(path, status)
        if (status /= 0) then
            print '(A)', 'FAIL: could not save axhline log figure'
            stop 1
        end if
        call fig%clear()
        call assert_hline_spans(path)
        print '(A)', 'test_axhline_spans_full_log_x: PASSED'
    end subroutine test_axhline_spans_full_log_x

    subroutine test_axvline_spans_full_log_y()
        call fig%initialize(width=640, height=480)
        call fig%add_plot(x, y)
        call fig%axvline(0.5_wp, linestyle='--')
        call fig%set_xscale('log')
        call fig%set_yscale('log')
        call fig%set_xlim(1.0e-2_wp, 2.0e1_wp)
        call fig%set_ylim(1.0e7_wp, 1.0e11_wp)
        path = trim(output_dir)//'test_axvline_log_span.txt'
        call fig%savefig_with_status(path, status)
        if (status /= 0) then
            print '(A)', 'FAIL: could not save axvline log figure'
            stop 1
        end if
        call fig%clear()
        call assert_vline_spans(path)
        print '(A)', 'test_axvline_spans_full_log_y: PASSED'
    end subroutine test_axvline_spans_full_log_y

    subroutine assert_hline_spans(file_path)
        !! The horizontal reference line is the text row with the most marker
        !! glyphs. Assert its markers reach both the left and right edges of the
        !! plot area, i.e. the line spans the full x-range.
        character(len=*), intent(in) :: file_path
        character(len=512) :: line, best
        integer :: unit, ios, cnt, best_cnt, width, first_col, last_col

        best = ''
        best_cnt = 0
        open (newunit=unit, file=file_path, status='old', action='read')
        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            cnt = count_marks(line)
            if (cnt > best_cnt) then
                best_cnt = cnt
                best = line
            end if
        end do
        close (unit)

        if (best_cnt < 5) then
            print '(A)', 'FAIL: no reference-line row found'
            stop 1
        end if
        width = len_trim(best)
        first_col = mark_index(best, .true.)
        last_col = mark_index(best, .false.)
        ! Left marker near the left border, right marker near the right border.
        if (first_col > width/4) then
            print '(A,I0,A,I0)', 'FAIL: hline does not reach left edge, first=', &
                first_col, ' width=', width
            stop 1
        end if
        if (last_col < (3*width)/4) then
            print '(A,I0,A,I0)', 'FAIL: hline truncated, last mark col=', &
                last_col, ' width=', width
            stop 1
        end if
    end subroutine assert_hline_spans

    subroutine assert_vline_spans(file_path)
        !! The vertical reference line occupies one column across many rows.
        !! Assert marker rows appear in both the top and bottom thirds of the
        !! plot, i.e. the line spans the full y-range.
        character(len=*), intent(in) :: file_path
        character(len=512) :: line
        integer :: unit, ios, row, nrows, top_hits, bottom_hits
        character(len=512), allocatable :: rows(:)

        allocate (rows(0))
        open (newunit=unit, file=file_path, status='old', action='read')
        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            rows = [rows, line]
        end do
        close (unit)

        nrows = size(rows)
        top_hits = 0
        bottom_hits = 0
        do row = 1, nrows
            if (count_marks(rows(row)) >= 1) then
                if (row <= nrows/3) top_hits = top_hits + 1
                if (row > (2*nrows)/3) bottom_hits = bottom_hits + 1
            end if
        end do
        if (top_hits < 1 .or. bottom_hits < 1) then
            print '(A,I0,A,I0)', 'FAIL: vline does not span full height, top=', &
                top_hits, ' bottom=', bottom_hits
            stop 1
        end if
    end subroutine assert_vline_spans

    integer function count_marks(line) result(cnt)
        character(len=*), intent(in) :: line
        integer :: j
        cnt = 0
        do j = 1, len_trim(line)
            if (line(j:j) == '#' .or. line(j:j) == '*' .or. line(j:j) == 'o') &
                cnt = cnt + 1
        end do
    end function count_marks

    integer function mark_index(line, want_first) result(idx)
        character(len=*), intent(in) :: line
        logical, intent(in) :: want_first
        integer :: j
        idx = 0
        do j = 1, len_trim(line)
            if (line(j:j) == '#' .or. line(j:j) == '*' .or. line(j:j) == 'o') then
                idx = j
                if (want_first) return
            end if
        end do
    end function mark_index

end program test_axhline_log_span
