module fortplot_test_pdf_tokenizer
    !! PDF stream tokenizer utilities for testing
    !!
    !! Single Responsibility: Tokenize and parse PDF stream content
    !! Extracted from fortplot_test_pdf_utils to keep modules under 500 lines.

    use, intrinsic :: iso_fortran_env, only: dp => real64, int8, int64
    implicit none

    private
    public :: find_subsequence
    public :: pdf_stream_has_stroke_rgb
    public :: pdf_stream_count_operator
    public :: pdf_next_token

contains

    integer function find_subsequence(arr, n, pat, start_idx) result(pos)
        integer(int64), intent(in) :: n
        character(len=1), intent(in) :: arr(n)
        character(len=*), intent(in) :: pat
        integer, intent(in) :: start_idx
        integer :: i, j, pat_len

        pat_len = len_trim(pat)
        pos = -1
        if (pat_len <= 0) return
        do i = max(1, start_idx), int(n) - pat_len + 1
            do j = 1, pat_len
                if (arr(i + j - 1) /= pat(j:j)) exit
                if (j == pat_len) then
                    pos = i
                    return
                end if
            end do
        end do
    end function find_subsequence

    logical function pdf_stream_has_stroke_rgb(stream_text, rgb, tol) result(found)
        character(len=*), intent(in) :: stream_text
        real(dp), intent(in) :: rgb(3)
        real(dp), intent(in) :: tol

        character(len=128) :: token
        integer :: token_len
        logical :: has_token
        integer :: pos

        character(len=64) :: t1, t2, t3
        integer :: t1_len, t2_len, t3_len
        real(dp) :: r_val, g_val, b_val
        logical :: ok_r, ok_g, ok_b

        found = .false.
        pos = 1

        t1 = ''
        t2 = ''
        t3 = ''
        t1_len = 0
        t2_len = 0
        t3_len = 0

        do
            call pdf_next_token(stream_text, pos, token, token_len, has_token)
            if (.not. has_token) exit

            if (token_len == 2) then
                if (token(1:2) == 'RG') then
                    ok_r = parse_real_token(t3, t3_len, r_val)
                    ok_g = parse_real_token(t2, t2_len, g_val)
                    ok_b = parse_real_token(t1, t1_len, b_val)

                    if (ok_r .and. ok_g .and. ok_b) then
                        if (abs(r_val - rgb(1)) <= tol .and. &
                            abs(g_val - rgb(2)) <= tol .and. &
                            abs(b_val - rgb(3)) <= tol) then
                            found = .true.
                            return
                        end if
                    end if
                end if
            end if

            call shift_recent_tokens(token, token_len, t1, t1_len, t2, t2_len, &
                                     t3, t3_len)
        end do
    end function pdf_stream_has_stroke_rgb

    integer function pdf_stream_count_operator(stream_text, op) result(count_op)
        character(len=*), intent(in) :: stream_text
        character(len=*), intent(in) :: op

        character(len=128) :: token
        integer :: token_len
        logical :: has_token
        integer :: pos
        integer :: op_len

        count_op = 0
        pos = 1
        op_len = len_trim(op)
        if (op_len <= 0) return

        do
            call pdf_next_token(stream_text, pos, token, token_len, has_token)
            if (.not. has_token) exit

            if (token_len == op_len) then
                if (token_len <= len(token)) then
                    if (token(1:token_len) == op(1:op_len)) count_op = count_op + 1
                end if
            end if
        end do
    end function pdf_stream_count_operator

    subroutine pdf_next_token(text, pos, token, token_len, has_token)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        character(len=*), intent(out) :: token
        integer, intent(out) :: token_len
        logical, intent(out) :: has_token

        integer :: n
        integer :: start_pos
        integer :: end_pos
        integer :: i
        integer :: copy_len
        character(len=1) :: ch

        token = ''
        token_len = 0
        has_token = .false.

        n = len(text)
        if (pos < 1) pos = 1

        do
            do while (pos <= n)
                if (.not. is_pdf_whitespace(text(pos:pos))) exit
                pos = pos + 1
            end do
            if (pos > n) return

            if (text(pos:pos) /= '%') exit
            call pdf_skip_comment(text, pos)
        end do

        start_pos = pos
        ch = text(pos:pos)

        select case (ch)
        case ('(')
            call pdf_scan_literal_string(text, pos, end_pos)
        case ('<')
            if (pos < n) then
                if (text(pos + 1:pos + 1) == '<') then
                    end_pos = pos + 1
                    pos = pos + 2
                else
                    call pdf_scan_hex_string(text, pos, end_pos)
                    pos = end_pos + 1
                end if
            else
                call pdf_scan_hex_string(text, pos, end_pos)
                pos = end_pos + 1
            end if
        case ('>')
            if (pos < n) then
                if (text(pos + 1:pos + 1) == '>') then
                    end_pos = pos + 1
                    pos = pos + 2
                else
                    end_pos = pos
                    pos = pos + 1
                end if
            else
                end_pos = pos
                pos = pos + 1
            end if
        case ('[', ']', '{', '}', ')')
            end_pos = pos
            pos = pos + 1
        case ('/')
            pos = pos + 1
            do while (pos <= n)
                if (is_pdf_whitespace(text(pos:pos))) exit
                if (is_pdf_delimiter(text(pos:pos))) exit
                pos = pos + 1
            end do
            end_pos = pos - 1
        case default
            do while (pos <= n)
                if (is_pdf_whitespace(text(pos:pos))) exit
                if (is_pdf_delimiter(text(pos:pos))) exit
                if (text(pos:pos) == '%') exit
                pos = pos + 1
            end do
            end_pos = pos - 1
        end select

        token_len = end_pos - start_pos + 1
        if (token_len <= 0) return

        copy_len = min(token_len, len(token))
        do i = 1, copy_len
            token(i:i) = text(start_pos + i - 1:start_pos + i - 1)
        end do
        has_token = .true.
    end subroutine pdf_next_token

    logical function is_pdf_whitespace(ch) result(is_ws)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_ws = (code == 0) .or. (code == 9) .or. (code == 10) .or. &
                (code == 12) .or. (code == 13) .or. (code == 32)
    end function is_pdf_whitespace

    logical function is_pdf_delimiter(ch) result(is_delim)
        character(len=1), intent(in) :: ch

        is_delim = (ch == '(') .or. (ch == ')') .or. (ch == '<') .or. &
                   (ch == '>') .or. (ch == '[') .or. (ch == ']') .or. &
                   (ch == '{') .or. (ch == '}') .or. (ch == '/') .or. &
                   (ch == '%')
    end function is_pdf_delimiter

    subroutine pdf_skip_comment(text, pos)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos

        integer :: n

        n = len(text)
        if (pos < 1) pos = 1
        if (pos > n) return
        if (text(pos:pos) /= '%') return

        do while (pos <= n)
            if (text(pos:pos) == achar(10) .or. text(pos:pos) == achar(13)) exit
            pos = pos + 1
        end do
        do while (pos <= n)
            if (text(pos:pos) /= achar(10) .and. text(pos:pos) /= achar(13)) exit
            pos = pos + 1
        end do
    end subroutine pdf_skip_comment

    subroutine pdf_scan_literal_string(text, pos, end_pos)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        integer, intent(out) :: end_pos

        integer :: n
        integer :: depth
        integer :: i
        integer :: j
        integer :: octal_digits

        n = len(text)
        if (pos < 1) pos = 1
        if (pos > n) then
            end_pos = n
            return
        end if
        if (text(pos:pos) /= '(') then
            end_pos = pos
            return
        end if

        depth = 1
        i = pos + 1
        do while (i <= n)
            if (text(i:i) == achar(92)) then
                if (i == n) then
                    i = i + 1
                    exit
                end if

                if (text(i + 1:i + 1) == achar(13)) then
                    i = i + 2
                    if (i <= n) then
                        if (text(i:i) == achar(10)) i = i + 1
                    end if
                    cycle
                end if

                if (text(i + 1:i + 1) == achar(10)) then
                    i = i + 2
                    cycle
                end if

                if (is_octal_digit(text(i + 1:i + 1))) then
                    octal_digits = 1
                    j = i + 2
                    do while (octal_digits < 3 .and. j <= n)
                        if (.not. is_octal_digit(text(j:j))) exit
                        octal_digits = octal_digits + 1
                        j = j + 1
                    end do
                    i = j
                    cycle
                end if

                i = i + 2
                cycle
            end if

            if (text(i:i) == '(') then
                depth = depth + 1
            else if (text(i:i) == ')') then
                depth = depth - 1
                if (depth == 0) exit
            end if

            i = i + 1
        end do

        if (i > n) then
            end_pos = n
            pos = n + 1
        else
            end_pos = i
            pos = end_pos + 1
        end if
    end subroutine pdf_scan_literal_string

    subroutine pdf_scan_hex_string(text, pos, end_pos)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        integer, intent(out) :: end_pos

        integer :: n
        integer :: i

        n = len(text)
        if (pos < 1) pos = 1
        if (pos > n) then
            end_pos = n
            return
        end if
        if (text(pos:pos) /= '<') then
            end_pos = pos
            return
        end if

        i = pos + 1
        do while (i <= n)
            if (text(i:i) == '>') exit
            i = i + 1
        end do

        if (i > n) then
            end_pos = n
        else
            end_pos = i
        end if
    end subroutine pdf_scan_hex_string

    logical function is_octal_digit(ch) result(is_digit)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_digit = (code >= iachar('0') .and. code <= iachar('7'))
    end function is_octal_digit

    subroutine shift_recent_tokens(token, token_len, t1, t1_len, t2, t2_len, &
                                   t3, t3_len)
        character(len=*), intent(in) :: token
        integer, intent(in) :: token_len
        character(len=*), intent(inout) :: t1, t2, t3
        integer, intent(inout) :: t1_len, t2_len, t3_len

        integer :: copy_len

        t3 = t2
        t3_len = t2_len
        t2 = t1
        t2_len = t1_len

        t1 = ''
        t1_len = min(token_len, len(t1))
        copy_len = t1_len
        if (copy_len > 0) t1(1:copy_len) = token(1:copy_len)
    end subroutine shift_recent_tokens

    logical function parse_real_token(token, token_len, value) result(ok)
        character(len=*), intent(in) :: token
        integer, intent(in) :: token_len
        real(dp), intent(out) :: value

        integer :: ios

        value = 0.0_dp
        ok = .false.
        if (token_len <= 0) return
        if (token_len > len(token)) return

        read (token(1:token_len), *, iostat=ios) value
        ok = (ios == 0)
    end function parse_real_token

end module fortplot_test_pdf_tokenizer
