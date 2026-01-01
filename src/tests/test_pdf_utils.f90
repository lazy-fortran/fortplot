module test_pdf_utils
    use, intrinsic :: iso_fortran_env, only: dp => real64, int8, int64
    use fortplot_zlib_core, only: zlib_decompress
    implicit none
contains
    subroutine extract_pdf_stream_text(filename, stream_text, status)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: stream_text
        integer, intent(out) :: status

        character(len=1), allocatable :: data(:)
        integer(int64) :: fsize
        integer :: content_begin, content_len
        integer :: ios, pos
        integer :: stream_start, stream_end
        integer :: unit
        integer :: i

        status = 0
        allocate (character(len=0) :: stream_text)

        open (newunit=unit, file=filename, access='stream', form='unformatted', &
              status='old', iostat=ios)
        if (ios /= 0) then
            status = -1
            return
        end if

        inquire (unit=unit, size=fsize)
        if (fsize <= 0_int64) then
            close (unit)
            return
        end if

        allocate (character(len=1) :: data(int(fsize)))
        read (unit, iostat=ios) data
        close (unit)
        if (ios /= 0) then
            status = -2
            return
        end if

        pos = 1
        do
            stream_start = find_subsequence(data, fsize, 'stream', pos)
            if (stream_start < 0) exit
            stream_end = find_subsequence(data, fsize, 'endstream', &
                                          stream_start + len('stream'))
            if (stream_end < 0) exit

            content_begin = stream_start + len('stream')
            if (content_begin <= int(fsize)) then
                if (data(content_begin) == achar(13)) then
                    content_begin = content_begin + 1
                    if (content_begin <= int(fsize)) then
                        if (data(content_begin) == achar(10)) then
                            content_begin = content_begin + 1
                        end if
                    end if
                else if (data(content_begin) == achar(10)) then
                    content_begin = content_begin + 1
                end if
            end if

            content_len = stream_end - content_begin
            if (content_len > 0) then
                block
                    integer(int8), allocatable :: compressed(:), decompressed_raw(:)
                    character(len=:), allocatable :: chunk_text
                    integer :: j
                    integer :: status_decomp

                    allocate (compressed(content_len))
                    do j = 1, content_len
                        compressed(j) = int(iachar(data(content_begin + j - 1)), int8)
                    end do
                    decompressed_raw = zlib_decompress(compressed, content_len, &
                                                       status_decomp, .false.)

                    if (status_decomp == 0 .and. size(decompressed_raw) > 0) then
                        allocate (character(len=size(decompressed_raw)) :: chunk_text)
                        call bytes_to_string(decompressed_raw, chunk_text)
                    else
                        allocate (character(len=content_len) :: chunk_text)
                        do j = 1, content_len
                            chunk_text(j:j) = data(content_begin + j - 1)
                        end do
                    end if

                    call append_string(stream_text, chunk_text)
                end block
            end if

            pos = stream_end + len('endstream')
        end do

    end subroutine extract_pdf_stream_text

    subroutine append_string(target, chunk)
        character(len=:), allocatable, intent(inout) :: target
        character(len=*), intent(in) :: chunk
        character(len=:), allocatable :: combined
        integer :: new_len
        integer :: old_len

        old_len = len(target)
        new_len = old_len + len(chunk)

        allocate (character(len=new_len) :: combined)
        if (old_len > 0) combined(1:old_len) = target
        if (len(chunk) > 0) combined(old_len + 1:new_len) = chunk
        call move_alloc(combined, target)
    end subroutine append_string

    subroutine bytes_to_string(bytes, text)
        integer(int8), intent(in) :: bytes(:)
        character(len=*), intent(out) :: text
        integer :: i

        do i = 1, size(bytes)
            text(i:i) = achar(iand(int(bytes(i)), 255))
        end do
    end subroutine bytes_to_string

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
        logical :: escaped

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
        escaped = .false.
        i = pos + 1
        do while (i <= n)
            if (escaped) then
                escaped = .false.
            else
                if (text(i:i) == '\') then
                    escaped = .true.
                else if (text(i:i) == '(') then
                    depth = depth + 1
                else if (text(i:i) == ')') then
                    depth = depth - 1
                    if (depth == 0) exit
                end if
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

end module test_pdf_utils
