module fortplot_ascii_mathtext
    !! ASCII mathtext and Unicode fallback utilities.
    !!
    !! Centralises the cleanup steps needed so that text produced for the
    !! ASCII backend is readable: LaTeX command expansion, math scope
    !! stripping, mathtext brace removal, and transliteration of common
    !! Unicode symbols to ASCII equivalents. Applied at text emission time
    !! so that tick labels, legend entries, annotations, axis labels, and
    !! titles all render without raw markup or U+XXXX escape fragments.

    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_unicode, only: escape_unicode_for_ascii
    implicit none

    private
    public :: sanitize_ascii_text

contains

    subroutine sanitize_ascii_text(input, output, out_len)
        !! Produce an ASCII-safe version of ``input`` suitable for placement
        !! onto the canvas: LaTeX commands become Unicode (or ASCII words),
        !! math delimiters and mathtext braces are stripped, and any
        !! remaining Unicode symbols are transliterated.
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: out_len

        character(len=len(output)) :: sqrt_out
        character(len=len(output)) :: latex_out
        character(len=len(output)) :: funcs_out
        character(len=len(output)) :: math_stripped
        character(len=len(output)) :: super_out
        character(len=len(output)) :: flattened
        character(len=len(output)) :: plain_power
        character(len=len(output)) :: unicode_out
        integer :: sqrt_len, latex_len, funcs_len, stripped_len, super_len
        integer :: flat_len, power_len

        call preprocess_ascii_sqrt(input, sqrt_out, sqrt_len)
        call process_latex_in_text(sqrt_out(1:sqrt_len), latex_out, latex_len)
        call strip_math_function_escapes(latex_out(1:latex_len), funcs_out, &
                                         funcs_len)
        call strip_math_delimiters(funcs_out(1:funcs_len), math_stripped, &
                                    stripped_len)
        call convert_superscripts(math_stripped(1:stripped_len), super_out, &
                                   super_len)
        call simplify_mathtext(super_out(1:super_len), flattened, flat_len)
        call convert_power_of_ten(flattened(1:flat_len), plain_power, power_len)
        call escape_unicode_for_ascii(plain_power(1:power_len), unicode_out)
        output = unicode_out
        out_len = len_trim(unicode_out)
    end subroutine sanitize_ascii_text

    subroutine strip_math_function_escapes(input, output, out_len)
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: out_len
        integer :: i, j, n, cmd_len

        n = len_trim(input)
        i = 1
        j = 0
        output = ''
        do while (i <= n)
            if (input(i:i) == '\') then
                cmd_len = matched_function_len(input, i, n)
                if (cmd_len > 0) then
                    output(j + 1:j + cmd_len) = input(i + 1:i + cmd_len)
                    j = j + cmd_len
                    i = i + cmd_len + 1
                    cycle
                end if
            end if
            j = j + 1
            output(j:j) = input(i:i)
            i = i + 1
        end do
        out_len = j
    end subroutine strip_math_function_escapes

    integer function matched_function_len(input, i, n) result(cmd_len)
        character(len=*), intent(in) :: input
        integer, intent(in) :: i, n

        cmd_len = 0
        if (i + 3 <= n) then
            select case (input(i + 1:i + 3))
            case ('sin', 'cos', 'tan', 'log', 'exp', 'lim')
                if (is_ascii_command_boundary(input, i + 4, n)) cmd_len = 3
                return
            end select
        end if
        if (i + 2 <= n) then
            if (input(i + 1:i + 2) == 'ln') then
                if (is_ascii_command_boundary(input, i + 3, n)) cmd_len = 2
            end if
        end if
    end function matched_function_len

    logical function is_ascii_command_boundary(input, pos, n)
        character(len=*), intent(in) :: input
        integer, intent(in) :: pos, n
        integer :: ch

        if (pos > n) then
            is_ascii_command_boundary = .true.
            return
        end if
        ch = iachar(input(pos:pos))
        is_ascii_command_boundary = .not. ((ch >= iachar('A') .and. ch <= iachar('Z')) .or. &
                                           (ch >= iachar('a') .and. ch <= iachar('z')))
    end function is_ascii_command_boundary

    subroutine strip_math_delimiters(input, output, out_len)
        !! Remove ``$`` math-scope delimiters while preserving content.
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: out_len
        integer :: i, j, n

        n = len_trim(input)
        j = 0
        output = ''
        do i = 1, n
            if (input(i:i) == '$') cycle
            j = j + 1
            output(j:j) = input(i:i)
        end do
        out_len = j
    end subroutine strip_math_delimiters

    subroutine simplify_mathtext(input, output, out_len)
        !! Convert mathtext scripts to plain text: simple powers such as
        !! ``10^{3}`` collapse to ``10^3``, while multi-character braced scripts
        !! keep a readable group ``^(...)`` / ``_(...)`` instead of concatenating.
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: out_len
        character(len=len(output)) :: content
        integer :: i, j, k, n, content_len

        n = len_trim(input)
        i = 1
        j = 0
        output = ''
        do while (i <= n)
            if (is_script_brace(input, i, n)) then
                content_len = 0
                k = i + 2
                do while (k <= n)
                    if (input(k:k) == '}') exit
                    content_len = content_len + 1
                    content(content_len:content_len) = input(k:k)
                    k = k + 1
                end do
                call emit_script(output, j, input(i:i), content(1:content_len))
                i = k
                if (i <= n) i = i + 1
                cycle
            end if

            if (input(i:i) == '{' .or. input(i:i) == '}') then
                i = i + 1
                cycle
            end if

            j = j + 1
            output(j:j) = input(i:i)
            i = i + 1
        end do
        out_len = j
    end subroutine simplify_mathtext

    logical function is_script_brace(input, i, n)
        !! True when position ``i`` starts a braced script ``^{`` or ``_{``.
        character(len=*), intent(in) :: input
        integer, intent(in) :: i, n

        is_script_brace = .false.
        if (i >= n) return
        if (input(i:i) /= '^' .and. input(i:i) /= '_') return
        is_script_brace = input(i + 1:i + 1) == '{'
    end function is_script_brace

    subroutine emit_script(output, j, marker, content)
        !! Emit ``marker`` followed by ``content``; wrap the content in
        !! parentheses when it is not a simple numeric power.
        character(len=*), intent(inout) :: output
        integer, intent(inout) :: j
        character(len=1), intent(in) :: marker
        character(len=*), intent(in) :: content
        integer :: k

        j = j + 1
        output(j:j) = marker
        if (is_simple_script(content)) then
            do k = 1, len(content)
                j = j + 1
                output(j:j) = content(k:k)
            end do
            return
        end if
        j = j + 1
        output(j:j) = '('
        do k = 1, len(content)
            j = j + 1
            output(j:j) = content(k:k)
        end do
        j = j + 1
        output(j:j) = ')'
    end subroutine emit_script

    logical function is_simple_script(s)
        !! A script is simple (no parentheses needed) when it is a single
        !! character or an optionally signed run of digits, e.g. ``2`` or ``-3``.
        character(len=*), intent(in) :: s
        integer :: k, start

        is_simple_script = .false.
        if (len(s) == 0) return
        if (len(s) == 1) then
            is_simple_script = .true.
            return
        end if
        start = 1
        if (s(1:1) == '-' .or. s(1:1) == '+') start = 2
        if (start > len(s)) return
        do k = start, len(s)
            if (s(k:k) < '0' .or. s(k:k) > '9') return
        end do
        is_simple_script = .true.
    end function is_simple_script

    subroutine preprocess_ascii_sqrt(input, output, out_len)
        !! Rewrite ``\sqrt{...}`` (and ``\sqrt x``) to ``sqrt(...)`` so the ASCII
        !! fallback keeps an explicit radicand group. Inner LaTeX commands stay
        !! untouched for the general parser stage that follows.
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: out_len
        integer :: i, j, n

        n = len_trim(input)
        i = 1
        j = 0
        output = ''
        do while (i <= n)
            if (is_sqrt_command(input, i, n)) then
                call emit_sqrt_group(input, n, i, output, j)
                cycle
            end if
            j = j + 1
            output(j:j) = input(i:i)
            i = i + 1
        end do
        out_len = j
    end subroutine preprocess_ascii_sqrt

    subroutine emit_sqrt_group(input, n, i, output, j)
        !! Consume ``\sqrt`` at ``i`` plus its radicand, emitting ``sqrt(...)``.
        character(len=*), intent(in) :: input
        integer, intent(in) :: n
        integer, intent(inout) :: i, j
        character(len=*), intent(inout) :: output
        integer :: depth

        i = i + 5
        do while (i <= n)
            if (input(i:i) /= ' ') exit
            i = i + 1
        end do
        if (i > n) then
            call emit_text(output, j, 'sqrt')
            return
        end if

        call emit_text(output, j, 'sqrt(')
        if (input(i:i) /= '{') then
            j = j + 1
            output(j:j) = input(i:i)
            i = i + 1
            call emit_text(output, j, ')')
            return
        end if

        i = i + 1
        depth = 1
        do while (i <= n)
            if (depth <= 0) exit
            if (input(i:i) == '{') then
                depth = depth + 1
            else if (input(i:i) == '}') then
                depth = depth - 1
                if (depth == 0) then
                    i = i + 1
                    exit
                end if
            end if
            j = j + 1
            output(j:j) = input(i:i)
            i = i + 1
        end do
        call emit_text(output, j, ')')
    end subroutine emit_sqrt_group

    logical function is_sqrt_command(text, i, n)
        !! True when ``\sqrt`` starts at ``i`` with a command boundary after it.
        character(len=*), intent(in) :: text
        integer, intent(in) :: i, n

        is_sqrt_command = .false.
        if (i + 4 > n) return
        if (text(i:i) /= '\') return
        if (text(i + 1:i + 4) /= 'sqrt') return
        if (i + 5 <= n) then
            if (is_alpha_char(text(i + 5:i + 5))) return
        end if
        is_sqrt_command = .true.
    end function is_sqrt_command

    subroutine convert_superscripts(input, output, out_len)
        !! Replace Latin-1 superscript digits (U+00B9/B2/B3, UTF-8 ``C2 B9/B2/B3``)
        !! with caret forms ``^1``/``^2``/``^3`` so exponents read as powers.
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: out_len
        integer :: i, j, n

        n = len_trim(input)
        i = 1
        j = 0
        output = ''
        do while (i <= n)
            if (is_superscript_digit(input, i, n)) then
                call emit_text(output, j, caret_digit(input(i + 1:i + 1)))
                i = i + 2
                cycle
            end if
            j = j + 1
            output(j:j) = input(i:i)
            i = i + 1
        end do
        out_len = j
    end subroutine convert_superscripts

    logical function is_superscript_digit(input, i, n)
        !! True when a two-byte Latin-1 superscript digit starts at ``i``.
        character(len=*), intent(in) :: input
        integer, intent(in) :: i, n

        is_superscript_digit = .false.
        if (i + 1 > n) return
        if (iachar(input(i:i)) /= 194) return
        select case (iachar(input(i + 1:i + 1)))
        case (185, 178, 179)
            is_superscript_digit = .true.
        end select
    end function is_superscript_digit

    function caret_digit(byte) result(text)
        !! Map a Latin-1 superscript continuation byte to its caret power form.
        character(len=1), intent(in) :: byte
        character(len=2) :: text

        select case (iachar(byte))
        case (185)
            text = '^1'
        case (179)
            text = '^3'
        case default
            text = '^2'
        end select
    end function caret_digit

    subroutine emit_text(output, j, text)
        !! Append literal ``text`` to ``output`` advancing the cursor ``j``.
        character(len=*), intent(inout) :: output
        integer, intent(inout) :: j
        character(len=*), intent(in) :: text
        integer :: k

        do k = 1, len(text)
            j = j + 1
            output(j:j) = text(k:k)
        end do
    end subroutine emit_text

    logical function is_alpha_char(ch)
        character(len=1), intent(in) :: ch
        integer :: v

        v = iachar(ch)
        is_alpha_char = (v >= iachar('A') .and. v <= iachar('Z')) .or. &
                        (v >= iachar('a') .and. v <= iachar('z'))
    end function is_alpha_char

    subroutine convert_power_of_ten(input, output, out_len)
        !! Convert ``10^N`` patterns to plain-text ``1eN`` so that ASCII
        !! tick labels read as ``1e4`` instead of ``10^4``.  Handles
        !! ``-10^N`` -> ``-1eN``, ``10^-N`` -> ``1e-N``, and ``10^0`` -> ``1``.
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: out_len
        integer :: i, j, k, n, exp_start, exp_end
        logical :: has_minus, neg_exp, is_zero_exp

        n = len(input)
        j = 0
        output = ''
        i = 1

        do while (i <= n)
            if (i + 1 <= n) then
                if (input(i:i + 1) == '10' .and. i + 2 <= n) then
                    if (input(i + 2:i + 2) == '^') then
                        has_minus = .false.
                        if (i > 1) then
                            if (input(i - 1:i - 1) == '-') has_minus = .true.
                        end if

                        exp_start = i + 3
                        if (exp_start > n) then
                            j = j + 1; output(j:j) = input(i:i)
                            i = i + 1
                            cycle
                        end if

                        neg_exp = .false.
                        if (input(exp_start:exp_start) == '-') then
                            neg_exp = .true.
                            exp_start = exp_start + 1
                        end if

                        exp_end = exp_start
                        do while (exp_end <= n)
                            if (input(exp_end:exp_end) < '0' .or. &
                                input(exp_end:exp_end) > '9') exit
                            exp_end = exp_end + 1
                        end do

                        if (exp_end > exp_start) then
                            is_zero_exp = .true.
                            do k = exp_start, exp_end - 1
                                if (input(k:k) /= '0') is_zero_exp = .false.
                            end do

                            if (has_minus .and. j >= 1) then
                                j = j - 1
                            end if

                            if (is_zero_exp) then
                                if (has_minus) then
                                    j = j + 1; output(j:j) = '-'
                                end if
                                j = j + 1; output(j:j) = '1'
                            else
                                if (has_minus) then
                                    j = j + 1; output(j:j) = '-'
                                end if
                                j = j + 1; output(j:j) = '1'
                                j = j + 1; output(j:j) = 'e'
                                if (neg_exp) then
                                    j = j + 1; output(j:j) = '-'
                                end if
                                do k = exp_start, exp_end - 1
                                    j = j + 1; output(j:j) = input(k:k)
                                end do
                            end if

                            i = exp_end
                            cycle
                        else
                            j = j + 1; output(j:j) = input(i:i)
                            i = i + 1
                            cycle
                        end if
                    else
                        j = j + 1; output(j:j) = input(i:i)
                        i = i + 1
                        cycle
                    end if
                else
                    j = j + 1; output(j:j) = input(i:i)
                    i = i + 1
                    cycle
                end if
            else
                j = j + 1
                output(j:j) = input(i:i)
                i = i + 1
            end if
        end do
        out_len = j
    end subroutine convert_power_of_ten

end module fortplot_ascii_mathtext
