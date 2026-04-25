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
    use, intrinsic :: iso_fortran_env, only: wp => real64
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

        character(len=len(output)) :: latex_out
        character(len=len(output)) :: math_stripped
        character(len=len(output)) :: flattened
        character(len=len(output)) :: plain_power
        character(len=len(output)) :: unicode_out
        integer :: latex_len, stripped_len, flat_len, power_len

        call process_latex_in_text(input, latex_out, latex_len)
        call strip_math_delimiters(latex_out(1:latex_len), math_stripped, &
                                    stripped_len)
        call simplify_mathtext(math_stripped(1:stripped_len), flattened, &
                                flat_len)
        call convert_power_of_ten(flattened(1:flat_len), plain_power, power_len)
        call escape_unicode_for_ascii(plain_power(1:power_len), unicode_out)
        output = unicode_out
        out_len = len_trim(unicode_out)
    end subroutine sanitize_ascii_text

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
        !! Convert mathtext constructs like ``10^{3}`` to ``10^3`` and
        !! drop braces produced by mathtext commands.
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: out_len
        integer :: i, j, n

        n = len_trim(input)
        i = 1
        j = 0
        output = ''
        do while (i <= n)
            if ((input(i:i) == '^' .or. input(i:i) == '_') .and. i < n) then
                if (input(i + 1:i + 1) == '{') then
                    j = j + 1
                    output(j:j) = input(i:i)
                    i = i + 2
                    do while (i <= n)
                        if (input(i:i) == '}') exit
                        j = j + 1
                        output(j:j) = input(i:i)
                        i = i + 1
                    end do
                    if (i <= n) i = i + 1
                    cycle
                end if
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
