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
        character(len=len(output)) :: unicode_out
        integer :: latex_len, stripped_len, flat_len

        call process_latex_in_text(input, latex_out, latex_len)
        call strip_math_delimiters(latex_out(1:latex_len), math_stripped, &
                                   stripped_len)
        call simplify_mathtext(math_stripped(1:stripped_len), flattened, &
                               flat_len)
        call escape_unicode_for_ascii(flattened(1:flat_len), unicode_out)
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

end module fortplot_ascii_mathtext
