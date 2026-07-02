program test_mathtext_operators
    !! Regression tests for issues 2045 and 2046: math operators render as bare
    !! Unicode, unknown LaTeX commands never drop a leading character, and PDF
    !! string escaping handles backslashes.
    use fortplot_latex_parser, only: process_latex_in_text, latex_to_unicode
    use fortplot_pdf_text_escape, only: escape_pdf_string
    use fortplot_unicode, only: utf8_to_codepoint, unicode_codepoint_to_ascii
    implicit none

    call test_operator_mapping()
    call test_operators_in_text()
    call test_math_functions_preserve_command()
    call test_unknown_command_keeps_name()
    call test_escaped_specials_stay_literal()
    call test_pdf_escape_backslash()
    call test_ascii_fallbacks()

    print *, "All mathtext operator tests passed!"

contains

    subroutine expect_codepoint(command, expected)
        character(len=*), intent(in) :: command
        integer, intent(in) :: expected
        character(len=20) :: unicode_char
        logical :: success
        integer :: codepoint

        call latex_to_unicode(command, unicode_char, success)
        if (.not. success) then
            print *, "ERROR: operator not mapped: ", command
            stop 1
        end if
        codepoint = utf8_to_codepoint(unicode_char, 1)
        if (codepoint /= expected) then
            print *, "ERROR: wrong codepoint for ", command, codepoint, &
                " expected ", expected
            stop 1
        end if
    end subroutine expect_codepoint

    subroutine test_operator_mapping()
        call expect_codepoint("times", 215)
        call expect_codepoint("cdot", 183)
        call expect_codepoint("pm", 177)
        call expect_codepoint("varphi", 966)
        call expect_codepoint("propto", 8733)
        call expect_codepoint("approx", 8776)
        call expect_codepoint("sim", 8764)
        call expect_codepoint("int", 8747)
        call expect_codepoint("infty", 8734)
        call expect_codepoint("partial", 8706)
        call expect_codepoint("nabla", 8711)
        call expect_codepoint("leq", 8804)
        call expect_codepoint("geq", 8805)
        call expect_codepoint("neq", 8800)
        call expect_codepoint("equiv", 8801)
        print *, "test_operator_mapping: PASSED"
    end subroutine test_operator_mapping

    subroutine test_operators_in_text()
        character(len=200) :: result
        integer :: rlen

        call process_latex_in_text("A = \int f dx", result, rlen)
        if (index(result(1:rlen), "int") /= 0 .or. index(result(1:rlen), '\') /= 0) then
            print *, "ERROR: \int not converted: ", result(1:rlen)
            stop 1
        end if

        call process_latex_in_text("a \sim b \approx c \propto d", result, rlen)
        if (index(result(1:rlen), "sim") /= 0 .or. &
            index(result(1:rlen), "approx") /= 0 .or. &
            index(result(1:rlen), "propto") /= 0) then
            print *, "ERROR: relation operators not converted: ", result(1:rlen)
            stop 1
        end if
        print *, "test_operators_in_text: PASSED"
    end subroutine test_operators_in_text

    subroutine test_math_functions_preserve_command()
        character(len=200) :: result
        integer :: rlen

        call process_latex_in_text("$\sin(x) + \cos(x) + \lim_{x}$", result, rlen)
        if (index(result(1:rlen), "\sin") == 0 .or. &
            index(result(1:rlen), "\cos") == 0 .or. &
            index(result(1:rlen), "\lim") == 0) then
            print *, "ERROR: math function command not preserved: ", result(1:rlen)
            stop 1
        end if
        print *, "test_math_functions_preserve_command: PASSED"
    end subroutine test_math_functions_preserve_command

    subroutine test_unknown_command_keeps_name()
        character(len=200) :: result
        integer :: rlen

        ! Unknown command drops the backslash but keeps the whole name; the
        ! leading letter must not be lost (issue 2045).
        call process_latex_in_text("bad \foobar end", result, rlen)
        if (index(result(1:rlen), "foobar") == 0) then
            print *, "ERROR: unknown command mangled: ", result(1:rlen)
            stop 1
        end if
        if (index(result(1:rlen), '\') /= 0) then
            print *, "ERROR: backslash retained for unknown command: ", result(1:rlen)
            stop 1
        end if
        print *, "test_unknown_command_keeps_name: PASSED"
    end subroutine test_unknown_command_keeps_name

    subroutine test_escaped_specials_stay_literal()
        character(len=200) :: result
        integer :: rlen

        ! Non-alphabetic escapes keep their backslash for the mathtext layer.
        call process_latex_in_text("x\_y a\^b", result, rlen)
        if (index(result(1:rlen), '\_') == 0 .or. index(result(1:rlen), '\^') == 0) then
            print *, "ERROR: escaped specials not preserved: ", result(1:rlen)
            stop 1
        end if
        print *, "test_escaped_specials_stay_literal: PASSED"
    end subroutine test_escaped_specials_stay_literal

    subroutine test_ascii_fallbacks()
        !! Every operator codepoint must have a readable ASCII fallback so the
        !! ASCII backend never emits raw U+XXXX fragments (issue 2045/2046).
        integer, parameter :: codepoints(15) = [ &
            215, 183, 177, 966, 8733, 8776, 8764, 8747, &
            8734, 8706, 8711, 8804, 8805, 8800, 8801 ]
        character(len=16) :: ascii_equiv
        integer :: i

        do i = 1, size(codepoints)
            call unicode_codepoint_to_ascii(codepoints(i), ascii_equiv)
            if (index(ascii_equiv, 'U+') /= 0) then
                print *, "ERROR: no ASCII fallback for codepoint ", codepoints(i), &
                    " got ", trim(ascii_equiv)
                stop 1
            end if
        end do
        print *, "test_ascii_fallbacks: PASSED"
    end subroutine test_ascii_fallbacks

    subroutine test_pdf_escape_backslash()
        character(len=32) :: out
        integer :: olen

        call escape_pdf_string(achar(92)//"n", out, olen)
        ! A literal backslash must be doubled so PDF readers do not treat the
        ! following character as an escape (issue 2045: \times -> imes).
        if (olen < 3) then
            print *, "ERROR: backslash not escaped, len=", olen
            stop 1
        end if
        if (out(1:3) /= achar(92)//achar(92)//"n") then
            print *, "ERROR: backslash escaping wrong: ", out(1:olen)
            stop 1
        end if
        print *, "test_pdf_escape_backslash: PASSED"
    end subroutine test_pdf_escape_backslash

end program test_mathtext_operators
