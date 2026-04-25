program test_latex_sqrt_conversion
    !! Test that \sqrt{...} is converted to Unicode radical in process_latex_in_text
    use fortplot_latex_parser
    use fortplot_unicode
    implicit none

    call test_sqrt_basic()
    call test_sqrt_with_nested_latex()
    call test_sqrt_in_legend_label()
    call test_sqrt_no_braces()
    call test_sqrt_nested_braces()
    call test_sqrt_empty_braces()

    print *, "All sqrt conversion tests passed!"

contains

    subroutine test_sqrt_basic()
        character(len=200) :: result_text
        integer :: result_len, codepoint

        call process_latex_in_text('\sqrt{x}', result_text, result_len)

        if (result_len < 3) then
            print *, 'FAIL: test_sqrt_basic - result too short'
            stop 1
        end if

        ! First character should be Unicode radical (U+221A = 8730)
        codepoint = utf8_to_codepoint(result_text, 1)
        if (codepoint /= 8730) then
            print *, 'FAIL: test_sqrt_basic - expected radical codepoint 8730, got', codepoint
            stop 1
        end if

        ! Should not contain literal \sqrt
        if (index(result_text(1:result_len), 'sqrt') /= 0) then
            print *, 'FAIL: test_sqrt_basic - literal sqrt still present'
            stop 1
        end if

        ! Check that 'x' appears in the result
        if (index(result_text(1:result_len), 'x') == 0) then
            print *, 'FAIL: test_sqrt_basic - x not found in result'
            stop 1
        end if

        ! Result should be exactly radical + x (4 bytes, no stray braces)
        if (result_len /= 4) then
            print *, 'FAIL: test_sqrt_basic - expected 4 bytes, got', result_len
            stop 1
        end if

        print *, 'test_sqrt_basic: PASSED'
    end subroutine

    subroutine test_sqrt_with_nested_latex()
        character(len=4096) :: result_text
        integer :: result_len, codepoint, pos

        call process_latex_in_text('\sqrt{2\pi\sigma^2}', result_text, result_len)

        if (result_len < 3) then
            print *, 'FAIL: test_sqrt_with_nested_latex - result too short'
            stop 1
        end if

        ! First character should be Unicode radical
        codepoint = utf8_to_codepoint(result_text, 1)
        if (codepoint /= 8730) then
            print *, 'FAIL: test_sqrt_with_nested_latex - expected radical, got', codepoint
            stop 1
        end if

        ! Inner content should have \pi and \sigma converted
        if (index(result_text(1:result_len), '\pi') /= 0) then
            print *, 'FAIL: test_sqrt_with_nested_latex - \pi not converted inside sqrt'
            stop 1
        end if

        if (index(result_text(1:result_len), '\sigma') /= 0) then
            print *, 'FAIL: test_sqrt_with_nested_latex - \sigma not converted inside sqrt'
            stop 1
        end if

        ! Braces should be stripped
        if (index(result_text(1:result_len), '{') /= 0) then
            print *, 'FAIL: test_sqrt_with_nested_latex - braces not stripped'
            stop 1
        end if

        print *, 'test_sqrt_with_nested_latex: PASSED'
    end subroutine

    subroutine test_sqrt_in_legend_label()
        character(len=4096) :: result_text
        integer :: result_len, codepoint

        ! Simulate the exact label from the unicode_demo example
        call process_latex_in_text( &
            'Gaussian: \rho(\xi) = e^{-\xi^2/2\sigma^2}/\sqrt{2\pi\sigma^2}', &
            result_text, result_len)

        if (result_len < 10) then
            print *, 'FAIL: test_sqrt_in_legend_label - result too short'
            stop 1
        end if

        ! Should contain radical symbol
        if (index(result_text(1:result_len), achar(226)//achar(136)//achar(154)) == 0) then
            print *, 'FAIL: test_sqrt_in_legend_label - radical symbol not found'
            stop 1
        end if

        ! Should NOT contain literal \sqrt
        if (index(result_text(1:result_len), '\sqrt') /= 0) then
            print *, 'FAIL: test_sqrt_in_legend_label - literal \sqrt still present'
            stop 1
        end if

        ! Greek letters should be converted
        if (index(result_text(1:result_len), '\rho') /= 0) then
            print *, 'FAIL: test_sqrt_in_legend_label - \rho not converted'
            stop 1
        end if

        if (index(result_text(1:result_len), '\xi') /= 0) then
            print *, 'FAIL: test_sqrt_in_legend_label - \xi not converted'
            stop 1
        end if

        if (index(result_text(1:result_len), '\sigma') /= 0) then
            print *, 'FAIL: test_sqrt_in_legend_label - \sigma not converted'
            stop 1
        end if

        if (index(result_text(1:result_len), '\pi') /= 0) then
            print *, 'FAIL: test_sqrt_in_legend_label - \pi not converted'
            stop 1
        end if

        print *, 'test_sqrt_in_legend_label: PASSED'
    end subroutine

    subroutine test_sqrt_no_braces()
        character(len=200) :: result_text
        integer :: result_len, codepoint

        ! \sqrt without braces: should convert to radical + single next char
        call process_latex_in_text('\sqrt x', result_text, result_len)

        if (result_len < 3) then
            print *, 'FAIL: test_sqrt_no_braces - result too short'
            stop 1
        end if

        codepoint = utf8_to_codepoint(result_text, 1)
        if (codepoint /= 8730) then
            print *, 'FAIL: test_sqrt_no_braces - expected radical, got', codepoint
            stop 1
        end if

        print *, 'test_sqrt_no_braces: PASSED'
    end subroutine

    subroutine test_sqrt_nested_braces()
        character(len=4096) :: result_text
        integer :: result_len, codepoint

        ! \sqrt with nested braces
        call process_latex_in_text('\sqrt{a_{b}}', result_text, result_len)

        if (result_len < 3) then
            print *, 'FAIL: test_sqrt_nested_braces - result too short'
            stop 1
        end if

        codepoint = utf8_to_codepoint(result_text, 1)
        if (codepoint /= 8730) then
            print *, 'FAIL: test_sqrt_nested_braces - expected radical, got', codepoint
            stop 1
        end if

        ! Inner braces should be preserved
        if (index(result_text(1:result_len), '{') == 0) then
            print *, 'FAIL: test_sqrt_nested_braces - inner braces stripped'
            stop 1
        end if

        print *, 'test_sqrt_nested_braces: PASSED'
    end subroutine

    subroutine test_sqrt_empty_braces()
        character(len=200) :: result_text
        integer :: result_len, codepoint

        call process_latex_in_text('\sqrt{}', result_text, result_len)

        if (result_len < 3) then
            print *, 'FAIL: test_sqrt_empty_braces - result too short'
            stop 1
        end if

        codepoint = utf8_to_codepoint(result_text, 1)
        if (codepoint /= 8730) then
            print *, 'FAIL: test_sqrt_empty_braces - expected radical, got', codepoint
            stop 1
        end if

        print *, 'test_sqrt_empty_braces: PASSED'
    end subroutine

end program test_latex_sqrt_conversion
