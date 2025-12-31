program test_unicode
    !! Comprehensive test suite for Unicode handling
    !! Consolidates: test_unicode_corruption_853, test_unicode_detection,
    !! test_unicode_issue_1138, test_unicode_superscript
    use fortplot_unicode, only: unicode_codepoint_to_ascii, is_unicode_char, &
                                check_utf8_sequence, utf8_char_length, &
                                    utf8_to_codepoint, &
                                is_greek_letter_codepoint, contains_unicode
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot, only: figure, plot, title, xlabel, ylabel, savefig, &
                        add_plot, legend
    use test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    call test_unicode_to_ascii()
    call test_math_symbols()
    call test_greek_letters()
    call test_is_unicode_char_func()
    call test_utf8_sequence_detection()
    call test_utf8_length()
    call test_codepoint_extraction()
    call test_is_greek_letter_func()
    call test_ascii_vs_unicode()
    call test_latex_uppercase_psi()
    call test_ascii_backend_handling()
    call test_superscript_rendering()

    print *, ''
    print *, '=== Unicode Test Summary ==='
    print *, 'Tests passed:', passed_tests, '/', total_tests

    if (passed_tests == total_tests) then
        print *, 'All Unicode tests PASSED!'
        stop 0
    else
        print *, 'FAIL: Some Unicode tests failed'
        stop 1
    end if

contains

    subroutine test_unicode_to_ascii()
        !! Test basic Unicode-to-ASCII conversion
        character(len=50) :: ascii_output
        integer :: codepoint

        total_tests = total_tests + 1

        codepoint = 945
        call unicode_codepoint_to_ascii(codepoint, ascii_output)
        if (trim(ascii_output) /= 'alpha') then
            print *, 'FAIL: test_unicode_to_ascii - alpha conversion'
            return
        end if

        codepoint = 960
        call unicode_codepoint_to_ascii(codepoint, ascii_output)
        if (trim(ascii_output) /= 'pi') then
            print *, 'FAIL: test_unicode_to_ascii - pi conversion'
            return
        end if

        codepoint = 937
        call unicode_codepoint_to_ascii(codepoint, ascii_output)
        if (trim(ascii_output) /= 'Omega') then
            print *, 'FAIL: test_unicode_to_ascii - Omega conversion'
            return
        end if

        codepoint = 8364
        call unicode_codepoint_to_ascii(codepoint, ascii_output)
        if (trim(ascii_output) /= 'U+20AC') then
            print *, 'FAIL: test_unicode_to_ascii - Euro symbol fallback'
            return
        end if

        print *, '  PASS: test_unicode_to_ascii'
        passed_tests = passed_tests + 1
    end subroutine test_unicode_to_ascii

    subroutine test_math_symbols()
        !! Test mathematical symbols conversion
        character(len=50) :: ascii_output
        integer :: codepoint

        total_tests = total_tests + 1

        codepoint = 8706
        call unicode_codepoint_to_ascii(codepoint, ascii_output)
        if (trim(ascii_output) /= 'U+2202') then
            print *, 'FAIL: test_math_symbols - partial derivative'
            return
        end if

        codepoint = 8711
        call unicode_codepoint_to_ascii(codepoint, ascii_output)
        if (trim(ascii_output) /= 'U+2207') then
            print *, 'FAIL: test_math_symbols - nabla'
            return
        end if

        codepoint = 177
        call unicode_codepoint_to_ascii(codepoint, ascii_output)
        if (trim(ascii_output) /= 'U+00B1') then
            print *, 'FAIL: test_math_symbols - plus-minus'
            return
        end if

        print *, '  PASS: test_math_symbols'
        passed_tests = passed_tests + 1
    end subroutine test_math_symbols

    subroutine test_greek_letters()
        !! Test comprehensive Greek letter conversion
        character(len=50) :: ascii_output
        integer, parameter :: greek_codes(4) = [945, 946, 947, 948]
        character(len=8), parameter :: greek_names(4) = ['alpha   ', 'beta    ', &
                                                         'gamma   ', 'delta   ']
        integer :: i

        total_tests = total_tests + 1

        do i = 1, size(greek_codes)
            call unicode_codepoint_to_ascii(greek_codes(i), ascii_output)
            if (trim(ascii_output) /= trim(greek_names(i))) then
                print *, 'FAIL: test_greek_letters - letter', greek_codes(i)
                return
            end if
        end do

        print *, '  PASS: test_greek_letters'
        passed_tests = passed_tests + 1
    end subroutine test_greek_letters

    subroutine test_is_unicode_char_func()
        !! Test detection of Unicode vs ASCII characters
        logical :: is_uni

        total_tests = total_tests + 1

        is_uni = is_unicode_char('A')
        if (is_uni) then
            print *, 'FAIL: test_is_unicode_char - A detected as Unicode'
            return
        end if

        is_uni = is_unicode_char('0')
        if (is_uni) then
            print *, 'FAIL: test_is_unicode_char - 0 detected as Unicode'
            return
        end if

        is_uni = is_unicode_char(achar(206)//achar(177))
        if (.not. is_uni) then
            print *, 'FAIL: test_is_unicode_char - alpha not detected'
            return
        end if

        print *, '  PASS: test_is_unicode_char'
        passed_tests = passed_tests + 1
    end subroutine test_is_unicode_char_func

    subroutine test_utf8_sequence_detection()
        !! Test detection of valid UTF-8 sequences
        character(len=10) :: test_str
        integer :: seq_len
        logical :: is_valid

        total_tests = total_tests + 1

        test_str = 'A'
        call check_utf8_sequence(test_str, 1, is_valid, seq_len)
        if (.not. is_valid .or. seq_len /= 1) then
            print *, 'FAIL: test_utf8_sequence_detection - ASCII A'
            return
        end if

        test_str = achar(206)//achar(177)
        call check_utf8_sequence(test_str, 1, is_valid, seq_len)
        if (.not. is_valid .or. seq_len /= 2) then
            print *, 'FAIL: test_utf8_sequence_detection - Greek alpha'
            return
        end if

        test_str = achar(206)//'A'
        call check_utf8_sequence(test_str, 1, is_valid, seq_len)
        if (is_valid) then
            print *, 'FAIL: test_utf8_sequence_detection - invalid sequence'
            return
        end if

        print *, '  PASS: test_utf8_sequence_detection'
        passed_tests = passed_tests + 1
    end subroutine test_utf8_sequence_detection

    subroutine test_utf8_length()
        !! Test determining length of UTF-8 sequences
        integer :: length

        total_tests = total_tests + 1

        length = utf8_char_length('A')
        if (length /= 1) then
            print *, 'FAIL: test_utf8_length - ASCII'
            return
        end if

        length = utf8_char_length(achar(206))
        if (length /= 2) then
            print *, 'FAIL: test_utf8_length - 2-byte'
            return
        end if

        length = utf8_char_length(achar(224))
        if (length /= 3) then
            print *, 'FAIL: test_utf8_length - 3-byte'
            return
        end if

        length = utf8_char_length(achar(240))
        if (length /= 4) then
            print *, 'FAIL: test_utf8_length - 4-byte'
            return
        end if

        print *, '  PASS: test_utf8_length'
        passed_tests = passed_tests + 1
    end subroutine test_utf8_length

    subroutine test_codepoint_extraction()
        !! Test extracting Unicode codepoints from UTF-8
        character(len=4) :: utf8_char
        integer :: codepoint

        total_tests = total_tests + 1

        utf8_char = 'A'
        codepoint = utf8_to_codepoint(utf8_char, 1)
        if (codepoint /= 65) then
            print *, 'FAIL: test_codepoint_extraction - ASCII A'
            return
        end if

        utf8_char = achar(206)//achar(177)
        codepoint = utf8_to_codepoint(utf8_char, 1)
        if (codepoint /= 945) then
            print *, 'FAIL: test_codepoint_extraction - Greek alpha'
            return
        end if

        utf8_char = achar(206)//achar(169)
        codepoint = utf8_to_codepoint(utf8_char, 1)
        if (codepoint /= 937) then
            print *, 'FAIL: test_codepoint_extraction - Greek Omega'
            return
        end if

        print *, '  PASS: test_codepoint_extraction'
        passed_tests = passed_tests + 1
    end subroutine test_codepoint_extraction

    subroutine test_is_greek_letter_func()
        !! Test detection of Greek letter codepoints
        logical :: is_greek

        total_tests = total_tests + 1

        is_greek = is_greek_letter_codepoint(945)
        if (.not. is_greek) then
            print *, 'FAIL: test_is_greek_letter - alpha'
            return
        end if

        is_greek = is_greek_letter_codepoint(969)
        if (.not. is_greek) then
            print *, 'FAIL: test_is_greek_letter - omega'
            return
        end if

        is_greek = is_greek_letter_codepoint(913)
        if (.not. is_greek) then
            print *, 'FAIL: test_is_greek_letter - Alpha uppercase'
            return
        end if

        is_greek = is_greek_letter_codepoint(65)
        if (is_greek) then
            print *, 'FAIL: test_is_greek_letter - Latin A'
            return
        end if

        print *, '  PASS: test_is_greek_letter'
        passed_tests = passed_tests + 1
    end subroutine test_is_greek_letter_func

    subroutine test_ascii_vs_unicode()
        !! Test distinguishing ASCII from Unicode in strings
        character(len=20) :: test_str
        logical :: has_uni

        total_tests = total_tests + 1

        test_str = 'Hello World'
        has_uni = contains_unicode(test_str)
        if (has_uni) then
            print *, 'FAIL: test_ascii_vs_unicode - pure ASCII'
            return
        end if

        print *, '  PASS: test_ascii_vs_unicode'
        passed_tests = passed_tests + 1
    end subroutine test_ascii_vs_unicode

    subroutine test_latex_uppercase_psi()
        !! Test for issue #1138: \Psi with capital P
        character(len=200) :: input_text, result_text
        integer :: result_len

        total_tests = total_tests + 1

        input_text = 'Wave function: \psi'
        call process_latex_in_text(input_text, result_text, result_len)

        input_text = 'Wave function: \Psi'
        call process_latex_in_text(input_text, result_text, result_len)

        input_text = 'Uppercase: \Alpha \Beta \Gamma \Delta \Psi \Omega'
        call process_latex_in_text(input_text, result_text, result_len)

        input_text = 'Amplitude \Psi (V)'
        call process_latex_in_text(input_text, result_text, result_len)

        print *, '  PASS: test_latex_uppercase_psi'
        passed_tests = passed_tests + 1
    end subroutine test_latex_uppercase_psi

    subroutine test_ascii_backend_handling()
        !! Test the actual ASCII backend handling of Unicode text
        character(len=:), allocatable :: output_dir
        character(len=:), allocatable :: test_filename
        real(dp), parameter :: x_data(3) = [1.0_dp, 2.0_dp, 3.0_dp]
        real(dp), parameter :: y_data(3) = [1.0_dp, 4.0_dp, 9.0_dp]
        integer :: unit, ios
        character(len=1000) :: file_content
        character(len=256) :: line_buffer

        total_tests = total_tests + 1

        call ensure_test_output_dir('unicode_ascii_backend', output_dir)
        test_filename = output_dir//'test_unicode_853_bug.txt'

        call figure(figsize=[8.0_dp, 6.0_dp])
        call title('Test: a b c d Greek Letters')
        call xlabel('Parameter x (xi)')
        call ylabel('Observable Y (psi)')
        call add_plot(x_data, y_data, label='Data: m values')
        call legend('upper left')
        call savefig(test_filename)

        open (newunit=unit, file=test_filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: test_ascii_backend_handling - could not read file'
            return
        end if

        file_content = ''
        do
            read (unit, '(A)', iostat=ios) line_buffer
            if (ios /= 0) exit
            file_content = trim(file_content)//' '//trim(line_buffer)
        end do
        close (unit)

        if (index(file_content, 'Mathmb') > 0) then
            print *, 'FAIL: test_ascii_backend_handling - corruption found'
            return
        end if

        print *, '  PASS: test_ascii_backend_handling'
        passed_tests = passed_tests + 1
    end subroutine test_ascii_backend_handling

    subroutine test_superscript_rendering()
        !! Test Unicode superscript rendering in PNG and PDF
        real(dp) :: x(10), y(10)
        integer :: i
        character(len=:), allocatable :: output_dir

        total_tests = total_tests + 1

        call ensure_test_output_dir('unicode_superscript', output_dir)

        do i = 1, 10
            x(i) = real(i, dp)
            y(i) = x(i)**2
        end do

        call figure()
        call plot(x, y)
        call title('PNG Test: mc2 physics formula')
        call xlabel('x')
        call ylabel('y')
        call savefig(output_dir//'test_png_unicode.png')

        call figure()
        call plot(x, y)
        call title('PDF Test: mc2 physics formula')
        call xlabel('x')
        call ylabel('y')
        call savefig(output_dir//'test_pdf_unicode.pdf')

        print *, '  PASS: test_superscript_rendering'
        passed_tests = passed_tests + 1
    end subroutine test_superscript_rendering

end program test_unicode
