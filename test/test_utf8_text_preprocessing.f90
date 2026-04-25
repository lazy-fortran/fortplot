program test_utf8_text_preprocessing
    !! Test UTF-8 handling in text preprocessing pipeline (fixes #1705)
    !! Verifies that multi-byte UTF-8 characters are preserved intact
    !! through preprocess_math_text, process_latex_in_text, and
    !! prepare_text_for_raster.
    use fortplot_text_layout, only: preprocess_math_text
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_text_helpers, only: prepare_text_for_raster
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    use fortplot, only: figure, subplots, suptitle, plot, ylabel, title, xlabel, savefig, &
                        subplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    logical :: all_passed

    all_passed = .true.

    call test_preprocess_math_text_utf8(all_passed)
    call test_process_latex_utf8(all_passed)
    call test_prepare_raster_utf8(all_passed)
    call test_suptitle_scenario(all_passed)

    if (all_passed) then
        print *, 'All UTF-8 text preprocessing tests PASSED!'
    else
        print *, 'Some UTF-8 text preprocessing tests FAILED!'
        stop 1
    end if
contains

    subroutine test_preprocess_math_text_utf8(passed)
        !! Verify preprocess_math_text preserves multi-byte UTF-8 characters
        logical, intent(out) :: passed
        character(len=256) :: result
        integer :: result_len, backslash_pos
        integer :: cp_input, cp_output

        passed = .true.

        ! Test superscript 2 (U+00B2): bytes C2 B2
        call preprocess_math_text('x²', result, result_len)
        if (result_len < 3) then
            print *, 'FAIL: x² preprocessing lost bytes (len=', result_len, ')'
            passed = .false.
            return
        end if
        cp_input = utf8_to_codepoint('x²', 2)
        cp_output = utf8_to_codepoint(result, 2)
        if (cp_input /= cp_output) then
            print *, 'FAIL: x² superscript codepoint changed:', cp_input, '->', cp_output
            passed = .false.
            return
        end if

        ! Test superscript 3 (U+00B3): bytes C2 B3
        call preprocess_math_text('x³', result, result_len)
        if (result_len < 3) then
            print *, 'FAIL: x³ preprocessing lost bytes (len=', result_len, ')'
            passed = .false.
            return
        end if
        cp_input = utf8_to_codepoint('x³', 2)
        cp_output = utf8_to_codepoint(result, 2)
        if (cp_input /= cp_output) then
            print *, 'FAIL: x³ superscript codepoint changed:', cp_input, '->', cp_output
            passed = .false.
            return
        end if

        ! Test Greek letter alpha (U+03B1): 2-byte UTF-8
        call preprocess_math_text('α', result, result_len)
        if (result_len < 2) then
            print *, 'FAIL: α preprocessing lost bytes (len=', result_len, ')'
            passed = .false.
            return
        end if
        cp_input = utf8_to_codepoint('α', 1)
        cp_output = utf8_to_codepoint(result, 1)
        if (cp_input /= cp_output) then
            print *, 'FAIL: α codepoint changed:', cp_input, '->', cp_output
            passed = .false.
            return
        end if

        ! Test mixed ASCII + UTF-8 with ^ escaping
        ! x²^2: 'x' (1 byte) + '²' (2 bytes) + '^' (1 byte) + '2' (1 byte) = 5 bytes
        ! After preprocessing: 'x' + '²' + '\^' + '2' = 6 bytes
        call preprocess_math_text('x²^2', result, result_len)
        if (result_len < 6) then
            print *, 'FAIL: x²^2 preprocessing lost bytes (len=', result_len, ')'
            passed = .false.
            return
        end if
        ! The ^ should be escaped to \^; find it by scanning for '\'
        backslash_pos = index(result(1:result_len), '\')
        if (backslash_pos <= 0) then
            print *, 'FAIL: ^ in x²^2 should be escaped to \^ but no backslash found'
            passed = .false.
            return
        end if
        if (result(backslash_pos + 1:backslash_pos + 1) /= '^') then
            print *, 'FAIL: backslash in x²^2 should be followed by ^'
            passed = .false.
            return
        end if

        print *, 'PASS: preprocess_math_text preserves UTF-8 characters'
    end subroutine test_preprocess_math_text_utf8

    subroutine test_process_latex_utf8(passed)
        !! Verify process_latex_in_text preserves multi-byte UTF-8 characters
        logical, intent(out) :: passed
        character(len=256) :: result
        integer :: result_len
        integer :: cp_input, cp_output

        passed = .true.

        ! Test superscript 2 passes through unchanged
        call process_latex_in_text('x²', result, result_len)
        if (result_len < 3) then
            print *, 'FAIL: process_latex x² lost bytes (len=', result_len, ')'
            passed = .false.
            return
        end if
        cp_input = utf8_to_codepoint('x²', 2)
        cp_output = utf8_to_codepoint(result, 2)
        if (cp_input /= cp_output) then
            print *, 'FAIL: process_latex x² codepoint changed:', cp_input, '->', cp_output
            passed = .false.
            return
        end if

        ! Test LaTeX command with UTF-8 after it
        call process_latex_in_text('\alpha²', result, result_len)
        ! \alpha should become α (U+03B1, 2 bytes), then ² (U+00B2, 2 bytes)
        if (result_len < 4) then
            print *, 'FAIL: process_latex \alpha² lost bytes (len=', result_len, ')'
            passed = .false.
            return
        end if

        print *, 'PASS: process_latex_in_text preserves UTF-8 characters'
    end subroutine test_process_latex_utf8

    subroutine test_prepare_raster_utf8(passed)
        !! Verify prepare_text_for_raster preserves multi-byte UTF-8 characters
        logical, intent(out) :: passed
        character(len=600) :: result
        integer :: cp_input, cp_output

        passed = .true.

        ! Test full pipeline with superscript
        call prepare_text_for_raster('x²', result)
        cp_input = utf8_to_codepoint('x²', 2)
        cp_output = utf8_to_codepoint(result, 2)
        if (cp_input /= cp_output) then
            print *, 'FAIL: prepare_raster x² codepoint changed:', cp_input, '->', cp_output
            passed = .false.
            return
        end if

        ! Test plain ASCII text (suptitle scenario)
        call prepare_text_for_raster('Polynomial Growth Comparison', result)
        if (trim(result) /= 'Polynomial Growth Comparison') then
            print *, 'FAIL: prepare_raster changed plain ASCII suptitle text'
            print *, '  Expected: Polynomial Growth Comparison'
            print *, '  Got     :', trim(result)
            passed = .false.
            return
        end if

        print *, 'PASS: prepare_text_for_raster preserves UTF-8 characters'
    end subroutine test_prepare_raster_utf8

    subroutine test_suptitle_scenario(passed)
        !! Test the exact scenario from issue #1705:
        !! 1x3 subplot with suptitle and ylabels containing Unicode superscripts
        logical, intent(out) :: passed
        integer :: i
        real(wp), allocatable :: x(:), y(:)
        character(len=128) :: output_file
        logical :: file_exists

        passed = .true.
        output_file = 'build/test/output/test_utf8_suptitle.png'

        allocate(x(100), y(100))
        x = [(real(i, wp), i = 1, 100)]
        y = x**2

        ! Use stateful API: figure, suptitle, subplot, ylabel, savefig
        call figure(figsize=[12.0_wp, 4.0_wp])
        call suptitle('Polynomial Growth Comparison')
        call subplot(1, 3, 1)
        call plot(x, y)
        call ylabel('x²')
        call title('Quadratic')
        call savefig(output_file)

        ! Check file was created
        inquire(file=output_file, exist=file_exists)
        if (.not. file_exists) then
            print *, 'FAIL: UTF-8 suptitle test did not create output file'
            passed = .false.
        else
            print *, 'PASS: 1x3 subplot with Unicode ylabel and ASCII suptitle renders without error'
        end if

        deallocate(x, y)
    end subroutine test_suptitle_scenario
end program test_utf8_text_preprocessing
