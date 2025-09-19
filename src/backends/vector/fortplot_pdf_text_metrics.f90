module fortplot_pdf_text_metrics
    !! PDF text measurement helpers

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext, ELEMENT_SQRT
    use fortplot_text_layout, only: has_mathtext, preprocess_math_text
    use fortplot_pdf_core, only: PDF_LABEL_SIZE
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length, check_utf8_sequence
    implicit none
    private

    public :: estimate_pdf_text_width
    ! Helvetica widths indexed by WinAnsi code points for exact PDF sizing
    ! Data derived from Matplotlib Helvetica AFM file (PSF compatible license)
    integer, parameter, private :: helvetica_width_table(0:255) = [ integer :: &
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, &
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, &
        278, 278, 355, 556, 556, 889, 667, 222, 333, 333, 389, 584, 278, 333, 278, 278, &
        556, 556, 556, 556, 556, 556, 556, 556, 556, 556, 278, 278, 584, 584, 584, 556, &
        1015, 667, 667, 722, 722, 667, 611, 778, 722, 278, 500, 667, 556, 833, 722, 778, &
        667, 778, 722, 667, 611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556, &
        222, 556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833, 556, 556, &
        556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500, 334, 260, 334, 584, 500, &
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, &
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, &
        500, 333, 556, 556, 167, 556, 556, 556, 556, 191, 333, 556, 333, 333, 500, 500, &
        500, 556, 556, 556, 278, 500, 537, 350, 222, 333, 333, 556, 1000, 1000, 500, 611, &
        500, 333, 333, 333, 333, 333, 333, 333, 333, 500, 333, 333, 500, 333, 333, 333, &
        1000, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, &
        500, 1000, 500, 370, 500, 500, 500, 500, 556, 778, 1000, 365, 500, 500, 500, 500, &
        500, 889, 500, 500, 500, 278, 500, 500, 222, 611, 944, 611, 500, 500, 500, 500 ]

contains

    real(wp) function estimate_pdf_text_width(text, font_size) result(width)
        !! Estimate rendered width (in PDF points) of a text string
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: font_size
        real(wp) :: fs

        fs = PDF_LABEL_SIZE
        if (present(font_size)) fs = font_size

        if (has_mathtext(text)) then
            width = estimate_mathtext_width(text, fs)
        else
            width = estimate_plain_text_width(text, fs)
        end if
    end function estimate_pdf_text_width

    real(wp) function estimate_plain_text_width(text, fs) result(w)
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: fs
        integer :: i, codepoint, char_len
        logical :: is_valid

        w = 0.0_wp
        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len <= 1) then
                codepoint = ichar(text(i:i))
                w = w + fs * real(helv_width_units(codepoint), wp) / 1000.0_wp
                i = i + 1
            else
                call check_utf8_sequence(text, i, is_valid, char_len)
                if (is_valid .and. i + char_len - 1 <= len_trim(text)) then
                    codepoint = utf8_to_codepoint(text, i)
                else
                    codepoint = 0
                end if
                w = w + fs * real(helv_width_units(codepoint), wp) / 1000.0_wp
                i = i + max(1, char_len)
            end if
        end do
    end function estimate_plain_text_width

    real(wp) function estimate_mathtext_width(text, fs) result(w)
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: fs
        type(mathtext_element_t), allocatable :: elements(:)
        character(len=4096) :: processed
        integer :: plen
        integer :: i

        w = 0.0_wp
        call preprocess_math_text(text, processed, plen)
        elements = parse_mathtext(processed(1:plen))
        do i = 1, size(elements)
            w = w + measure_mathtext_element_width(elements(i), fs)
        end do
    end function estimate_mathtext_width

    real(wp) function measure_mathtext_element_width(element, base_font_size) result(w)
        type(mathtext_element_t), intent(in) :: element
        real(wp), intent(in) :: base_font_size
        real(wp) :: elem_font_size
        integer :: i, codepoint, char_len

        w = 0.0_wp
        elem_font_size = base_font_size * element%font_size_ratio

        ! Include radical head width for sqrt elements, then measure radicand recursively
        if (element%element_type == ELEMENT_SQRT) then
            w = w + 0.6_wp * elem_font_size
            w = w + estimate_mathtext_width(element%text, elem_font_size)
            return
        end if

        i = 1
        do while (i <= len_trim(element%text))
            char_len = utf8_char_length(element%text(i:i))
            if (char_len <= 1) then
                codepoint = ichar(element%text(i:i))
                w = w + elem_font_size * real(helv_width_units(codepoint), wp) / &
                    1000.0_wp
                i = i + 1
            else
                codepoint = utf8_to_codepoint(element%text, i)
                w = w + elem_font_size * real(helv_width_units(codepoint), wp) / &
                    1000.0_wp
                i = i + max(1, char_len)
            end if
        end do
    end function measure_mathtext_element_width

    integer function helv_width_units(codepoint) result(wu)
        !! Return Helvetica advance width in 1000-unit em for given codepoint
        integer, intent(in) :: codepoint

        if (codepoint >= 0 .and. codepoint <= 255) then
            wu = helvetica_width_table(codepoint)
        else
            wu = 500
        end if
    end function helv_width_units

end module fortplot_pdf_text_metrics
