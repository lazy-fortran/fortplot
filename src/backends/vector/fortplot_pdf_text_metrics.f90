module fortplot_pdf_text_metrics
    !! PDF text measurement helpers

    use iso_fortran_env, only: wp => real64
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext
    use fortplot_pdf_core, only: PDF_LABEL_SIZE
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length, check_utf8_sequence
    implicit none
    private

    public :: estimate_pdf_text_width

contains

    real(wp) function estimate_pdf_text_width(text, font_size) result(width)
        !! Estimate rendered width (in PDF points) of a text string
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: font_size
        real(wp) :: fs

        fs = PDF_LABEL_SIZE
        if (present(font_size)) fs = font_size

        if (index(text, '^') > 0 .or. index(text, '_') > 0) then
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
        integer :: i

        w = 0.0_wp
        elements = parse_mathtext(text)
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
        !! Return Helvetica (Base14) advance width in 1000-unit em for given codepoint
        integer, intent(in) :: codepoint

        select case(codepoint)
        case(32)
            wu = 278
        case(45)
            wu = 333
        case(46)
            wu = 278
        case(48:57)
            wu = 556
        case(40, 41)
            wu = 333
        case(43)
            wu = 584
        case(44)
            wu = 278
        case(47)
            wu = 278
        case(69)
            wu = 556
        case(101)
            wu = 444
        case(88, 120)
            wu = 667
        case default
            if (codepoint >= 65 .and. codepoint <= 90) then
                wu = 667
            else if (codepoint >= 97 .and. codepoint <= 122) then
                wu = 444
            else
                wu = 500
            end if
        end select
    end function helv_width_units

end module fortplot_pdf_text_metrics
