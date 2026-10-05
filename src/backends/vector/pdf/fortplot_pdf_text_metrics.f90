module fortplot_pdf_text_metrics
    !! PDF text measurement helpers

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext, &
                                ELEMENT_NORMAL, ELEMENT_SQRT, ELEMENT_FRACTION, &
                                ELEMENT_SPACE, &
                                mathtext_scripts_share_anchor
    use fortplot_text_layout, only: has_mathtext, preprocess_math_text
    use fortplot_pdf_core, only: PDF_LABEL_SIZE
    use fortplot_pdf_text_escape, only: unicode_to_symbol_char, &
        lookup_script_fallback, SCRIPT_SCALE
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length, check_utf8_sequence
    implicit none
    private

    public :: estimate_pdf_text_width
    public :: helv_width_units, measure_mathtext_elements_width
    public :: measure_mathtext_element_width

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

    ! Adobe Symbol AFM advances, matching the font selected for Unicode symbols.
    integer, parameter :: symbol_width_table(0:255) = [ integer :: &
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, &
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, &
        500, 500, 500, 500, 500, 500, 500, 500, 250, 333, 713, 500, &
        549, 833, 778, 439, 333, 333, 500, 549, 250, 549, 250, 278, &
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 278, 278, &
        549, 549, 549, 444, 549, 722, 667, 722, 612, 611, 763, 603, &
        722, 333, 631, 722, 686, 889, 722, 722, 768, 741, 556, 592, &
        611, 690, 439, 768, 645, 795, 611, 333, 863, 333, 658, 500, &
        500, 631, 549, 549, 494, 439, 521, 411, 603, 329, 603, 549, &
        549, 576, 521, 549, 549, 521, 549, 603, 439, 576, 713, 686, &
        493, 686, 494, 480, 200, 480, 549, 500, 500, 500, 500, 500, &
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, &
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, &
        500, 500, 500, 500, 750, 620, 247, 549, 167, 713, 500, 753, &
        753, 753, 753, 1042, 987, 603, 987, 603, 400, 549, 411, 549, &
        549, 713, 494, 460, 549, 549, 549, 549, 1000, 603, 1000, 658, &
        823, 686, 795, 987, 768, 768, 823, 768, 768, 713, 713, 713, &
        713, 713, 713, 713, 768, 713, 790, 790, 890, 823, 549, 250, &
        713, 603, 603, 1042, 987, 603, 987, 603, 494, 329, 790, 790, &
        786, 713, 384, 384, 384, 384, 384, 384, 494, 494, 494, 494, &
        500, 329, 274, 686, 686, 686, 384, 384, 384, 384, 384, 384, &
        494, 494, 494, 500 ]

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
        !! Note: Uses len not len_trim to preserve trailing spaces in mathtext elements
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: fs
        integer :: i, codepoint, char_len, text_len
        logical :: is_valid

        text_len = len(text)
        w = 0.0_wp
        i = 1
        do while (i <= text_len)
            char_len = utf8_char_length(text(i:i))
            if (char_len <= 1) then
                codepoint = ichar(text(i:i))
                w = w + fs * real(helv_width_units(codepoint), wp) / 1000.0_wp
                i = i + 1
            else
                call check_utf8_sequence(text, i, is_valid, char_len)
                if (is_valid .and. i + char_len - 1 <= text_len) then
                    codepoint = utf8_to_codepoint(text, i)
                else
                    codepoint = 0
                end if
                w = w + fs * real(helv_width_units(codepoint), wp) / 1000.0_wp
                i = i + max(1, char_len)
            end if
        end do
    end function estimate_plain_text_width

    recursive real(wp) function estimate_mathtext_width(text, fs) result(w)
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: fs
        type(mathtext_element_t), allocatable :: elements(:)
        character(len=4096) :: processed
        integer :: plen
        integer :: i

        w = 0.0_wp
        call preprocess_math_text(text, processed, plen)
        elements = parse_mathtext(processed(1:plen))
        w = measure_mathtext_elements_width(elements, fs)
    end function estimate_mathtext_width

    recursive real(wp) function measure_mathtext_elements_width(elements, fs) result(w)
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(in) :: fs
        real(wp) :: anchor, element_x
        integer :: i

        w = 0.0_wp
        anchor = 0.0_wp
        do i = 1, size(elements)
            element_x = w
            if (mathtext_scripts_share_anchor(elements, i)) element_x = anchor
            anchor = element_x
            w = max(w, element_x + measure_mathtext_element_width(elements(i), fs))
        end do
    end function measure_mathtext_elements_width

    recursive real(wp) function measure_mathtext_element_width(element, &
                                                               base_font_size) result(w)
        type(mathtext_element_t), intent(in) :: element
        real(wp), intent(in) :: base_font_size
        real(wp) :: elem_font_size, denominator_width
        type(mathtext_element_t), allocatable :: children(:)

        elem_font_size = base_font_size * element%font_size_ratio
        if (element%element_type == ELEMENT_SPACE) then
            w = 0.2_wp * estimate_plain_text_width('m', elem_font_size)
        else if (element%element_type == ELEMENT_FRACTION) then
            children = parse_mathtext(element%text, element%italic)
            w = measure_mathtext_elements_width(children, 0.7_wp * elem_font_size)
            children = parse_mathtext(element%denominator, element%italic)
            denominator_width = measure_mathtext_elements_width(children, &
                0.7_wp * elem_font_size)
            w = max(w, denominator_width) + 0.125_wp * elem_font_size
        else if (element%element_type /= ELEMENT_NORMAL) then
            children = parse_mathtext(element%text, element%italic)
            w = measure_mathtext_elements_width(children, elem_font_size)
            if (element%element_type == ELEMENT_SQRT) w = w + 0.6_wp * elem_font_size
        else
            w = estimate_plain_text_width(element%text, elem_font_size)
        end if
    end function measure_mathtext_element_width

    integer function helv_width_units(codepoint) result(wu)
        !! Return Helvetica advance width in 1000-unit em for given codepoint
        integer, intent(in) :: codepoint
        character(len=8) :: symbol_char
        character(len=1) :: base
        integer :: symbol_code, status

        call unicode_to_symbol_char(codepoint, symbol_char)
        if (len_trim(symbol_char) > 0) then
            read (symbol_char(2:), '(O3)', iostat=status) symbol_code
            if (status == 0) then
                if (symbol_code >= 0 .and. symbol_code <= 255) then
                    wu = symbol_width_table(symbol_code)
                    return
                end if
            end if
        end if

        if (codepoint >= 0 .and. codepoint <= 255) then
            wu = helvetica_width_table(codepoint)
        else if (codepoint == 8722) then
            ! U+2212 minus renders via the Helvetica /minus glyph.
            wu = 584
        else if (codepoint == 8211) then
            ! U+2013 en dash renders as the WinAnsi en dash.
            wu = 556
        else if (codepoint == 8212) then
            ! U+2014 em dash.
            wu = 1000
        else if (codepoint == 295 .or. codepoint == 8463) then
            ! h-bar drawn as Helvetica h with an overstruck macron.
            wu = 556
        else if (codepoint == 8214) then
            ! Double vertical line drawn as two Helvetica bars.
            wu = 2*helvetica_width_table(124)
        else if (is_script_codepoint(codepoint, base)) then
            wu = nint(SCRIPT_SCALE*real(helvetica_width_table(iachar(base)), wp))
        else
            wu = 500
        end if
    end function helv_width_units

    logical function is_script_codepoint(codepoint, base) result(is_script)
        integer, intent(in) :: codepoint
        character(len=1), intent(out) :: base
        integer :: rise

        call lookup_script_fallback(codepoint, base, rise)
        is_script = rise /= 0
    end function is_script_codepoint

end module fortplot_pdf_text_metrics
