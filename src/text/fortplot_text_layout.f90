module fortplot_text_layout
    !! Shared text layout utilities (width/height calculations, mathtext helpers)
    use fortplot_stb_truetype
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    use fortplot_logging, only: log_error
    use fortplot_text_fonts, only: init_text_system, get_global_font, get_font_scale, &
                                  is_font_initialized, get_font_scale_for_size, &
                                  get_font_metrics
    use fortplot_mathtext, only: parse_mathtext, mathtext_element_t
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: has_mathtext
    public :: preprocess_math_text
    public :: calculate_text_width, calculate_text_width_with_size, calculate_text_height
    public :: calculate_text_descent
    public :: calculate_mathtext_width_internal, calculate_text_width_with_size_internal
    public :: calculate_text_height_with_size_internal, calculate_mathtext_height_internal
    public :: DEFAULT_FONT_SIZE, TITLE_FONT_SIZE, LABEL_FONT_SIZE, TICK_FONT_SIZE

    integer, parameter :: DEFAULT_FONT_SIZE = 16  ! ~12pt at 96 DPI
    integer, parameter :: TITLE_FONT_SIZE = 20     ! ~15pt at 96 DPI
    integer, parameter :: LABEL_FONT_SIZE = 16     ! ~12pt at 96 DPI
    integer, parameter :: TICK_FONT_SIZE = 13      ! ~10pt at 96 DPI

contains

    pure function has_mathtext(text) result(is_mathtext)
        !! Check if text contains math segments delimited by '$...$'
        character(len=*), intent(in) :: text
        logical :: is_mathtext
        integer :: first_dollar, second_dollar

        first_dollar = index(text, '$')
        if (first_dollar <= 0) then
            is_mathtext = .false.
        else
            second_dollar = index(text(first_dollar+1:), '$')
            is_mathtext = (second_dollar > 0)
        end if
    end function has_mathtext

    function calculate_text_width(text) result(width)
        !! Calculate the pixel width of text using STB TrueType with UTF-8 support
        !! Supports mathematical notation with superscripts and subscripts
        character(len=*), intent(in) :: text
        integer :: width
        integer :: i, char_code, advance_width, left_side_bearing
        integer :: char_len
        type(stb_fontinfo_t) :: font
        real(wp) :: scale
        type(mathtext_element_t), allocatable :: elements(:)
        integer :: ix0, iy0, ix1, iy1
        integer :: pen_px, rightmost
        character(len=4096) :: processed
        integer :: plen

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                call log_error("text width initialization failed")
                width = len_trim(text) * 8  ! Fallback estimate
                return
            end if
        end if

        if (has_mathtext(text)) then
            call preprocess_math_text(text, processed, plen)
            elements = parse_mathtext(processed(1:plen))
            width = calculate_mathtext_width_internal(elements, &
                real(DEFAULT_FONT_SIZE, wp))
            return
        end if

        font = get_global_font()
        scale = get_font_scale()

        width = 0
        rightmost = 0
        pen_px = 0
        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len == 0) then
                char_code = iachar(text(i:i))
                i = i + 1
            else
                char_code = utf8_to_codepoint(text, i)
                i = i + char_len
            end if

            call stb_get_codepoint_bitmap_box(font, char_code, scale, scale, &
                ix0, iy0, ix1, iy1)
            rightmost = max(rightmost, pen_px + ix1)
            call stb_get_codepoint_hmetrics(font, char_code, advance_width, &
                left_side_bearing)
            pen_px = pen_px + int(real(advance_width) * scale)
        end do
        width = max(pen_px, rightmost)
    end function calculate_text_width

    function calculate_text_width_with_size(text, pixel_height) result(width)
        !! Calculate text width using a specific font size
        !! Supports mathematical notation with superscripts and subscripts
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: pixel_height
        integer :: width
        type(mathtext_element_t), allocatable :: elements(:)
        character(len=4096) :: processed
        integer :: plen

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                width = int(len_trim(text) * pixel_height * 0.6_wp)
                return
            end if
        end if

        if (has_mathtext(text)) then
            call preprocess_math_text(text, processed, plen)
            elements = parse_mathtext(processed(1:plen))
            width = calculate_mathtext_width_internal(elements, pixel_height)
            return
        end if

        width = calculate_text_width_with_size_internal(text, pixel_height)
    end function calculate_text_width_with_size

    function calculate_text_height(text) result(height)
        !! Calculate the pixel height of text using STB TrueType
        !! Supports mathematical notation with superscripts and subscripts
        character(len=*), intent(in) :: text
        integer :: height
        integer :: ascent, descent, line_gap
        type(stb_fontinfo_t) :: font
        real(wp) :: scale
        type(mathtext_element_t), allocatable :: elements(:)
        character(len=4096) :: processed
        integer :: plen

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                height = DEFAULT_FONT_SIZE  ! Fallback
                return
            end if
        end if

        if (has_mathtext(text)) then
            call preprocess_math_text(text, processed, plen)
            elements = parse_mathtext(processed(1:plen))
            height = calculate_mathtext_height_internal(elements, &
                real(DEFAULT_FONT_SIZE, wp))
            return
        end if

        font = get_global_font()
        scale = get_font_scale()

        call stb_get_font_vmetrics(font, ascent, descent, line_gap)
        height = int(real(ascent - descent) * scale)

        if (height <= 0) height = DEFAULT_FONT_SIZE
    end function calculate_text_height

    function calculate_text_descent(text) result(descent_pixels)
        !! Calculate the descent (below baseline) portion of text in pixels
        character(len=*), intent(in) :: text
        integer :: descent_pixels
        integer :: ascent, descent, line_gap
        type(stb_fontinfo_t) :: font
        real(wp) :: scale

        associate(unused_text => text); end associate

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                descent_pixels = 4
                return
            end if
        end if

        font = get_global_font()
        scale = get_font_scale()

        call stb_get_font_vmetrics(font, ascent, descent, line_gap)
        descent_pixels = int(abs(real(descent) * scale))
        if (descent_pixels <= 0) descent_pixels = 4
    end function calculate_text_descent

    recursive function calculate_mathtext_width_internal(elements, &
            base_font_size) result(total_width)
        !! Calculate total width of mathematical text elements
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(in) :: base_font_size
        integer :: total_width

        integer :: i, element_width
        real(wp) :: element_font_size

        total_width = 0

        do i = 1, size(elements)
            element_font_size = base_font_size * elements(i)%font_size_ratio
            if (elements(i)%element_type == 3) then
                element_width = calculate_mathtext_width_internal(&
                    parse_mathtext(elements(i)%text), element_font_size)
                total_width = total_width + int(0.6_wp * element_font_size) + &
                    element_width
            else
                element_width = calculate_text_width_with_size_internal(&
                    elements(i)%text, element_font_size)
                total_width = total_width + element_width
            end if
        end do
    end function calculate_mathtext_width_internal

    subroutine preprocess_math_text(input_text, result_text, result_len)
        !! Remove '$' delimiters and escape '^'/'_' outside math so they render literally
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: result_text
        integer, intent(out) :: result_len
        integer :: i, n, pos
        logical :: in_math
        character(len=1) :: ch

        result_text = ''
        result_len = 0
        pos = 1
        n = len_trim(input_text)
        in_math = .false.

        i = 1
        do while (i <= n)
            ch = input_text(i:i)
            if (ch == '$') then
                in_math = .not. in_math
                i = i + 1
                cycle
            end if

            if (.not. in_math .and. (ch == '_' .or. ch == '^')) then
                ! Escape to prevent math parsing
                result_text(pos:pos) = '\'
                pos = pos + 1
                result_text(pos:pos) = ch
                pos = pos + 1
                i = i + 1
                cycle
            end if

            result_text(pos:pos) = ch
            pos = pos + 1
            i = i + 1
        end do

        result_len = pos - 1
    end subroutine preprocess_math_text

    function calculate_text_width_with_size_internal(text, pixel_height) &
            result(width)
        !! Internal text width calculation to avoid circular dependencies
        !! Note: Uses len(text) not len_trim to preserve trailing spaces in mathtext
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: pixel_height
        integer :: width
        integer :: i, char_code, advance_width, left_side_bearing
        integer :: char_len, text_len
        type(stb_fontinfo_t) :: font
        real(wp) :: scale
        integer :: ix0, iy0, ix1, iy1
        integer :: pen_px, rightmost

        text_len = len(text)

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                width = int(text_len * pixel_height * 0.6_wp)
                return
            end if
        end if

        font = get_global_font()
        scale = get_font_scale_for_size(pixel_height)

        width = 0
        rightmost = 0
        pen_px = 0
        i = 1
        do while (i <= text_len)
            char_len = utf8_char_length(text(i:i))
            if (char_len == 0) then
                char_code = iachar(text(i:i))
                i = i + 1
            else
                char_code = utf8_to_codepoint(text, i)
                i = i + char_len
            end if

            call stb_get_codepoint_bitmap_box(font, char_code, scale, scale, &
                ix0, iy0, ix1, iy1)
            rightmost = max(rightmost, pen_px + ix1)
            call stb_get_codepoint_hmetrics(font, char_code, advance_width, &
                left_side_bearing)
            pen_px = pen_px + int(real(advance_width) * scale)
        end do
        width = max(pen_px, rightmost)
    end function calculate_text_width_with_size_internal

    function calculate_text_height_with_size_internal(pixel_height) &
            result(height)
        !! Internal text height calculation based on font metrics
        real(wp), intent(in) :: pixel_height
        integer :: height
        real(wp) :: ascent, descent, line_gap, scale
        logical :: success

        call get_font_metrics(ascent, descent, line_gap, success)
        if (.not. success) then
            height = int(pixel_height)
            return
        end if

        scale = pixel_height / (ascent - descent)
        height = int((ascent - descent) * scale)
    end function calculate_text_height_with_size_internal

    function calculate_mathtext_height_internal(elements, base_font_size) &
            result(total_height)
        !! Calculate total height of mathematical text elements
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(in) :: base_font_size
        integer :: total_height

        integer :: i
        real(wp) :: ascent, descent, line_gap, element_font_size
        real(wp) :: max_above_baseline, max_below_baseline
        logical :: success

        call get_font_metrics(ascent, descent, line_gap, success)
        if (.not. success) then
            total_height = int(base_font_size)
            return
        end if

        max_above_baseline = ascent * base_font_size / (ascent - descent)
        max_below_baseline = abs(descent) * base_font_size / (ascent - descent)

        do i = 1, size(elements)
            element_font_size = base_font_size * elements(i)%font_size_ratio

            select case (elements(i)%element_type)
            case (1)
                max_above_baseline = max(max_above_baseline, &
                    elements(i)%vertical_offset * base_font_size + &
                    ascent * element_font_size / (ascent - descent))
            case (2)
                max_below_baseline = max(max_below_baseline, &
                    -elements(i)%vertical_offset * base_font_size + &
                    abs(descent) * element_font_size / (ascent - descent))
            case (3)
                max_above_baseline = max(max_above_baseline, &
                    ascent * element_font_size / (ascent - descent))
                max_below_baseline = max(max_below_baseline, &
                    abs(descent) * element_font_size / (ascent - descent))
            end select
        end do

        total_height = int(max_above_baseline + max_below_baseline)
    end function calculate_mathtext_height_internal

end module fortplot_text_layout
