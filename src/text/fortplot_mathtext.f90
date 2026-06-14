module fortplot_mathtext
    !! Mathematical text rendering with superscripts and subscripts
    !! Supports matplotlib-like syntax: x^2, y_i, x_{text}, y^{superscript}
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    implicit none

    private
    public :: mathtext_element_t, parse_mathtext
    public :: ELEMENT_NORMAL, ELEMENT_SUPERSCRIPT, ELEMENT_SUBSCRIPT, ELEMENT_SQRT

    ! Mathematical text element types
    integer, parameter :: ELEMENT_NORMAL = 0
    integer, parameter :: ELEMENT_SUPERSCRIPT = 1
    integer, parameter :: ELEMENT_SUBSCRIPT = 2
    integer, parameter :: ELEMENT_SQRT = 3

    ! Font scaling factors (matching matplotlib's approach)
    real(wp), parameter :: SHRINK_FACTOR = 0.7_wp  ! Super/subscript size ratio
    real(wp), parameter :: SUPERSCRIPT_RAISE = 0.6_wp  ! Fraction of font height to raise
    real(wp), parameter :: SUBSCRIPT_LOWER = 0.2_wp   ! Fraction of font height to lower

    type :: mathtext_element_t
        character(len=:), allocatable :: text
        integer :: element_type = ELEMENT_NORMAL
        real(wp) :: font_size_ratio = 1.0_wp
        real(wp) :: vertical_offset = 0.0_wp  ! In pixels, positive = up
        logical :: italic = .false.
            !! True for runs that originate inside a '$...$' math segment.
            !! matplotlib renders math variables in italic; backends apply a
            !! synthetic oblique shear to alphabetic glyphs of italic runs.
    end type mathtext_element_t

contains

    function parse_mathtext(input_text) result(elements)
        !! Parse mathematical text into renderable elements
        character(len=*), intent(in) :: input_text
        type(mathtext_element_t), allocatable :: elements(:)

        integer :: i, n, current_len
        character(len=len(input_text)) :: current_text
        type(mathtext_element_t) :: temp_elements(len(input_text))
        integer :: element_count
        logical :: in_math

        element_count = 0
        n = len_trim(input_text)
        i = 1
        current_text = ''
        current_len = 0
        in_math = .false.

        do while (i <= n)
            if (input_text(i:i) == '$') then
                ! Unescaped '$' toggles math mode; runs inside render italic.
                call flush_current_text(current_text, current_len, temp_elements, &
                                        element_count, in_math)
                in_math = .not. in_math
                i = i + 1
            else if (input_text(i:i) == '^') then
                call flush_script_base(current_text, current_len, temp_elements, &
                                       element_count, in_math)
                i = i + 1
                call parse_superscript_subscript(input_text, i, n, temp_elements, &
                                               element_count, ELEMENT_SUPERSCRIPT, &
                                               in_math)
            else if (input_text(i:i) == '_') then
                call flush_script_base(current_text, current_len, temp_elements, &
                                       element_count, in_math)
                i = i + 1
                call parse_superscript_subscript(input_text, i, n, temp_elements, &
                                               element_count, ELEMENT_SUBSCRIPT, &
                                               in_math)
            else if (input_text(i:i) == '\') then
                call handle_mathtext_escape(input_text, i, n, current_text, &
                                            current_len, temp_elements, &
                                            element_count, in_math)
            else
                call append_current_text(current_text, current_len, input_text(i:i))
                i = i + 1
            end if
        end do

        call flush_current_text(current_text, current_len, temp_elements, &
                                element_count, in_math)

        allocate(elements(element_count))
        elements(1:element_count) = temp_elements(1:element_count)

    end function parse_mathtext

    subroutine flush_script_base(current_text, current_len, elements, &
                                 element_count, in_math)
        character(len=*), intent(inout) :: current_text
        integer, intent(inout) :: current_len
        type(mathtext_element_t), intent(inout) :: elements(:)
        integer, intent(inout) :: element_count
        logical, intent(in) :: in_math

        integer :: start_idx, byte

        if (current_len <= 0) return

        start_idx = current_len
        do while (start_idx > 1)
            byte = iachar(current_text(start_idx:start_idx))
            if (iand(byte, 192) /= 128) exit
            start_idx = start_idx - 1
        end do

        if (start_idx > 1) then
            element_count = element_count + 1
            call create_element(elements(element_count), &
                                current_text(1:start_idx - 1), &
                                ELEMENT_NORMAL, 1.0_wp, 0.0_wp, in_math)
        end if

        element_count = element_count + 1
        call create_element(elements(element_count), &
                            current_text(start_idx:current_len), &
                            ELEMENT_NORMAL, 1.0_wp, 0.0_wp, in_math)
        current_text = ''
        current_len = 0
    end subroutine flush_script_base

    subroutine handle_mathtext_escape(input_text, i, n, current_text, &
                                      current_len, elements, element_count, in_math)
        character(len=*), intent(in) :: input_text
        integer, intent(inout) :: i
        integer, intent(in) :: n
        character(len=*), intent(inout) :: current_text
        integer, intent(inout) :: current_len
        type(mathtext_element_t), intent(inout) :: elements(:)
        integer, intent(inout) :: element_count
        logical, intent(in) :: in_math

        if (i + 4 <= n) then
            if (input_text(i + 1:i + 4) == 'sqrt') then
                call flush_current_text(current_text, current_len, elements, &
                                        element_count, in_math)
                i = i + 5
                call parse_sqrt_content(input_text, i, n, elements, &
                                        element_count, in_math)
                return
            end if
        end if

        if (i + 5 <= n) then
            if (input_text(i + 1:i + 5) == 'times') then
                ! Emit U+00D7 (multiplication sign) so log mantissa labels like
                ! 4.2 x 10^1 render with matplotlib's cross glyph.
                call append_current_text(current_text, current_len, &
                                         achar(195)//achar(151))
                i = i + 6
                return
            end if
        end if

        if (i + 1 <= n) then
            select case (input_text(i + 1:i + 1))
            case ('_', '^', '$', '\')
                call append_current_text(current_text, current_len, &
                                         input_text(i + 1:i + 1))
                i = i + 2
            case default
                call append_current_text(current_text, current_len, input_text(i:i))
                i = i + 1
            end select
        else
            call append_current_text(current_text, current_len, input_text(i:i))
            i = i + 1
        end if
    end subroutine handle_mathtext_escape

    subroutine append_current_text(current_text, current_len, text)
        character(len=*), intent(inout) :: current_text
        integer, intent(inout) :: current_len
        character(len=*), intent(in) :: text

        current_len = current_len + len(text)
        current_text(current_len - len(text) + 1:current_len) = text
    end subroutine append_current_text

    subroutine flush_current_text(current_text, current_len, elements, &
                                  element_count, in_math)
        character(len=*), intent(inout) :: current_text
        integer, intent(inout) :: current_len
        type(mathtext_element_t), intent(inout) :: elements(:)
        integer, intent(inout) :: element_count
        logical, intent(in) :: in_math

        if (current_len <= 0) return

        element_count = element_count + 1
        call create_element(elements(element_count), current_text(1:current_len), &
                            ELEMENT_NORMAL, 1.0_wp, 0.0_wp, in_math)
        current_text = ''
        current_len = 0
    end subroutine flush_current_text

    subroutine parse_superscript_subscript(input_text, start_i, n, elements, &
                                           element_count, element_type, in_math)
        !! Parse superscript or subscript content
        character(len=*), intent(in) :: input_text
        integer, intent(inout) :: start_i
        integer, intent(in) :: n
        type(mathtext_element_t), intent(inout) :: elements(:)
        integer, intent(inout) :: element_count
        integer, intent(in) :: element_type
        logical, intent(in) :: in_math

        character(len=n) :: script_text
        integer :: i, brace_count, script_len
        logical :: in_braces
        real(wp) :: font_size_ratio, vertical_offset

        script_text = ''
        script_len = 0  ! Track actual length of script_text content
        i = start_i

        if (i > n) return

        ! Check if we have braces for multi-character script
        if (input_text(i:i) == '{') then
            in_braces = .true.
            brace_count = 1
            i = i + 1  ! Skip opening brace

            do while (i <= n .and. brace_count > 0)
                if (input_text(i:i) == '{') then
                    brace_count = brace_count + 1
                    script_len = script_len + 1
                    script_text(script_len:script_len) = input_text(i:i)
                else if (input_text(i:i) == '}') then
                    brace_count = brace_count - 1
                    if (brace_count > 0) then
                        script_len = script_len + 1
                        script_text(script_len:script_len) = input_text(i:i)
                    end if
                else
                    script_len = script_len + 1
                    script_text(script_len:script_len) = input_text(i:i)
                end if
                i = i + 1
            end do
        else
            ! Single character script
            script_len = 1
            script_text(1:1) = input_text(i:i)
            i = i + 1
        end if

        ! Set font size and vertical offset
        font_size_ratio = SHRINK_FACTOR
        if (element_type == ELEMENT_SUPERSCRIPT) then
            vertical_offset = SUPERSCRIPT_RAISE  ! Positive to move up in PDF coordinates
        else if (element_type == ELEMENT_SUBSCRIPT) then
            vertical_offset = -SUBSCRIPT_LOWER  ! Negative to move down in PDF coordinates
        else
            vertical_offset = 0.0_wp
        end if

        ! Create element
        if (script_len > 0) then
            element_count = element_count + 1
            call create_element(elements(element_count), script_text(1:script_len), &
                              element_type, font_size_ratio, vertical_offset, in_math)
        end if

        start_i = i
    end subroutine parse_superscript_subscript

    subroutine parse_sqrt_content(input_text, start_i, n, elements, &
                                  element_count, in_math)
        !! Parse square root content
        character(len=*), intent(in) :: input_text
        integer, intent(inout) :: start_i
        integer, intent(in) :: n
        type(mathtext_element_t), intent(inout) :: elements(:)
        integer, intent(inout) :: element_count
        logical, intent(in) :: in_math

        character(len=n) :: rad_text
        integer :: i, brace_count, rad_len

        rad_text = ''
        rad_len = 0
        i = start_i

        if (i > n) return

        if (input_text(i:i) == '{') then
            brace_count = 1
            i = i + 1
            do while (i <= n .and. brace_count > 0)
                if (input_text(i:i) == '{') then
                    brace_count = brace_count + 1
                    rad_len = rad_len + 1
                    rad_text(rad_len:rad_len) = input_text(i:i)
                else if (input_text(i:i) == '}') then
                    brace_count = brace_count - 1
                    if (brace_count > 0) then
                        rad_len = rad_len + 1
                        rad_text(rad_len:rad_len) = input_text(i:i)
                    end if
                else
                    rad_len = rad_len + 1
                    rad_text(rad_len:rad_len) = input_text(i:i)
                end if
                i = i + 1
            end do
        else
            rad_len = 1
            rad_text(1:1) = input_text(i:i)
            i = i + 1
        end if

        if (rad_len > 0) then
            element_count = element_count + 1
            call create_element(elements(element_count), rad_text(1:rad_len), &
                              ELEMENT_SQRT, 1.0_wp, 0.0_wp, in_math)
        end if

        start_i = i
    end subroutine parse_sqrt_content

    subroutine create_element(element, text, element_type, font_size_ratio, &
                              vertical_offset, italic)
        !! Create a mathtext element with proper string handling
        type(mathtext_element_t), intent(out) :: element
        character(len=*), intent(in) :: text
        integer, intent(in) :: element_type
        real(wp), intent(in) :: font_size_ratio, vertical_offset
        logical, intent(in) :: italic

        ! Store text properly - preserve all spaces
        element%text = text
        element%element_type = element_type
        element%font_size_ratio = font_size_ratio
        element%vertical_offset = vertical_offset
        element%italic = italic
   end subroutine create_element


end module fortplot_mathtext
