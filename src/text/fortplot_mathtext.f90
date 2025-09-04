module fortplot_mathtext
    !! Mathematical text rendering with superscripts and subscripts
    !! Supports matplotlib-like syntax: x^2, y_i, x_{text}, y^{superscript}
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    implicit none
    
    private
    public :: mathtext_element_t, parse_mathtext, render_mathtext_elements
    public :: calculate_mathtext_width, calculate_mathtext_height
    
    ! Mathematical text element types
    integer, parameter :: ELEMENT_NORMAL = 0
    integer, parameter :: ELEMENT_SUPERSCRIPT = 1
    integer, parameter :: ELEMENT_SUBSCRIPT = 2
    
    ! Font scaling factors (matching matplotlib's approach)
    real(wp), parameter :: SHRINK_FACTOR = 0.7_wp  ! Super/subscript size ratio
    real(wp), parameter :: SUPERSCRIPT_RAISE = 0.6_wp  ! Fraction of font height to raise
    real(wp), parameter :: SUBSCRIPT_LOWER = 0.2_wp   ! Fraction of font height to lower
    
    type :: mathtext_element_t
        character(len=:), allocatable :: text
        integer :: element_type = ELEMENT_NORMAL
        real(wp) :: font_size_ratio = 1.0_wp
        real(wp) :: vertical_offset = 0.0_wp  ! In pixels, positive = up
    end type mathtext_element_t
    
contains

    function parse_mathtext(input_text) result(elements)
        !! Parse mathematical text into renderable elements
        character(len=*), intent(in) :: input_text
        type(mathtext_element_t), allocatable :: elements(:)
        
        integer :: i, n, brace_count, current_len
        character(len=len(input_text)) :: current_text
        logical :: in_superscript, in_subscript, in_braces
        type(mathtext_element_t) :: temp_elements(len(input_text))
        integer :: element_count
        
        element_count = 0
        n = len_trim(input_text)
        i = 1
        current_text = ''
        current_len = 0  ! Track actual length of current_text content
        in_superscript = .false.
        in_subscript = .false.
        in_braces = .false.
        brace_count = 0
        
        do while (i <= n)
            if (input_text(i:i) == '^') then
                ! Split last character from current text if it exists
                if (current_len > 1) then
                    ! Store everything except the last character
                    element_count = element_count + 1
                    call create_element(temp_elements(element_count), &
                                      current_text(1:current_len-1), &
                                      ELEMENT_NORMAL, 1.0_wp, 0.0_wp)
                    ! Store the last character separately
                    element_count = element_count + 1
                    call create_element(temp_elements(element_count), &
                                      current_text(current_len:current_len), &
                                      ELEMENT_NORMAL, 1.0_wp, 0.0_wp)
                else if (current_len == 1) then
                    ! Just one character, store it
                    element_count = element_count + 1
                    call create_element(temp_elements(element_count), &
                                      current_text(1:1), &
                                      ELEMENT_NORMAL, 1.0_wp, 0.0_wp)
                end if
                current_text = ''
                current_len = 0
                
                i = i + 1
                call parse_superscript_subscript(input_text, i, n, temp_elements, &
                                               element_count, ELEMENT_SUPERSCRIPT)
                
            else if (input_text(i:i) == '_') then
                ! Split last character from current text if it exists
                if (current_len > 1) then
                    ! Store everything except the last character
                    element_count = element_count + 1
                    call create_element(temp_elements(element_count), &
                                      current_text(1:current_len-1), &
                                      ELEMENT_NORMAL, 1.0_wp, 0.0_wp)
                    ! Store the last character separately
                    element_count = element_count + 1
                    call create_element(temp_elements(element_count), &
                                      current_text(current_len:current_len), &
                                      ELEMENT_NORMAL, 1.0_wp, 0.0_wp)
                else if (current_len == 1) then
                    ! Just one character, store it
                    element_count = element_count + 1
                    call create_element(temp_elements(element_count), &
                                      current_text(1:1), &
                                      ELEMENT_NORMAL, 1.0_wp, 0.0_wp)
                end if
                current_text = ''
                current_len = 0
                
                i = i + 1
                call parse_superscript_subscript(input_text, i, n, temp_elements, &
                                               element_count, ELEMENT_SUBSCRIPT)
                
            else
                current_len = current_len + 1
                current_text(current_len:current_len) = input_text(i:i)
                i = i + 1
            end if
        end do
        
        ! Store any remaining text (preserve spaces)
        if (current_len > 0) then
            element_count = element_count + 1
            call create_element(temp_elements(element_count), &
                              current_text(1:current_len), &
                              ELEMENT_NORMAL, 1.0_wp, 0.0_wp)
        end if
        
        ! Allocate and copy final elements
        allocate(elements(element_count))
        elements(1:element_count) = temp_elements(1:element_count)
        
    end function parse_mathtext

    subroutine parse_superscript_subscript(input_text, start_i, n, elements, &
                                          element_count, element_type)
        !! Parse superscript or subscript content
        character(len=*), intent(in) :: input_text
        integer, intent(inout) :: start_i
        integer, intent(in) :: n
        type(mathtext_element_t), intent(inout) :: elements(:)
        integer, intent(inout) :: element_count
        integer, intent(in) :: element_type
        
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
                              element_type, font_size_ratio, vertical_offset)
        end if
        
        start_i = i
    end subroutine parse_superscript_subscript

    subroutine create_element(element, text, element_type, font_size_ratio, vertical_offset)
        !! Create a mathtext element with proper string handling
        type(mathtext_element_t), intent(out) :: element
        character(len=*), intent(in) :: text
        integer, intent(in) :: element_type
        real(wp), intent(in) :: font_size_ratio, vertical_offset
        
        ! Store text properly - preserve all spaces
        element%text = text
        element%element_type = element_type
        element%font_size_ratio = font_size_ratio
        element%vertical_offset = vertical_offset
    end subroutine create_element

    function calculate_mathtext_width(elements, base_font_size) result(total_width)
        !! Calculate total width of mathematical text elements
        !! This function signature is used by text_rendering but implementation moved there
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(in) :: base_font_size
        integer :: total_width
        
        ! This is a placeholder - actual implementation is in text_rendering module
        ! to avoid circular dependencies
        total_width = 0
        
        ! Suppress unused parameter warnings
        associate(unused_elements => elements, unused_size => base_font_size)
        end associate
        
    end function calculate_mathtext_width

    function calculate_mathtext_height(elements, base_font_size) result(total_height)
        !! Calculate total height of mathematical text elements
        !! This function signature is used by text_rendering but implementation moved there
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(in) :: base_font_size
        integer :: total_height
        
        ! This is a placeholder - actual implementation is in text_rendering module
        ! to avoid circular dependencies
        total_height = int(base_font_size)
        
        ! Suppress unused parameter warnings
        associate(unused_elements => elements, unused_size => base_font_size)
        end associate
        
    end function calculate_mathtext_height

    subroutine render_mathtext_elements(image_data, width, height, x, y, elements, &
                                       r, g, b, base_font_size)
        !! Render mathematical text elements to image
        !! This function signature is used by text_rendering but implementation moved there
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        type(mathtext_element_t), intent(in) :: elements(:)
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: base_font_size
        
        ! This is a placeholder - actual implementation is in text_rendering module
        ! to avoid circular dependencies
        
        ! Suppress unused parameter warnings - avoid referencing assumed-size arrays
        if (.false.) then
            image_data(1) = image_data(1)  ! Reference array without bounds
        end if
        if (.false.) then
            associate(unused_w => width, unused_h => height, &
                     unused_x => x, unused_y => y, unused_elements => elements, &
                     unused_r => r, unused_g => g, unused_b => b, unused_size => base_font_size)
            end associate
        end if
        
    end subroutine render_mathtext_elements

end module fortplot_mathtext