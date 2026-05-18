module fortplot_ascii_text_elements
     !! ASCII terminal plotting backend - Text Element Management
     !!
     !! This module handles text element creation, storage, and coordinate
     !! conversion for ASCII canvas rendering.
     !!
     !! Author: fortplot contributors

     use fortplot_ascii_mathtext, only: sanitize_ascii_text
     use fortplot_ascii_primitives, only: ascii_draw_text_primitive
     use fortplot_ascii_utils, only: text_element_t
     use, intrinsic :: iso_fortran_env, only: wp => real64
     implicit none

     private
     public :: add_text_element, store_text_element

contains

    subroutine add_text_element(text_elements, num_text_elements, x, y, text, &
                                current_r, current_g, current_b, &
                                x_min, x_max, y_min, y_max, plot_width, plot_height)
        !! Create and store a text element on the ASCII canvas
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(inout) :: num_text_elements
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: current_r, current_g, current_b
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height

        integer :: text_x, text_y
        character(len=500) :: processed_text
        integer :: processed_len

        ! Produce ASCII-safe text: LaTeX -> Unicode -> strip math delimiters,
        ! simplify mathtext, and transliterate remaining symbols.
        call sanitize_ascii_text(text, processed_text, processed_len)

        ! Store text element for later rendering
        if (num_text_elements < size(text_elements)) then
            num_text_elements = num_text_elements + 1

            ! Convert coordinates - check if already in screen coordinates
            if (x >= 1.0_wp .and. x <= real(plot_width, wp) .and. &
                y >= 1.0_wp .and. y <= real(plot_height, wp)) then
                ! Already in screen coordinates (e.g., from legend)
                text_x = nint(x)
                text_y = nint(y)
            else
                ! Convert from data coordinates to canvas coordinates
                text_x = nint((x - x_min)/(x_max - x_min)*real(plot_width, wp))
                text_y = nint((y_max - y)/(y_max - y_min)*real(plot_height, wp))
            end if

            ! Clamp to canvas bounds. Reserve margin on the right so text
            ! never touches the border ``|`` glyph (issue #1706).
            text_x = max(2, min(text_x, max(2, plot_width - processed_len - 1)))
            text_y = max(1, min(text_y, plot_height))

            text_elements(num_text_elements)%text = processed_text(1:processed_len)
            text_elements(num_text_elements)%x = text_x
            text_elements(num_text_elements)%y = text_y
            text_elements(num_text_elements)%color_r = current_r
            text_elements(num_text_elements)%color_g = current_g
            text_elements(num_text_elements)%color_b = current_b
        end if
    end subroutine add_text_element

    subroutine store_text_element(text_elements, num_text_elements, text_x, text_y, &
                                   processed_text, x, y, text, x_min, x_max, y_min, y_max, &
                                   plot_width, plot_height, current_r, current_g, current_b)
        !! Store a pre-processed text element for later ASCII rendering
        type(text_element_t), intent(inout) :: text_elements(:)
        integer, intent(inout) :: num_text_elements
        integer, intent(out) :: text_x, text_y
        character(len=:), allocatable, intent(out) :: processed_text
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: plot_width, plot_height
        real(wp), intent(in) :: current_r, current_g, current_b

        call ascii_draw_text_primitive(text_x, text_y, processed_text, &
                                       x, y, text, x_min, x_max, y_min, y_max, &
                                       plot_width, plot_height, current_r, current_g, current_b)

        num_text_elements = num_text_elements + 1
        text_elements(num_text_elements)%text = processed_text
        text_elements(num_text_elements)%x = text_x
        text_elements(num_text_elements)%y = text_y
        text_elements(num_text_elements)%color_r = current_r
        text_elements(num_text_elements)%color_g = current_g
        text_elements(num_text_elements)%color_b = current_b
    end subroutine store_text_element

end module fortplot_ascii_text_elements
