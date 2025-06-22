module fortplot_text
    use iso_c_binding
    implicit none
    
    private
    public :: init_text_system, cleanup_text_system, render_text_to_image, calculate_text_width
    
    ! Glyph information structure (matches C wrapper)
    type, bind(C) :: glyph_info_t
        integer(c_int) :: width
        integer(c_int) :: height
        integer(c_int) :: left
        integer(c_int) :: top
        integer(c_int) :: advance_x
        type(c_ptr) :: buffer
        integer(c_int) :: buffer_size
    end type glyph_info_t

    ! FreeType wrapper C interfaces
    interface
        function ft_wrapper_init_system_font() bind(C, name="ft_wrapper_init_system_font")
            import :: c_int
            integer(c_int) :: ft_wrapper_init_system_font
        end function ft_wrapper_init_system_font
        
        subroutine ft_wrapper_cleanup() bind(C, name="ft_wrapper_cleanup")
        end subroutine ft_wrapper_cleanup
        
        function ft_wrapper_render_char(char_code, glyph_info) bind(C, name="ft_wrapper_render_char")
            import :: c_int, glyph_info_t
            integer(c_int), value :: char_code
            type(glyph_info_t), intent(out) :: glyph_info
            integer(c_int) :: ft_wrapper_render_char
        end function ft_wrapper_render_char
        
        subroutine ft_wrapper_free_glyph(glyph_info) bind(C, name="ft_wrapper_free_glyph")
            import :: glyph_info_t
            type(glyph_info_t), intent(inout) :: glyph_info
        end subroutine ft_wrapper_free_glyph
        
        function ft_wrapper_render_text(text, width, height, buffer) bind(C, name="ft_wrapper_render_text")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: text(*)
            integer(c_int), intent(out) :: width, height
            type(c_ptr), intent(out) :: buffer
            integer(c_int) :: ft_wrapper_render_text
        end function ft_wrapper_render_text
        
        subroutine ft_wrapper_free_text_buffer(buffer) bind(C, name="ft_wrapper_free_text_buffer")
            import :: c_ptr
            type(c_ptr), value :: buffer
        end subroutine ft_wrapper_free_text_buffer
        
        function ft_wrapper_get_kerning(left_char, right_char) bind(C, name="ft_wrapper_get_kerning")
            import :: c_int
            integer(c_int), value :: left_char, right_char
            integer(c_int) :: ft_wrapper_get_kerning
        end function ft_wrapper_get_kerning
        
        function ft_wrapper_is_initialized() bind(C, name="ft_wrapper_is_initialized")
            import :: c_int
            integer(c_int) :: ft_wrapper_is_initialized
        end function ft_wrapper_is_initialized
    end interface
    
    ! Module variables
    logical :: text_system_initialized = .false.
    
contains

    function init_text_system() result(success)
        logical :: success
        integer(c_int) :: error
        
        success = .false.
        
        if (text_system_initialized) then
            success = .true.
            return
        end if
        
        error = ft_wrapper_init_system_font()
        if (error /= 0) then
            print *, "Error: Could not initialize FreeType wrapper"
            return
        end if
        
        text_system_initialized = .true.
        success = .true.
    end function init_text_system

    subroutine cleanup_text_system()
        if (text_system_initialized) then
            call ft_wrapper_cleanup()
            text_system_initialized = .false.
        end if
    end subroutine cleanup_text_system

    function calculate_text_width(text) result(width)
        !! Calculate the pixel width of text for proper alignment
        character(len=*), intent(in) :: text
        integer :: width
        integer :: i, char_code, next_char_code, kerning_offset
        integer(c_int) :: error
        type(glyph_info_t) :: glyph_info
        
        if (.not. text_system_initialized) then
            if (.not. init_text_system()) then
                width = len(text) * 8  ! Fallback: 8 pixels per character
                return
            end if
        end if
        
        width = 0
        do i = 1, len_trim(text)
            char_code = iachar(text(i:i))
            error = ft_wrapper_render_char(char_code, glyph_info)
            if (error == 0) then
                ! Add glyph advance (spacing to next character)
                width = width + glyph_info%advance_x / 64  ! Advance is in 1/64 pixel units
                
                ! Add kerning if not the last character
                if (i < len_trim(text)) then
                    next_char_code = iachar(text(i+1:i+1))
                    kerning_offset = ft_wrapper_get_kerning(char_code, next_char_code)
                    width = width + kerning_offset / 64
                end if
                
                ! Clean up glyph buffer
                call ft_wrapper_free_glyph(glyph_info)
            else
                ! Fallback for unsupported characters
                width = width + 8
            end if
        end do
    end function calculate_text_width

    subroutine render_text_to_image(image_data, width, height, x, y, text, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        integer :: pen_x, pen_y, i, char_code, next_char_code, kerning_offset
        integer(c_int) :: error
        type(glyph_info_t) :: glyph_info
        
        if (.not. text_system_initialized) then
            if (.not. init_text_system()) then
                call render_simple_placeholder(image_data, width, height, x, y, r, g, b)
                return
            end if
        end if
        
        pen_x = x
        pen_y = y
        
        do i = 1, len_trim(text)
            char_code = iachar(text(i:i))
            
            error = ft_wrapper_render_char(char_code, glyph_info)
            if (error /= 0) then
                cycle
            end if
            call render_glyph_from_wrapper(image_data, width, height, pen_x, pen_y, glyph_info, r, g, b)
            
            if (i < len_trim(text)) then
                next_char_code = iachar(text(i+1:i+1))
                kerning_offset = ft_wrapper_get_kerning(char_code, next_char_code)
                pen_x = pen_x + glyph_info%advance_x + kerning_offset
            else
                pen_x = pen_x + glyph_info%advance_x
            end if
            
            call ft_wrapper_free_glyph(glyph_info)
        end do
    end subroutine render_text_to_image

    subroutine render_glyph_from_wrapper(image_data, width, height, pen_x, pen_y, glyph_info, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, pen_x, pen_y
        type(glyph_info_t), intent(in) :: glyph_info
        integer(1), intent(in) :: r, g, b
        integer(1), pointer :: bitmap_buffer(:)
        integer :: glyph_x, glyph_y, img_x, img_y, row, col, pixel_idx
        integer(1) :: alpha
        real :: alpha_f, bg_r, bg_g, bg_b
        
        if (glyph_info%width <= 0 .or. glyph_info%height <= 0) then
            return
        end if
        
        if (.not. c_associated(glyph_info%buffer)) then
            call render_simple_character_block(image_data, width, height, pen_x, pen_y, r, g, b)
            return
        end if
        
        call c_f_pointer(glyph_info%buffer, bitmap_buffer, [glyph_info%buffer_size])
        
        glyph_x = pen_x + glyph_info%left
        glyph_y = pen_y - glyph_info%top
        
        do row = 0, glyph_info%height - 1
            do col = 0, glyph_info%width - 1
                img_x = glyph_x + col
                img_y = glyph_y + row
                
                if (img_x >= 0 .and. img_x < width .and. img_y >= 0 .and. img_y < height) then
                    alpha = bitmap_buffer(row * glyph_info%width + col + 1)
                    pixel_idx = img_y * (1 + width * 3) + 1 + img_x * 3 + 1
                    
                    alpha_f = real(int(alpha, kind=selected_int_kind(2)) + merge(256, 0, alpha < 0)) / 255.0
                    bg_r = real(int(image_data(pixel_idx), &
                        kind=selected_int_kind(2)) + merge(256, 0, image_data(pixel_idx) < 0))
                    bg_g = real(int(image_data(pixel_idx + 1), &
                        kind=selected_int_kind(2)) + merge(256, 0, image_data(pixel_idx + 1) < 0))
                    bg_b = real(int(image_data(pixel_idx + 2), &
                        kind=selected_int_kind(2)) + merge(256, 0, image_data(pixel_idx + 2) < 0))
                    
                    image_data(pixel_idx) = int(bg_r * (1.0 - alpha_f), 1)
                    image_data(pixel_idx + 1) = int(bg_g * (1.0 - alpha_f), 1)
                    image_data(pixel_idx + 2) = int(bg_b * (1.0 - alpha_f), 1)
                end if
            end do
        end do
    end subroutine render_glyph_from_wrapper
    

    subroutine render_simple_character_block(image_data, width, height, x, y, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        integer(1), intent(in) :: r, g, b
        integer :: img_x, img_y, pixel_idx
        integer(1) :: black_r, black_g, black_b
        
        black_r = 0_1
        black_g = 0_1
        black_b = 0_1
        
        do img_y = y, min(y + 5, height - 1)
            do img_x = x, min(x + 3, width - 1)
                if (img_x >= 0 .and. img_y >= 0) then
                    pixel_idx = img_y * (1 + width * 3) + 1 + img_x * 3 + 1
                    if (pixel_idx > 0 .and. pixel_idx <= height * (1 + width * 3) - 2) then
                        image_data(pixel_idx) = black_r
                        image_data(pixel_idx + 1) = black_g
                        image_data(pixel_idx + 2) = black_b
                    end if
                end if
            end do
        end do
    end subroutine render_simple_character_block

    subroutine render_simple_placeholder(image_data, width, height, x, y, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        integer(1), intent(in) :: r, g, b
        integer :: pixel_idx, img_x, img_y, max_idx
        
        max_idx = height * (1 + width * 3)
        
        
        do img_y = y, min(y + 6, height - 1)
            do img_x = x, min(x + 4, width - 1)
                if (img_x >= 0 .and. img_y >= 0) then
                    pixel_idx = img_y * (1 + width * 3) + 1 + img_x * 3 + 1
                    if (pixel_idx > 0 .and. pixel_idx <= max_idx - 2) then
                        image_data(pixel_idx) = r
                        image_data(pixel_idx + 1) = g
                        image_data(pixel_idx + 2) = b
                    end if
                end if
            end do
        end do
    end subroutine render_simple_placeholder

    
    subroutine render_character_bitmap(image_data, width, height, x, y, char, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        character(len=1), intent(in) :: char
        integer(1), intent(in) :: r, g, b
        integer :: img_x, img_y, pixel_idx, char_code
        integer(1) :: black_r, black_g, black_b
        logical :: pixel_set
        
        black_r = 0_1
        black_g = 0_1
        black_b = 0_1
        
        char_code = iachar(char)
        
        do img_y = y, min(y + 7, height - 1)
            do img_x = x, min(x + 5, width - 1)
                if (img_x >= 0 .and. img_y >= 0) then
                    pixel_set = get_character_pixel(char_code, img_x - x, img_y - y)
                    if (pixel_set) then
                        pixel_idx = img_y * (1 + width * 3) + 1 + img_x * 3 + 1
                        if (pixel_idx > 0 .and. pixel_idx <= height * (1 + width * 3) - 2) then
                            image_data(pixel_idx) = black_r
                            image_data(pixel_idx + 1) = black_g
                            image_data(pixel_idx + 2) = black_b
                        end if
                    end if
                end if
            end do
        end do
    end subroutine render_character_bitmap
    
    function get_character_pixel(char_code, x, y) result(pixel_set)
        integer, intent(in) :: char_code, x, y
        logical :: pixel_set
        
        pixel_set = .false.
        
        select case (char_code)
        case (48) ! '0'
            pixel_set = (x == 0 .or. x == 3) .and. (y >= 1 .and. y <= 6) .or. &
                       (y == 0 .or. y == 7) .and. (x >= 1 .and. x <= 2)
        case (49) ! '1'
            pixel_set = x == 2 .and. (y >= 0 .and. y <= 7)
        case (50) ! '2'
            pixel_set = (y == 0 .or. y == 3 .or. y == 7) .and. (x >= 0 .and. x <= 3) .or. &
                       x == 3 .and. (y >= 1 .and. y <= 2) .or. &
                       x == 0 .and. (y >= 4 .and. y <= 6)
        case (51) ! '3'
            pixel_set = (y == 0 .or. y == 3 .or. y == 7) .and. (x >= 0 .and. x <= 3) .or. &
                       x == 3 .and. ((y >= 1 .and. y <= 2) .or. (y >= 4 .and. y <= 6))
        case (53) ! '5'
            pixel_set = (y == 0 .or. y == 3 .or. y == 7) .and. (x >= 0 .and. x <= 3) .or. &
                       x == 0 .and. (y >= 1 .and. y <= 2) .or. &
                       x == 3 .and. (y >= 4 .and. y <= 6)
        case (55) ! '7'
            pixel_set = y == 0 .and. (x >= 0 .and. x <= 3) .or. &
                       x == 3 .and. (y >= 1 .and. y <= 7)
        case (45) ! '-'
            pixel_set = y == 3 .and. (x >= 0 .and. x <= 3)
        case (46) ! '.'
            pixel_set = y == 7 .and. x == 1
        case default
            pixel_set = (x >= 1 .and. x <= 2) .and. (y >= 2 .and. y <= 5)
        end select
    end function get_character_pixel

end module fortplot_text
