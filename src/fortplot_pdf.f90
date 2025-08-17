module fortplot_pdf
    use fortplot_context
    use fortplot_vector, only: vector_stream_writer, vector_graphics_state
    use fortplot_latex_parser
    use fortplot_unicode
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area, get_axis_tick_positions
    use fortplot_ticks, only: generate_scale_aware_tick_labels, find_nice_tick_locations, format_tick_value_smart
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position, &
                                         calculate_x_axis_label_position, calculate_y_axis_label_position, &
                                         calculate_x_tick_label_position_pdf, calculate_y_tick_label_position_pdf
    use fortplot_text, only: calculate_text_width
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: pdf_context, create_pdf_canvas, draw_pdf_axes_and_labels, draw_mixed_font_text
    
    type, extends(vector_stream_writer) :: pdf_stream_writer
    contains
        procedure :: write_command => pdf_write_command
        procedure :: write_move => pdf_write_move
        procedure :: write_line => pdf_write_line
        procedure :: write_stroke => pdf_write_stroke
        procedure :: write_color => pdf_write_color
        procedure :: write_line_width => pdf_write_line_width
        procedure :: save_state => pdf_save_state
        procedure :: restore_state => pdf_restore_state
    end type pdf_stream_writer

    type, extends(plot_context) :: pdf_context
        type(pdf_stream_writer) :: stream_writer
        ! Plot area calculations (using common margin functionality)  
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
    contains
        procedure :: line => draw_pdf_line
        procedure :: color => set_pdf_color
        procedure :: text => draw_pdf_text
        procedure :: save => write_pdf_file
        procedure :: set_line_width => set_pdf_line_width
        procedure :: save_graphics_state => pdf_save_graphics_state
        procedure :: restore_graphics_state => pdf_restore_graphics_state
        procedure :: draw_marker => draw_pdf_marker
        procedure :: set_marker_colors => pdf_set_marker_colors
        procedure :: set_marker_colors_with_alpha => pdf_set_marker_colors_with_alpha
    end type pdf_context
    
contains

    function create_pdf_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(pdf_context) :: ctx
        
        call setup_canvas(ctx, width, height)
        call initialize_pdf_stream(ctx)
        
        ! Set up matplotlib-style margins using common module
        ctx%margins = plot_margins_t()  ! Use defaults
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_pdf_canvas

    subroutine initialize_pdf_stream(ctx)
        type(pdf_context), intent(inout) :: ctx
        
        call ctx%stream_writer%initialize_stream()
        
        call ctx%stream_writer%add_to_stream("q")
        call ctx%stream_writer%add_to_stream("1 w")  ! Set default line width to 1 point (for axes)
        call ctx%stream_writer%add_to_stream("1 J")
        call ctx%stream_writer%add_to_stream("1 j")
        call ctx%stream_writer%add_to_stream("0 0 1 RG")
    end subroutine initialize_pdf_stream
    
    subroutine draw_pdf_line(this, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: pdf_x1, pdf_y1, pdf_x2, pdf_y2
        
        call normalize_to_pdf_coords(this, x1, y1, pdf_x1, pdf_y1)
        call normalize_to_pdf_coords(this, x2, y2, pdf_x2, pdf_y2)
        call this%stream_writer%draw_vector_line(pdf_x1, pdf_y1, pdf_x2, pdf_y2)
    end subroutine draw_pdf_line
    
    subroutine set_pdf_color(this, r, g, b)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        
        call this%stream_writer%set_vector_color(r, g, b)
    end subroutine set_pdf_color
    
    subroutine set_pdf_line_width(this, width)
        !! Set line width for PDF drawing
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: width
        
        call this%stream_writer%set_vector_line_width(width)
    end subroutine set_pdf_line_width
    
    subroutine draw_pdf_text(this, x, y, text)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: pdf_x, pdf_y
        character(len=500) :: processed_text
        integer :: processed_len
        
        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text, processed_text, processed_len)
        
        call normalize_to_pdf_coords(this, x, y, pdf_x, pdf_y)
        
        ! Render text with mixed font support
        call draw_mixed_font_text(this, pdf_x, pdf_y, processed_text(1:processed_len))
    end subroutine draw_pdf_text

    subroutine draw_mixed_font_text(this, x, y, text)
        !! Draw text with automatic font switching for Greek letters
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        
        integer :: i, char_len, codepoint, symbol_char
        character(len=1) :: current_char
        character(len=200) :: text_cmd
        character(len=500) :: current_segment
        character(len=1000) :: escaped_segment
        logical :: in_symbol_font, need_font_switch
        integer :: segment_pos, escaped_len
        
        call this%stream_writer%add_to_stream("BT")
        write(text_cmd, '("/F1 12 Tf")') 
        call this%stream_writer%add_to_stream(text_cmd)
        write(text_cmd, '(F8.2, 1X, F8.2, 1X, "Td")') x, y
        call this%stream_writer%add_to_stream(text_cmd)
        
        i = 1
        in_symbol_font = .false.
        current_segment = ""
        segment_pos = 1
        
        do while (i <= len_trim(text))
            current_char = text(i:i)
            need_font_switch = .false.
            
            ! Check if this is a Unicode character (high bit set)
            if (iachar(current_char) > 127) then
                ! Get the Unicode codepoint
                char_len = utf8_char_length(text(i:i))
                if (char_len > 0 .and. i + char_len - 1 <= len_trim(text)) then
                    codepoint = utf8_to_codepoint(text, i)
                    call unicode_to_symbol_char(codepoint, symbol_char)
                    
                    if (symbol_char > 0) then
                        ! This is a Greek letter
                        if (.not. in_symbol_font) then
                            ! Need to switch to Symbol font
                            need_font_switch = .true.
                            ! Output current segment before switching
                            if (segment_pos > 1) then
                                ! Escape parentheses for Helvetica font
                                call escape_pdf_string(current_segment(1:segment_pos-1), escaped_segment, escaped_len)
                                write(text_cmd, '("(", A, ") Tj")') escaped_segment(1:escaped_len)
                                call this%stream_writer%add_to_stream(text_cmd)
                                current_segment = ""
                                segment_pos = 1
                            end if
                            ! Switch to Symbol
                            call this%stream_writer%add_to_stream("/F2 12 Tf")
                            in_symbol_font = .true.
                        end if
                        ! Add Greek character
                        current_segment(segment_pos:segment_pos) = char(symbol_char)
                        segment_pos = segment_pos + 1
                        i = i + char_len
                    else
                        ! Non-Greek Unicode
                        if (in_symbol_font) then
                            ! Need to switch back to Helvetica
                            need_font_switch = .true.
                            ! Output current segment before switching
                            if (segment_pos > 1) then
                                ! Symbol font doesn't need escaping
                                write(text_cmd, '("(", A, ") Tj")') current_segment(1:segment_pos-1)
                                call this%stream_writer%add_to_stream(text_cmd)
                                current_segment = ""
                                segment_pos = 1
                            end if
                            ! Switch to Helvetica
                            call this%stream_writer%add_to_stream("/F1 12 Tf")
                            in_symbol_font = .false.
                        end if
                        ! Add ASCII representation
                        call unicode_codepoint_to_pdf_escape(codepoint, current_segment(segment_pos:))
                        segment_pos = segment_pos + len_trim(current_segment(segment_pos:))
                        i = i + char_len
                    end if
                else
                    ! Invalid UTF-8, skip
                    i = i + 1
                end if
            else
                ! Regular ASCII character
                ! All ASCII characters must be in Helvetica font
                if (in_symbol_font) then
                    ! Need to switch to Helvetica for ASCII characters
                    need_font_switch = .true.
                    ! Output current segment before switching
                    if (segment_pos > 1) then
                        ! Symbol font doesn't need escaping
                        write(text_cmd, '("(", A, ") Tj")') current_segment(1:segment_pos-1)
                        call this%stream_writer%add_to_stream(text_cmd)
                        current_segment = ""
                        segment_pos = 1
                    end if
                    ! Switch to Helvetica
                    call this%stream_writer%add_to_stream("/F1 12 Tf")
                    in_symbol_font = .false.
                end if
                
                ! Add ASCII character
                current_segment(segment_pos:segment_pos) = current_char
                segment_pos = segment_pos + 1
                i = i + 1
            end if
        end do
        
        ! Output final segment
        if (segment_pos > 1) then
            if (in_symbol_font) then
                ! Symbol font doesn't need escaping
                write(text_cmd, '("(", A, ") Tj")') current_segment(1:segment_pos-1)
            else
                ! Helvetica font needs escaping
                call escape_pdf_string(current_segment(1:segment_pos-1), escaped_segment, escaped_len)
                write(text_cmd, '("(", A, ") Tj")') escaped_segment(1:escaped_len)
            end if
            call this%stream_writer%add_to_stream(text_cmd)
        end if
        
        call this%stream_writer%add_to_stream("ET")
    end subroutine draw_mixed_font_text

    subroutine draw_rotated_mixed_font_text(this, x, y, text)
        !! Draw rotated text with automatic font switching for Greek letters
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        
        integer :: i, j, char_len, codepoint, symbol_char, next_codepoint
        character(len=1) :: current_char
        character(len=200) :: text_cmd
        character(len=500) :: current_segment
        logical :: in_symbol_font, next_is_greek
        integer :: segment_pos
        
        call this%stream_writer%add_to_stream("BT")
        
        ! Start with Helvetica font
        write(text_cmd, '("/F1 12 Tf")') 
        call this%stream_writer%add_to_stream(text_cmd)
        
        ! Rotation matrix for -90 degrees (counter-clockwise): [cos(-90) -sin(-90) sin(-90) cos(-90) x y] = [0 1 -1 0 x y]
        write(text_cmd, '("0 1 -1 0 ", F8.2, " ", F8.2, " Tm")') x, y
        call this%stream_writer%add_to_stream(text_cmd)
        
        i = 1
        in_symbol_font = .false.
        current_segment = ""
        segment_pos = 1
        
        do while (i <= len_trim(text))
            current_char = text(i:i)
            
            ! Check if this is a Unicode character (high bit set)
            if (iachar(current_char) > 127) then
                ! Get the Unicode codepoint
                char_len = utf8_char_length(text(i:i))
                if (char_len > 0 .and. i + char_len - 1 <= len_trim(text)) then
                    codepoint = utf8_to_codepoint(text, i)
                    call unicode_to_symbol_char(codepoint, symbol_char)
                    
                    if (symbol_char > 0) then
                        ! Greek letter - switch to Symbol font if needed
                        if (.not. in_symbol_font) then
                            ! Output current Helvetica segment
                            if (segment_pos > 1) then
                                write(text_cmd, '("(", A, ") Tj")') current_segment(1:segment_pos-1)
                                call this%stream_writer%add_to_stream(text_cmd)
                            end if
                            ! Switch to Symbol font
                            call this%stream_writer%add_to_stream("/F2 12 Tf")
                            in_symbol_font = .true.
                            current_segment = ""
                            segment_pos = 1
                        end if
                        current_segment(segment_pos:segment_pos) = char(symbol_char)
                        segment_pos = segment_pos + 1
                    else
                        ! Non-Greek Unicode - use ASCII fallback in Helvetica
                        if (in_symbol_font) then
                            ! Output current Symbol segment
                            if (segment_pos > 1) then
                                write(text_cmd, '("(", A, ") Tj")') current_segment(1:segment_pos-1)
                                call this%stream_writer%add_to_stream(text_cmd)
                            end if
                            ! Switch back to Helvetica
                            call this%stream_writer%add_to_stream("/F1 12 Tf")
                            in_symbol_font = .false.
                            current_segment = ""
                            segment_pos = 1
                        end if
                        ! Add ASCII fallback
                        call unicode_codepoint_to_pdf_escape(codepoint, current_segment(segment_pos:))
                        segment_pos = segment_pos + len_trim(current_segment(segment_pos:))
                    end if
                    
                    i = i + char_len
                else
                    ! Invalid Unicode sequence, skip
                    i = i + 1
                end if
            else
                ! Regular ASCII character
                ! Check if next non-space character is Greek
                next_is_greek = .false.
                if (current_char == '(' .or. current_char == ' ') then
                    j = i + 1
                    ! Look for the next meaningful character
                    do while (j <= len_trim(text))
                        if (text(j:j) == ' ') then
                            j = j + 1
                        else
                            exit
                        end if
                    end do
                    if (j <= len_trim(text) .and. iachar(text(j:j)) > 127) then
                        char_len = utf8_char_length(text(j:j))
                        if (char_len > 0 .and. j + char_len - 1 <= len_trim(text)) then
                            next_codepoint = utf8_to_codepoint(text, j)
                            call unicode_to_symbol_char(next_codepoint, symbol_char)
                            if (symbol_char > 0) next_is_greek = .true.
                        end if
                    end if
                end if
                
                ! Handle font switching
                if (in_symbol_font) then
                    ! Always switch back to Helvetica for parentheses and most ASCII
                    if (current_char == '(' .or. current_char == ')' .or. &
                        (.not. next_is_greek .and. current_char /= ' ')) then
                        ! Output current Symbol segment before switching back
                        if (segment_pos > 1) then
                            write(text_cmd, '("(", A, ") Tj")') current_segment(1:segment_pos-1)
                            call this%stream_writer%add_to_stream(text_cmd)
                        end if
                        ! Switch back to Helvetica
                        call this%stream_writer%add_to_stream("/F1 12 Tf")
                        in_symbol_font = .false.
                        current_segment = ""
                        segment_pos = 1
                    end if
                else
                    ! In Helvetica - check if we need to switch to Symbol
                    if (next_is_greek .and. (current_char == '(' .or. current_char == ' ')) then
                        ! Output current segment including this character
                        current_segment(segment_pos:segment_pos) = current_char
                        segment_pos = segment_pos + 1
                        if (segment_pos > 1) then
                            write(text_cmd, '("(", A, ") Tj")') current_segment(1:segment_pos-1)
                            call this%stream_writer%add_to_stream(text_cmd)
                        end if
                        current_segment = ""
                        segment_pos = 1
                        i = i + 1
                        cycle  ! Skip the normal character addition
                    end if
                end if
                
                ! Add character to current segment
                current_segment(segment_pos:segment_pos) = current_char
                segment_pos = segment_pos + 1
                i = i + 1
            end if
        end do
        
        ! Output final segment
        if (segment_pos > 1) then
            write(text_cmd, '("(", A, ") Tj")') current_segment(1:segment_pos-1)
            call this%stream_writer%add_to_stream(text_cmd)
        end if
        
        call this%stream_writer%add_to_stream("ET")
    end subroutine draw_rotated_mixed_font_text
    
    subroutine draw_pdf_text_direct(this, x, y, text)
        !! Draw text at direct PDF coordinates (no data coordinate transformation)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=500) :: processed_text
        integer :: processed_len
        
        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text, processed_text, processed_len)
        
        ! Render text with mixed font support
        call draw_mixed_font_text(this, x, y, processed_text(1:processed_len))
    end subroutine draw_pdf_text_direct

    subroutine draw_pdf_text_bold(this, x, y, text)
        !! Draw bold text using isolated graphics state (no global state pollution)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        
        ! Use PDF's q/Q operators to isolate state changes
        call this%stream_writer%save_state()
        call draw_bold_text_isolated(this, x, y, text)
        call this%stream_writer%restore_state()
    end subroutine draw_pdf_text_bold
    
    subroutine write_pdf_file(this, filename)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer :: unit
        
        call finalize_pdf_stream(this)
        call create_pdf_document(unit, filename, this)
        print *, "PDF file '", trim(filename), "' created successfully!"
    end subroutine write_pdf_file

    subroutine normalize_to_pdf_coords(ctx, x, y, pdf_x, pdf_y)
        class(pdf_context), intent(in) :: ctx
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pdf_x, pdf_y
        
        ! Transform coordinates to plot area (like matplotlib)
        ! Note: PDF coordinates have Y=0 at bottom (same as plot coordinates)
        pdf_x = (x - ctx%x_min) / (ctx%x_max - ctx%x_min) * real(ctx%plot_area%width, wp) + real(ctx%plot_area%left, wp)
        pdf_y = (y - ctx%y_min) / (ctx%y_max - ctx%y_min) * real(ctx%plot_area%height, wp) + &
                real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
    end subroutine normalize_to_pdf_coords

    subroutine draw_vector_line(ctx, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x1, y1, x2, y2
        character(len=50) :: move_cmd, line_cmd
        
        write(move_cmd, '(F8.2, 1X, F8.2, 1X, "m")') x1, y1
        write(line_cmd, '(F8.2, 1X, F8.2, 1X, "l")') x2, y2
        
        call ctx%stream_writer%add_to_stream(move_cmd)
        call ctx%stream_writer%add_to_stream(line_cmd)
        call ctx%stream_writer%add_to_stream("S")
    end subroutine draw_vector_line

    subroutine finalize_pdf_stream(ctx)
        type(pdf_context), intent(inout) :: ctx
        call ctx%stream_writer%add_to_stream("Q")
    end subroutine finalize_pdf_stream

    
    subroutine create_pdf_document(unit, filename, ctx)
        integer, intent(out) :: unit
        character(len=*), intent(in) :: filename
        type(pdf_context), intent(in) :: ctx
        
        open(newunit=unit, file=filename, access='stream', form='unformatted', status='replace')
        call write_pdf_structure(unit, ctx)
        close(unit)
    end subroutine create_pdf_document

    subroutine write_pdf_structure(unit, ctx)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer :: obj_positions(6), xref_pos
        
        call write_string_to_unit(unit, "%PDF-1.4")
        call write_all_objects(unit, ctx, obj_positions)
        call write_xref_and_trailer(unit, obj_positions, xref_pos)
    end subroutine write_pdf_structure
    
    subroutine write_all_objects(unit, ctx, positions)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: positions(6)
        
        call write_catalog_object(unit, positions(1))
        call write_pages_object(unit, ctx, positions(2))
        call write_page_object(unit, ctx, positions(3))
        call write_content_object(unit, ctx, positions(4))
        call write_helvetica_font_object(unit, positions(5))
        call write_symbol_font_object(unit, positions(6))
    end subroutine write_all_objects

    subroutine write_xref_and_trailer(unit, positions, xref_pos)
        integer, intent(in) :: unit, positions(6)
        integer, intent(out) :: xref_pos
        character(len=200) :: xref_entry
        character(len=100) :: trailer_str
        
        xref_pos = get_position(unit)
        call write_string_to_unit(unit, "xref")
        call write_string_to_unit(unit, "0 7")
        call write_string_to_unit(unit, "0000000000 65535 f")
        
        write(xref_entry, '(I10.10, " 00000 n")') positions(1)
        call write_string_to_unit(unit, xref_entry)
        write(xref_entry, '(I10.10, " 00000 n")') positions(2)
        call write_string_to_unit(unit, xref_entry)
        write(xref_entry, '(I10.10, " 00000 n")') positions(3)
        call write_string_to_unit(unit, xref_entry)
        write(xref_entry, '(I10.10, " 00000 n")') positions(4)
        call write_string_to_unit(unit, xref_entry)
        write(xref_entry, '(I10.10, " 00000 n")') positions(5)
        call write_string_to_unit(unit, xref_entry)
        write(xref_entry, '(I10.10, " 00000 n")') positions(6)
        call write_string_to_unit(unit, xref_entry)
        
        call write_string_to_unit(unit, "trailer")
        call write_string_to_unit(unit, "<</Size 7/Root 1 0 R>>")
        call write_string_to_unit(unit, "startxref")
        write(trailer_str, '(I0)') xref_pos
        call write_string_to_unit(unit, trailer_str)
        call write_string_to_unit(unit, "%%EOF")
    end subroutine write_xref_and_trailer

    subroutine write_catalog_object(unit, pos)
        integer, intent(in) :: unit
        integer, intent(out) :: pos
        pos = get_position(unit)
        call write_string_to_unit(unit, "1 0 obj")
        call write_string_to_unit(unit, "<</Type /Catalog/Pages 2 0 R>>")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_catalog_object

    subroutine write_pages_object(unit, ctx, pos)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: pos
        
        pos = get_position(unit)
        call write_string_to_unit(unit, "2 0 obj")
        call write_string_to_unit(unit, "<</Type /Pages/Kids [3 0 R]/Count 1>>")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_pages_object

    subroutine write_page_object(unit, ctx, pos)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: pos
        character(len=200) :: page_str, size_str
        
        pos = get_position(unit)
        write(size_str, '(I0, 1X, I0)') ctx%width, ctx%height
        call write_string_to_unit(unit, "3 0 obj")
        page_str = "<</Type /Page/Parent 2 0 R/MediaBox [0 0 " // trim(size_str) // "]" // &
                   "/Contents 4 0 R/Resources <</Font <</F1 5 0 R/F2 6 0 R>>>>>>>>>"
        call write_string_to_unit(unit, page_str)
        call write_string_to_unit(unit, "endobj")
    end subroutine write_page_object

    subroutine write_content_object(unit, ctx, pos)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: pos
        character(len=50) :: len_str
        
        pos = get_position(unit)
        write(len_str, '("/Length ", I0)') len(ctx%stream_writer%content_stream)
        call write_string_to_unit(unit, "4 0 obj")
        call write_string_to_unit(unit, "<<" // trim(len_str) // ">>")
        call write_string_to_unit(unit, "stream")
        call write_string_to_unit(unit, ctx%stream_writer%content_stream)
        call write_string_to_unit(unit, "endstream")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_content_object

    subroutine write_helvetica_font_object(unit, pos)
        integer, intent(in) :: unit
        integer, intent(out) :: pos
        
        pos = get_position(unit)
        call write_string_to_unit(unit, "5 0 obj")
        call write_string_to_unit(unit, "<</Type /Font/Subtype /Type1/BaseFont /Helvetica>>")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_helvetica_font_object

    subroutine write_symbol_font_object(unit, pos)
        integer, intent(in) :: unit
        integer, intent(out) :: pos
        
        pos = get_position(unit)
        call write_string_to_unit(unit, "6 0 obj")
        call write_string_to_unit(unit, "<</Type /Font/Subtype /Type1/BaseFont /Symbol>>")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_symbol_font_object

    integer function get_position(unit)
        integer, intent(in) :: unit
        inquire(unit=unit, pos=get_position)
        get_position = get_position - 1
    end function get_position

    subroutine write_string_to_unit(unit, str)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: str
        integer :: i
        integer(1), allocatable :: bytes(:)
        
        allocate(bytes(len(str) + 1))
        do i = 1, len(str)
            bytes(i) = int(iachar(str(i:i)), 1)
        end do
        bytes(len(str) + 1) = 10_1
        write(unit) bytes
        deallocate(bytes)
    end subroutine write_string_to_unit


    subroutine unicode_codepoint_to_pdf_escape(codepoint, escape_seq)
        !! Convert Unicode codepoint to PDF escape sequence
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: escape_seq
        
        ! For now, convert Greek letters to ASCII equivalents
        ! TODO: Use proper PDF Unicode escape sequences
        select case (codepoint)
        case (945) ! α
            escape_seq = "alpha"
        case (946) ! β
            escape_seq = "beta"
        case (947) ! γ
            escape_seq = "gamma"
        case (948) ! δ
            escape_seq = "delta"
        case (949) ! ε
            escape_seq = "epsilon"
        case (950) ! ζ
            escape_seq = "zeta"
        case (951) ! η
            escape_seq = "eta"
        case (952) ! θ
            escape_seq = "theta"
        case (953) ! ι
            escape_seq = "iota"
        case (954) ! κ
            escape_seq = "kappa"
        case (955) ! λ
            escape_seq = "lambda"
        case (956) ! μ
            escape_seq = "mu"
        case (957) ! ν
            escape_seq = "nu"
        case (958) ! ξ
            escape_seq = "xi"
        case (959) ! ο
            escape_seq = "omicron"
        case (960) ! π
            escape_seq = "pi"
        case (961) ! ρ
            escape_seq = "rho"
        case (963) ! σ
            escape_seq = "sigma"
        case (964) ! τ
            escape_seq = "tau"
        case (965) ! υ
            escape_seq = "upsilon"
        case (966) ! φ
            escape_seq = "phi"
        case (967) ! χ
            escape_seq = "chi"
        case (968) ! ψ
            escape_seq = "psi"
        case (969) ! ω
            escape_seq = "omega"
        case (913) ! Α
            escape_seq = "Alpha"
        case (914) ! Β
            escape_seq = "Beta"
        case (915) ! Γ
            escape_seq = "Gamma"
        case (916) ! Δ
            escape_seq = "Delta"
        case (917) ! Ε
            escape_seq = "Epsilon"
        case (918) ! Ζ
            escape_seq = "Zeta"
        case (919) ! Η
            escape_seq = "Eta"
        case (920) ! Θ
            escape_seq = "Theta"
        case (921) ! Ι
            escape_seq = "Iota"
        case (922) ! Κ
            escape_seq = "Kappa"
        case (923) ! Λ
            escape_seq = "Lambda"
        case (924) ! Μ
            escape_seq = "Mu"
        case (925) ! Ν
            escape_seq = "Nu"
        case (926) ! Ξ
            escape_seq = "Xi"
        case (927) ! Ο
            escape_seq = "Omicron"
        case (928) ! Π
            escape_seq = "Pi"
        case (929) ! Ρ
            escape_seq = "Rho"
        case (931) ! Σ
            escape_seq = "Sigma"
        case (932) ! Τ
            escape_seq = "Tau"
        case (933) ! Υ
            escape_seq = "Upsilon"
        case (934) ! Φ
            escape_seq = "Phi"
        case (935) ! Χ
            escape_seq = "Chi"
        case (936) ! Ψ
            escape_seq = "Psi"
        case (937) ! Ω
            escape_seq = "Omega"
        case (178) ! ²
            escape_seq = "2"
        case default
            ! For other Unicode characters, use a placeholder
            write(escape_seq, '("U+", Z4.4)') codepoint
        end select
    end subroutine unicode_codepoint_to_pdf_escape

    subroutine unicode_to_symbol_char(unicode_codepoint, symbol_char)
        !! Map Unicode Greek letters to Symbol font character positions
        integer, intent(in) :: unicode_codepoint
        integer, intent(out) :: symbol_char
        
        ! Map Unicode Greek letters to Symbol font character positions
        select case (unicode_codepoint)
        ! Lowercase Greek letters
        case (945); symbol_char = 97   ! α -> 'a'
        case (946); symbol_char = 98   ! β -> 'b'
        case (947); symbol_char = 103  ! γ -> 'g'
        case (948); symbol_char = 100  ! δ -> 'd'
        case (949); symbol_char = 101  ! ε -> 'e'
        case (950); symbol_char = 122  ! ζ -> 'z'
        case (951); symbol_char = 104  ! η -> 'h'
        case (952); symbol_char = 113  ! θ -> 'q'
        case (953); symbol_char = 105  ! ι -> 'i'
        case (954); symbol_char = 107  ! κ -> 'k'
        case (955); symbol_char = 108  ! λ -> 'l'
        case (956); symbol_char = 109  ! μ -> 'm'
        case (957); symbol_char = 110  ! ν -> 'n'
        case (958); symbol_char = 120  ! ξ -> 'x'
        case (959); symbol_char = 111  ! ο -> 'o'
        case (960); symbol_char = 112  ! π -> 'p'
        case (961); symbol_char = 114  ! ρ -> 'r'
        case (963); symbol_char = 115  ! σ -> 's'
        case (964); symbol_char = 116  ! τ -> 't'
        case (965); symbol_char = 117  ! υ -> 'u'
        case (966); symbol_char = 102  ! φ -> 'f'
        case (967); symbol_char = 99   ! χ -> 'c'
        case (968); symbol_char = 121  ! ψ -> 'y'
        case (969); symbol_char = 119  ! ω -> 'w'
        
        ! Uppercase Greek letters
        case (913); symbol_char = 65   ! Α -> 'A'
        case (914); symbol_char = 66   ! Β -> 'B'
        case (915); symbol_char = 71   ! Γ -> 'G'
        case (916); symbol_char = 68   ! Δ -> 'D'
        case (917); symbol_char = 69   ! Ε -> 'E'
        case (918); symbol_char = 90   ! Ζ -> 'Z'
        case (919); symbol_char = 72   ! Η -> 'H'
        case (920); symbol_char = 81   ! Θ -> 'Q'
        case (921); symbol_char = 73   ! Ι -> 'I'
        case (922); symbol_char = 75   ! Κ -> 'K'
        case (923); symbol_char = 76   ! Λ -> 'L'
        case (924); symbol_char = 77   ! Μ -> 'M'
        case (925); symbol_char = 78   ! Ν -> 'N'
        case (926); symbol_char = 88   ! Ξ -> 'X'
        case (927); symbol_char = 79   ! Ο -> 'O'
        case (928); symbol_char = 80   ! Π -> 'P'
        case (929); symbol_char = 82   ! Ρ -> 'R'
        case (931); symbol_char = 83   ! Σ -> 'S'
        case (932); symbol_char = 84   ! Τ -> 'T'
        case (933); symbol_char = 85   ! Υ -> 'U'
        case (934); symbol_char = 70   ! Φ -> 'F'
        case (935); symbol_char = 67   ! Χ -> 'C'
        case (936); symbol_char = 89   ! Ψ -> 'Y'
        case (937); symbol_char = 87   ! Ω -> 'W'
        
        case default
            symbol_char = 0  ! Not a Greek letter
        end select
    end subroutine unicode_to_symbol_char
    
    subroutine escape_pdf_string(input, output, output_len)
        !! Escape special characters for PDF string literals
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: output_len
        integer :: i, j
        
        j = 1
        do i = 1, len(input)
            ! Escape parentheses and backslashes in PDF strings
            if (input(i:i) == '(' .or. input(i:i) == ')' .or. input(i:i) == '\') then
                if (j <= len(output)) then
                    output(j:j) = '\'
                    j = j + 1
                end if
            end if
            if (j <= len(output)) then
                output(j:j) = input(i:i)
                j = j + 1
            end if
        end do
        output_len = j - 1
    end subroutine escape_pdf_string

    subroutine draw_pdf_axes_and_labels(ctx, xscale, yscale, symlog_threshold, &
                                      x_min_orig, x_max_orig, y_min_orig, y_max_orig, &
                                      title, xlabel, ylabel, &
                                      grid_enabled, grid_axis, grid_which, &
                                      grid_alpha, grid_linestyle, grid_color)
        !! Draw plot axes and frame for PDF backend with scale-aware tick generation
        !! Now matches PNG backend behavior with nice tick boundaries
        type(pdf_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: xscale, yscale
        real(wp), intent(in), optional :: symlog_threshold
        real(wp), intent(in), optional :: x_min_orig, x_max_orig, y_min_orig, y_max_orig
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        logical, intent(in), optional :: grid_enabled
        character(len=*), intent(in), optional :: grid_axis, grid_which, grid_linestyle
        real(wp), intent(in), optional :: grid_alpha
        real(wp), intent(in), optional :: grid_color(3)
        
        real(wp) :: x_tick_values(20), y_tick_values(20)
        real(wp) :: x_positions(20), y_positions(20)
        character(len=20) :: x_labels(20), y_labels(20)
        real(wp) :: nice_x_min, nice_x_max, nice_x_step
        real(wp) :: nice_y_min, nice_y_max, nice_y_step
        integer :: num_x_ticks, num_y_ticks, i
        real(wp) :: data_x_min, data_x_max, data_y_min, data_y_max
        
        ! Set color to black for axes
        call ctx%color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! Use provided data ranges or backend ranges
        if (present(x_min_orig) .and. present(x_max_orig)) then
            data_x_min = x_min_orig
            data_x_max = x_max_orig
        else
            data_x_min = ctx%x_min
            data_x_max = ctx%x_max
        end if
        
        if (present(y_min_orig) .and. present(y_max_orig)) then
            data_y_min = y_min_orig
            data_y_max = y_max_orig
        else
            data_y_min = ctx%y_min
            data_y_max = ctx%y_max
        end if
        
        ! Draw plot frame
        call draw_pdf_frame(ctx)
        
        ! Generate nice tick values based on scale type (matching PNG backend)
        if (present(xscale) .and. trim(xscale) /= 'linear') then
            ! For non-linear scales, use the old approach for now
            call get_axis_tick_positions(ctx%plot_area, 5, 5, x_positions, y_positions, num_x_ticks, num_y_ticks)
        else
            ! For linear scale, use nice tick locations and adjust boundaries to match
            call find_nice_tick_locations(data_x_min, data_x_max, 5, &
                                        nice_x_min, nice_x_max, nice_x_step, &
                                        x_tick_values, num_x_ticks)
            
            call find_nice_tick_locations(data_y_min, data_y_max, 5, &
                                        nice_y_min, nice_y_max, nice_y_step, &
                                        y_tick_values, num_y_ticks)
            
            ! Update the context boundaries to match the nice tick boundaries
            if (num_x_ticks > 0) then
                ctx%x_min = x_tick_values(1)
                ctx%x_max = x_tick_values(num_x_ticks)
            end if
            
            if (num_y_ticks > 0) then
                ctx%y_min = y_tick_values(1)
                ctx%y_max = y_tick_values(num_y_ticks)
            end if
            
            ! Convert tick values to PDF coordinates
            do i = 1, num_x_ticks
                x_positions(i) = ctx%plot_area%left + &
                                (x_tick_values(i) - ctx%x_min) / (ctx%x_max - ctx%x_min) * ctx%plot_area%width
            end do
            
            ! For Y axis in PDF (origin at bottom, no flipping needed)
            do i = 1, num_y_ticks
                y_positions(i) = ctx%plot_area%bottom + &
                                (y_tick_values(i) - ctx%y_min) / (ctx%y_max - ctx%y_min) * ctx%plot_area%height
            end do
        end if
        
        ! Generate tick labels based on actual tick values
        if (present(xscale) .and. trim(xscale) /= 'linear') then
            call generate_scale_aware_tick_labels(data_x_min, data_x_max, num_x_ticks, x_labels, xscale, symlog_threshold)
        else
            do i = 1, num_x_ticks
                x_labels(i) = format_tick_value_smart(x_tick_values(i), 8)
            end do
        end if
        
        if (present(yscale) .and. trim(yscale) /= 'linear') then
            call generate_scale_aware_tick_labels(data_y_min, data_y_max, num_y_ticks, y_labels, yscale, symlog_threshold)
        else
            do i = 1, num_y_ticks
                y_labels(i) = format_tick_value_smart(y_tick_values(i), 8)
            end do
        end if
        
        ! Draw tick marks and labels
        call draw_pdf_tick_marks(ctx, x_positions, y_positions, num_x_ticks, num_y_ticks)
        call draw_pdf_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x_ticks, num_y_ticks)
        
        ! Draw title and axis labels
        call draw_pdf_title_and_labels(ctx, title, xlabel, ylabel)
        
        ! Draw grid lines if enabled
        ! Draw grid lines if enabled
        ! FIXED: Check if grid_enabled is present AND true to avoid accessing uninitialized memory
        if (present(grid_enabled)) then
            if (grid_enabled) then
                call draw_pdf_grid_lines(ctx, x_positions, y_positions, num_x_ticks, num_y_ticks, &
                                       grid_axis, grid_which, grid_alpha, grid_linestyle, grid_color)
            end if
        end if
    end subroutine draw_pdf_axes_and_labels

    subroutine draw_pdf_grid_lines(ctx, x_positions, y_positions, num_x_ticks, num_y_ticks, &
                                 grid_axis, grid_which, grid_alpha, grid_linestyle, grid_color)
        !! Draw grid lines at tick positions for PDF backend
        type(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x_ticks, num_y_ticks
        character(len=*), intent(in), optional :: grid_axis, grid_which, grid_linestyle
        real(wp), intent(in), optional :: grid_alpha
        real(wp), intent(in), optional :: grid_color(3)
        
        character(len=10) :: axis_choice, which_choice
        real(wp) :: alpha_value, line_color(3)
        integer :: i
        real(wp) :: grid_y_top, grid_y_bottom, grid_x_left, grid_x_right
        character(len=100) :: draw_cmd
        
        ! Set default values
        axis_choice = 'both'
        which_choice = 'major'
        alpha_value = 0.3_wp
        line_color = [0.5_wp, 0.5_wp, 0.5_wp]
        
        if (present(grid_axis)) axis_choice = grid_axis
        if (present(grid_which)) which_choice = grid_which
        if (present(grid_alpha)) alpha_value = grid_alpha
        if (present(grid_color)) line_color = grid_color
        
        ! Calculate plot area boundaries (PDF coordinates: Y=0 at bottom)
        grid_y_bottom = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
        grid_y_top = real(ctx%height - ctx%plot_area%bottom, wp)
        grid_x_left = real(ctx%plot_area%left, wp)
        grid_x_right = real(ctx%plot_area%left + ctx%plot_area%width, wp)
        
        ! Set grid line color with transparency
        write(draw_cmd, '(F4.2, 1X, F4.2, 1X, F4.2, 1X, "RG")') line_color(1), line_color(2), line_color(3)
        call ctx%stream_writer%add_to_stream(draw_cmd)
        
        ! Draw vertical grid lines (at x tick positions)
        if (axis_choice == 'both' .or. axis_choice == 'x') then
            do i = 1, num_x_ticks
                ! Convert from raster to PDF coordinates
                write(draw_cmd, '(F8.2, 1X, F8.2, 1X, "m")') &
                    x_positions(i), grid_y_bottom
                call ctx%stream_writer%add_to_stream(draw_cmd)
                write(draw_cmd, '(F8.2, 1X, F8.2, 1X, "l")') &
                    x_positions(i), grid_y_top
                call ctx%stream_writer%add_to_stream(draw_cmd)
                call ctx%stream_writer%add_to_stream("S")
            end do
        end if
        
        ! Draw horizontal grid lines (at y tick positions)
        if (axis_choice == 'both' .or. axis_choice == 'y') then
            do i = 1, num_y_ticks
                ! Convert Y position to PDF coordinates
                write(draw_cmd, '(F8.2, 1X, F8.2, 1X, "m")') &
                    grid_x_left, real(ctx%height, wp) - y_positions(i)
                call ctx%stream_writer%add_to_stream(draw_cmd)
                write(draw_cmd, '(F8.2, 1X, F8.2, 1X, "l")') &
                    grid_x_right, real(ctx%height, wp) - y_positions(i)
                call ctx%stream_writer%add_to_stream(draw_cmd)
                call ctx%stream_writer%add_to_stream("S")
            end do
        end if
    end subroutine draw_pdf_grid_lines
    
    subroutine draw_pdf_frame(ctx)
        !! Draw the plot frame for PDF backend
        type(pdf_context), intent(inout) :: ctx
        real(wp) :: left, right, top, bottom
        
        ! Calculate frame coordinates (PDF coordinates Y=0 at bottom)
        left = real(ctx%plot_area%left, wp)
        right = real(ctx%plot_area%left + ctx%plot_area%width, wp)
        bottom = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
        top = real(ctx%height - ctx%plot_area%bottom, wp)
        
        ! Draw frame (rectangle)
        call draw_vector_line(ctx, left, bottom, right, bottom)  ! Bottom edge
        call draw_vector_line(ctx, left, top, right, top)        ! Top edge
        call draw_vector_line(ctx, left, bottom, left, top)      ! Left edge  
        call draw_vector_line(ctx, right, bottom, right, top)    ! Right edge
    end subroutine draw_pdf_frame
    
    subroutine draw_pdf_tick_marks(ctx, x_positions, y_positions, num_x, num_y)
        !! Draw tick marks for PDF backend
        type(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        integer, intent(in) :: num_x, num_y
        integer :: i
        real(wp) :: bottom, top, left, right, pdf_y_pos
        
        ! Calculate axis positions
        bottom = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
        top = real(ctx%height - ctx%plot_area%bottom, wp)
        left = real(ctx%plot_area%left, wp)
        right = real(ctx%plot_area%left + ctx%plot_area%width, wp)
        
        ! Draw X-axis tick marks (at bottom of plot)
        do i = 1, num_x
            call draw_vector_line(ctx, x_positions(i), bottom, x_positions(i), bottom - 5.0_wp)
        end do
        
        ! Draw Y-axis tick marks (at left of plot)  
        do i = 1, num_y
            ! Convert Y position to PDF coordinates
            pdf_y_pos = real(ctx%height - ctx%plot_area%bottom, wp) - &
                       (y_positions(i) - real(ctx%plot_area%bottom, wp))
            call draw_vector_line(ctx, left, pdf_y_pos, left - 5.0_wp, pdf_y_pos)
        end do
    end subroutine draw_pdf_tick_marks
    
    subroutine draw_pdf_tick_labels(ctx, x_positions, y_positions, x_labels, y_labels, num_x, num_y)
        !! Draw tick labels for PDF backend like matplotlib
        type(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_positions(:), y_positions(:)
        character(len=*), intent(in) :: x_labels(:), y_labels(:)
        integer, intent(in) :: num_x, num_y
        integer :: i
        real(wp) :: label_x, label_y, tick_y, bottom, left
        character(len=200) :: text_cmd
        
        bottom = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height, wp)
        left = real(ctx%plot_area%left, wp)
        
        ! Draw X-axis tick labels with proper spacing and center alignment
        do i = 1, num_x
            ! Use PDF-specific tick label positioning (native PDF coordinates)
            call calculate_x_tick_label_position_pdf(x_positions(i), bottom, &
                                                   trim(x_labels(i)), label_x, label_y)
            
            call ctx%stream_writer%add_to_stream("BT")
            write(text_cmd, '("/F1 12 Tf")')
            call ctx%stream_writer%add_to_stream(text_cmd)
            write(text_cmd, '(F8.2, 1X, F8.2, 1X, "Td")') label_x, label_y
            call ctx%stream_writer%add_to_stream(text_cmd)
            write(text_cmd, '("(", A, ") Tj")') trim(x_labels(i))
            call ctx%stream_writer%add_to_stream(text_cmd)
            call ctx%stream_writer%add_to_stream("ET")
        end do
        
        ! Draw Y-axis tick labels with right alignment and proper spacing
        do i = 1, num_y
            ! Convert Y position to PDF coordinates for positioning calculation
            tick_y = real(ctx%height - ctx%plot_area%bottom, wp) - &
                     (y_positions(i) - real(ctx%plot_area%bottom, wp))
            
            ! Use PDF-specific tick label positioning (native PDF coordinates)
            call calculate_y_tick_label_position_pdf(tick_y, real(ctx%plot_area%left, wp), &
                                                   trim(y_labels(i)), label_x, label_y)
            
            call ctx%stream_writer%add_to_stream("BT")
            write(text_cmd, '("/F1 12 Tf")')
            call ctx%stream_writer%add_to_stream(text_cmd)
            write(text_cmd, '(F8.2, 1X, F8.2, 1X, "Td")') label_x, label_y
            call ctx%stream_writer%add_to_stream(text_cmd)
            write(text_cmd, '("(", A, ") Tj")') trim(y_labels(i))
            call ctx%stream_writer%add_to_stream(text_cmd)
            call ctx%stream_writer%add_to_stream("ET")
        end do
    end subroutine draw_pdf_tick_labels
    
    subroutine draw_pdf_title_and_labels(ctx, title, xlabel, ylabel)
        !! Draw figure title and axis labels for PDF
        type(pdf_context), intent(inout) :: ctx
        character(len=*), intent(in), optional :: title, xlabel, ylabel
        real(wp) :: label_x, label_y, text_width
        
        ! Draw title at top center with proper margin (matplotlib-style)
        if (present(title)) then
            ! Center horizontally across the entire figure width (like matplotlib)
            text_width = real(calculate_text_width(trim(title)), wp)
            if (text_width <= 0.0_wp) then
                text_width = real(len_trim(title) * 8, wp)  ! 8 pixels per char for 12pt font
            end if
            label_x = real(ctx%width, wp) / 2.0_wp - text_width / 2.0_wp
            ! Position title at top of PDF - high Y value for PDF coordinates (Y=0 at bottom)
            ! Just below the top edge, similar to matplotlib spacing
            label_y = real(ctx%height - 25, wp)  ! 25 pixels down from top edge
            call draw_pdf_text_direct(ctx, label_x, label_y, title)  ! Normal weight (non-bold)
        end if
        
        ! Draw X-axis label using proper axis label positioning
        if (present(xlabel)) then
            call calculate_x_axis_label_position(real(ctx%plot_area%left + ctx%plot_area%width / 2, wp), &
                                               real(ctx%plot_area%bottom + ctx%plot_area%height, wp), &
                                               xlabel, label_x, label_y)
            ! Convert to PDF coordinates
            label_y = real(ctx%plot_area%bottom - 40, wp)  ! 40px below plot (matplotlib exact)
            call draw_pdf_text_direct(ctx, label_x, label_y, xlabel)
        end if
        
        ! Draw Y-axis label rotated 90 degrees matplotlib-style
        if (present(ylabel)) then
            call draw_vertical_text_pdf(ctx, ylabel)
        end if
    end subroutine draw_pdf_title_and_labels
    
    subroutine draw_vertical_text_pdf(ctx, text)
        !! Draw text rotated 90 degrees using PDF text matrix
        type(pdf_context), intent(inout) :: ctx
        character(len=*), intent(in) :: text
        real(wp) :: label_x, label_y
        character(len=500) :: processed_text
        integer :: processed_len
        
        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text, processed_text, processed_len)
        
        ! Position for rotated Y-axis label using proper axis label positioning
        call calculate_y_axis_label_position(real(ctx%plot_area%bottom + ctx%plot_area%height / 2, wp), &
                                           real(ctx%plot_area%left, wp), trim(processed_text(1:processed_len)), label_x, label_y)
        ! Convert to PDF coordinates (Y is flipped)
        label_y = real(ctx%height - ctx%plot_area%bottom - ctx%plot_area%height / 2, wp)
        
        ! Draw rotated text with mixed font support
        call draw_rotated_mixed_font_text(ctx, label_x, label_y, processed_text(1:processed_len))
    end subroutine draw_vertical_text_pdf

    ! Graphics state management routines - encapsulate PDF's mutable global state
    
    subroutine pdf_save_graphics_state(this)
        !! Save current graphics state using PDF's q operator
        class(pdf_context), intent(inout) :: this
        call this%stream_writer%save_state()
    end subroutine pdf_save_graphics_state
    
    subroutine pdf_restore_graphics_state(this)
        !! Restore graphics state using PDF's Q operator  
        class(pdf_context), intent(inout) :: this
        call this%stream_writer%restore_state()
    end subroutine pdf_restore_graphics_state
    
    
    subroutine draw_bold_text_isolated(this, x, y, text)
        !! Draw bold text in isolated state (called via with_graphics_state)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=500) :: processed_text
        integer :: processed_len
        real(wp) :: text_width, centered_x
        
        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text, processed_text, processed_len)
        
        ! Calculate text width for centering (fallback to character estimation if text system fails)
        text_width = real(calculate_text_width(trim(processed_text(1:processed_len))), wp) * 1.17_wp  ! Scale for 14pt vs 12pt
        if (text_width <= 0.0_wp) then
            ! Fallback: estimate 8 pixels per character for 14pt font
            text_width = real(processed_len * 8, wp)
        end if
        centered_x = x - text_width / 2.0_wp
        
        ! Draw bold text with mixed font support (simplified bold rendering)
        call draw_mixed_font_text(this, centered_x, y, processed_text(1:processed_len))
    end subroutine draw_bold_text_isolated

    ! PDF-specific vector interface implementations
    
    subroutine pdf_write_command(this, command)
        class(pdf_stream_writer), intent(inout) :: this
        character(len=*), intent(in) :: command
        call this%add_to_stream(command)
    end subroutine pdf_write_command
    
    subroutine pdf_write_move(this, x, y)
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=50) :: move_cmd
        write(move_cmd, '(F8.2, 1X, F8.2, 1X, "m")') x, y
        call this%add_to_stream(move_cmd)
    end subroutine pdf_write_move
    
    subroutine pdf_write_line(this, x, y)
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=50) :: line_cmd
        write(line_cmd, '(F8.2, 1X, F8.2, 1X, "l")') x, y
        call this%add_to_stream(line_cmd)
    end subroutine pdf_write_line
    
    subroutine pdf_write_stroke(this)
        class(pdf_stream_writer), intent(inout) :: this
        call this%add_to_stream("S")
    end subroutine pdf_write_stroke
    
    subroutine pdf_write_color(this, r, g, b)
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        character(len=50) :: color_cmd
        write(color_cmd, '(F4.2, 1X, F4.2, 1X, F4.2, 1X, "RG")') r, g, b
        call this%add_to_stream(color_cmd)
    end subroutine pdf_write_color

    subroutine pdf_write_fill_color(this, r, g, b)
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        character(len=50) :: color_cmd
        write(color_cmd, '(F4.2, 1X, F4.2, 1X, F4.2, 1X, "rg")') r, g, b
        call this%add_to_stream(color_cmd)
    end subroutine pdf_write_fill_color
    
    subroutine pdf_write_line_width(this, width)
        class(pdf_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: width
        character(len=20) :: width_cmd
        write(width_cmd, '(F4.1, 1X, "w")') width
        call this%add_to_stream(width_cmd)
    end subroutine pdf_write_line_width
    
    subroutine pdf_save_state(this)
        class(pdf_stream_writer), intent(inout) :: this
        call this%add_to_stream("q")
    end subroutine pdf_save_state
    
    subroutine pdf_restore_state(this)
        class(pdf_stream_writer), intent(inout) :: this
        call this%add_to_stream("Q")
    end subroutine pdf_restore_state

    subroutine draw_pdf_marker(this, x, y, style)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp) :: pdf_x, pdf_y

        call normalize_to_pdf_coords(this, x, y, pdf_x, pdf_y)

        call draw_pdf_marker_by_style(this, pdf_x, pdf_y, style)
    end subroutine draw_pdf_marker

    subroutine draw_pdf_marker_by_style(this, pdf_x, pdf_y, style)
        !! Draw PDF marker using shared style dispatch logic (DRY compliance)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: pdf_x, pdf_y
        character(len=*), intent(in) :: style
        real(wp) :: marker_size
        
        marker_size = get_marker_size(style)
        
        select case (trim(style))
        case (MARKER_CIRCLE)
            call draw_pdf_circle_with_outline(this, pdf_x, pdf_y, marker_size)
        case (MARKER_SQUARE)
            call draw_pdf_square_with_outline(this, pdf_x, pdf_y, marker_size)
        case (MARKER_DIAMOND)
            call draw_pdf_diamond_with_outline(this, pdf_x, pdf_y, marker_size)
        case (MARKER_CROSS)
            call draw_pdf_x_marker(this, pdf_x, pdf_y, marker_size)
        end select
    end subroutine draw_pdf_marker_by_style

    subroutine pdf_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b
        
        ! Suppress unused parameter warnings
        associate(unused_int => this%width, &
                  unused_real => edge_r + edge_g + edge_b + face_r + face_g + face_b); end associate
        
        ! PDF backend doesn't support separate marker colors yet
        ! This is a stub implementation for interface compliance
    end subroutine pdf_set_marker_colors

    subroutine pdf_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, &
                                                face_r, face_g, face_b, face_alpha)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        
        ! Suppress unused parameter warnings
        associate(unused_int => this%width, &
                  unused_real => edge_r + edge_g + edge_b + edge_alpha + &
                                face_r + face_g + face_b + face_alpha); end associate
        
        ! PDF backend doesn't support separate marker colors or transparency yet
        ! This is a stub implementation for interface compliance
    end subroutine pdf_set_marker_colors_with_alpha

    subroutine draw_pdf_circle_with_outline(this, cx, cy, radius)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: cx, cy, radius
        character(len=200) :: circle_cmd

        ! Draw a circle using bezier curves with current fill color and black outline
        call this%stream_writer%add_to_stream("q")
        
        ! Move to starting point
        write(circle_cmd, '(F8.2, 1X, F8.2, 1X, "m")') cx + radius, cy
        call this%stream_writer%add_to_stream(circle_cmd)
        
        ! First quadrant
        write(circle_cmd, '(F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, "c")') &
             cx + radius, cy + 0.552284749831_wp * radius, &
             cx + 0.552284749831_wp * radius, cy + radius, cx, cy + radius
        call this%stream_writer%add_to_stream(circle_cmd)
        
        ! Second quadrant  
        write(circle_cmd, '(F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, "c")') &
             cx - 0.552284749831_wp * radius, cy + radius, &
             cx - radius, cy + 0.552284749831_wp * radius, cx - radius, cy
        call this%stream_writer%add_to_stream(circle_cmd)
        
        ! Third quadrant
        write(circle_cmd, '(F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, "c")') &
             cx - radius, cy - 0.552284749831_wp * radius, &
             cx - 0.552284749831_wp * radius, cy - radius, cx, cy - radius
        call this%stream_writer%add_to_stream(circle_cmd)
        
        ! Fourth quadrant
        write(circle_cmd, '(F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, "c")') &
             cx + 0.552284749831_wp * radius, cy - radius, &
             cx + radius, cy - 0.552284749831_wp * radius, cx + radius, cy
        call this%stream_writer%add_to_stream(circle_cmd)
        
        ! Set fill color to current stroke color, then fill and stroke with black outline
        call pdf_write_fill_color(this%stream_writer, this%stream_writer%current_state%stroke_r, &
                                  this%stream_writer%current_state%stroke_g, &
                                  this%stream_writer%current_state%stroke_b)
        call this%stream_writer%add_to_stream("0 0 0 RG")  ! Black outline
        call this%stream_writer%add_to_stream("B")  ! Fill and stroke
        call this%stream_writer%add_to_stream("Q")
    end subroutine draw_pdf_circle_with_outline

    subroutine draw_pdf_square_with_outline(this, cx, cy, size)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: cx, cy, size
        character(len=200) :: rect_cmd
        real(wp) :: half_size

        half_size = size * 0.5_wp
        call this%stream_writer%add_to_stream("q")
        
        ! Draw rectangle
        write(rect_cmd, '(F8.2, 1X, F8.2, 1X, F8.2, 1X, F8.2, 1X, "re")') &
             cx - half_size, cy - half_size, size, size
        call this%stream_writer%add_to_stream(rect_cmd)
        
        ! Set fill color to current stroke color, then fill and stroke with black outline
        call pdf_write_fill_color(this%stream_writer, this%stream_writer%current_state%stroke_r, &
                                  this%stream_writer%current_state%stroke_g, &
                                  this%stream_writer%current_state%stroke_b)
        call this%stream_writer%add_to_stream("0 0 0 RG")  ! Black outline
        call this%stream_writer%add_to_stream("B")  ! Fill and stroke
        call this%stream_writer%add_to_stream("Q")
    end subroutine draw_pdf_square_with_outline

    subroutine draw_pdf_diamond_with_outline(this, cx, cy, size)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: cx, cy, size
        character(len=200) :: diamond_cmd
        real(wp) :: half_size

        half_size = size * 0.5_wp
        call this%stream_writer%add_to_stream("q")
        
        ! Draw diamond as four lines
        write(diamond_cmd, '(F8.2, 1X, F8.2, 1X, "m")') cx, cy + half_size  ! Top
        call this%stream_writer%add_to_stream(diamond_cmd)
        write(diamond_cmd, '(F8.2, 1X, F8.2, 1X, "l")') cx + half_size, cy  ! Right
        call this%stream_writer%add_to_stream(diamond_cmd)
        write(diamond_cmd, '(F8.2, 1X, F8.2, 1X, "l")') cx, cy - half_size  ! Bottom
        call this%stream_writer%add_to_stream(diamond_cmd)
        write(diamond_cmd, '(F8.2, 1X, F8.2, 1X, "l")') cx - half_size, cy  ! Left
        call this%stream_writer%add_to_stream(diamond_cmd)
        call this%stream_writer%add_to_stream("h")  ! Close path
        
        ! Set fill color to current stroke color, then fill and stroke with black outline
        call pdf_write_fill_color(this%stream_writer, this%stream_writer%current_state%stroke_r, &
                                  this%stream_writer%current_state%stroke_g, &
                                  this%stream_writer%current_state%stroke_b)
        call this%stream_writer%add_to_stream("0 0 0 RG")  ! Black outline
        call this%stream_writer%add_to_stream("B")  ! Fill and stroke
        call this%stream_writer%add_to_stream("Q")
    end subroutine draw_pdf_diamond_with_outline

    subroutine draw_pdf_x_marker(this, cx, cy, size)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: cx, cy, size
        character(len=200) :: x_cmd
        real(wp) :: half_size

        half_size = size * 0.5_wp
        call this%stream_writer%add_to_stream("q")
        call this%stream_writer%add_to_stream("0 0 0 RG")  ! Black color for X
        
        ! Draw X as two diagonal lines
        write(x_cmd, '(F8.2, 1X, F8.2, 1X, "m")') cx - half_size, cy - half_size
        call this%stream_writer%add_to_stream(x_cmd)
        write(x_cmd, '(F8.2, 1X, F8.2, 1X, "l")') cx + half_size, cy + half_size
        call this%stream_writer%add_to_stream(x_cmd)
        call this%stream_writer%add_to_stream("S")
        
        write(x_cmd, '(F8.2, 1X, F8.2, 1X, "m")') cx - half_size, cy + half_size
        call this%stream_writer%add_to_stream(x_cmd)
        write(x_cmd, '(F8.2, 1X, F8.2, 1X, "l")') cx + half_size, cy - half_size
        call this%stream_writer%add_to_stream(x_cmd)
        call this%stream_writer%add_to_stream("S")
        call this%stream_writer%add_to_stream("Q")
    end subroutine draw_pdf_x_marker

end module fortplot_pdf
