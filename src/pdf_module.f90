module pdf_module
    use plot_context_module
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: pdf_context, create_pdf_canvas
    
    type, extends(plot_context) :: pdf_context
        character(len=:), allocatable :: content_stream
        real(wp) :: stroke_r, stroke_g, stroke_b
    contains
        procedure :: line => draw_pdf_line
        procedure :: color => set_pdf_color
        procedure :: text => draw_pdf_text
        procedure :: save => write_pdf_file
    end type pdf_context
    
contains

    function create_pdf_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(pdf_context) :: ctx
        
        call setup_canvas(ctx, width, height)
        call initialize_pdf_stream(ctx)
    end function create_pdf_canvas

    subroutine initialize_pdf_stream(ctx)
        type(pdf_context), intent(inout) :: ctx
        
        ctx%content_stream = ""
        ctx%stroke_r = 0.0_wp
        ctx%stroke_g = 0.0_wp
        ctx%stroke_b = 1.0_wp
        
        call add_to_stream(ctx, "q")
        call add_to_stream(ctx, "2 w")
        call add_to_stream(ctx, "0 0 1 RG")
    end subroutine initialize_pdf_stream
    
    subroutine draw_pdf_line(this, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: pdf_x1, pdf_y1, pdf_x2, pdf_y2
        
        call normalize_to_pdf_coords(this, x1, y1, pdf_x1, pdf_y1)
        call normalize_to_pdf_coords(this, x2, y2, pdf_x2, pdf_y2)
        call draw_vector_line(this, pdf_x1, pdf_y1, pdf_x2, pdf_y2)
    end subroutine draw_pdf_line
    
    subroutine set_pdf_color(this, r, g, b)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        character(len=50) :: color_cmd
        
        this%stroke_r = r
        this%stroke_g = g  
        this%stroke_b = b
        
        write(color_cmd, '(F4.2, 1X, F4.2, 1X, F4.2, 1X, "RG")') r, g, b
        call add_to_stream(this, color_cmd)
    end subroutine set_pdf_color
    
    subroutine draw_pdf_text(this, x, y, text)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: pdf_x, pdf_y
        character(len=200) :: text_cmd
        
        call normalize_to_pdf_coords(this, x, y, pdf_x, pdf_y)
        
        call add_to_stream(this, "BT")
        write(text_cmd, '("/F1 8 Tf")') 
        call add_to_stream(this, text_cmd)
        write(text_cmd, '(F8.2, 1X, F8.2, 1X, "Td")') pdf_x, pdf_y
        call add_to_stream(this, text_cmd)
        write(text_cmd, '("(", A, ") Tj")') trim(text)
        call add_to_stream(this, text_cmd)
        call add_to_stream(this, "ET")
    end subroutine draw_pdf_text
    
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
        
        pdf_x = (x - ctx%x_min) / (ctx%x_max - ctx%x_min) * real(ctx%width, wp)
        pdf_y = (1.0_wp - (y - ctx%y_min) / (ctx%y_max - ctx%y_min)) * real(ctx%height, wp)
    end subroutine normalize_to_pdf_coords

    subroutine draw_vector_line(ctx, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x1, y1, x2, y2
        character(len=50) :: move_cmd, line_cmd
        
        write(move_cmd, '(F8.2, 1X, F8.2, 1X, "m")') x1, y1
        write(line_cmd, '(F8.2, 1X, F8.2, 1X, "l")') x2, y2
        
        call add_to_stream(ctx, move_cmd)
        call add_to_stream(ctx, line_cmd)
        call add_to_stream(ctx, "S")
    end subroutine draw_vector_line

    subroutine finalize_pdf_stream(ctx)
        type(pdf_context), intent(inout) :: ctx
        call add_to_stream(ctx, "Q")
    end subroutine finalize_pdf_stream

    subroutine add_to_stream(ctx, command)
        type(pdf_context), intent(inout) :: ctx
        character(len=*), intent(in) :: command
        
        if (allocated(ctx%content_stream)) then
            ctx%content_stream = ctx%content_stream // command // char(10)
        else
            ctx%content_stream = command // char(10)
        end if
    end subroutine add_to_stream
    
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
        integer :: obj_positions(5), xref_pos
        
        call write_string_to_unit(unit, "%PDF-1.4")
        call write_all_objects(unit, ctx, obj_positions)
        call write_xref_and_trailer(unit, obj_positions, xref_pos)
    end subroutine write_pdf_structure
    
    subroutine write_all_objects(unit, ctx, positions)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: positions(5)
        
        call write_catalog_object(unit, positions(1))
        call write_pages_object(unit, ctx, positions(2))
        call write_page_object(unit, ctx, positions(3))
        call write_content_object(unit, ctx, positions(4))
        call write_font_object(unit, positions(5))
    end subroutine write_all_objects

    subroutine write_xref_and_trailer(unit, positions, xref_pos)
        integer, intent(in) :: unit, positions(5)
        integer, intent(out) :: xref_pos
        character(len=200) :: xref_entry
        character(len=100) :: trailer_str
        
        xref_pos = get_position(unit)
        call write_string_to_unit(unit, "xref")
        call write_string_to_unit(unit, "0 6")
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
        
        call write_string_to_unit(unit, "trailer")
        call write_string_to_unit(unit, "<</Size 6/Root 1 0 R>>")
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
        write(page_str, '("<</Type /Page/Parent 2 0 R/MediaBox [0 0 ", A, "]/Contents 4 0 R/Resources <</Font <</F1 5 0 R>>>>>>>>")') trim(size_str)
        call write_string_to_unit(unit, page_str)
        call write_string_to_unit(unit, "endobj")
    end subroutine write_page_object

    subroutine write_content_object(unit, ctx, pos)
        integer, intent(in) :: unit
        type(pdf_context), intent(in) :: ctx
        integer, intent(out) :: pos
        character(len=50) :: len_str
        
        pos = get_position(unit)
        write(len_str, '("/Length ", I0)') len(ctx%content_stream)
        call write_string_to_unit(unit, "4 0 obj")
        call write_string_to_unit(unit, "<<" // trim(len_str) // ">>")
        call write_string_to_unit(unit, "stream")
        call write_string_to_unit(unit, ctx%content_stream)
        call write_string_to_unit(unit, "endstream")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_content_object

    subroutine write_font_object(unit, pos)
        integer, intent(in) :: unit
        integer, intent(out) :: pos
        
        pos = get_position(unit)
        call write_string_to_unit(unit, "5 0 obj")
        call write_string_to_unit(unit, "<</Type /Font/Subtype /Type1/BaseFont /Helvetica>>")
        call write_string_to_unit(unit, "endobj")
    end subroutine write_font_object

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

end module pdf_module