module fortplot_pdf_objects
    !! PDF object writers for document structure elements

    use, intrinsic :: iso_fortran_env, only: wp => real64, int8, int64
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_pdf_stream, only: stream_pos0, write_pdf_line, &
                                   write_string_to_unit, write_binary_to_unit
    use fortplot_pdf_text_metrics, only: helv_width_units
    use fortplot_text_fonts, only: find_font_by_name, find_any_available_font
    use fortplot_zlib_core, only: zlib_compress_into

    implicit none
    private

    public :: write_info_object
    public :: write_catalog_object
    public :: write_pages_object
    public :: write_page_object
    public :: write_image_object
    public :: write_null_object
    public :: write_extgstate_object
    public :: write_content_object
    public :: write_helvetica_font_object
    public :: write_symbol_font_object
    public :: write_helvetica_font_descriptor_object
    public :: write_helvetica_font_file_object
    public :: load_helvetica_font_file

    integer, parameter :: PDF_HELVETICA_DESCRIPTOR_OBJ = 9
    integer, parameter :: PDF_HELVETICA_FILE_OBJ = 10
    integer, parameter :: PDF_EXTGSTATE_BASE_OBJ = 11

contains

    subroutine write_info_object(unit, pos)
        integer, intent(in) :: unit
        integer(int64), intent(out) :: pos
        character(len=64) :: line

        pos = stream_pos0(unit)
        write (line, '(I0, A)') 1, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<>>')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_info_object

    subroutine write_catalog_object(unit, pos)
        !! Write PDF catalog object
        integer, intent(in) :: unit
        integer(int64), intent(out) :: pos
        character(len=64) :: line

        pos = stream_pos0(unit)
        write (line, '(I0, A)') 2, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Catalog')
        write (line, '(A, I0, A)') '/Pages ', 3, ' 0 R'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_catalog_object

    subroutine write_pages_object(unit, pos)
        !! Write PDF pages object
        integer, intent(in) :: unit
        integer(int64), intent(out) :: pos
        character(len=64) :: line

        pos = stream_pos0(unit)
        write (line, '(I0, A)') 3, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Pages')
        write (line, '(A, I0, A)') '/Kids [', 4, ' 0 R]'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '/Count 1')
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_pages_object

    subroutine write_page_object(unit, ctx, pos)
        !! Write PDF page object
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer(int64), intent(out) :: pos
        character(len=128) :: line
        integer :: i, obj

        pos = stream_pos0(unit)
        write (line, '(I0, A)') 4, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Page')
        write (line, '(A, I0, A)') '/Parent ', 3, ' 0 R'
        call write_pdf_line(unit, trim(line))
        write (line, '(A, F0.1, 1X, F0.1, A)') '/MediaBox [0 0 ', ctx%width, &
            ctx%height, ']'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '/Resources <<')
        call write_pdf_line(unit, '  /Font <<')
        write (line, '(A, I0, A)') '    /F5 ', 5, ' 0 R'
        call write_pdf_line(unit, trim(line))
        write (line, '(A, I0, A)') '    /F6 ', 6, ' 0 R'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '  >>')
        if (ctx%has_image) then
            call write_pdf_line(unit, '  /XObject <<')
            write (line, '(A, I0, A)') '    /Im1 ', 8, ' 0 R'
            call write_pdf_line(unit, trim(line))
            call write_pdf_line(unit, '  >>')
        end if
        if (ctx%extgstate_count > 0) then
            call write_pdf_line(unit, '  /ExtGState <<')
            do i = 1, ctx%extgstate_count
                obj = PDF_EXTGSTATE_BASE_OBJ + i - 1
                write (line, '(A, I0, A, I0, A)') '    /GS', i, ' ', obj, ' 0 R'
                call write_pdf_line(unit, trim(line))
            end do
            call write_pdf_line(unit, '  >>')
        end if
        call write_pdf_line(unit, '>>')
        write (line, '(A, I0, A)') '/Contents ', 7, ' 0 R'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_page_object

    subroutine write_image_object(unit, ctx, pos)
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer(int64), intent(out) :: pos
        integer :: n
        character(len=64) :: line

        pos = stream_pos0(unit)
        write (line, '(I0, A)') 8, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /XObject')
        call write_pdf_line(unit, '/Subtype /Image')
        write (line, '(A, I0)') '/Width ', ctx%image_width
        call write_pdf_line(unit, trim(line))
        write (line, '(A, I0)') '/Height ', ctx%image_height
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '/ColorSpace /DeviceRGB')
        call write_pdf_line(unit, '/BitsPerComponent 8')
        call write_pdf_line(unit, '/Interpolate false')
        n = len(ctx%image_data)
        write (line, '(A, I0)') '/Length ', n
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '/Filter /FlateDecode')
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'stream')
        call write_binary_to_unit(unit, ctx%image_data, n)
        call write_pdf_line(unit, '')
        call write_pdf_line(unit, 'endstream')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_image_object

    subroutine write_null_object(unit, obj, pos)
        integer, intent(in) :: unit
        integer, intent(in) :: obj
        integer(int64), intent(out) :: pos
        character(len=64) :: line

        pos = stream_pos0(unit)
        write (line, '(I0, A)') obj, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, 'null')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_null_object

    subroutine write_extgstate_object(unit, obj, stroke_milli, fill_milli, pos)
        integer, intent(in) :: unit
        integer, intent(in) :: obj
        integer, intent(in) :: stroke_milli, fill_milli
        integer(int64), intent(out) :: pos
        character(len=64) :: line
        character(len=128) :: dict
        real(wp) :: stroke_alpha, fill_alpha
        character(len=16) :: stroke_txt, fill_txt

        stroke_alpha = real(stroke_milli, wp)/1000.0_wp
        fill_alpha = real(fill_milli, wp)/1000.0_wp

        pos = stream_pos0(unit)
        write (line, '(I0, A)') obj, ' 0 obj'
        call write_pdf_line(unit, trim(line))

        write (stroke_txt, '(F5.3)') stroke_alpha
        write (fill_txt, '(F5.3)') fill_alpha
        dict = '<< /Type /ExtGState /CA '//trim(adjustl(stroke_txt))// &
               ' /ca '//trim(adjustl(fill_txt))//' >>'
        call write_pdf_line(unit, trim(dict))
        call write_pdf_line(unit, 'endobj')
    end subroutine write_extgstate_object

    subroutine write_content_object(unit, ctx, pos)
        !! Write PDF content stream object
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer(int64), intent(out) :: pos
        integer :: stream_len
        ! Flate (zlib) compression buffers
        integer(int8), allocatable :: in_bytes(:)
        integer(int8), allocatable :: out_bytes(:)
        integer :: out_len
        integer :: i, n
        character(len=:), allocatable :: compressed_str
        character(len=64) :: line

        stream_len = len_trim(ctx%stream_data)

        pos = stream_pos0(unit)
        write (line, '(I0, A)') 7, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        if (stream_len > 0) then
            allocate (in_bytes(stream_len))
            do i = 1, stream_len
                in_bytes(i) = int(iachar(ctx%stream_data(i:i)), int8)
            end do
            call zlib_compress_into(in_bytes, stream_len, out_bytes, out_len)
            ! Build a character buffer from compressed bytes
            n = out_len
            compressed_str = repeat(' ', n)
            do i = 1, n
                compressed_str(i:i) = achar(iand(int(out_bytes(i), kind=4), 255))
            end do
            write (line, '(A, I0)') '/Length ', n
            call write_pdf_line(unit, trim(line))
            call write_pdf_line(unit, '/Filter /FlateDecode')
        else
            write (line, '(A, I0)') '/Length ', stream_len
            call write_pdf_line(unit, trim(line))
        end if
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'stream')

        ! Write the actual (possibly compressed) stream data
        if (stream_len > 0) then
            call write_binary_to_unit(unit, compressed_str, len(compressed_str))
        else
            call write_string_to_unit(unit, ctx%stream_data)
        end if

        call write_pdf_line(unit, '')
        call write_pdf_line(unit, 'endstream')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_content_object

    subroutine write_helvetica_font_object(unit, pos, has_embedded_font)
        !! Write Helvetica font object
        integer, intent(in) :: unit
        integer(int64), intent(out) :: pos
        logical, intent(in) :: has_embedded_font
        character(len=64) :: line

        pos = stream_pos0(unit)
        write (line, '(I0, A)') 5, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Font')
        if (has_embedded_font) then
            call write_pdf_line(unit, '/Subtype /TrueType')
        else
            call write_pdf_line(unit, '/Subtype /Type1')
        end if
        call write_pdf_line(unit, '/BaseFont /Helvetica')
        if (has_embedded_font) then
            call write_pdf_line(unit, '/FirstChar 0')
            call write_pdf_line(unit, '/LastChar 255')
            call write_helvetica_widths(unit)
            write (line, '(A, I0, A)') '/FontDescriptor ', &
                PDF_HELVETICA_DESCRIPTOR_OBJ, ' 0 R'
            call write_pdf_line(unit, trim(line))
        end if
        ! WinAnsiEncoding lacks the math minus glyph that matplotlib uses for
        ! negative labels. Remap the unused control slot 31 to Helvetica's
        ! /minus glyph (U+2212) via a Differences array so negative ticks
        ! render and extract as a true typographic minus, not a hyphen.
        call write_pdf_line(unit, '/Encoding <<')
        call write_pdf_line(unit, '  /Type /Encoding')
        call write_pdf_line(unit, '  /BaseEncoding /WinAnsiEncoding')
        call write_pdf_line(unit, '  /Differences [31 /minus]')
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_helvetica_font_object

    subroutine write_helvetica_widths(unit)
        integer, intent(in) :: unit
        character(len=256) :: line, width_text
        integer :: codepoint, width_count

        call write_pdf_line(unit, '/Widths [')
        line = ''
        width_count = 0
        do codepoint = 0, 255
            write (width_text, '(I0)') helv_width_units(codepoint)
            if (len_trim(line) + len_trim(width_text) + 1 > 80) then
                call write_pdf_line(unit, trim(line))
                line = ''
            end if
            if (len_trim(line) == 0) then
                line = trim(width_text)
            else
                line = trim(line)//' '//trim(width_text)
            end if
            width_count = width_count + 1
        end do
        if (len_trim(line) > 0) call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, ']')
    end subroutine write_helvetica_widths

    subroutine write_helvetica_font_descriptor_object(unit, pos, has_embedded_font)
        integer, intent(in) :: unit
        integer(int64), intent(out) :: pos
        logical, intent(in) :: has_embedded_font
        character(len=64) :: line

        if (.not. has_embedded_font) then
            call write_null_object(unit, PDF_HELVETICA_DESCRIPTOR_OBJ, pos)
            return
        end if

        pos = stream_pos0(unit)
        write (line, '(I0, A)') PDF_HELVETICA_DESCRIPTOR_OBJ, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /FontDescriptor')
        call write_pdf_line(unit, '/FontName /Helvetica')
        call write_pdf_line(unit, '/Flags 32')
        call write_pdf_line(unit, '/FontBBox [-166 -225 1000 931]')
        call write_pdf_line(unit, '/ItalicAngle 0')
        call write_pdf_line(unit, '/Ascent 931')
        call write_pdf_line(unit, '/Descent -225')
        call write_pdf_line(unit, '/CapHeight 718')
        call write_pdf_line(unit, '/StemV 80')
        write (line, '(A, I0, A)') '/FontFile2 ', PDF_HELVETICA_FILE_OBJ, ' 0 R'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_helvetica_font_descriptor_object

    subroutine write_helvetica_font_file_object(unit, pos, font_data)
        integer, intent(in) :: unit
        integer(int64), intent(out) :: pos
        character(len=*), intent(in) :: font_data
        character(len=64) :: line
        integer :: n

        if (len(font_data) == 0) then
            call write_null_object(unit, PDF_HELVETICA_FILE_OBJ, pos)
            return
        end if

        n = len(font_data)
        pos = stream_pos0(unit)
        write (line, '(I0, A)') PDF_HELVETICA_FILE_OBJ, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        write (line, '(A, I0)') '/Length ', n
        call write_pdf_line(unit, trim(line))
        write (line, '(A, I0)') '/Length1 ', n
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'stream')
        call write_binary_to_unit(unit, font_data, n)
        call write_pdf_line(unit, '')
        call write_pdf_line(unit, 'endstream')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_helvetica_font_file_object

    subroutine load_helvetica_font_file(font_data, found)
        character(len=:), allocatable, intent(out) :: font_data
        logical, intent(out) :: found
        character(len=256) :: font_path

        found = find_font_by_name('Helvetica', font_path)
        if (.not. found) found = find_any_available_font(font_path)
        if (found) call read_font_file(trim(font_path), font_data, found)
        if (.not. found) font_data = ''
    end subroutine load_helvetica_font_file

    subroutine read_font_file(font_path, font_data, ok)
        character(len=*), intent(in) :: font_path
        character(len=:), allocatable, intent(out) :: font_data
        logical, intent(out) :: ok
        integer :: unit, ios, file_size

        ok = .false.
        inquire (file=font_path, size=file_size, iostat=ios)
        if (ios /= 0 .or. file_size <= 0) then
            font_data = ''
            return
        end if

        allocate (character(len=file_size) :: font_data)
        open (newunit=unit, file=font_path, access='stream', form='unformatted', &
              action='read', status='old', iostat=ios)
        if (ios /= 0) then
            font_data = ''
            return
        end if
        read (unit, iostat=ios) font_data
        close (unit)
        ok = ios == 0
        if (.not. ok) font_data = ''
    end subroutine read_font_file

    subroutine write_symbol_font_object(unit, pos)
        !! Write Symbol font object
        integer, intent(in) :: unit
        integer(int64), intent(out) :: pos
        character(len=64) :: line

        pos = stream_pos0(unit)
        write (line, '(I0, A)') 6, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Font')
        call write_pdf_line(unit, '/Subtype /Type1')
        call write_pdf_line(unit, '/BaseFont /Symbol')
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_symbol_font_object

end module fortplot_pdf_objects
