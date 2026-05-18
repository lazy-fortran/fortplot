module fortplot_pdf_objects
    !! PDF object writers for document structure elements

    use, intrinsic :: iso_fortran_env, only: wp => real64, int8, int64
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_pdf_stream, only: stream_pos0, write_pdf_line, &
                                   write_string_to_unit, write_binary_to_unit
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
        write (line, '(A, I0, A, I0, A)') '    /F5 ', 5, ' ', 5, ' 0 R'
        call write_pdf_line(unit, trim(line))
        write (line, '(A, I0, A, I0, A)') '    /F6 ', 6, ' ', 6, ' 0 R'
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
                obj = 9 + i - 1
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

    subroutine write_helvetica_font_object(unit, pos)
        !! Write Helvetica font object
        integer, intent(in) :: unit
        integer(int64), intent(out) :: pos
        character(len=64) :: line

        pos = stream_pos0(unit)
        write (line, '(I0, A)') 5, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Font')
        call write_pdf_line(unit, '/Subtype /Type1')
        call write_pdf_line(unit, '/BaseFont /Helvetica')
        call write_pdf_line(unit, '/Encoding /WinAnsiEncoding')
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_helvetica_font_object

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
