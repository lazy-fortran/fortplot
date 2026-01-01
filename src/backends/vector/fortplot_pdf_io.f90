module fortplot_pdf_io
    !! PDF file I/O operations
    !! Handles PDF document structure, writing, and file management

    use, intrinsic :: iso_fortran_env, only: wp => real64, int8, int64
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_zlib_core, only: zlib_compress_into
    use fortplot_logging, only: log_error
    implicit none
    private

    ! Public procedures
    public :: write_pdf_file
    public :: write_string_to_unit
    public :: write_binary_to_unit

    ! PDF structure constants
    integer, parameter :: PDF_INFO_OBJ = 1
    integer, parameter :: PDF_CATALOG_OBJ = 2
    integer, parameter :: PDF_PAGES_OBJ = 3
    integer, parameter :: PDF_PAGE_OBJ = 4
    integer, parameter :: PDF_HELVETICA_OBJ = 5
    integer, parameter :: PDF_SYMBOL_OBJ = 6
    integer, parameter :: PDF_CONTENT_OBJ = 7
    integer, parameter :: PDF_IMAGE_OBJ = 8
    integer, parameter :: PDF_EXTGSTATE_BASE_OBJ = 9

contains

    subroutine write_pdf_file(this, filename, success)
        !! Write PDF context to file
        class(pdf_context_core), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical, intent(out), optional :: success
        integer :: unit, ios

        if (present(success)) success = .false.

        ! Open as a byte-addressable stream so xref offsets are accurate.
        open (newunit=unit, file=filename, status='replace', access='stream', &
              form='unformatted', action='write', iostat=ios)

        if (ios /= 0) then
            call log_error('pdf_io: failed to open file for writing: '//trim(filename))
            if (present(success)) then
                return  ! Return with success = .false.
            else
                return
            end if
        end if

        ! Write PDF document
        call create_pdf_document(unit, this)

        ! Close file
        close (unit)

        if (present(success)) success = .true.
    end subroutine write_pdf_file

    subroutine create_pdf_document(unit, ctx)
        !! Create complete PDF document structure
        integer, intent(in) :: unit
        type(pdf_context_core), intent(inout) :: ctx
        character(len=5) :: bin_line

        ! Write PDF header
        call write_pdf_line(unit, '%PDF-1.4')
        bin_line = '%'//achar(128)//achar(129)//achar(130)//achar(131)
        call write_pdf_line(unit, bin_line)

        ! Write PDF structure
        call write_pdf_structure(unit, ctx)
    end subroutine create_pdf_document

    subroutine write_pdf_structure(unit, ctx)
        !! Write complete PDF structure with all objects
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer(int64), allocatable :: positions(:)
        integer(int64) :: xref_pos
        integer :: last_obj

        ! Allocate position tracking array
        last_obj = PDF_IMAGE_OBJ+ctx%extgstate_count
        allocate (positions(last_obj))
        positions = 0

        ! Write all objects and track positions
        call write_all_objects(unit, ctx, positions)

        ! Write cross-reference table and trailer
        call write_xref_and_trailer(unit, positions, xref_pos)
    end subroutine write_pdf_structure

    subroutine write_all_objects(unit, ctx, positions)
        !! Write all PDF objects
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer(int64), intent(inout) :: positions(:)
        integer :: i, obj

        call write_info_object(unit, positions(PDF_INFO_OBJ))

        ! Write catalog object
        call write_catalog_object(unit, positions(PDF_CATALOG_OBJ))

        ! Write pages object
        call write_pages_object(unit, positions(PDF_PAGES_OBJ))

        ! Write page object
        call write_page_object(unit, ctx, positions(PDF_PAGE_OBJ))

        ! Write font objects
        call write_helvetica_font_object(unit, positions(PDF_HELVETICA_OBJ))
        call write_symbol_font_object(unit, positions(PDF_SYMBOL_OBJ))

        if (ctx%has_image) then
            call write_image_object(unit, ctx, positions(PDF_IMAGE_OBJ))
        else
            call write_null_object(unit, PDF_IMAGE_OBJ, positions(PDF_IMAGE_OBJ))
        end if

        do i = 1, ctx%extgstate_count
            obj = PDF_EXTGSTATE_BASE_OBJ+i-1
            call write_extgstate_object(unit, obj, ctx%extgstate_stroke_milli(i), &
                                        ctx%extgstate_fill_milli(i), positions(obj))
        end do

        ! Write content stream object
        call write_content_object(unit, ctx, positions(PDF_CONTENT_OBJ))
    end subroutine write_all_objects

    subroutine write_xref_and_trailer(unit, positions, xref_pos)
        !! Write cross-reference table and trailer
        integer, intent(in) :: unit
        integer(int64), intent(in) :: positions(:)
        integer(int64), intent(out) :: xref_pos
        integer :: i, num_objects
        character(len=64) :: line

        num_objects = size(positions)

        ! Get current position for xref
        xref_pos = stream_pos0(unit)

        ! Write xref header
        call write_pdf_line(unit, 'xref')
        write (line, '(I0, 1X, I0)') 0, num_objects+1
        call write_pdf_line(unit, trim(line))

        ! Write xref entries
        call write_pdf_line(unit, '0000000000 65535 f')
        do i = 1, num_objects
            write (line, '(I10.10, A)') positions(i), ' 00000 n'
            call write_pdf_line(unit, trim(line))
        end do

        ! Write trailer
        call write_pdf_line(unit, 'trailer')
        call write_pdf_line(unit, '<<')
        write (line, '(A, I0)') '/Size ', num_objects+1
        call write_pdf_line(unit, trim(line))
        write (line, '(A, I0, A)') '/Root ', PDF_CATALOG_OBJ, ' 0 R'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'startxref')
        write (line, '(I0)') xref_pos
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '%%EOF')
    end subroutine write_xref_and_trailer

    subroutine write_info_object(unit, pos)
        integer, intent(in) :: unit
        integer(int64), intent(out) :: pos
        character(len=64) :: line

        pos = stream_pos0(unit)
        write (line, '(I0, A)') PDF_INFO_OBJ, ' 0 obj'
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
        write (line, '(I0, A)') PDF_CATALOG_OBJ, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Catalog')
        write (line, '(A, I0, A)') '/Pages ', PDF_PAGES_OBJ, ' 0 R'
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
        write (line, '(I0, A)') PDF_PAGES_OBJ, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Pages')
        write (line, '(A, I0, A)') '/Kids [', PDF_PAGE_OBJ, ' 0 R]'
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
        write (line, '(I0, A)') PDF_PAGE_OBJ, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Page')
        write (line, '(A, I0, A)') '/Parent ', PDF_PAGES_OBJ, ' 0 R'
        call write_pdf_line(unit, trim(line))
        write (line, '(A, F0.1, 1X, F0.1, A)') '/MediaBox [0 0 ', ctx%width, &
            ctx%height, ']'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '/Resources <<')
        call write_pdf_line(unit, '  /Font <<')
        write (line, '(A, I0, A, I0, A)') '    /F', PDF_HELVETICA_OBJ, ' ', &
            PDF_HELVETICA_OBJ, ' 0 R'
        call write_pdf_line(unit, trim(line))
        write (line, '(A, I0, A, I0, A)') '    /F', PDF_SYMBOL_OBJ, ' ', &
            PDF_SYMBOL_OBJ, ' 0 R'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '  >>')
        if (ctx%has_image) then
            call write_pdf_line(unit, '  /XObject <<')
            write (line, '(A, I0, A)') '    /Im1 ', PDF_IMAGE_OBJ, ' 0 R'
            call write_pdf_line(unit, trim(line))
            call write_pdf_line(unit, '  >>')
        end if
        if (ctx%extgstate_count > 0) then
            call write_pdf_line(unit, '  /ExtGState <<')
            do i = 1, ctx%extgstate_count
                obj = PDF_EXTGSTATE_BASE_OBJ+i-1
                write (line, '(A, I0, A, I0, A)') '    /GS', i, ' ', obj, ' 0 R'
                call write_pdf_line(unit, trim(line))
            end do
            call write_pdf_line(unit, '  >>')
        end if
        call write_pdf_line(unit, '>>')
        write (line, '(A, I0, A)') '/Contents ', PDF_CONTENT_OBJ, ' 0 R'
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
        write (line, '(I0, A)') PDF_IMAGE_OBJ, ' 0 obj'
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
        write (line, '(I0, A)') PDF_CONTENT_OBJ, ' 0 obj'
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
        write (line, '(I0, A)') PDF_HELVETICA_OBJ, ' 0 obj'
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
        write (line, '(I0, A)') PDF_SYMBOL_OBJ, ' 0 obj'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '<<')
        call write_pdf_line(unit, '/Type /Font')
        call write_pdf_line(unit, '/Subtype /Type1')
        call write_pdf_line(unit, '/BaseFont /Symbol')
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'endobj')
    end subroutine write_symbol_font_object

    integer(int64) function stream_pos0(unit) result(pos0)
        !! Get current 0-based byte offset in a stream file.
        integer, intent(in) :: unit
        integer(int64) :: pos1

        inquire (unit=unit, pos=pos1)
        pos0 = max(0_int64, pos1-1_int64)
    end function stream_pos0

    subroutine write_pdf_line(unit, line)
        !! Write a single PDF line terminated by CRLF.
        integer, intent(in) :: unit
        character(len=*), intent(in) :: line
        character(len=2), parameter :: crlf = achar(13)//achar(10)

        call write_binary_to_unit(unit, line, len(line))
        call write_binary_to_unit(unit, crlf, 2)
    end subroutine write_pdf_line

    subroutine write_string_to_unit(unit, str)
        !! Write string to unit, handling long strings properly
        integer, intent(in) :: unit
        character(len=*), intent(in) :: str
        integer :: i, chunk_size
        integer :: str_len
        character(len=16) :: file_form

        str_len = len_trim(str)
        chunk_size = 1000  ! Write in chunks to avoid line length issues
        inquire (unit=unit, form=file_form)

        ! Write string in chunks
        do i = 1, str_len, chunk_size
            if (i+chunk_size-1 <= str_len) then
                call write_unit_chunk(unit, file_form, str(i:i+chunk_size-1))
            else
                call write_unit_chunk(unit, file_form, str(i:str_len))
            end if
        end do
    end subroutine write_string_to_unit

    subroutine write_binary_to_unit(unit, str, nbytes)
        !! Write binary string to unit using exact length (no trimming)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: str
        integer, intent(in) :: nbytes
        integer :: i, chunk_size, last
        character(len=16) :: file_form
        chunk_size = 1000
        if (nbytes <= 0) return
        inquire (unit=unit, form=file_form)
        i = 1
        do while (i <= nbytes)
            last = min(nbytes, i+chunk_size-1)
            call write_unit_chunk(unit, file_form, str(i:last))
            i = last+1
        end do
    end subroutine write_binary_to_unit

    subroutine write_unit_chunk(unit, file_form, chunk)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: file_form
        character(len=*), intent(in) :: chunk

        if (trim(file_form) == 'FORMATTED') then
            write (unit, '(A)', advance='no') chunk
        else
            write (unit) chunk
        end if
    end subroutine write_unit_chunk

end module fortplot_pdf_io
