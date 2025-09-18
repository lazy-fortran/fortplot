module fortplot_pdf_io
    !! PDF file I/O operations
    !! Handles PDF document structure, writing, and file management
    
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core
    use, intrinsic :: iso_fortran_env, only: int8
    use fortplot_zlib_core, only: zlib_compress
    use fortplot_logging, only: log_error
    implicit none
    private
    
    ! Public procedures
    public :: write_pdf_file
    public :: create_pdf_document
    public :: write_string_to_unit
    public :: write_binary_to_unit
    
    ! PDF structure constants
    integer, parameter :: PDF_VERSION_OBJ = 1
    integer, parameter :: PDF_CATALOG_OBJ = 2
    integer, parameter :: PDF_PAGES_OBJ = 3
    integer, parameter :: PDF_PAGE_OBJ = 4
    integer, parameter :: PDF_HELVETICA_OBJ = 5
    integer, parameter :: PDF_SYMBOL_OBJ = 6
    integer, parameter :: PDF_CONTENT_OBJ = 7
    integer, parameter :: PDF_IMAGE_OBJ   = 8

contains

    subroutine write_pdf_file(this, filename, success)
        !! Write PDF context to file
        class(pdf_context_core), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical, intent(out), optional :: success
        integer :: unit, ios
        
        if (present(success)) success = .false.
        
        ! Open file for writing
        open(newunit=unit, file=filename, status='replace', &
             form='formatted', action='write', iostat=ios)
        
        if (ios /= 0) then
            call log_error('pdf_io: failed to open file for writing: ' // trim(filename))
            if (present(success)) then
                return  ! Return with success = .false.
            else
                return
            end if
        end if
        
        ! Write PDF document
        call create_pdf_document(unit, filename, this)
        
        ! Close file
        close(unit)
        
        if (present(success)) success = .true.
    end subroutine write_pdf_file

    subroutine create_pdf_document(unit, filename, ctx)
        !! Create complete PDF document structure
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename  ! Placeholder for future use
        type(pdf_context_core), intent(inout) :: ctx
        ! Reference unused argument to keep interface stable
        associate(unused_fn_len => len_trim(filename)); end associate
        
        ! Write PDF header
        write(unit, '(A)') '%PDF-1.4'
        write(unit, '(A)') '%'//char(128)//char(129)//char(130)//char(131)
        
        ! Write PDF structure
        call write_pdf_structure(unit, ctx)
    end subroutine create_pdf_document

    subroutine write_pdf_structure(unit, ctx)
        !! Write complete PDF structure with all objects
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer, allocatable :: positions(:)
        integer :: xref_pos
        
        ! Allocate position tracking array
        allocate(positions(8))
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
        integer, intent(inout) :: positions(:)
        
        ! Write catalog object
        call write_catalog_object(unit, positions(PDF_CATALOG_OBJ))
        
        ! Write pages object
        call write_pages_object(unit, ctx, positions(PDF_PAGES_OBJ))
        
        ! Write page object
        call write_page_object(unit, ctx, positions(PDF_PAGE_OBJ))
        
        ! Write font objects
        call write_helvetica_font_object(unit, positions(PDF_HELVETICA_OBJ))
        call write_symbol_font_object(unit, positions(PDF_SYMBOL_OBJ))
        
        ! Write image XObject if present
        if (ctx%has_image) then
            call write_image_object(unit, ctx, positions(PDF_IMAGE_OBJ))
        end if
        ! Write content stream object
        call write_content_object(unit, ctx, positions(PDF_CONTENT_OBJ))
    end subroutine write_all_objects

    subroutine write_xref_and_trailer(unit, positions, xref_pos)
        !! Write cross-reference table and trailer
        integer, intent(in) :: unit
        integer, intent(in) :: positions(:)
        integer, intent(out) :: xref_pos
        integer :: i, num_objects
        
        num_objects = size(positions)
        
        ! Get current position for xref
        inquire(unit=unit, pos=xref_pos)
        
        ! Write xref header
        write(unit, '(A)') 'xref'
        write(unit, '(A, I0, 1X, I0)') '0 ', num_objects + 1
        
        ! Write xref entries
        write(unit, '(A)') '0000000000 65535 f'
        do i = 1, num_objects
            write(unit, '(I10.10, A)') positions(i), ' 00000 n'
        end do
        
        ! Write trailer
        write(unit, '(A)') 'trailer'
        write(unit, '(A)') '<<'
        write(unit, '(A, I0)') '/Size ', num_objects + 1
        write(unit, '(A, I0, A)') '/Root ', PDF_CATALOG_OBJ, ' 0 R'
        write(unit, '(A)') '>>'
        write(unit, '(A)') 'startxref'
        write(unit, '(I0)') xref_pos
        write(unit, '(A)') '%%EOF'
    end subroutine write_xref_and_trailer

    subroutine write_catalog_object(unit, pos)
        !! Write PDF catalog object
        integer, intent(in) :: unit
        integer, intent(out) :: pos
        
        inquire(unit=unit, pos=pos)
        write(unit, '(I0, A)') PDF_CATALOG_OBJ, ' 0 obj'
        write(unit, '(A)') '<<'
        write(unit, '(A)') '/Type /Catalog'
        write(unit, '(A, I0, A)') '/Pages ', PDF_PAGES_OBJ, ' 0 R'
        write(unit, '(A)') '>>'
        write(unit, '(A)') 'endobj'
    end subroutine write_catalog_object

    subroutine write_pages_object(unit, ctx, pos)
        !! Write PDF pages object
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer, intent(out) :: pos
        ! Reference ctx to keep interface stable
        associate(unused_w => ctx%width); end associate
        
        inquire(unit=unit, pos=pos)
        write(unit, '(I0, A)') PDF_PAGES_OBJ, ' 0 obj'
        write(unit, '(A)') '<<'
        write(unit, '(A)') '/Type /Pages'
        write(unit, '(A, I0, A)') '/Kids [', PDF_PAGE_OBJ, ' 0 R]'
        write(unit, '(A)') '/Count 1'
        write(unit, '(A)') '>>'
        write(unit, '(A)') 'endobj'
    end subroutine write_pages_object

    subroutine write_page_object(unit, ctx, pos)
        !! Write PDF page object
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer, intent(out) :: pos
        
        inquire(unit=unit, pos=pos)
        write(unit, '(I0, A)') PDF_PAGE_OBJ, ' 0 obj'
        write(unit, '(A)') '<<'
        write(unit, '(A)') '/Type /Page'
        write(unit, '(A, I0, A)') '/Parent ', PDF_PAGES_OBJ, ' 0 R'
        write(unit, '(A, F0.1, 1X, F0.1, A)') '/MediaBox [0 0 ', ctx%width, ctx%height, ']'
        write(unit, '(A)') '/Resources <<'
        write(unit, '(A)') '  /Font <<'
        write(unit, '(A, I0, A, I0, A)') '    /F', PDF_HELVETICA_OBJ, ' ', PDF_HELVETICA_OBJ, ' 0 R'
        write(unit, '(A, I0, A, I0, A)') '    /F', PDF_SYMBOL_OBJ, ' ', PDF_SYMBOL_OBJ, ' 0 R'
        write(unit, '(A)') '  >>'
        if (ctx%has_image) then
            write(unit, '(A)') '  /XObject <<'
            write(unit, '(A, I0, A)') '    /Im1 ', PDF_IMAGE_OBJ, ' 0 R'
            write(unit, '(A)') '  >>'
        end if
        write(unit, '(A)') '>>'
        write(unit, '(A, I0, A)') '/Contents ', PDF_CONTENT_OBJ, ' 0 R'
        write(unit, '(A)') '>>'
        write(unit, '(A)') 'endobj'
    end subroutine write_page_object

    subroutine write_image_object(unit, ctx, pos)
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer, intent(out) :: pos
        integer :: n
        
        inquire(unit=unit, pos=pos)
        write(unit, '(I0, A)') PDF_IMAGE_OBJ, ' 0 obj'
        write(unit, '(A)') '<<'
        write(unit, '(A)') '/Type /XObject'
        write(unit, '(A)') '/Subtype /Image'
        write(unit, '(A, I0)') '/Width ', ctx%image_width
        write(unit, '(A, I0)') '/Height ', ctx%image_height
        write(unit, '(A)') '/ColorSpace /DeviceRGB'
        write(unit, '(A)') '/BitsPerComponent 8'
        write(unit, '(A)') '/Interpolate false'
        n = len(ctx%image_data)
        write(unit, '(A, I0)') '/Length ', n
        write(unit, '(A)') '/Filter /FlateDecode'
        write(unit, '(A)') '>>'
        write(unit, '(A)') 'stream'
        call write_binary_to_unit(unit, ctx%image_data, n)
        write(unit, '(A)') ''
        write(unit, '(A)') 'endstream'
        write(unit, '(A)') 'endobj'
    end subroutine write_image_object

    subroutine write_content_object(unit, ctx, pos)
        !! Write PDF content stream object
        integer, intent(in) :: unit
        type(pdf_context_core), intent(in) :: ctx
        integer, intent(out) :: pos
        integer :: stream_len
        ! Flate (zlib) compression buffers
        integer(int8), allocatable :: in_bytes(:)
        integer(int8), allocatable :: out_bytes(:)
        integer :: out_len
        integer :: i, n
        character(len=:), allocatable :: compressed_str

        stream_len = len_trim(ctx%stream_data)
        
        inquire(unit=unit, pos=pos)
        write(unit, '(I0, A)') PDF_CONTENT_OBJ, ' 0 obj'
        write(unit, '(A)') '<<'
        if (stream_len > 0) then
            allocate(in_bytes(stream_len))
            do i = 1, stream_len
                in_bytes(i) = int(iachar(ctx%stream_data(i:i)), int8)
            end do
            out_bytes = zlib_compress(in_bytes, stream_len, out_len)
            ! Build a character buffer from compressed bytes
            n = out_len
            compressed_str = repeat(' ', n)
            do i = 1, n
                compressed_str(i:i) = achar(iand(int(out_bytes(i), kind=4), 255))
            end do
            write(unit, '(A, I0)') '/Length ', n
            write(unit, '(A)') '/Filter /FlateDecode'
        else
            write(unit, '(A, I0)') '/Length ', stream_len
        end if
        write(unit, '(A)') '>>'
        write(unit, '(A)') 'stream'
        
        ! Write the actual (possibly compressed) stream data
        if (stream_len > 0) then
            call write_binary_to_unit(unit, compressed_str, len(compressed_str))
        else
            call write_string_to_unit(unit, ctx%stream_data)
        end if
        
        write(unit, '(A)') ''
        write(unit, '(A)') 'endstream'
        write(unit, '(A)') 'endobj'
    end subroutine write_content_object

    subroutine write_helvetica_font_object(unit, pos)
        !! Write Helvetica font object
        integer, intent(in) :: unit
        integer, intent(out) :: pos
        
        inquire(unit=unit, pos=pos)
        write(unit, '(I0, A)') PDF_HELVETICA_OBJ, ' 0 obj'
        write(unit, '(A)') '<<'
        write(unit, '(A)') '/Type /Font'
        write(unit, '(A)') '/Subtype /Type1'
        write(unit, '(A)') '/BaseFont /Helvetica'
        write(unit, '(A)') '/Encoding /WinAnsiEncoding'
        write(unit, '(A)') '>>'
        write(unit, '(A)') 'endobj'
    end subroutine write_helvetica_font_object

    subroutine write_symbol_font_object(unit, pos)
        !! Write Symbol font object
        integer, intent(in) :: unit
        integer, intent(out) :: pos
        
        inquire(unit=unit, pos=pos)
        write(unit, '(I0, A)') PDF_SYMBOL_OBJ, ' 0 obj'
        write(unit, '(A)') '<<'
        write(unit, '(A)') '/Type /Font'
        write(unit, '(A)') '/Subtype /Type1'
        write(unit, '(A)') '/BaseFont /Symbol'
        write(unit, '(A)') '>>'
        write(unit, '(A)') 'endobj'
    end subroutine write_symbol_font_object

    subroutine write_string_to_unit(unit, str)
        !! Write string to unit, handling long strings properly
        integer, intent(in) :: unit
        character(len=*), intent(in) :: str
        integer :: i, chunk_size
        integer :: str_len
        
        str_len = len_trim(str)
        chunk_size = 1000  ! Write in chunks to avoid line length issues
        
        ! Write string in chunks
        do i = 1, str_len, chunk_size
            if (i + chunk_size - 1 <= str_len) then
                write(unit, '(A)', advance='no') str(i:i+chunk_size-1)
            else
                write(unit, '(A)', advance='no') str(i:str_len)
            end if
        end do
    end subroutine write_string_to_unit

    subroutine write_binary_to_unit(unit, str, nbytes)
        !! Write binary string to unit using exact length (no trimming)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: str
        integer, intent(in) :: nbytes
        integer :: i, chunk_size, last
        chunk_size = 1000
        if (nbytes <= 0) return
        i = 1
        do while (i <= nbytes)
            last = min(nbytes, i + chunk_size - 1)
            write(unit, '(A)', advance='no') str(i:last)
            i = last + 1
        end do
    end subroutine write_binary_to_unit

end module fortplot_pdf_io
