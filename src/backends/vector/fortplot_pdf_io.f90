module fortplot_pdf_io
    !! PDF file I/O operations
    !! Handles PDF document structure, writing, and file management

    use, intrinsic :: iso_fortran_env, only: wp => real64, int8, int64
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_pdf_objects, only: &
        write_info_object, write_catalog_object, write_pages_object, &
        write_page_object, write_image_object, write_null_object, &
        write_extgstate_object, write_content_object, &
        write_helvetica_font_object, write_symbol_font_object
    use fortplot_pdf_stream, only: stream_pos0, write_pdf_line, &
                                   write_string_to_unit, write_binary_to_unit
    use fortplot_logging, only: log_error
    implicit none
    private

    ! Public procedures
    public :: write_pdf_file

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
        last_obj = PDF_IMAGE_OBJ + ctx%extgstate_count
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
            obj = PDF_EXTGSTATE_BASE_OBJ + i - 1
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
        write (line, '(I0, 1X, I0)') 0, num_objects + 1
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
        write (line, '(A, I0)') '/Size ', num_objects + 1
        call write_pdf_line(unit, trim(line))
        write (line, '(A, I0, A)') '/Root ', PDF_CATALOG_OBJ, ' 0 R'
        call write_pdf_line(unit, trim(line))
        write (line, '(A, I0, A)') '/Info ', PDF_INFO_OBJ, ' 0 R'
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '>>')
        call write_pdf_line(unit, 'startxref')
        write (line, '(I0)') xref_pos
        call write_pdf_line(unit, trim(line))
        call write_pdf_line(unit, '%%EOF')
    end subroutine write_xref_and_trailer

end module fortplot_pdf_io
