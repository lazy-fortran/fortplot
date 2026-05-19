module fortplot_pdf_stream
    !! Low-level PDF stream writing utilities

    use, intrinsic :: iso_fortran_env, only: int64

    implicit none
    private

    public :: stream_pos0
    public :: write_pdf_line
    public :: write_string_to_unit
    public :: write_binary_to_unit

contains

    integer(int64) function stream_pos0(unit) result(pos0)
        !! Get current 0-based byte offset in a stream file.
        integer, intent(in) :: unit
        integer(int64) :: pos1

        inquire (unit=unit, pos=pos1)
        pos0 = max(0_int64, pos1 - 1_int64)
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
            if (i + chunk_size - 1 <= str_len) then
                call write_unit_chunk(unit, file_form, str(i:i + chunk_size - 1))
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
            last = min(nbytes, i + chunk_size - 1)
            call write_unit_chunk(unit, file_form, str(i:last))
            i = last + 1
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

end module fortplot_pdf_stream
