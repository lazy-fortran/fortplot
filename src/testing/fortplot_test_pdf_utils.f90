module fortplot_test_pdf_utils
    !! PDF stream extraction utilities for testing
    !!
    !! Single Responsibility: Extract and parse text from PDF file streams
    !! Tokenizer functionality is in fortplot_test_pdf_tokenizer.

    use, intrinsic :: iso_fortran_env, only: dp => real64, int8, int64
    use fortplot_zlib_core, only: zlib_decompress
    use fortplot_test_pdf_tokenizer, only: find_subsequence
    implicit none
contains
    subroutine extract_pdf_stream_text(filename, stream_text, status)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: stream_text
        integer, intent(out) :: status

        character(len=1), allocatable :: data(:)
        integer(int64) :: fsize
        integer :: content_begin, content_len
        integer :: ios, pos
        integer :: stream_start, stream_end
        integer :: content_obj
        integer :: unit

        status = 0
        allocate (character(len=0) :: stream_text)

        open (newunit=unit, file=filename, access='stream', form='unformatted', &
              status='old', iostat=ios)
        if (ios /= 0) then
            status = -1
            return
        end if

        inquire (unit=unit, size=fsize)
        if (fsize <= 0_int64) then
            close (unit)
            return
        end if

        allocate (character(len=1) :: data(int(fsize)))
        read (unit, iostat=ios) data
        close (unit)
        if (ios /= 0) then
            status = -2
            return
        end if

        content_obj = find_subsequence(data, fsize, '7 0 obj', 1)
        if (content_obj > 0) then
            pos = content_obj
        else
            pos = 1
        end if

        stream_start = find_subsequence(data, fsize, 'stream', pos)
        if (stream_start < 0) return
        stream_end = find_subsequence(data, fsize, 'endstream', &
                                      stream_start + len('stream'))
        if (stream_end < 0) return

        content_begin = stream_start + len('stream')
        if (content_begin <= int(fsize)) then
            if (data(content_begin) == achar(13)) then
                content_begin = content_begin + 1
                if (content_begin <= int(fsize)) then
                    if (data(content_begin) == achar(10)) then
                        content_begin = content_begin + 1
                    end if
                end if
            else if (data(content_begin) == achar(10)) then
                content_begin = content_begin + 1
            end if
        end if

        content_len = stream_end - content_begin
        if (content_len <= 0) return

        block
            integer(int8), allocatable :: compressed(:), decompressed_raw(:)
            character(len=:), allocatable :: chunk_text
            integer :: j
            integer :: status_decomp

            allocate (compressed(content_len))
            do j = 1, content_len
                compressed(j) = int(iachar(data(content_begin + j - 1)), int8)
            end do
            decompressed_raw = zlib_decompress(compressed, content_len, &
                                               status_decomp, .false.)

            if (status_decomp == 0 .and. size(decompressed_raw) > 0) then
                allocate (character(len=size(decompressed_raw)) :: chunk_text)
                call bytes_to_string(decompressed_raw, chunk_text)
            else
                allocate (character(len=content_len) :: chunk_text)
                do j = 1, content_len
                    chunk_text(j:j) = data(content_begin + j - 1)
                end do
            end if

            call append_string(stream_text, chunk_text)
        end block

    end subroutine extract_pdf_stream_text

    subroutine append_string(target, chunk)
        character(len=:), allocatable, intent(inout) :: target
        character(len=*), intent(in) :: chunk
        character(len=:), allocatable :: combined
        integer :: new_len
        integer :: old_len

        old_len = len(target)
        new_len = old_len + len(chunk)

        allocate (character(len=new_len) :: combined)
        if (old_len > 0) combined(1:old_len) = target
        if (len(chunk) > 0) combined(old_len + 1:new_len) = chunk
        call move_alloc(combined, target)
    end subroutine append_string

    subroutine bytes_to_string(bytes, text)
        integer(int8), intent(in) :: bytes(:)
        character(len=*), intent(out) :: text
        integer :: i

        do i = 1, size(bytes)
            text(i:i) = achar(iand(int(bytes(i)), 255))
        end do
    end subroutine bytes_to_string

end module fortplot_test_pdf_utils
