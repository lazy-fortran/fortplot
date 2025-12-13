module test_pdf_utils
    use, intrinsic :: iso_fortran_env, only: int8, int64
    use fortplot_zlib_core, only: zlib_decompress
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
        integer :: unit
        integer :: i

        status = 0
        allocate(character(len=0) :: stream_text)

        open(newunit=unit, file=filename, access='stream', form='unformatted', &
             status='old', iostat=ios)
        if (ios /= 0) then
            status = -1
            return
        end if

        inquire(unit=unit, size=fsize)
        if (fsize <= 0_int64) then
            close(unit)
            return
        end if

        allocate(character(len=1) :: data(int(fsize)))
        read(unit, iostat=ios) data
        close(unit)
        if (ios /= 0) then
            status = -2
            deallocate(data)
            return
        end if

        pos = 1
        do
            stream_start = find_subsequence(data, fsize, 'stream', pos)
            if (stream_start < 0) exit
            stream_end = find_subsequence(data, fsize, 'endstream', &
                stream_start + len('stream'))
            if (stream_end < 0) exit

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
            if (content_len > 0) then
                block
                    integer(int8), allocatable :: compressed(:), decompressed_raw(:)
                    character(len=:), allocatable :: chunk_text
                    integer :: j
                    integer :: status_decomp

                    allocate(compressed(content_len))
                    do j = 1, content_len
                        compressed(j) = int(iachar(data(content_begin + j - 1)), int8)
                    end do
                    decompressed_raw = zlib_decompress(compressed, content_len, &
                        status_decomp, .false.)

                    if (status_decomp == 0 .and. size(decompressed_raw) > 0) then
                        allocate(character(len=size(decompressed_raw)) :: chunk_text)
                        call bytes_to_string(decompressed_raw, chunk_text)
                    else
                        allocate(character(len=content_len) :: chunk_text)
                        do j = 1, content_len
                            chunk_text(j:j) = data(content_begin + j - 1)
                        end do
                    end if

                    call append_string(stream_text, chunk_text)
                end block
            end if

            pos = stream_end + len('endstream')
        end do

        deallocate(data)
    end subroutine extract_pdf_stream_text

    subroutine append_string(target, chunk)
        character(len=:), allocatable, intent(inout) :: target
        character(len=*), intent(in) :: chunk
        character(len=:), allocatable :: combined
        integer :: new_len
        integer :: old_len

        old_len = len(target)
        new_len = old_len + len(chunk)

        allocate(character(len=new_len) :: combined)
        if (old_len > 0) combined(1:old_len) = target
        if (len(chunk) > 0) combined(old_len+1:new_len) = chunk
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

    integer function find_subsequence(arr, n, pat, start_idx) result(pos)
        integer(int64), intent(in) :: n
        character(len=1), intent(in) :: arr(n)
        character(len=*), intent(in) :: pat
        integer, intent(in) :: start_idx
        integer :: i, j, pat_len

        pat_len = len_trim(pat)
        pos = -1
        if (pat_len <= 0) return
        do i = max(1, start_idx), int(n) - pat_len + 1
            do j = 1, pat_len
                if (arr(i + j - 1) /= pat(j:j)) exit
                if (j == pat_len) then
                    pos = i
                    return
                end if
            end do
        end do
    end function find_subsequence

end module test_pdf_utils
