program test_pdf_debug
    use, intrinsic :: iso_fortran_env, only: dp => real64, int8, int64
    use fortplot
    use fortplot_errors, only: SUCCESS
    use fortplot_zlib_core, only: zlib_decompress
    implicit none
    
    character(len=:), allocatable :: pdf_path
    integer :: save_status
    integer :: ios, unit
    integer(int64) :: fsize
    character(len=1), allocatable :: data(:)
    character(len=:), allocatable :: stream
    integer :: stream_start, stream_end
    integer :: content_begin, content_len
    integer :: j, status_decomp
    integer(int8), allocatable :: compressed(:)
    integer(int8), allocatable :: decompressed_raw(:)
    character(len=:), allocatable :: chunk_text
    integer :: pos
    
    ! Create a simple line plot
    call figure()
    call plot([0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], &
              [0.0_dp, 0.5_dp, 0.0_dp, -0.5_dp, 0.0_dp], 'b-')
    call xlim(0.0_dp, 4.0_dp)
    call ylim(-1.0_dp, 1.0_dp)
    
    pdf_path = 'build/test/output/coord_map_985_debug.pdf'
    call savefig(pdf_path)
    
    ! Read the PDF file
    open (newunit=unit, file=pdf_path, access='stream', form='unformatted', &
          status='old', iostat=ios)
    inquire (unit=unit, size=fsize)
    allocate (character(len=1) :: data(int(fsize)))
    read (unit, iostat=ios) data
    close (unit)
    
    print *, 'File size:', fsize
    
    ! Find stream/endstream
    pos = 1
    stream_start = find_sub(data, fsize, 'stream', pos)
    print *, 'stream at:', stream_start
    
    if (stream_start > 0) then
        stream_end = find_sub(data, fsize, 'endstream', stream_start + 6)
        print *, 'endstream at:', stream_end
        
        content_begin = stream_start + 6
        if (data(content_begin) == achar(13)) content_begin = content_begin + 1
        if (data(content_begin) == achar(10)) content_begin = content_begin + 1
        
        content_len = stream_end - content_begin
        print *, 'content_begin:', content_begin
        print *, 'content_len:', content_len
        
        ! Print first 20 bytes of content
        print *, 'First 20 bytes of content:'
        do j = content_begin, min(stream_end - 1, content_begin + 19)
            write(*, '(I3, 2X, Z2.2, 2X, A1)') j - stream_start, iand(iachar(data(j:j)), 255), data(j:j)
        end do
        
        ! Try to decompress
        allocate (compressed(content_len))
        do j = 1, content_len
            compressed(j) = int(iand(iachar(data(content_begin + j - 1)), 255), int8)
        end do
        
        decompressed_raw = zlib_decompress(compressed, content_len, status_decomp, .false.)
        print *, 'decompress status:', status_decomp
        print *, 'decompressed size:', size(decompressed_raw)
        
        if (size(decompressed_raw) > 0) then
            allocate (character(len=size(decompressed_raw)) :: chunk_text)
            call bytes_to_string(decompressed_raw, chunk_text)
            
            ! Print first 500 chars
            print *, 'Decompressed (first 500 chars):'
            print '(A)', chunk_text(1:min(500, len(chunk_text)))
            
            ! Look for ' re S'
            if (index(chunk_text, ' re S') > 0) then
                print *, 'FOUND: " re S" in stream'
            else
                print *, 'NOT FOUND: " re S" in stream'
            end if
        else
            print *, 'Decompression failed or returned empty'
        end if
    end if
    
contains

    integer function find_sub(arr, n, pat, start_idx) result(pos)
        integer(int64), intent(in) :: n
        character(len=1), intent(in) :: arr(n)
        character(len=*), intent(in) :: pat
        integer, intent(in) :: start_idx
        integer :: i, j, pat_len
        
        pat_len = len(pat)
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
    end function find_sub
    
    subroutine bytes_to_string(bytes, text)
        integer(int8), intent(in) :: bytes(:)
        character(len=*), intent(out) :: text
        integer :: i
        
        do i = 1, size(bytes)
            text(i:i) = achar(iand(int(bytes(i)), 255))
        end do
    end subroutine bytes_to_string

end program test_pdf_debug
