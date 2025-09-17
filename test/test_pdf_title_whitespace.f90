program test_pdf_title_whitespace
    !! Verifies PDF backend preserves spaces in titles (no whitespace loss)
    use fortplot
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = 'test/output/test_pdf_title_whitespace.pdf'
    character(len=:), allocatable :: stream_text
    integer :: space_tj_count, status, pos, idx

    call figure()
    call title('HELLO WORLD TEST')
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    call savefig(out_pdf)

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF stream'
        stop 1
    end if

    ! Ensure space glyphs are emitted as their own Tj segments: "( ) Tj"
    space_tj_count = 0
    pos = 1
    do
        idx = index(stream_text(pos:), '( ) Tj')
        if (idx == 0) exit
        space_tj_count = space_tj_count + 1
        pos = pos + idx
        if (pos > len(stream_text)) exit
    end do
    if (space_tj_count < 2) then
        print *, 'FAIL: expected at least 2 space Tj segments, found=', space_tj_count
        stop 1
    end if

    print *, 'PASS: PDF title preserves spaces'
end program test_pdf_title_whitespace
