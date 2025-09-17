program test_pdf_pcolormesh_inline_image
    !! Verify that pcolormesh PDF contains an inline image (BI ... ID ... EI)
    !! Robust to Flate-compressed content streams by inflating the PDF stream
    !! text before searching for inline image markers.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: fn = 'test/output/test_pdf_inline_image.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status
    logical :: has_bi, has_id, has_ei
    logical :: has_inline_tokens, has_xobject

    call figure()
    call pcolormesh([0.0_wp, 0.5_wp, 1.0_wp], &
        [0.0_wp, 0.5_wp, 1.0_wp], &
        reshape([0.1_wp, 0.2_wp, 0.3_wp, 0.4_wp, 0.5_wp, 0.6_wp, 0.7_wp, 0.8_wp, &
                 0.9_wp], [3, 3]))
    call savefig(fn)

    call extract_pdf_stream_text(fn, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF stream for inline image detection'
        stop 1
    end if

    has_bi = index(stream_text, ' BI ') > 0 .or. index(stream_text, 'BI /W') > 0
    has_id = index(stream_text, ' ID') > 0
    has_ei = index(stream_text, 'EI') > 0
    has_inline_tokens = has_bi .and. has_id .and. has_ei

    has_xobject = index(stream_text, '/Im') > 0 .and. index(stream_text, ' Do') > 0

    if (.not. (has_inline_tokens .or. has_xobject)) then
        print *, 'FAIL: PDF stream missing inline image (BI/ID/EI) or XObject ' // &
            'invocation'
        stop 2
    end if

    print *, 'PASS: pcolormesh PDF contains image data'
end program test_pdf_pcolormesh_inline_image
