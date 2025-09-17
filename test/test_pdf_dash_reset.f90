program test_pdf_dash_reset
    !! Verify that PDF axes frame/ticks use solid dash pattern regardless of prior plot styles
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, plot, legend, savefig, title
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = 'build/test/output/pdf_dash_reset.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status
    logical :: exists

    call title('Dash Reset Test')
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp], label='line', linestyle='--')
    call legend()
    call savefig(out_pdf)

    ! Read PDF file as text to search for solid dash command '[] 0 d'
    inquire(file=out_pdf, exist=exists)
    if (.not. exists) then
        print *, 'FAIL: PDF not created: ', trim(out_pdf)
        stop 1
    end if

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF stream'
        stop 1
    end if

    if (index(stream_text, '[] 0 d') == 0) then
        print *, 'FAIL: PDF stream missing solid dash reset ([] 0 d)'
        stop 1
    end if

    print *, 'PASS: PDF axes/ticks dash reset present'
end program test_pdf_dash_reset
