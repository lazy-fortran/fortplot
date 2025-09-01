program test_pdf_io_error_handling
    !! Verify library does not abort on PDF open failure (issue #900)
    !! Also verify optional success flag is returned as .false. on failure
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_pdf_io,   only: write_pdf_file
    implicit none

    type(pdf_context_core) :: ctx
    character(len=*), parameter :: bad_path = 'test/output/does-not-exist/file.pdf'
    logical :: ok

    ! Initialize minimal context
    ctx%width  = 100.0
    ctx%height = 100.0
    ctx%stream_data = 'BT /F5 12 Tf 10 10 Td (Hello) Tj ET'

    ! Call without success argument; should NOT terminate the program
    call write_pdf_file(ctx, bad_path)

    ! Call with success argument; should return .false. on failure
    call write_pdf_file(ctx, bad_path, success=ok)
    if (ok) then
        print *, 'FAIL: expected success=.false. on open failure'
        stop 1
    end if

    print *, 'PASS: write_pdf_file did not abort and returned success=.false. on open failure'
end program test_pdf_io_error_handling
