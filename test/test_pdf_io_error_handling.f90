program test_pdf_io_error_handling
    !! Verify library does not abort on PDF open failure (issue #900)
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_pdf_io,   only: write_pdf_file
    implicit none

    type(pdf_context_core) :: ctx
    character(len=*), parameter :: bad_path = 'test/output/does-not-exist/file.pdf'

    ! Initialize minimal context
    ctx%width  = 100.0
    ctx%height = 100.0
    ctx%stream_data = 'BT /F5 12 Tf 10 10 Td (Hello) Tj ET'

    ! Call without success argument; should NOT terminate the program
    call write_pdf_file(ctx, bad_path)

    print *, 'PASS: write_pdf_file did not abort on open failure'
end program test_pdf_io_error_handling

