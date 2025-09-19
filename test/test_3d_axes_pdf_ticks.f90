program test_3d_axes_pdf_ticks
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_system_runtime, only: create_directory_runtime
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    type(pdf_context) :: ctx
    integer, parameter :: W = 400, H = 300
    character(len=:), allocatable :: out_pdf
    character(len=:), allocatable :: stream
    integer :: status
    logical :: ok

    ! Create PDF canvas and set coordinate system
    ctx = create_pdf_canvas(W, H)
    ctx%x_min = 0.0_wp; ctx%x_max = 1.0_wp
    ctx%y_min = 0.0_wp; ctx%y_max = 1.0_wp

    ! Draw 3D axes (tick labels should be rendered via backend text)
    call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
         0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, &
         has_3d_plots=.true., z_min=0.0_wp, z_max=1.0_wp)

    ! Save to test output
    call create_directory_runtime('test/output', ok)
    out_pdf = 'test/output/test_3d_axes_ticks.pdf'
    call ctx%save(out_pdf)

    ! Extract PDF content stream and assert some tick labels exist
    call extract_pdf_stream_text(out_pdf, stream, status)
    if (status /= 0) then
        print *, 'FAIL: could not read PDF stream'
        stop 1
    end if

    if (index(stream, '0.0') <= 0 .and. index(stream, '(0.0)') <= 0) then
        print *, 'FAIL: expected 3D tick label 0.0 in PDF stream'
        stop 2
    end if

    if (index(stream, '1.0') <= 0 .and. index(stream, '(1.0)') <= 0) then
        print *, 'FAIL: expected 3D tick label 1.0 in PDF stream'
        stop 3
    end if

    print *, 'PASS: 3D axes tick labels found in PDF stream'
end program test_3d_axes_pdf_ticks
