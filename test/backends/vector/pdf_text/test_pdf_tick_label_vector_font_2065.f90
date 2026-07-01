program test_pdf_tick_label_vector_font_2065
    !! Regression test for issue #2065: ordinary PDF tick/axis labels must render
    !! as embedded-Helvetica vector text (font-select + text-show operators), not
    !! as a bitmap/image fallback. A stream that routes labels through an image
    !! XObject (a `Do` invocation with no separate raster plot present) fails.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    type(pdf_context) :: ctx
    integer, parameter :: W = 640, H = 480
    character(len=:), allocatable :: path, stream_text
    character(len=:), allocatable :: xl, yl
    integer :: status
    logical :: dir_ok

    ctx = create_pdf_canvas(W, H)
    ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
    ctx%y_min = 0.0_wp; ctx%y_max = 0.5_wp

    xl = 'X Axis'
    yl = 'Y Axis'
    call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
         0.0_wp, 10.0_wp, 0.0_wp, 0.5_wp, &
         xlabel=xl, ylabel=yl, has_3d_plots=.false.)

    call create_directory_runtime('build/test/output', dir_ok)
    path = 'build/test/output/tick_label_vector_font_2065.pdf'
    call ctx%save(path)

    call extract_pdf_stream_text(path, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: could not read PDF stream'
        stop 1
    end if

    if (index(stream_text, ' Tf') == 0) then
        print *, 'FAIL: no font-select (Tf) operator in stream'
        stop 2
    end if

    if (index(stream_text, ') Tj') == 0) then
        print *, 'FAIL: no vector text-show (Tj) operator in stream'
        stop 3
    end if

    if (index(stream_text, ' Do') > 0) then
        print *, 'FAIL: text-only page emits an image XObject (Do) fallback'
        stop 4
    end if

    print *, 'PASS: PDF labels render as vector Helvetica text, no image fallback'

end program test_pdf_tick_label_vector_font_2065
