program test_pdf_tick_label_vector_font_2065
    !! Regression test for issue #2065: ordinary PDF tick/axis labels must render
    !! as embedded-Helvetica vector text (font-select + text-show operators), not
    !! as a bitmap/image fallback. A stream that routes labels through an image
    !! XObject (a `Do` invocation with no separate raster plot present) fails.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, plot, xlabel, ylabel, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=:), allocatable :: path, stream_text
    character(len=:), allocatable :: pdf_text
    real(wp) :: x(2), y(2)
    integer :: status
    logical :: dir_ok

    x = [0.0_wp, 10.0_wp]
    y = [0.0_wp, 0.5_wp]

    call create_directory_runtime('build/test/output', dir_ok)
    path = 'build/test/output/tick_label_vector_font_2065.pdf'
    call figure(figsize=[6.4_wp, 4.8_wp])
    call plot(x, y)
    call xlabel('X Axis')
    call ylabel('Y Axis')
    call savefig(path)

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

    call read_pdf_bytes(path, pdf_text, status)
    if (status /= 0) then
        print *, 'FAIL: could not read PDF bytes'
        stop 5
    end if

    if (index(pdf_text, '/FontDescriptor') == 0) then
        print *, 'FAIL: Helvetica font has no descriptor'
        stop 6
    end if

    if (index(pdf_text, '/FontFile2') == 0) then
        print *, 'FAIL: Helvetica font is not embedded as FontFile2'
        stop 7
    end if

    print *, 'PASS: PDF labels render as vector Helvetica text, no image fallback'

contains

    subroutine read_pdf_bytes(filename, bytes, stat)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: bytes
        integer, intent(out) :: stat
        integer :: unit, file_size

        stat = 0
        inquire (file=filename, size=file_size, iostat=stat)
        if (stat /= 0 .or. file_size <= 0) then
            stat = 1
            bytes = ''
            return
        end if

        allocate (character(len=file_size) :: bytes)
        open (newunit=unit, file=filename, access='stream', form='unformatted', &
              action='read', status='old', iostat=stat)
        if (stat /= 0) then
            bytes = ''
            return
        end if
        read (unit, iostat=stat) bytes
        close (unit)
    end subroutine read_pdf_bytes

end program test_pdf_tick_label_vector_font_2065
