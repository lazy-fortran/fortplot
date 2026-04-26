program test_pie_legend_east_pdf
    !! Verify pie chart legend at 'east' position renders outside plot area in PDF.
    !! Regression test for issue #1680: legend box overlapped pie slice labels
    !! causing merged text (e.g. "OnlineNorth" instead of separate entries).
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, pie, title, legend, savefig
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = 'build/test/output/test_pie_legend_east.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status
    logical :: found_north, found_online, found_merged
    real(wp) :: values(5)
    character(len=6) :: labels(5)

    values = [30.0_wp, 22.0_wp, 18.0_wp, 15.0_wp, 15.0_wp]
    labels(1) = 'North'
    labels(2) = 'East'
    labels(3) = 'South'
    labels(4) = 'West'
    labels(5) = 'Online'

    call figure(figsize=[6.0_wp, 6.0_wp])
    call pie(values, labels=labels)
    call title('Regional revenue share')
    call legend('east')
    call savefig(out_pdf)

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF stream'
        stop 1
    end if

    found_north = index(stream_text, '(North)') > 0
    found_online = index(stream_text, '(Online)') > 0
    found_merged = index(stream_text, '(OnlineNorth)') > 0 .or. &
                   index(stream_text, '(NorthOnline)') > 0

    if (.not. found_north) then
        print *, 'FAIL: legend entry "North" not found in PDF stream'
        stop 1
    end if
    if (.not. found_online) then
        print *, 'FAIL: pie slice label "Online" not found in PDF stream'
        stop 1
    end if
    if (found_merged) then
        print *, 'FAIL: legend and slice label merged in PDF (issue #1680)'
        stop 1
    end if

    print *, 'PASS: pie legend at east position does not merge with slice labels in PDF'
end program test_pie_legend_east_pdf
