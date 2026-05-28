program test_pie_legend_inside
    !! Pie chart legends must render fully inside the plot frame.
    !!
    !! Regression for issue #1960: the pie legend was force-placed at 'east'
    !! (x_max + margin), so the box body and labels were clipped at the image
    !! edge. The override now uses matplotlib's 'best' (an inside corner), so
    !! the box must satisfy box%x + box%width <= data_width.
    !!
    !! Also keeps the issue #1680 check: legend entries and pie slice labels
    !! still both appear in the PDF stream without merging.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, pie, title, legend, savefig
    use fortplot_legend, only: LEGEND_UPPER_RIGHT
    use fortplot_legend_layout, only: legend_box_t, calculate_legend_box
    use fortplot_test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = 'build/test/output/test_pie_legend_inside.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status
    logical :: found_north, found_online, found_merged
    real(wp) :: values(5)
    character(len=6) :: labels(5)
    type(legend_box_t) :: box
    real(wp), parameter :: data_width = 1.0_wp, data_height = 1.0_wp

    values = [30.0_wp, 22.0_wp, 18.0_wp, 15.0_wp, 15.0_wp]
    labels(1) = 'North'
    labels(2) = 'East'
    labels(3) = 'South'
    labels(4) = 'West'
    labels(5) = 'Online'

    ! Containment: the inside corner that 'best' resolves to for a pie (no
    ! competing artists, so upper right wins) must fit within the frame.
    box = calculate_legend_box(labels, data_width, data_height, size(labels), &
                               LEGEND_UPPER_RIGHT, 432, 432)
    if (box%x < 0.0_wp) then
        print *, 'FAIL: legend box left edge left of frame: box%x =', box%x
        stop 1
    end if
    if (box%x + box%width > data_width) then
        print *, 'FAIL: legend box right edge past frame: box%x+width =', &
            box%x + box%width, ' > ', data_width
        stop 1
    end if

    call figure(figsize=[6.0_wp, 6.0_wp])
    call pie(values, labels=labels)
    call title('Regional revenue share')
    call legend()
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

    print *, 'PASS: pie legend renders inside the plot frame without merging'
end program test_pie_legend_inside
