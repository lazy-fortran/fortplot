program test_pdf_marker_alpha_extgstate_1543
    use, intrinsic :: iso_fortran_env, only: wp => real64, int64
    use fortplot_figure_core, only: figure_t
    use test_pdf_utils, only: extract_pdf_stream_text, find_subsequence, &
                              pdf_stream_count_operator
    implicit none

    character(len=*), parameter :: out_pdf = 'build/test/output/'// &
                                   'pdf_marker_alpha_extgstate_1543.pdf'

    real(wp) :: x(3), y(3)
    type(figure_t) :: fig
    character(len=:), allocatable :: stream_text
    integer :: status
    integer :: gs_count
    integer :: unit, ios
    integer(int64) :: fsize
    character(len=1), allocatable :: data(:)
    integer :: pos_extgstate, pos_ca, pos_type
    logical :: exists

    x = [0.25_wp, 0.50_wp, 0.75_wp]
    y = [0.50_wp, 0.50_wp, 0.50_wp]

    call fig%initialize(width=220, height=180, backend='pdf')
    call fig%set_xlim(0.0_wp, 1.0_wp)
    call fig%set_ylim(0.0_wp, 1.0_wp)
    call fig%scatter(x, y, marker='o', facecolor=[0.0_wp, 0.0_wp, 0.0_wp], &
                     edgecolor=[1.0_wp, 0.0_wp, 0.0_wp], alpha=0.4_wp, &
                     linewidth=0.0_wp)
    call fig%savefig(out_pdf)

    inquire (file=out_pdf, exist=exists)
    if (.not. exists) then
        print *, 'FAIL: PDF not created: ', trim(out_pdf)
        stop 1
    end if

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF stream'
        stop 1
    end if

    gs_count = pdf_stream_count_operator(stream_text, 'gs')
    if (gs_count <= 0) then
        print *, 'FAIL: expected gs operator for ExtGState alpha'
        stop 1
    end if
    if (index(stream_text, '/GS') == 0) then
        print *, 'FAIL: expected GS resource name in PDF stream'
        stop 1
    end if

    open (newunit=unit, file=out_pdf, access='stream', form='unformatted', &
          status='old', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ', trim(out_pdf)
        stop 1
    end if

    inquire (unit=unit, size=fsize)
    if (fsize <= 0_int64) then
        close (unit)
        print *, 'FAIL: empty PDF file: ', trim(out_pdf)
        stop 1
    end if

    allocate (character(len=1) :: data(int(fsize)))
    read (unit, iostat=ios) data
    close (unit)
    if (ios /= 0) then
        print *, 'FAIL: cannot read PDF bytes: ', trim(out_pdf)
        stop 1
    end if

    pos_extgstate = find_subsequence(data, fsize, '/ExtGState', 1)
    pos_type = find_subsequence(data, fsize, '/Type /ExtGState', 1)
    pos_ca = find_subsequence(data, fsize, '/CA 0.000 /ca 0.400', 1)

    if (pos_extgstate < 0 .or. pos_type < 0) then
        print *, 'FAIL: expected ExtGState resource dictionary in PDF'
        stop 1
    end if
    if (pos_ca < 0) then
        print *, 'FAIL: expected independent stroke/fill alpha (CA/ca) in PDF'
        stop 1
    end if

    print *, 'PASS: PDF marker alpha uses ExtGState (gs operator)'
end program test_pdf_marker_alpha_extgstate_1543
