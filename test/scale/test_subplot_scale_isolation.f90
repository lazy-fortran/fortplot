program test_subplot_scale_isolation
    !! set_yscale('log') on one subplot must not change the other subplots.
    !! Oracle: PDF tick-label text. Panel 1 spans y in [1, 1000] on a log
    !! axis (decade labels, no linear 200/400/600/800 ladder); panel 2 spans
    !! y in [0.2, 1] on its default linear axis (0.4/0.6/0.8 labels).
    use fortplot
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    real(wp) :: x(4), y1(4), y2(4)
    character(len=:), allocatable :: dir, txt
    integer :: failures

    call ensure_test_output_dir('subplot_scale_isolation', dir)
    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
    y1 = [1.0_wp, 10.0_wp, 100.0_wp, 1000.0_wp]
    y2 = [0.2_wp, 0.5_wp, 0.8_wp, 1.0_wp]

    call figure(figsize=[8.0_wp, 4.0_wp])
    call subplot(1, 2, 1)
    call plot(x, y1)
    call set_yscale('log')
    call subplot(1, 2, 2)
    call plot(x, y2)
    call savefig(dir//'scales.pdf')
    call savefig(dir//'scales.png')

    txt = pdf_text(dir//'scales.pdf')
    if (len(txt) == 0) stop 0
    failures = 0
    if (index(txt, '0.4') == 0 .or. index(txt, '0.6') == 0) then
        print *, 'FAIL: linear panel lost its linear ticks (log scale leaked)'
        failures = failures + 1
    end if
    if (index(txt, '400') > 0 .or. index(txt, '600') > 0) then
        print *, 'FAIL: log panel shows linear ticks'
        failures = failures + 1
    end if
    if (failures > 0) then
        print *, 'pdftotext: ', txt
        stop 1
    end if
    print *, 'PASS: axis scales are per subplot'

contains

    function pdf_text(pdf) result(txt)
        character(len=*), intent(in) :: pdf
        character(len=:), allocatable :: txt
        character(len=512) :: line
        integer :: stat, unit, ios

        txt = ''
        call execute_command_line('command -v pdftotext >/dev/null 2>&1', &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, 'SKIPPED: pdftotext not available'
            return
        end if
        call execute_command_line('pdftotext "'//pdf//'" "'//pdf//'.txt"', &
                                  exitstat=stat)
        if (stat /= 0) error stop 'pdftotext failed'
        open (newunit=unit, file=pdf//'.txt', status='old', action='read')
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            txt = txt//trim(line)//' '
        end do
        close (unit)
    end function pdf_text

end program test_subplot_scale_isolation
