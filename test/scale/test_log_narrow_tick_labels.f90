program test_log_narrow_tick_labels
    !! Log axes spanning fewer than about two decades label their sub-decade
    !! ticks in plain decimal (0.5, 1, 2, 5) instead of m x 10^p mathtext;
    !! wide log axes keep 10^n decade labels.
    !! Oracles: hand-written expected label lists for the shared tick API
    !! (used by PNG, PDF and subplot renderers) and pdftotext of rendered PDFs.
    use fortplot
    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    integer :: failures
    character(len=:), allocatable :: dir

    failures = 0
    call check_labels(0.5_wp, 8.0_wp, [character(len=20) :: '0.5', '1', '2', '5'])
    call check_labels(0.2_wp, 1.0_wp, [character(len=20) :: '0.2', '0.5', '1'])
    call check_labels(1.0_wp, 50.0_wp, &
                      [character(len=20) :: '1', '2', '5', '10', '20', '50'])
    call check_labels(3.0_wp, 9.0_wp, &
                      [character(len=20) :: '3', '4', '5', '6', '7', '8', '9'])
    call check_labels(0.044_wp, 0.047_wp, &
                      [character(len=20) :: '0.044', '0.045', '0.046', '0.047'])
    call check_labels(1.0_wp, 1.0e4_wp, [character(len=20) :: '$10^{0}$', &
                      '$10^{1}$', '$10^{2}$', '$10^{3}$', '$10^{4}$'])
    call check_labels(2.0e4_wp, 9.0e4_wp, [character(len=20) :: &
                      '$2\times10^{4}$', '$5\times10^{4}$'], allow_extra=.true.)

    call ensure_test_output_dir('log_narrow_tick_labels', dir)
    call check_rendered(dir)
    if (failures > 0) then
        print *, 'FAIL:', failures, 'narrow log tick label checks failed'
        stop 1
    end if
    print *, 'PASS: narrow log axes use plain decimal tick labels'

contains

    subroutine check_labels(lo, hi, expected, allow_extra)
        real(wp), intent(in) :: lo, hi
        character(len=*), intent(in) :: expected(:)
        logical, intent(in), optional :: allow_extra
        real(wp) :: ticks(MAX_TICKS)
        character(len=50) :: got(MAX_TICKS)
        integer :: n, i, j
        logical :: subset, ok

        subset = .false.
        if (present(allow_extra)) subset = allow_extra
        call compute_scale_ticks('log', lo, hi, 1.0_wp, ticks, n)
        do i = 1, n
            got(i) = format_tick_label(ticks(i), 'log', data_min=lo, data_max=hi)
        end do
        if (subset) then
            ok = .true.
            do j = 1, size(expected)
                ok = ok .and. any(got(1:n) == expected(j))
            end do
        else
            ok = n == size(expected)
            if (ok) ok = all(got(1:n) == expected)
        end if
        if (.not. ok) then
            failures = failures + 1
            print *, 'FAIL: log range', lo, hi
            print '(a,*(1x,a))', '  expected:', (trim(expected(i)), i=1, size(expected))
            print '(a,*(1x,a))', '  got:     ', (trim(got(i)), i=1, n)
        end if
    end subroutine check_labels

    subroutine check_rendered(dir)
        character(len=*), intent(in) :: dir
        real(wp) :: x(4), y(4)
        character(len=:), allocatable :: txt

        x = [0.5_wp, 1.5_wp, 4.0_wp, 8.0_wp]
        y = [0.2_wp, 0.35_wp, 0.6_wp, 1.0_wp]

        call figure(figsize=[6.0_wp, 4.0_wp])
        call plot(x, y)
        call set_xscale('log')
        call set_yscale('log')
        call savefig(dir//'single.pdf')
        call savefig(dir//'single.png')
        txt = pdf_text(dir//'single.pdf')
        call check_text('single', txt)

        call figure(figsize=[8.0_wp, 4.0_wp])
        call subplot(1, 2, 1)
        call plot(x, y)
        call set_xscale('log')
        call subplot(1, 2, 2)
        call plot(x, y)
        call set_yscale('log')
        call savefig(dir//'subplots.pdf')
        call savefig(dir//'subplots.png')
        txt = pdf_text(dir//'subplots.pdf')
        call check_text('subplots', txt)
    end subroutine check_rendered

    subroutine check_text(name, txt)
        character(len=*), intent(in) :: name, txt
        if (len(txt) == 0) return
        if (index(txt, '0.5') == 0 .or. index(txt, '0.2') == 0) then
            failures = failures + 1
            print *, 'FAIL: ', name, ' PDF lacks plain 0.5/0.2 log tick labels'
            print *, 'pdftotext: ', txt
        end if
        if (index(txt, char(195)//char(151)) > 0) then
            failures = failures + 1
            print *, 'FAIL: ', name, ' PDF shows m x 10^p labels on a narrow log axis'
            print *, 'pdftotext: ', txt
        end if
    end subroutine check_text

    function pdf_text(pdf) result(txt)
        character(len=*), intent(in) :: pdf
        character(len=:), allocatable :: txt
        character(len=512) :: line
        integer :: stat, unit, ios

        txt = ''
        call execute_command_line('command -v pdftotext >/dev/null 2>&1', &
                                  exitstat=stat)
        if (stat /= 0) then
            print *, 'SKIPPED: pdftotext not available for PDF text checks'
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

end program test_log_narrow_tick_labels
