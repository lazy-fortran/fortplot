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
    call check_symlog_parameters()

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

    subroutine check_symlog_parameters()
        integer, parameter :: width = 1000, height = 500
        real(wp) :: rgb(width, height, 3), thresholds(2), fractions(2)
        real(wp) :: left, span, expected_x, expected_y, actual_x, actual_y
        integer :: panel, i, j, blue_count, red_count
        type(figure_t), pointer :: fig

        thresholds = [0.1_wp, 10.0_wp]
        ! For limits [0,100], x=y=10 maps to 3/4 and 1/2 respectively.
        fractions = [0.75_wp, 0.5_wp]
        call figure(figsize=[10.0_wp, 5.0_wp])
        do panel = 1, 2
            call subplot(1, 2, panel)
            call plot([10.0_wp, 10.0_wp], [0.0_wp, 100.0_wp], &
                color=[0.0_wp, 0.0_wp, 1.0_wp])
            call plot([0.0_wp, 100.0_wp], [10.0_wp, 10.0_wp], &
                color=[1.0_wp, 0.0_wp, 0.0_wp])
            call xlim(0.0_wp, 100.0_wp)
            call ylim(0.0_wp, 100.0_wp)
            call set_xscale('symlog', linthresh=thresholds(panel))
            call set_yscale('symlog', linthresh=thresholds(panel))
        end do
        call savefig(dir//'symlog_parameters.png')
        fig => get_global_figure()
        call fig%extract_rgb_data_for_animation(rgb)
        call savefig(dir//'symlog_parameters.pdf')
        span = 0.775_wp / 2.2_wp * width
        do panel = 1, 2
            left = 0.125_wp * width + (panel - 1) * 1.2_wp * span
            expected_x = left + fractions(panel) * span
            expected_y = (0.12_wp + (1.0_wp - fractions(panel)) * 0.77_wp) * height
            blue_count = 0
            red_count = 0
            actual_x = 0.0_wp
            actual_y = 0.0_wp
            do j = 1, height
                do i = max(1, int(left)), min(width, int(left + span))
                    if (rgb(i, j, 3) - rgb(i, j, 1) > 0.5_wp) then
                        blue_count = blue_count + 1
                        actual_x = actual_x + i
                    end if
                    if (rgb(i, j, 1) - rgb(i, j, 3) > 0.5_wp) then
                        red_count = red_count + 1
                        actual_y = actual_y + j
                    end if
                end do
            end do
            if (blue_count == 0 .or. red_count == 0) &
                error stop 'symlog reference lines missing'
            actual_x = actual_x / blue_count
            actual_y = actual_y / red_count
            if (abs(actual_x - expected_x) > 3.0_wp .or. &
                abs(actual_y - expected_y) > 3.0_wp) then
                print *, 'panel, expected/actual x/y:', panel, expected_x, &
                    actual_x, expected_y, actual_y
                error stop 'symlog threshold leaked between panels'
            end if
        end do
        print *, 'PASS: independent per-panel symlog x/y geometry'
    end subroutine check_symlog_parameters

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
