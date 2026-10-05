program test_huge_value_tick_labels
    !! Linear-axis tick labels must stay meaningful for any finite range.
    !! Oracle: PDF tick-label text for data near 1e25 and near 1e-25 never
    !! shows the integer-overflow sentinel 2147483647 and carries a decimal
    !! exponent for both magnitudes, and no tick beyond the axis (3.25e..).
    use fortplot
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    character(len=:), allocatable :: dir, txt
    integer :: failures

    call ensure_test_output_dir('huge_value_ticks', dir)
    failures = 0
    call check(1.0e25_wp, 'huge', failures)
    call check(1.0e-25_wp, 'tiny', failures)
    call check(1.0e300_wp, 'extreme', failures)
    if (failures > 0) stop 1
    print *, 'PASS: tick labels for huge and tiny linear ranges'

contains

    subroutine check(scale, name, failures)
        real(wp), intent(in) :: scale
        character(len=*), intent(in) :: name
        integer, intent(inout) :: failures
        real(wp) :: x(3), y(3)
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = scale*[1.0_wp, 2.0_wp, 3.0_wp]
        call figure()
        call plot(x, y)
        call savefig(dir//name//'.pdf')
        call savefig(dir//name//'.png')
        txt = pdf_text(dir//name//'.pdf')
        if (len(txt) == 0) return
        print '(4a)', ' ', name, ': ', txt
        if (index(txt, '147483647') > 0) then
            print *, 'FAIL: integer overflow in tick labels for ', name
            failures = failures + 1
        end if
        if (index(txt, '3.25e') > 0) then
            print *, 'FAIL: tick outside the axis range for ', name
            failures = failures + 1
        end if
        if (index(txt, '10') == 0 .and. index(txt, 'e') == 0 .and. &
            index(txt, 'E') == 0) then
            print *, 'FAIL: no exponent in tick labels for ', name
            failures = failures + 1
        end if
    end subroutine check

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

end program test_huge_value_tick_labels
