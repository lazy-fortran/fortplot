program test_errorbar_ascii_rendering
    !! Regression test for Issue #1325: Errorbar plots were empty
    !! Ensures errorbar renders markers/segments in ASCII backend output
    use fortplot
    implicit none

    real(wp), dimension(5) :: x, y, yerr
    character(len=256) :: line
    logical :: found_marker
    integer :: unit, ios, i
    integer :: render_rows

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 2.5_wp, 3.0_wp, 3.5_wp, 4.0_wp]
    yerr = [0.2_wp, 0.3_wp, 0.25_wp, 0.35_wp, 0.15_wp]

    call figure()
    call errorbar(x, y, yerr=yerr, label='err', marker='o', &
                  color=[0.0_wp, 1.0_wp, 0.0_wp])
    call title('Errorbar ASCII Rendering')
    call savefig('test/output/test_errorbar_ascii.txt')

    ! Look for evidence of rendering within the plot area:
    ! Count rows that contain any density/marker characters. Require a minimum
    ! number of rows to avoid passing on axis-only output.
    found_marker = .false.
    render_rows = 0
    open(newunit=unit, file='test/output/test_errorbar_ascii.txt', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ASCII output for reading'
        stop 1
    end if

    do i = 1, 200
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        ! ASCII plot area lines start with '|'
        if (len_trim(line) > 0 .and. line(1:1) == '|') then
            if (index(line, ':') > 0 .or. index(line, '#') > 0 .or. &
                index(line, '*') > 0 .or. index(line, 'o') > 0 .or. &
                index(line, '%') > 0 .or. index(line, '+') > 0 .or. &
                index(line, '.') > 0) then
                render_rows = render_rows + 1
            end if
        end if
    end do
    close(unit)

    if (render_rows >= 8) then
        print *, 'PASS: errorbar content rendered in ASCII output'
    else
        print *, 'FAIL: insufficient plot content in ASCII output (rows=', render_rows, ')'
        stop 1
    end if

end program test_errorbar_ascii_rendering
