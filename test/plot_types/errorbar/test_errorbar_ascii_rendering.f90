program test_errorbar_ascii_rendering
    !! Regression test for Issue #1325: Errorbar plots were empty
    !! Ensures errorbar renders markers/segments in ASCII backend output
    use fortplot
    implicit none

    real(wp), dimension(5) :: x, y, yerr
    character(len=256) :: line
    integer :: unit, ios, i, c
    integer :: marker_glyphs

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 2.5_wp, 3.0_wp, 3.5_wp, 4.0_wp]
    yerr = [0.2_wp, 0.3_wp, 0.25_wp, 0.35_wp, 0.15_wp]

    call figure()
    call errorbar(x, y, yerr=yerr, label='err', marker='o', &
                  color=[0.0_wp, 1.0_wp, 0.0_wp])
    call title('Errorbar ASCII Rendering')
    call savefig('build/test/output/test_errorbar_ascii.txt')

    ! Issue #1325 was an empty errorbar plot. Count actual marker/cap glyphs
    ! drawn in the plot area. Tick-label characters ('.', digits) and axis rules
    ! ('-') are excluded so the check measures rendered errorbar content, not the
    ! number of tick labels (which legitimately varies with the tick step).
    ! Five data points must yield at least five marker glyphs.
    marker_glyphs = 0
    open(newunit=unit, file='build/test/output/test_errorbar_ascii.txt', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ASCII output for reading'
        stop 1
    end if

    do i = 1, 200
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        marker_glyphs = marker_glyphs + count_glyphs(line)
    end do
    close(unit)

    if (marker_glyphs >= 5) then
        print *, 'PASS: errorbar content rendered in ASCII output'
    else
        print *, 'FAIL: insufficient errorbar content in ASCII output (glyphs=', &
            marker_glyphs, ')'
        stop 1
    end if

contains

    integer function count_glyphs(line) result(n)
        character(len=*), intent(in) :: line
        character(len=*), parameter :: glyphs = 'o@*#:%+'
        integer :: j, k
        n = 0
        do j = 1, len_trim(line)
            do k = 1, len(glyphs)
                if (line(j:j) == glyphs(k:k)) then
                    n = n + 1
                    exit
                end if
            end do
        end do
    end function count_glyphs

end program test_errorbar_ascii_rendering
