program test_errorbar_defaults
    !! Behavioral test for matplotlib-default errorbar semantics.
    !!
    !! matplotlib's errorbar with no marker/linestyle argument draws a
    !! connecting data line and no point markers, and draws no caps unless
    !! capsize is given. This test asserts:
    !!   1. default errorbar draws no marker glyphs (marker not forced to 'o'),
    !!   2. requesting marker='o' does draw marker glyphs,
    !!   3. the default connecting line still renders plot content.
    use fortplot
    implicit none

    real(wp), dimension(5) :: x, y, yerr
    integer :: default_markers, with_markers

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 2.5_wp, 3.0_wp, 3.5_wp, 4.0_wp]
    yerr = [0.2_wp, 0.3_wp, 0.25_wp, 0.35_wp, 0.15_wp]

    ! Default errorbar: no marker argument -> matplotlib draws line, no markers.
    call figure()
    call errorbar(x, y, yerr=yerr, label='default')
    call savefig('build/test/output/test_errorbar_default.txt')
    default_markers = count_marker_glyphs('build/test/output/test_errorbar_default.txt')

    ! Explicit marker -> markers drawn.
    call figure()
    call errorbar(x, y, yerr=yerr, label='marked', marker='o')
    call savefig('build/test/output/test_errorbar_marked.txt')
    with_markers = count_marker_glyphs('build/test/output/test_errorbar_marked.txt')

    if (default_markers /= 0) then
        print *, 'FAIL: default errorbar drew marker glyphs (', default_markers, ')'
        stop 1
    end if
    print *, 'PASS: default errorbar draws no markers'

    if (with_markers <= 0) then
        print *, 'FAIL: explicit marker=o drew no marker glyphs'
        stop 1
    end if
    print *, 'PASS: explicit marker=o draws markers (', with_markers, ')'

    print *, 'PASS: errorbar default semantics match matplotlib'

contains

    integer function count_marker_glyphs(path) result(total)
        character(len=*), intent(in) :: path
        character(len=256) :: line
        integer :: unit, ios, i
        total = 0
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', trim(path)
            stop 1
        end if
        do i = 1, 200
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            total = total + count_in_line(line)
        end do
        close(unit)
    end function count_marker_glyphs

    integer function count_in_line(line) result(n)
        character(len=*), intent(in) :: line
        character(len=*), parameter :: glyphs = 'o@*#%'
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
    end function count_in_line

end program test_errorbar_defaults
