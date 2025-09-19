program test_errorbar_ascii_rendering
    !! Regression test for Issue #1325: Errorbar plots were empty
    !! Ensures errorbar renders markers/segments in ASCII backend output
    use fortplot
    implicit none

    real(wp), dimension(5) :: x, y, yerr
    character(len=256) :: line
    logical :: found_marker
    integer :: unit, ios, i

    x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    y = [2.0_wp, 2.5_wp, 3.0_wp, 3.5_wp, 4.0_wp]
    yerr = [0.2_wp, 0.3_wp, 0.25_wp, 0.35_wp, 0.15_wp]

    call figure()
    call errorbar(x, y, yerr=yerr, label='err', marker='o')
    call title('Errorbar ASCII Rendering')
    call savefig('test/output/test_errorbar_ascii.txt')

    ! Look for marker character 'o' which indicates points were rendered
    found_marker = .false.
    open(newunit=unit, file='test/output/test_errorbar_ascii.txt', status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ASCII output for reading'
        stop 1
    end if

    do i = 1, 200
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, 'o') > 0) then
            found_marker = .true.
            exit
        end if
    end do
    close(unit)

    if (.not. found_marker) then
        print *, 'FAIL: errorbar markers not found in ASCII output'
        stop 1
    else
        print *, 'PASS: errorbar markers rendered in ASCII output'
    end if

end program test_errorbar_ascii_rendering

