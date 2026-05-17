program test_svg_savefig
    !! End-to-end test for SVG backend via the high-level savefig("x.svg") path.
    !! Exercises the full pipeline: stateful API -> get_backend_from_filename ->
    !! rendering pipeline -> SVG backend adapter.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    character(len=*), parameter :: outfile = 'build/test/output/test_svg_savefig.svg'
    logical :: file_exists, dir_ok
    integer :: file_size, unit, ios
    character(len=1024) :: line
    logical :: has_svg_open, has_svg_close, has_path_or_line
    real(wp) :: x(10), y(10)
    integer :: i

    call create_directory_runtime('build/test/output', dir_ok)

    do i = 1, 10
        x(i) = real(i, wp)
        y(i) = real(i, wp) ** 2
    end do

    call figure()
    call plot(x, y, label='x^2')
    call title('SVG savefig smoke test')
    call xlabel('x')
    call ylabel('y')
    call savefig(outfile)

    ! Assert file was created
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, 'FAIL: SVG file not created: ', outfile
        stop 1
    end if
    if (file_size <= 0) then
        print *, 'FAIL: SVG file is empty'
        stop 1
    end if
    print *, 'PASS: SVG file created (', file_size, ' bytes)'

    ! Assert well-formed SVG content
    has_svg_open = .false.
    has_svg_close = .false.
    has_path_or_line = .false.

    open(newunit=unit, file=outfile, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open SVG file for reading'
        stop 1
    end if
    do while (ios == 0)
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, '<svg') > 0) has_svg_open = .true.
        if (index(line, '</svg>') > 0) has_svg_close = .true.
        if (index(line, '<path') > 0 .or. index(line, '<line') > 0) &
            has_path_or_line = .true.
    end do
    close(unit)

    if (.not. has_svg_open) then
        print *, 'FAIL: <svg opening tag not found'
        stop 1
    end if
    print *, 'PASS: <svg opening tag present'

    if (.not. has_svg_close) then
        print *, 'FAIL: </svg> closing tag not found'
        stop 1
    end if
    print *, 'PASS: </svg> closing tag present'

    if (.not. has_path_or_line) then
        print *, 'FAIL: no <path or <line element found (no plot data rendered)'
        stop 1
    end if
    print *, 'PASS: plot data element (<path or <line) present'

    print *, 'All SVG savefig end-to-end tests PASSED'

end program test_svg_savefig
