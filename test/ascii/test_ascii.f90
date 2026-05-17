program test_ascii
    !! Test program for ASCII terminal plotting functionality
    !!
    !! Creates sine and cosine plots, saves to a file, and verifies the
    !! file content contains recognisable ASCII plot characters.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    use fortplot_system_runtime, only: is_windows, create_directory_runtime
    implicit none

    real(wp), dimension(100) :: x, sx, cx
    type(figure_t) :: ascii_fig
    integer :: i
    character(len=*), parameter :: outfile = 'build/test/output/ascii_smoke.txt'
    logical :: dir_ok, file_exists
    integer :: file_size, unit, ios, nlines
    character(len=512) :: line
    logical :: has_plot_chars

    call create_directory_runtime('build/test/output', dir_ok)

    x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
    sx = sin(x)
    cx = cos(x)

    call ascii_fig%initialize(80, 24)
    call ascii_fig%set_title("Sine and Cosine Functions (ASCII)")
    call ascii_fig%add_plot(x, sx, label="sin(x)")
    call ascii_fig%add_plot(x, cx, label="cos(x)")

    call ascii_fig%savefig(outfile)

    ! Verify the file was written
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: ASCII savefig did not create ", outfile
        stop 1
    end if
    if (file_size <= 0) then
        print *, "FAIL: ASCII output file is empty"
        stop 1
    end if
    print *, "PASS: ASCII file created (", file_size, " bytes)"

    ! Verify the file has plot content
    nlines = 0
    has_plot_chars = .false.
    open(newunit=unit, file=outfile, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "FAIL: cannot read ", outfile
        stop 1
    end if
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        nlines = nlines + 1
        if (index(line, '|') > 0 .or. index(line, '-') > 0 .or. &
            index(line, '*') > 0 .or. index(line, '+') > 0) then
            has_plot_chars = .true.
        end if
    end do
    close(unit)

    if (nlines < 5) then
        print *, "FAIL: ASCII output too short (", nlines, " lines)"
        stop 1
    end if
    if (.not. has_plot_chars) then
        print *, "FAIL: ASCII output has no plot characters"
        stop 1
    end if

    print *, "PASS: ASCII content valid (", nlines, " lines)"
    print *, "ascii terminal plot created successfully!"

end program test_ascii
