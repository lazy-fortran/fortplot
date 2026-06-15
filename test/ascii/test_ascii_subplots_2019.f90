program test_ascii_subplots_2019
    !! Regression test for issue #2019: saving a subplot grid to the ASCII
    !! backend segfaulted. render_subplots took the non-tight margin path for
    !! ASCII (compute_tight_subplot_margins only supports raster/PDF) and there
    !! evaluated len_trim on an unallocated suptitle via a compound .and.,
    !! relying on short-circuit evaluation that Fortran does not guarantee.
    !! Assert the save completes and writes a non-empty file.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, plot, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(wp) :: x(5), y(5)
    integer :: i, unit, ios, line_count
    character(len=*), parameter :: outfile = &
        'build/test/output/ascii_subplots_2019.txt'
    character(len=512) :: line
    logical :: dir_ok, file_exists

    call create_directory_runtime('build/test/output', dir_ok)

    do i = 1, 5
        x(i) = real(i, wp)
        y(i) = real(i*i, wp)
    end do

    ! No suptitle: this is what exposed the unallocated-string crash.
    call figure()
    call subplot(1, 2, 1)
    call plot(x, y)
    call subplot(1, 2, 2)
    call plot(x, y)
    call savefig(outfile)

    inquire(file=outfile, exist=file_exists)
    if (.not. file_exists) then
        print *, 'FAIL: ASCII subplot save did not create output file'
        stop 1
    end if

    line_count = 0
    open(newunit=unit, file=outfile, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: could not open ASCII subplot output'
        stop 1
    end if
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) > 0) line_count = line_count + 1
    end do
    close(unit)

    if (line_count == 0) then
        print *, 'FAIL: ASCII subplot output is empty'
        stop 1
    end if

    print *, 'PASS: ASCII subplot save renders without crashing'

end program test_ascii_subplots_2019
