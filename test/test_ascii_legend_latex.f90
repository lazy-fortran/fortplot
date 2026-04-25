program test_ascii_legend_latex
    !! Test that ASCII backend strips LaTeX $...$ delimiters from legend entries
    !! Regression test for issue #1701.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(wp), dimension(50) :: x, y1, y2
    type(figure_t) :: fig
    integer :: i
    character(len=*), parameter :: outfile = 'build/test/output/ascii_legend_latex.txt'
    logical :: dir_ok, file_exists
    integer :: unit, ios
    character(len=512) :: line
    logical :: found_dollar, found_clean

    call create_directory_runtime('build/test/output', dir_ok)

    x = [(real(i, wp), i = 0, 49)] / 10.0_wp
    y1 = sin(x)
    y2 = cos(x)

    call fig%initialize(80, 24)
    call fig%add_plot(x, y1, label="$sin(x)$")
    call fig%add_plot(x, y2, label="$cos(x)$")
    call fig%legend()
    call fig%savefig(outfile)

    ! Read output and check for $ delimiters in legend lines
    found_dollar = .false.
    found_clean = .false.

    open(newunit = unit, file = outfile, status = 'old', action = 'read', iostat = ios)
    if (ios /= 0) then
        print *, "FAIL: cannot read ", outfile
        stop 1
    end if

    do
        read(unit, '(A)', iostat = ios) line
        if (ios /= 0) exit

        ! Check legend lines (they contain "--" prefix)
        if (index(line, '--') > 0) then
            ! Legend line should NOT contain $ delimiters
            if (index(line, '$') > 0) then
                found_dollar = .true.
            end if
            ! Legend line should contain cleaned text
            if (index(line, 'sin(x)') > 0 .or. index(line, 'cos(x)') > 0) then
                found_clean = .true.
            end if
        end if
    end do
    close(unit)

    if (found_dollar) then
        print *, "FAIL: ASCII legend contains raw $ delimiters"
        stop 1
    end if

    if (.not. found_clean) then
        print *, "FAIL: ASCII legend missing expected cleaned labels"
        stop 1
    end if

    print *, "PASS: ASCII legend strips LaTeX $ delimiters correctly"

end program test_ascii_legend_latex
