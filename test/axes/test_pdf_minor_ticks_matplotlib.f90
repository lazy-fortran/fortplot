program test_pdf_minor_ticks_matplotlib
    !! Matplotlib 3.11 oracle, with both limits fixed to [0,1]:
    !! ax.minorticks_on() gives .05,.10,.15,...,.85,.90,.95 (15 per axis).
    !! For log limits [1,100], LogLocator gives 2..9 and 20..90 (16 per axis).
    !! Count actual outward 2pt segments in the final saved figure PDF.
    use fortplot, only: figure_t, wp
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    use fortplot_test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    type(figure_t) :: fig
    character(len=:), allocatable :: output_dir, path, stream
    integer :: status

    call ensure_test_output_dir('pdf_minor_ticks_matplotlib', output_dir)
    call fig%initialize()
    call fig%plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    call fig%set_xlim(0.0_wp, 1.0_wp)
    call fig%set_ylim(0.0_wp, 1.0_wp)
    path = output_dir//'linear_disabled.pdf'
    call fig%savefig(path)
    call extract_pdf_stream_text(path, stream, status)
    if (status /= 0) stop 1
    call check_counts(0)

    call fig%minorticks_on()
    path = output_dir//'linear_enabled.pdf'
    call fig%savefig(path)
    call extract_pdf_stream_text(path, stream, status)
    if (status /= 0) stop 1
    call check_counts(15)

    ! Reinitialization must discard the preceding PDF's 15 minor ticks.
    call fig%initialize()
    call fig%plot([1.0_wp, 100.0_wp], [1.0_wp, 100.0_wp])
    call fig%set_xscale('log')
    call fig%set_yscale('log')
    call fig%set_xlim(1.0_wp, 100.0_wp)
    call fig%set_ylim(1.0_wp, 100.0_wp)
    path = output_dir//'log_default.pdf'
    call fig%savefig(path)
    call extract_pdf_stream_text(path, stream, status)
    if (status /= 0) stop 1
    call check_counts(16)
    print *, 'PASS: PDF default and enabled minor ticks match matplotlib'

contains

    subroutine check_counts(expected)
        integer, intent(in) :: expected
        integer :: first, last, ios, nx, ny
        real(wp) :: x1, y1, x2, y2
        character(len=:), allocatable :: line
        character(len=8) :: move_op, line_op, stroke_op

        nx = 0
        ny = 0
        first = 1
        do while (first <= len(stream))
            last = index(stream(first:), new_line('a'))
            if (last == 0) exit
            line = stream(first:first + last - 2)
            first = first + last
            if (index(line, ' l S') == 0) cycle
            read (line, *, iostat=ios) x1, y1, move_op, x2, y2, line_op, stroke_op
            if (ios /= 0) cycle
            if (move_op /= 'm' .or. line_op /= 'l' .or. stroke_op /= 'S') cycle
            if (abs(x2 - x1) < 1.0e-6_wp .and. &
                abs(y2 - y1 + 2.0_wp) < 1.0e-6_wp) nx = nx + 1
            if (abs(y2 - y1) < 1.0e-6_wp .and. &
                abs(x2 - x1 + 2.0_wp) < 1.0e-6_wp) ny = ny + 1
        end do
        if (nx /= expected .or. ny /= expected) then
            print *, 'FAIL: ', path, ': expected', expected, &
                'minor ticks per axis; got', nx, ny
            stop 2
        end if
    end subroutine check_counts

end program test_pdf_minor_ticks_matplotlib
