program test_pdf_y_tick_label_clearance_2065
    !! Regression test for issue #2065: PDF y tick labels must keep a positive
    !! horizontal clearance from the y-axis spine (they visually overlapped the
    !! axis line before). A y tick label whose right edge reaches the spine (or
    !! sits closer than MIN_PAD_PT) fails this test.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: REFERENCE_DPI
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_margins, only: plot_margins_t, plot_area_t
    use fortplot_pdf_coordinate, only: calculate_pdf_plot_area
    use fortplot_pdf_text, only: estimate_pdf_text_width
    use fortplot_pdf_core, only: PDF_TICK_LABEL_SIZE
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    real(wp), parameter :: MIN_PAD_PT = 3.0_wp
    type(pdf_context) :: ctx
    integer, parameter :: W = 640, H = 480
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    character(len=:), allocatable :: path, stream_text
    character(len=:), allocatable :: xl, yl
    integer :: status
    logical :: dir_ok
    real(wp) :: plot_left
    integer :: n_y_labels
    real(wp) :: min_pad_seen

    margins = plot_margins_t()
    block
        real(wp) :: width_pts, height_pts
        integer :: wpt_i, hpt_i
        width_pts = real(W, wp)*72.0_wp/REFERENCE_DPI
        height_pts = real(H, wp)*72.0_wp/REFERENCE_DPI
        wpt_i = max(1, nint(width_pts))
        hpt_i = max(1, nint(height_pts))
        call calculate_pdf_plot_area(wpt_i, hpt_i, margins, plot_area)
    end block
    plot_left = real(plot_area%left, wp)

    ctx = create_pdf_canvas(W, H)
    ctx%x_min = 1.0_wp; ctx%x_max = 100.0_wp
    ctx%y_min = 0.0_wp; ctx%y_max = 0.5_wp

    xl = 'X Axis'
    yl = 'Y Axis'
    call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
         1.0_wp, 100.0_wp, 0.0_wp, 0.5_wp, &
         xlabel=xl, ylabel=yl, has_3d_plots=.false.)

    call create_directory_runtime('build/test/output', dir_ok)
    path = 'build/test/output/y_tick_label_clearance_2065.pdf'
    call ctx%save(path)

    call extract_pdf_stream_text(path, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: could not read PDF stream'
        stop 1
    end if

    call check_y_tick_label_clearance(stream_text, plot_left, MIN_PAD_PT, &
                                      n_y_labels, min_pad_seen)

    if (n_y_labels < 2) then
        print *, 'FAIL: fewer than two y tick labels detected (', n_y_labels, ')'
        stop 2
    end if

    if (min_pad_seen < MIN_PAD_PT) then
        print *, 'FAIL: y tick label clearance', min_pad_seen, &
            'pt below required', MIN_PAD_PT, 'pt'
        stop 3
    end if

    print *, 'PASS: y tick labels clear axis spine (', n_y_labels, &
        'labels, min pad', min_pad_seen, 'pt)'

contains

    subroutine check_y_tick_label_clearance(stream, spine_x, min_pad, count_out, &
                                            min_pad_out)
        !! Scan Tm/Tj text groups. Groups whose right edge falls left of the
        !! spine are y tick labels; record the smallest clearance found.
        character(len=*), intent(in) :: stream
        real(wp), intent(in) :: spine_x, min_pad
        integer, intent(out) :: count_out
        real(wp), intent(out) :: min_pad_out

        integer :: pos, tm_at, x_end, tj_open, tj_close, next_bt
        real(wp) :: tx, ty, right_edge, pad
        character(len=:), allocatable :: label
        character(len=*), parameter :: TM_TAG = '1 0 0 1 '

        count_out = 0
        min_pad_out = huge(1.0_wp)
        pos = 1
        do
            tm_at = index(stream(pos:), TM_TAG)
            if (tm_at == 0) exit
            tm_at = pos + tm_at - 1
            pos = tm_at + len(TM_TAG)

            x_end = index(stream(pos:), ' Tm')
            if (x_end == 0) exit
            x_end = pos + x_end - 1
            call parse_two_reals(stream(pos:x_end - 1), tx, ty, status)
            if (status /= 0) cycle

            next_bt = index(stream(x_end:), 'BT')
            tj_open = index(stream(x_end:), '(')
            if (tj_open == 0) cycle
            if (next_bt > 0 .and. next_bt < tj_open) cycle
            tj_open = x_end + tj_open - 1
            tj_close = index(stream(tj_open:), ') Tj')
            if (tj_close == 0) cycle
            tj_close = tj_open + tj_close - 1
            label = stream(tj_open + 1:tj_close - 1)

            right_edge = tx + estimate_pdf_text_width(label, PDF_TICK_LABEL_SIZE)
            if (right_edge <= spine_x - 0.5_wp) then
                pad = spine_x - right_edge
                count_out = count_out + 1
                min_pad_out = min(min_pad_out, pad)
            end if
        end do

        if (count_out == 0) min_pad_out = 0.0_wp
    end subroutine check_y_tick_label_clearance

    subroutine parse_two_reals(text, a, b, stat)
        character(len=*), intent(in) :: text
        real(wp), intent(out) :: a, b
        integer, intent(out) :: stat

        read (text, *, iostat=stat) a, b
    end subroutine parse_two_reals

end program test_pdf_y_tick_label_clearance_2065
