program test_axis_label_offsets_pdf
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_margins, only: plot_margins_t, plot_area_t
    use fortplot_pdf_coordinate, only: calculate_pdf_plot_area
    use fortplot_system_runtime, only: create_directory_runtime
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    type(pdf_context) :: ctx
    integer, parameter :: W = 640, H = 480
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    character(len=:), allocatable :: path
    character(len=:), allocatable :: xl, yl
    character(len=:), allocatable :: stream_text
    integer :: status
    character(len=32) :: y_xtick_expect_str
    real(wp) :: y_xtick_expect
    logical :: found_xtick, dir_ok

    margins = plot_margins_t()
    ! Compute PDF canvas size in points (matplotlib semantics: 100 DPI, 1 in = 72 pt)
    ! Match create_pdf_canvas rounding to integer points
    block
        real(wp) :: width_pts, height_pts
        integer :: wpt_i, hpt_i
        width_pts  = real(W,  wp) * 72.0_wp / 100.0_wp
        height_pts = real(H,  wp) * 72.0_wp / 100.0_wp
        wpt_i = max(1, nint(width_pts))
        hpt_i = max(1, nint(height_pts))
        ! Use the PDF backend plot-area calculation to match PDF coordinates (Y=0 at bottom)
        call calculate_pdf_plot_area(wpt_i, hpt_i, margins, plot_area)
    end block

    ! Expected baseline for X tick labels: 15 pt below plot bottom in PDF coords
    y_xtick_expect = real(plot_area%bottom, wp) - 15.0_wp
    write(y_xtick_expect_str, '(F0.3)') y_xtick_expect

    ctx = create_pdf_canvas(W, H)
    ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
    ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp

    xl = 'X Axis'
    yl = 'Y Axis'
    call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
         0.0_wp, 10.0_wp, 0.0_wp, 10.0_wp, &
         xlabel=xl, ylabel=yl, has_3d_plots=.false.)

    ! Ensure output directory exists for isolated test execution
    call create_directory_runtime('test/output', dir_ok)

    path = 'test/output/axis_label_offsets.pdf'
    call ctx%save(path)

    call extract_pdf_stream_text(path, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: could not read PDF stream '
        stop 1
    end if

    ! Verify X tick labels baseline Y position exists in PDF stream
    found_xtick = index(stream_text, ' '//trim(y_xtick_expect_str)//' Tm') > 0
    if (.not. found_xtick) then
        print *, 'FAIL: X tick label baseline not found (expected ', trim(y_xtick_expect_str), ')'
        stop 2
    end if

    print *, 'PASS: X tick labels baseline (15px below) verified in PDF stream'
end program test_axis_label_offsets_pdf
