program test_axis_label_offsets_pdf
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_margins, only: plot_margins_t, plot_area_t
    use fortplot_pdf_coordinate, only: calculate_pdf_plot_area
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    type(pdf_context) :: ctx
    integer, parameter :: W = 640, H = 480
    type(plot_margins_t) :: margins
    type(plot_area_t) :: plot_area
    character(len=:), allocatable :: path
    character(len=:), allocatable :: xl, yl
    integer :: unit, ios
    character(len=65536) :: buf
    integer :: read_len
    character(len=32) :: y_xtick_expect_str
    real(wp) :: y_xtick_expect
    logical :: found_xtick, dir_ok

    margins = plot_margins_t()
    ! Use the PDF backend plot-area calculation to match PDF coordinates (Y=0 at bottom)
    call calculate_pdf_plot_area(W, H, margins, plot_area)

    ! Expected baseline for X tick labels: 15px below plot bottom in PDF coords
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

    open(newunit=unit, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: could not open ', trim(path)
        stop 1
    end if

    buf = ''
    read_len = 0
    do
        read(unit, '(A)', iostat=ios) buf(read_len+1:)
        if (ios /= 0) exit
        read_len = len_trim(buf)
        if (read_len > len(buf) - 256) exit
    end do
    close(unit)

    ! Verify X tick labels baseline Y position exists in PDF stream
    found_xtick = index(buf, ' '//trim(y_xtick_expect_str)//' Tm') > 0
    if (.not. found_xtick) then
        print *, 'FAIL: X tick label baseline not found (expected ', trim(y_xtick_expect_str), ')'
        stop 2
    end if

    print *, 'PASS: X tick labels baseline (15px below) verified in PDF stream'
end program test_axis_label_offsets_pdf
