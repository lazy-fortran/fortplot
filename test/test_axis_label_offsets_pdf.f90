program test_axis_label_offsets_pdf
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area
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
    character(len=32) :: y_expect_str, x_expect_str
    real(wp) :: xlab_y_expect, ylab_x_expect
    logical :: found_y, found_x

    margins = plot_margins_t()
    call calculate_plot_area(W, H, margins, plot_area)

    ! Expected offsets (matplotlib-compatible): X label 50px below, Y label 98px left
    xlab_y_expect = real(plot_area%bottom, wp) - 50.0_wp
    ylab_x_expect = real(plot_area%left, wp) - 98.0_wp

    write(y_expect_str, '(F0.3)') xlab_y_expect
    write(x_expect_str, '(F0.3)') ylab_x_expect

    ctx = create_pdf_canvas(W, H)
    ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
    ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp

    xl = 'X Axis'
    yl = 'Y Axis'
    call ctx%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
         0.0_wp, 10.0_wp, 0.0_wp, 10.0_wp, &
         xlabel=xl, ylabel=yl, has_3d_plots=.false.)

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

    ! Check for X-axis label Y position (search for formatted y followed by ' Tm')
    found_y = index(buf, ' '//trim(y_expect_str)//' Tm') > 0

    ! Check for rotated Y-axis label X position (matrix prefix then x value)
    found_x = index(buf, '0 1 -1 0 '//trim(x_expect_str)//' ') > 0

    if (.not. found_y) then
        print *, 'FAIL: X-axis label Y offset not found (expected ', trim(y_expect_str), ')'
        stop 2
    end if

    if (.not. found_x) then
        print *, 'FAIL: Y-axis label X offset not found (expected ', trim(x_expect_str), ')'
        stop 3
    end if

    print *, 'PASS: Axis label offsets (X: -50px, Y: -98px) verified in PDF stream'
end program test_axis_label_offsets_pdf
