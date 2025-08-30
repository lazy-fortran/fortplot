program test_axes_order_338
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_test_helpers, only: test_get_temp_path, test_initialize_environment
    implicit none

    type(pdf_context) :: ctx
    integer :: i
    real(wp) :: x, y, prev_x, prev_y

    write(*, '(A)') "Testing axes/content order in PDF"

    call test_initialize_environment("axes_order_338")
    ctx = create_pdf_canvas(600, 400)
    ctx%x_min = 0.0_wp
    ctx%x_max = 10.0_wp
    ctx%y_min = -2.0_wp
    ctx%y_max = 2.0_wp

    ! Draw content BEFORE explicit axes call
    call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)
    do i = 1, 50
        x = real(i-1, wp) * 10.0_wp / 49.0_wp
        y = sin(x)
        if (i > 1) then
            call ctx%line(prev_x, prev_y, x, y)
        end if
        prev_x = x
        prev_y = y
    end do

    ! Explicitly render axes
    call ctx%render_axes("Test Plot", "X Axis", "Y Axis")

    ! Draw MORE content AFTER axes call
    call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)
    do i = 1, 50
        x = real(i-1, wp) * 10.0_wp / 49.0_wp
        y = cos(x)
        if (i > 1) then
            call ctx%line(prev_x, prev_y, x, y)
        end if
        prev_x = x
        prev_y = y
    end do

    call ctx%save(test_get_temp_path("test_axes_order.pdf"))
    write(*, '(A)') "Generated test_axes_order.pdf in test output directory"
    write(*, '(A)') "Check: Should see both blue sine and red cosine curves with axes"

end program test_axes_order_338