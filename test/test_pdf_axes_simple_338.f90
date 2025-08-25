program test_pdf_axes_simple_338
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    implicit none

    type(pdf_context) :: ctx
    integer :: i
    real(wp) :: x, y, prev_x, prev_y

    write(*, '(A)') "Testing PDF axes fix for Issue #338"

    ctx = create_pdf_canvas(600, 400)
    ctx%x_min = 0.0_wp
    ctx%x_max = 10.0_wp
    ctx%y_min = -2.0_wp
    ctx%y_max = 2.0_wp

    ! Draw a sine wave
    call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)
    do i = 1, 100
        x = real(i-1, wp) * 10.0_wp / 99.0_wp
        y = sin(x)
        
        if (i > 1) then
            call ctx%line(prev_x, prev_y, x, y)
        end if
        prev_x = x
        prev_y = y
    end do

    call ctx%save("test_axes_simple_338.pdf")
    write(*, '(A)') "Generated: test_axes_simple_338.pdf"
    write(*, '(A)') "Expected: PDF with visible axes, tick marks, and labels"
    write(*, '(A)') "If axes are visible, Issue #338 is fixed!"

end program test_pdf_axes_simple_338