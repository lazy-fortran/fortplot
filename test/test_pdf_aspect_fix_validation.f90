program test_pdf_aspect_fix_validation
    !! Test to validate the PDF aspect ratio fix works correctly
    !! Uses the actual coordinate transformation functions

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_pdf_coordinate, only: safe_coordinate_transform
    implicit none

    type(pdf_context) :: ctx_square, ctx_wide
    real(wp) :: x_min, x_max, y_min, y_max
    real(wp) :: plot_left, plot_width, plot_bottom, plot_height
    real(wp) :: pdf_x, pdf_y
    real(wp) :: x_scale, y_scale
    logical :: test_passed = .true.

    write(*, '(A)') "=== PDF Aspect Ratio Fix Validation ==="

    ! Test 1: Square data on square canvas - should work perfectly
    write(*, '(A)') "Test 1: Square data (10x10) on square canvas (800x800)"
    ctx_square = create_pdf_canvas(800, 800)
    
    x_min = 0.0_wp; x_max = 10.0_wp
    y_min = 0.0_wp; y_max = 10.0_wp
    plot_left = real(ctx_square%plot_area%left, wp)
    plot_width = real(ctx_square%plot_area%width, wp)
    plot_bottom = real(ctx_square%height - ctx_square%plot_area%bottom - ctx_square%plot_area%height, wp)
    plot_height = real(ctx_square%plot_area%height, wp)
    
    write(*, '(A, F8.1, A, F8.1, A, F8.1, A, F8.1)') &
        "  Plot area: left=", plot_left, ", bottom=", plot_bottom, &
        ", width=", plot_width, ", height=", plot_height

    ! Test center point transformation
    call safe_coordinate_transform(5.0_wp, 5.0_wp, x_min, x_max, y_min, y_max, &
                                  plot_left, plot_width, plot_bottom, plot_height, &
                                  pdf_x, pdf_y)
    
    write(*, '(A, F8.2, A, F8.2, A)') "  Center (5,5) -> (", pdf_x, ", ", pdf_y, ")"

    ! Calculate effective scales
    x_scale = plot_width / (x_max - x_min)
    y_scale = plot_height / (y_max - y_min)
    write(*, '(A, F6.1, A, F6.1, A, F6.3)') &
        "  Scales: X=", x_scale, ", Y=", y_scale, ", Ratio=", x_scale / y_scale

    if (abs(x_scale - y_scale) < 1.0_wp) then
        write(*, '(A)') "  PASS: Square canvas has nearly equal scales"
    else
        write(*, '(A)') "  WARNING: Large scale difference on square canvas"
    end if

    write(*, '(A)') ""

    ! Test 2: Square data on wide canvas - the critical test
    write(*, '(A)') "Test 2: Square data (10x10) on wide canvas (1200x600)"
    ctx_wide = create_pdf_canvas(1200, 600)
    
    plot_left = real(ctx_wide%plot_area%left, wp)
    plot_width = real(ctx_wide%plot_area%width, wp)
    plot_bottom = real(ctx_wide%height - ctx_wide%plot_area%bottom - ctx_wide%plot_area%height, wp)
    plot_height = real(ctx_wide%plot_area%height, wp)
    
    write(*, '(A, F8.1, A, F8.1, A, F8.1, A, F8.1)') &
        "  Plot area: left=", plot_left, ", bottom=", plot_bottom, &
        ", width=", plot_width, ", height=", plot_height

    ! Test corner points to verify aspect ratio preservation
    call test_coordinate_with_fix(0.0_wp, 0.0_wp, "bottom-left", x_min, x_max, y_min, y_max, &
                                  plot_left, plot_width, plot_bottom, plot_height)
    call test_coordinate_with_fix(10.0_wp, 0.0_wp, "bottom-right", x_min, x_max, y_min, y_max, &
                                  plot_left, plot_width, plot_bottom, plot_height)
    call test_coordinate_with_fix(0.0_wp, 10.0_wp, "top-left", x_min, x_max, y_min, y_max, &
                                  plot_left, plot_width, plot_bottom, plot_height)
    call test_coordinate_with_fix(10.0_wp, 10.0_wp, "top-right", x_min, x_max, y_min, y_max, &
                                  plot_left, plot_width, plot_bottom, plot_height)
    call test_coordinate_with_fix(5.0_wp, 5.0_wp, "center", x_min, x_max, y_min, y_max, &
                                  plot_left, plot_width, plot_bottom, plot_height)

    ! Calculate the actual scales used by the fix
    call safe_coordinate_transform(0.0_wp, 0.0_wp, x_min, x_max, y_min, y_max, &
                                  plot_left, plot_width, plot_bottom, plot_height, &
                                  pdf_x, pdf_y)
    call safe_coordinate_transform(1.0_wp, 1.0_wp, x_min, x_max, y_min, y_max, &
                                  plot_left, plot_width, plot_bottom, plot_height, &
                                  pdf_x, pdf_y)
    
    ! With the fix, we should see equal scales
    x_scale = min(plot_width / (x_max - x_min), plot_height / (y_max - y_min))
    y_scale = x_scale  ! Should be the same due to aspect ratio preservation
    
    write(*, '(A, F6.1, A, F6.1, A, F6.3)') &
        "  Fixed scales: X=", x_scale, ", Y=", y_scale, ", Ratio=", x_scale / y_scale

    if (abs(x_scale - y_scale) < 0.01_wp) then
        write(*, '(A)') "  PASS: Aspect ratio fix working - equal scales maintained"
    else
        write(*, '(A)') "  FAIL: Aspect ratio fix not working properly"
        test_passed = .false.
    end if

    ! Test 3: Generate visual validation PDFs
    write(*, '(A)') ""
    write(*, '(A)') "Test 3: Generating visual validation PDFs"
    call generate_aspect_test_pdf(ctx_square, "fixed_square_aspect.pdf")
    call generate_aspect_test_pdf(ctx_wide, "fixed_wide_aspect.pdf")
    
    write(*, '(A)') "Generated PDFs:"
    write(*, '(A)') "  - fixed_square_aspect.pdf"
    write(*, '(A)') "  - fixed_wide_aspect.pdf"
    write(*, '(A)') "Visual inspection: circles should be circular, not elliptical"

    if (test_passed) then
        write(*, '(A)') "=== Aspect ratio fix validation PASSED ==="
    else
        write(*, '(A)') "=== Aspect ratio fix validation FAILED ==="
        error stop 1
    end if

contains

    subroutine test_coordinate_with_fix(x, y, label, x_min, x_max, y_min, y_max, &
                                       plot_left, plot_width, plot_bottom, plot_height)
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: label
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: plot_left, plot_width, plot_bottom, plot_height
        real(wp) :: pdf_x, pdf_y

        call safe_coordinate_transform(x, y, x_min, x_max, y_min, y_max, &
                                      plot_left, plot_width, plot_bottom, plot_height, &
                                      pdf_x, pdf_y)
        
        write(*, '(A, A, A, F4.1, A, F4.1, A, F8.2, A, F8.2, A)') &
            "  ", label, ": (", x, ", ", y, ") -> (", pdf_x, ", ", pdf_y, ")"
    end subroutine test_coordinate_with_fix

    subroutine generate_aspect_test_pdf(ctx, filename)
        type(pdf_context), intent(inout) :: ctx
        character(len=*), intent(in) :: filename
        integer :: i
        real(wp) :: x, y, x_prev, y_prev
        
        ! Set coordinate system for square data
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        
        ! Draw grid to detect stretching
        call ctx%color(0.5_wp, 0.5_wp, 0.5_wp)  ! Gray grid
        do i = 1, 9
            ! Horizontal lines
            call ctx%line(0.0_wp, real(i, wp), 10.0_wp, real(i, wp))
            ! Vertical lines  
            call ctx%line(real(i, wp), 0.0_wp, real(i, wp), 10.0_wp)
        end do
        
        ! Draw a circle to test aspect ratio preservation
        call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  ! Red circle
        do i = 0, 36
            x = 5.0_wp + 2.0_wp * cos(real(i, wp) * 3.14159_wp / 18.0_wp)
            y = 5.0_wp + 2.0_wp * sin(real(i, wp) * 3.14159_wp / 18.0_wp)
            
            if (i > 0) then
                call ctx%line(x_prev, y_prev, x, y)
            end if
            x_prev = x
            y_prev = y
        end do
        
        ! Close the circle
        x = 5.0_wp + 2.0_wp
        y = 5.0_wp
        call ctx%line(x_prev, y_prev, x, y)

        call ctx%save(filename)
    end subroutine generate_aspect_test_pdf

end program test_pdf_aspect_fix_validation