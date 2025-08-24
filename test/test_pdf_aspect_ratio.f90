program test_pdf_aspect_ratio
    !! Focused test for PDF aspect ratio regression (Issue #296)
    !! Tests that PDF scaling factors maintain correct relationships

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    implicit none

    type(pdf_context) :: ctx_square, ctx_wide, ctx_tall
    real(wp) :: scale_x, scale_y
    logical :: test_passed = .true.

    write(*, '(A)') "=== PDF Aspect Ratio Test ==="

    ! Test 1: Square figure scaling
    write(*, '(A)') "Test 1: Square figure (800x800)"
    ctx_square = create_pdf_canvas(800, 800)
    ctx_square%x_min = 0.0_wp; ctx_square%x_max = 10.0_wp
    ctx_square%y_min = 0.0_wp; ctx_square%y_max = 10.0_wp

    scale_x = ctx_square%get_width_scale()
    scale_y = ctx_square%get_height_scale()

    write(*, '(A, F6.3, A, F6.3)') "  Width scale: ", scale_x, ", Height scale: ", scale_y
    
    ! The key issue: scales should be equal for square figures
    if (abs(scale_x - scale_y) > 1.0e-5_wp) then
        write(*, '(A)') "  FAIL: Square figure should have equal X and Y scales"
        write(*, '(A, F8.6)') "  Scale difference: ", abs(scale_x - scale_y)
        test_passed = .false.
    else
        write(*, '(A)') "  PASS: Equal scaling for square figure"
    end if

    ! Test 2: Wide figure (2:1 aspect ratio)
    write(*, '(A)') "Test 2: Wide figure (1200x600)"
    ctx_wide = create_pdf_canvas(1200, 600)
    ctx_wide%x_min = 0.0_wp; ctx_wide%x_max = 10.0_wp
    ctx_wide%y_min = 0.0_wp; ctx_wide%y_max = 10.0_wp

    scale_x = ctx_wide%get_width_scale()
    scale_y = ctx_wide%get_height_scale()

    write(*, '(A, F6.3, A, F6.3)') "  Width scale: ", scale_x, ", Height scale: ", scale_y
    
    ! Since page size now matches figure size, both should be 1.0
    if (abs(scale_x - 1.0_wp) > 1.0e-5_wp .or. abs(scale_y - 1.0_wp) > 1.0e-5_wp) then
        write(*, '(A)') "  FAIL: Scales should be 1.0 for page-matched figure"
        test_passed = .false.
    else
        write(*, '(A)') "  PASS: Correct scaling (1.0) for wide figure"
    end if

    ! Test 3: Tall figure (1:2 aspect ratio)
    write(*, '(A)') "Test 3: Tall figure (600x1200)"
    ctx_tall = create_pdf_canvas(600, 1200)
    ctx_tall%x_min = 0.0_wp; ctx_tall%x_max = 10.0_wp
    ctx_tall%y_min = 0.0_wp; ctx_tall%y_max = 10.0_wp

    scale_x = ctx_tall%get_width_scale()
    scale_y = ctx_tall%get_height_scale()

    write(*, '(A, F6.3, A, F6.3)') "  Width scale: ", scale_x, ", Height scale: ", scale_y
    
    if (abs(scale_x - 1.0_wp) > 1.0e-5_wp .or. abs(scale_y - 1.0_wp) > 1.0e-5_wp) then
        write(*, '(A)') "  FAIL: Scales should be 1.0 for page-matched figure"
        test_passed = .false.
    else
        write(*, '(A)') "  PASS: Correct scaling (1.0) for tall figure"
    end if

    ! Test 4: Draw simple patterns to visualize distortion
    write(*, '(A)') "Test 4: Drawing test patterns"
    
    call draw_test_pattern(ctx_square)
    call draw_test_pattern(ctx_wide)
    call draw_test_pattern(ctx_tall)

    call ctx_square%save("test_square_pattern.pdf")
    call ctx_wide%save("test_wide_pattern.pdf") 
    call ctx_tall%save("test_tall_pattern.pdf")

    write(*, '(A)') "Generated test PDFs with patterns for visual inspection"

    if (test_passed) then
        write(*, '(A)') "=== All tests PASSED ==="
    else
        write(*, '(A)') "=== Some tests FAILED - PDF plots may appear stretched ==="
        error stop 1
    end if

contains

    subroutine draw_test_pattern(ctx)
        type(pdf_context), intent(inout) :: ctx
        integer :: i
        real(wp) :: x, y, x_prev, y_prev
        
        ! Draw grid pattern to detect stretching
        call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)  ! Blue
        
        ! Horizontal lines
        do i = 1, 9
            y = real(i, wp)
            call ctx%line(0.0_wp, y, 10.0_wp, y)
        end do
        
        ! Vertical lines  
        do i = 1, 9
            x = real(i, wp)
            call ctx%line(x, 0.0_wp, x, 10.0_wp)
        end do
        
        ! Draw circles that should remain circular (not elliptical)
        call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)  ! Red
        
        ! Draw "circle" using line segments - will show distortion if present
        do i = 0, 35
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
    end subroutine draw_test_pattern

end program test_pdf_aspect_ratio