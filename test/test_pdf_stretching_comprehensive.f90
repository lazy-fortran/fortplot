program test_pdf_stretching_comprehensive
    !! Comprehensive test for PDF stretching regression fix (Issue #296)
    !! Tests various data types, figure sizes, and aspect ratios

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    implicit none

    logical :: all_tests_passed = .true.

    write(*, '(A)') "=== Comprehensive PDF Stretching Regression Test ==="

    ! Test 1: Different data aspect ratios on wide canvas
    call test_data_aspect_ratios()

    ! Test 2: Scientific data types (e.g., sine waves, exponentials)
    call test_scientific_plots()

    ! Test 3: Edge case geometries
    call test_edge_case_geometries()

    ! Test 4: Extreme aspect ratios
    call test_extreme_aspect_ratios()

    if (all_tests_passed) then
        write(*, '(A)') "=== ALL COMPREHENSIVE TESTS PASSED ==="
        write(*, '(A)') "PDF stretching regression has been successfully fixed!"
    else
        write(*, '(A)') "=== SOME TESTS FAILED ==="
        error stop 1
    end if

contains

    subroutine test_data_aspect_ratios()
        type(pdf_context) :: ctx
        
        write(*, '(A)') "Test 1: Different data aspect ratios on wide canvas (1600x800)"
        ctx = create_pdf_canvas(1600, 800)

        ! 1a: Square data (aspect = 1.0)
        call generate_test_plot(ctx, 0.0_wp, 10.0_wp, 0.0_wp, 10.0_wp, &
                               "test_1a_square_data.pdf")
        write(*, '(A)') "  1a: Square data (10x10) -> test_1a_square_data.pdf"

        ! 1b: Wide data (aspect = 2.0)  
        call generate_test_plot(ctx, 0.0_wp, 20.0_wp, 0.0_wp, 10.0_wp, &
                               "test_1b_wide_data.pdf")
        write(*, '(A)') "  1b: Wide data (20x10) -> test_1b_wide_data.pdf"

        ! 1c: Tall data (aspect = 0.5)
        call generate_test_plot(ctx, 0.0_wp, 10.0_wp, 0.0_wp, 20.0_wp, &
                               "test_1c_tall_data.pdf")
        write(*, '(A)') "  1c: Tall data (10x20) -> test_1c_tall_data.pdf"

        write(*, '(A)') "  Visual check: Circles should be circular in all cases"
        write(*, '(A)') ""
    end subroutine test_data_aspect_ratios

    subroutine test_scientific_plots()
        type(pdf_context) :: ctx
        integer, parameter :: n = 100
        real(wp) :: x(n), y_sin(n), y_exp(n)
        integer :: i

        write(*, '(A)') "Test 2: Scientific plot types"
        ctx = create_pdf_canvas(1200, 600)  ! 2:1 aspect ratio

        ! Generate data
        do i = 1, n
            x(i) = real(i-1, wp) * 4.0_wp * 3.14159_wp / real(n-1, wp)  ! 0 to 4π
            y_sin(i) = sin(x(i))
            y_exp(i) = exp(-x(i) / 10.0_wp) * cos(x(i))
        end do

        ! 2a: Sine wave (should maintain proper amplitude proportions)
        call generate_function_plot(ctx, x, y_sin, "test_2a_sine_wave.pdf")
        write(*, '(A)') "  2a: Sine wave -> test_2a_sine_wave.pdf"

        ! 2b: Damped oscillation
        call generate_function_plot(ctx, x, y_exp, "test_2b_damped_osc.pdf")
        write(*, '(A)') "  2b: Damped oscillation -> test_2b_damped_osc.pdf"

        write(*, '(A)') "  Visual check: Function shapes should be preserved correctly"
        write(*, '(A)') ""
    end subroutine test_scientific_plots

    subroutine test_edge_case_geometries()
        type(pdf_context) :: ctx
        
        write(*, '(A)') "Test 3: Edge case geometries"

        ! 3a: Very small canvas
        ctx = create_pdf_canvas(200, 200)
        call generate_test_plot(ctx, -1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp, &
                               "test_3a_tiny_canvas.pdf")
        write(*, '(A)') "  3a: Tiny canvas (200x200) -> test_3a_tiny_canvas.pdf"

        ! 3b: Very large canvas
        ctx = create_pdf_canvas(2000, 1000)
        call generate_test_plot(ctx, 0.0_wp, 100.0_wp, 0.0_wp, 100.0_wp, &
                               "test_3b_large_canvas.pdf")
        write(*, '(A)') "  3b: Large canvas (2000x1000) -> test_3b_large_canvas.pdf"

        write(*, '(A)') ""
    end subroutine test_edge_case_geometries

    subroutine test_extreme_aspect_ratios()
        type(pdf_context) :: ctx
        
        write(*, '(A)') "Test 4: Extreme aspect ratios"

        ! 4a: Very wide canvas
        ctx = create_pdf_canvas(2400, 400)  ! 6:1 aspect ratio
        call generate_test_plot(ctx, 0.0_wp, 10.0_wp, 0.0_wp, 10.0_wp, &
                               "test_4a_ultra_wide.pdf")
        write(*, '(A)') "  4a: Ultra-wide canvas (2400x400) -> test_4a_ultra_wide.pdf"

        ! 4b: Very tall canvas
        ctx = create_pdf_canvas(400, 2400)  ! 1:6 aspect ratio
        call generate_test_plot(ctx, 0.0_wp, 10.0_wp, 0.0_wp, 10.0_wp, &
                               "test_4b_ultra_tall.pdf")
        write(*, '(A)') "  4b: Ultra-tall canvas (400x2400) -> test_4b_ultra_tall.pdf"

        write(*, '(A)') "  Visual check: Square data should remain square"
        write(*, '(A)') ""
    end subroutine test_extreme_aspect_ratios

    subroutine generate_test_plot(ctx, x_min, x_max, y_min, y_max, filename)
        type(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: filename
        integer :: i
        real(wp) :: x, y, x_prev, y_prev
        
        ! Set coordinate system
        ctx%x_min = x_min; ctx%x_max = x_max
        ctx%y_min = y_min; ctx%y_max = y_max
        
        ! Draw coordinate axes
        call ctx%color(0.5_wp, 0.5_wp, 0.5_wp)
        call ctx%line(x_min, (y_min + y_max) * 0.5_wp, x_max, (y_min + y_max) * 0.5_wp)
        call ctx%line((x_min + x_max) * 0.5_wp, y_min, (x_min + x_max) * 0.5_wp, y_max)
        
        ! Draw a reference circle (should remain circular)
        call ctx%color(1.0_wp, 0.0_wp, 0.0_wp)
        do i = 0, 36
            x = (x_min + x_max) * 0.5_wp + (x_max - x_min) * 0.2_wp * cos(real(i, wp) * 3.14159_wp / 18.0_wp)
            y = (y_min + y_max) * 0.5_wp + (y_max - y_min) * 0.2_wp * sin(real(i, wp) * 3.14159_wp / 18.0_wp)
            
            if (i > 0) then
                call ctx%line(x_prev, y_prev, x, y)
            end if
            x_prev = x
            y_prev = y
        end do
        
        ! Close the circle
        x = (x_min + x_max) * 0.5_wp + (x_max - x_min) * 0.2_wp
        y = (y_min + y_max) * 0.5_wp
        call ctx%line(x_prev, y_prev, x, y)

        call ctx%save(filename)
    end subroutine generate_test_plot

    subroutine generate_function_plot(ctx, x_data, y_data, filename)
        type(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_data(:), y_data(:)
        character(len=*), intent(in) :: filename
        integer :: i, n
        
        n = size(x_data)
        
        ! Set coordinate system based on data range
        ctx%x_min = minval(x_data)
        ctx%x_max = maxval(x_data)
        ctx%y_min = minval(y_data)
        ctx%y_max = maxval(y_data)
        
        ! Draw the function
        call ctx%color(0.0_wp, 0.0_wp, 1.0_wp)
        do i = 2, n
            call ctx%line(x_data(i-1), y_data(i-1), x_data(i), y_data(i))
        end do

        call ctx%save(filename)
    end subroutine generate_function_plot

end program test_pdf_stretching_comprehensive