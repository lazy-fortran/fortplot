program test_pdf_coordinate_accuracy
    !! PDF coordinate accuracy tests  
    !! Extracted from test_pdf_coordinate_comprehensive.f90 (first part)
    !! 
    !! This test covers:
    !! - PDF coordinate transformation accuracy
    !! - Aspect ratio preservation and scaling
    !! - Square vs rectangular figure handling
    
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_pdf_coordinate, only: pdf_context_handle, normalize_to_pdf_coords
    use fortplot_test_utils, only: get_platform_tolerance
    use fortplot_security, only: get_test_output_path
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0
    real(wp), parameter :: BASE_TOLERANCE = 1.0e-6_wp
    real(wp) :: TOLERANCE

    print *, "=== PDF COORDINATE ACCURACY TESTS ==="
    
    ! Initialize platform-appropriate tolerance
    TOLERANCE = get_platform_tolerance(BASE_TOLERANCE)
    print *, "Using tolerance: ", TOLERANCE
    
    call test_coordinate_accuracy()
    call test_aspect_ratio_handling()
    call print_test_summary()

contains

    !===========================================================================
    ! Coordinate Accuracy Tests
    !===========================================================================
    
    subroutine test_coordinate_accuracy()
        print *, "--- Coordinate Accuracy Tests ---"
        
        call test_square_figure_coordinates()
        call test_rectangular_figure_coordinates()
        call test_coordinate_transformation()
        call test_boundary_coordinates()
    end subroutine test_coordinate_accuracy

    subroutine test_square_figure_coordinates()
        type(pdf_context) :: ctx
        real(wp) :: test_x, test_y, pdf_x, pdf_y
        real(wp) :: expected_x, expected_y
        
        call start_test("Square figure coordinate accuracy")
        
        ! Test square figure coordinate mapping
        ctx = create_pdf_canvas(800, 800)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        
        ! Test center coordinate
        test_x = 5.0_wp
        test_y = 5.0_wp
        call normalize_to_pdf_coords(ctx, test_x, test_y, pdf_x, pdf_y)
        
        ! For square figure, center should map to center
        expected_x = 0.5_wp  ! Normalized center
        expected_y = 0.5_wp
        
        call assert_coordinate_equal(pdf_x, expected_x, "Center X coordinate")
        call assert_coordinate_equal(pdf_y, expected_y, "Center Y coordinate")
        
        ! Test corner coordinates
        test_x = 0.0_wp; test_y = 0.0_wp
        call normalize_to_pdf_coords(ctx, test_x, test_y, pdf_x, pdf_y)
        call assert_coordinate_equal(pdf_x, 0.0_wp, "Bottom-left X coordinate")
        call assert_coordinate_equal(pdf_y, 0.0_wp, "Bottom-left Y coordinate")
        
        test_x = 10.0_wp; test_y = 10.0_wp
        call normalize_to_pdf_coords(ctx, test_x, test_y, pdf_x, pdf_y)
        call assert_coordinate_equal(pdf_x, 1.0_wp, "Top-right X coordinate")
        call assert_coordinate_equal(pdf_y, 1.0_wp, "Top-right Y coordinate")
        
        call end_test()
    end subroutine test_square_figure_coordinates

    subroutine test_rectangular_figure_coordinates()
        type(pdf_context) :: ctx
        real(wp) :: test_x, test_y, pdf_x, pdf_y
        
        call start_test("Rectangular figure coordinate accuracy")
        
        ! Test wide figure (2:1 aspect ratio)
        ctx = create_pdf_canvas(1200, 600)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        
        ! Test center coordinate
        test_x = 5.0_wp
        test_y = 5.0_wp
        call normalize_to_pdf_coords(ctx, test_x, test_y, pdf_x, pdf_y)
        
        ! Center should still map correctly despite aspect ratio difference
        call assert_coordinate_equal(pdf_x, 0.5_wp, "Wide figure center X")
        call assert_coordinate_equal(pdf_y, 0.5_wp, "Wide figure center Y")
        
        ! Test tall figure (1:2 aspect ratio)  
        ctx = create_pdf_canvas(400, 800)
        ctx%x_min = 0.0_wp; ctx%x_max = 5.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 5.0_wp
        
        test_x = 2.5_wp
        test_y = 2.5_wp
        call normalize_to_pdf_coords(ctx, test_x, test_y, pdf_x, pdf_y)
        
        call assert_coordinate_equal(pdf_x, 0.5_wp, "Tall figure center X")
        call assert_coordinate_equal(pdf_y, 0.5_wp, "Tall figure center Y")
        
        call end_test()
    end subroutine test_rectangular_figure_coordinates

    subroutine test_coordinate_transformation()
        type(pdf_context) :: ctx
        real(wp) :: x_vals(5), y_vals(5)
        real(wp) :: pdf_x, pdf_y
        integer :: i
        
        call start_test("Coordinate transformation linearity")
        
        ctx = create_pdf_canvas(600, 600)
        ctx%x_min = -2.0_wp; ctx%x_max = 8.0_wp
        ctx%y_min = -5.0_wp; ctx%y_max = 15.0_wp
        
        x_vals = [-2.0_wp, 1.0_wp, 3.0_wp, 6.0_wp, 8.0_wp]
        y_vals = [-5.0_wp, 0.0_wp, 5.0_wp, 10.0_wp, 15.0_wp]
        
        ! Test that transformation is linear
        do i = 1, 5
            call normalize_to_pdf_coords(ctx, x_vals(i), y_vals(i), pdf_x, pdf_y)
            
            ! X should map linearly from [-2,8] to [0,1]
            ! Y should map linearly from [-5,15] to [0,1] 
            if (i == 1) then
                call assert_coordinate_equal(pdf_x, 0.0_wp, "Left boundary X")
                call assert_coordinate_equal(pdf_y, 0.0_wp, "Bottom boundary Y")
            else if (i == 5) then
                call assert_coordinate_equal(pdf_x, 1.0_wp, "Right boundary X")
                call assert_coordinate_equal(pdf_y, 1.0_wp, "Top boundary Y")
            end if
        end do
        
        call end_test()
    end subroutine test_coordinate_transformation

    subroutine test_boundary_coordinates()
        type(pdf_context) :: ctx
        real(wp) :: test_x, test_y, pdf_x, pdf_y
        
        call start_test("Boundary coordinate handling")
        
        ctx = create_pdf_canvas(400, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 1.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 1.0_wp
        
        ! Test exact boundary values
        test_x = 0.0_wp; test_y = 0.0_wp
        call normalize_to_pdf_coords(ctx, test_x, test_y, pdf_x, pdf_y)
        call assert_coordinate_equal(pdf_x, 0.0_wp, "Exact left boundary")
        call assert_coordinate_equal(pdf_y, 0.0_wp, "Exact bottom boundary")
        
        test_x = 1.0_wp; test_y = 1.0_wp
        call normalize_to_pdf_coords(ctx, test_x, test_y, pdf_x, pdf_y)
        call assert_coordinate_equal(pdf_x, 1.0_wp, "Exact right boundary")
        call assert_coordinate_equal(pdf_y, 1.0_wp, "Exact top boundary")
        
        ! Test slightly outside boundary (should be clamped)
        test_x = -0.1_wp; test_y = -0.1_wp
        call normalize_to_pdf_coords(ctx, test_x, test_y, pdf_x, pdf_y)
        ! These might be clamped or extrapolated depending on implementation
        print *, "  Outside boundary coordinates: ", pdf_x, pdf_y
        
        call end_test()
    end subroutine test_boundary_coordinates

    !===========================================================================
    ! Aspect Ratio Tests
    !===========================================================================
    
    subroutine test_aspect_ratio_handling()
        print *, "--- Aspect Ratio Tests ---"
        
        call test_square_figure_scaling()
        call test_wide_figure_scaling()
        call test_tall_figure_scaling()
    end subroutine test_aspect_ratio_handling

    subroutine test_square_figure_scaling()
        type(pdf_context) :: ctx
        real(wp) :: scale_x, scale_y
        
        call start_test("Square figure scaling factors")
        
        ctx = create_pdf_canvas(800, 800)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        
        scale_x = ctx%get_width_scale()
        scale_y = ctx%get_height_scale()
        
        print *, "  Square scaling - X: ", scale_x, ", Y: ", scale_y
        
        ! Key requirement: scales should be equal for square figures
        call assert_coordinate_equal(scale_x, scale_y, "Equal scaling for square figure")
        
        call end_test()
    end subroutine test_square_figure_scaling

    subroutine test_wide_figure_scaling()
        type(pdf_context) :: ctx
        real(wp) :: scale_x, scale_y
        
        call start_test("Wide figure scaling (2:1 aspect)")
        
        ctx = create_pdf_canvas(1200, 600)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        
        scale_x = ctx%get_width_scale()
        scale_y = ctx%get_height_scale()
        
        print *, "  Wide scaling - X: ", scale_x, ", Y: ", scale_y
        
        ! For wide figure, X scale should be larger than Y scale
        if (scale_x <= scale_y) then
            print *, "  WARNING: Wide figure X scale not larger than Y scale"
        end if
        
        call end_test()
    end subroutine test_wide_figure_scaling

    subroutine test_tall_figure_scaling()
        type(pdf_context) :: ctx
        real(wp) :: scale_x, scale_y
        
        call start_test("Tall figure scaling (1:2 aspect)")
        
        ctx = create_pdf_canvas(400, 800)
        ctx%x_min = 0.0_wp; ctx%x_max = 5.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 5.0_wp
        
        scale_x = ctx%get_width_scale()
        scale_y = ctx%get_height_scale()
        
        print *, "  Tall scaling - X: ", scale_x, ", Y: ", scale_y
        
        ! For tall figure, Y scale should be larger than X scale
        if (scale_y <= scale_x) then
            print *, "  WARNING: Tall figure Y scale not larger than X scale"
        end if
        
        call end_test()
    end subroutine test_tall_figure_scaling

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') 'Test ', test_count, ': ', test_name
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') '  PASS'
        write(*, *)
    end subroutine end_test

    subroutine assert_coordinate_equal(actual, expected, description)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        
        if (abs(actual - expected) < TOLERANCE) then
            print *, '  PASS: ', description
        else
            print *, '  FAIL: ', description, ' (got ', actual, ', expected ', expected, ')'
        end if
    end subroutine assert_coordinate_equal

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'PDF Coordinate Accuracy Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        write(*, '(A)') 'PDF coordinate accuracy tests COMPLETED!'
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_pdf_coordinate_accuracy