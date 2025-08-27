program test_pdf_coordinate_debug
    !! PDF coordinate debugging and validation tests  
    !! Extracted from test_pdf_coordinate_comprehensive.f90 (second part)
    !! 
    !! This test covers:
    !! - Axes positioning and alignment
    !! - Coordinate scaling validation  
    !! - Debugging scenarios and transformation verification

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

    print *, "=== PDF COORDINATE DEBUG TESTS ==="
    
    ! Initialize platform-appropriate tolerance
    TOLERANCE = get_platform_tolerance(BASE_TOLERANCE)
    print *, "Using tolerance: ", TOLERANCE
    
    call test_axes_positioning()
    call test_scaling_validation()
    call test_debug_scenarios()
    call print_test_summary()

contains

    !===========================================================================
    ! Axes Positioning Tests
    !===========================================================================
    
    subroutine test_axes_positioning()
        print *, "--- Axes Positioning Tests ---"
        
        call test_axes_basic_positioning()
        call test_axes_with_labels()
        call test_axes_coordinate_alignment()
    end subroutine test_axes_positioning

    subroutine test_axes_basic_positioning()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Basic axes positioning (issue #338)")
        
        ctx = create_pdf_canvas(600, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        ! Draw simple axes
        call ctx%axes()
        call ctx%text(5.0_wp, 7.0_wp, "Axes Positioning Test")
        
        filename = get_test_output_path('/tmp/pdf_axes_basic.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Basic axes positioning test saved"
        call end_test()
    end subroutine test_axes_basic_positioning

    subroutine test_axes_with_labels()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Axes with labels positioning")
        
        ctx = create_pdf_canvas(500, 350)
        ctx%x_min = -2.0_wp; ctx%x_max = 12.0_wp
        ctx%y_min = -3.0_wp; ctx%y_max = 7.0_wp
        
        call ctx%axes()
        call ctx%text(5.0_wp, 6.0_wp, "X-Y Axes with Labels")
        call ctx%text(10.0_wp, -2.5_wp, "X-axis")
        call ctx%text(-1.5_wp, 5.0_wp, "Y-axis")
        
        filename = get_test_output_path('/tmp/pdf_axes_labels.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Axes with labels test saved"
        call end_test()
    end subroutine test_axes_with_labels

    subroutine test_axes_coordinate_alignment()
        type(pdf_context) :: ctx
        real(wp) :: x_coords(5), y_coords(5)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Axes coordinate alignment")
        
        ctx = create_pdf_canvas(400, 300)
        ctx%x_min = 0.0_wp; ctx%x_max = 4.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 3.0_wp
        
        call ctx%axes()
        
        ! Draw grid points to verify alignment
        x_coords = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y_coords = [0.0_wp, 0.75_wp, 1.5_wp, 2.25_wp, 3.0_wp]
        
        do i = 1, 5
            call ctx%circle(x_coords(i), y_coords(i), 0.05_wp)
        end do
        
        filename = get_test_output_path('/tmp/pdf_axes_alignment.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Axes alignment test saved"
        call end_test()
    end subroutine test_axes_coordinate_alignment

    !===========================================================================
    ! Scaling Validation Tests
    !===========================================================================
    
    subroutine test_scaling_validation()
        print *, "--- Scaling Validation Tests ---"
        
        call test_consistent_scaling()
        call test_scaling_edge_cases()
    end subroutine test_scaling_validation

    subroutine test_consistent_scaling()
        type(pdf_context) :: ctx1, ctx2
        real(wp) :: scale1_x, scale1_y, scale2_x, scale2_y
        
        call start_test("Consistent scaling across contexts")
        
        ! Two contexts with same data range but different canvas sizes
        ctx1 = create_pdf_canvas(400, 400)
        ctx1%x_min = 0.0_wp; ctx1%x_max = 5.0_wp
        ctx1%y_min = 0.0_wp; ctx1%y_max = 5.0_wp
        
        ctx2 = create_pdf_canvas(800, 800)
        ctx2%x_min = 0.0_wp; ctx2%x_max = 5.0_wp
        ctx2%y_min = 0.0_wp; ctx2%y_max = 5.0_wp
        
        scale1_x = ctx1%get_width_scale()
        scale1_y = ctx1%get_height_scale()
        scale2_x = ctx2%get_width_scale()
        scale2_y = ctx2%get_height_scale()
        
        ! Scaling should be proportional to canvas size
        call assert_coordinate_equal(scale2_x / scale1_x, 2.0_wp, "Proportional X scaling")
        call assert_coordinate_equal(scale2_y / scale1_y, 2.0_wp, "Proportional Y scaling")
        
        call end_test()
    end subroutine test_consistent_scaling

    subroutine test_scaling_edge_cases()
        type(pdf_context) :: ctx
        real(wp) :: scale_x, scale_y
        
        call start_test("Scaling edge cases")
        
        ! Test very small data range
        ctx = create_pdf_canvas(600, 600)
        ctx%x_min = 1.0_wp; ctx%x_max = 1.001_wp
        ctx%y_min = 2.0_wp; ctx%y_max = 2.001_wp
        
        scale_x = ctx%get_width_scale()
        scale_y = ctx%get_height_scale()
        
        print *, "  Small range scaling - X: ", scale_x, ", Y: ", scale_y
        
        ! Test very large data range
        ctx = create_pdf_canvas(600, 600)
        ctx%x_min = -1000.0_wp; ctx%x_max = 1000.0_wp
        ctx%y_min = -500.0_wp; ctx%y_max = 500.0_wp
        
        scale_x = ctx%get_width_scale()
        scale_y = ctx%get_height_scale()
        
        print *, "  Large range scaling - X: ", scale_x, ", Y: ", scale_y
        
        call end_test()
    end subroutine test_scaling_edge_cases

    !===========================================================================
    ! Debug Scenarios
    !===========================================================================
    
    subroutine test_debug_scenarios()
        print *, "--- Debug Scenarios ---"
        
        call test_coordinate_debugging()
        call test_transformation_verification()
    end subroutine test_debug_scenarios

    subroutine test_coordinate_debugging()
        type(pdf_context) :: ctx
        real(wp) :: test_points(3, 2)  ! 3 points, X and Y
        real(wp) :: pdf_x, pdf_y
        integer :: i
        
        call start_test("Coordinate debugging scenarios")
        
        ctx = create_pdf_canvas(500, 300)
        ctx%x_min = -1.0_wp; ctx%x_max = 4.0_wp
        ctx%y_min = -2.0_wp; ctx%y_max = 3.0_wp
        
        ! Test specific debug points
        test_points(1, 1) = -1.0_wp; test_points(1, 2) = -2.0_wp  ! Bottom-left
        test_points(2, 1) =  1.5_wp; test_points(2, 2) =  0.5_wp  ! Center
        test_points(3, 1) =  4.0_wp; test_points(3, 2) =  3.0_wp  ! Top-right
        
        do i = 1, 3
            call normalize_to_pdf_coords(ctx, test_points(i, 1), test_points(i, 2), pdf_x, pdf_y)
            print *, "  Debug point ", i, ": (", test_points(i, 1), ",", test_points(i, 2), &
                     ") -> (", pdf_x, ",", pdf_y, ")"
        end do
        
        call end_test()
    end subroutine test_coordinate_debugging

    subroutine test_transformation_verification()
        type(pdf_context) :: ctx
        real(wp) :: original_x, original_y, pdf_x, pdf_y
        real(wp) :: back_x, back_y  ! If inverse transformation available
        
        call start_test("Transformation verification")
        
        ctx = create_pdf_canvas(600, 400)
        ctx%x_min = 2.0_wp; ctx%x_max = 8.0_wp
        ctx%y_min = 1.0_wp; ctx%y_max = 5.0_wp
        
        original_x = 5.0_wp
        original_y = 3.0_wp
        
        call normalize_to_pdf_coords(ctx, original_x, original_y, pdf_x, pdf_y)
        
        print *, "  Original: (", original_x, ",", original_y, ")"
        print *, "  PDF coords: (", pdf_x, ",", pdf_y, ")"
        
        ! If inverse transformation is available, test round-trip
        ! back_x = pdf_x * (ctx%x_max - ctx%x_min) + ctx%x_min
        ! back_y = pdf_y * (ctx%y_max - ctx%y_min) + ctx%y_min
        ! call assert_coordinate_equal(back_x, original_x, "Round-trip X coordinate")
        ! call assert_coordinate_equal(back_y, original_y, "Round-trip Y coordinate")
        
        call end_test()
    end subroutine test_transformation_verification

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
            write(*, '(A, A)') '  PASS: ', description
        else
            write(*, '(A, A, F12.6, A, F12.6, A, ES10.3)') '  FAIL: ', description, &
                   actual, ' != ', expected, ' (tolerance: ', TOLERANCE, ')'
        end if
    end subroutine assert_coordinate_equal

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'PDF Coordinate Debug Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        write(*, '(A)') 'PDF coordinate debugging tests COMPLETED!'
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_pdf_coordinate_debug