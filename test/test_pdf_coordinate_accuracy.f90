program test_pdf_coordinate_accuracy
    !! Focused test for PDF coordinate transformation accuracy
    !! Tests that coordinates map correctly without distortion

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_pdf_coordinate, only: pdf_context_handle, normalize_to_pdf_coords
    use fortplot_test_utils, only: get_platform_tolerance
    implicit none

    type(pdf_context) :: ctx
    real(wp) :: test_x, test_y, pdf_x, pdf_y
    real(wp) :: expected_x, expected_y
    logical :: test_passed = .true.
    real(wp), parameter :: BASE_TOLERANCE = 1.0e-6_wp  ! Base precision requirement
    real(wp) :: TOLERANCE

    write(*, '(A)') "=== PDF Coordinate Accuracy Test ==="

    ! Initialize platform-appropriate tolerance
    TOLERANCE = get_platform_tolerance(BASE_TOLERANCE)
    write(*, '(A, ES10.3)') "Using tolerance: ", TOLERANCE

    ! Test square figure coordinate mapping
    write(*, '(A)') "Test: Square figure (800x800) coordinate accuracy"
    ctx = create_pdf_canvas(800, 800)
    ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
    ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp

    write(*, '(A)') "Plot area details:"
    write(*, '(A, I0, A, I0, A, I0, A, I0)') &
        "  Left: ", ctx%plot_area%left, ", Bottom: ", ctx%plot_area%bottom, &
        ", Width: ", ctx%plot_area%width, ", Height: ", ctx%plot_area%height
    write(*, '(A, I0, A, I0)') "  Canvas: ", ctx%width, " x ", ctx%height

    ! Test coordinate transformation consistency
    write(*, '(A)') ""
    write(*, '(A)') "Testing coordinate mapping precision:"

    ! Test corner coordinates
    call test_coordinate_mapping(ctx, 0.0_wp, 0.0_wp, "bottom-left", test_passed)
    call test_coordinate_mapping(ctx, 10.0_wp, 0.0_wp, "bottom-right", test_passed)
    call test_coordinate_mapping(ctx, 0.0_wp, 10.0_wp, "top-left", test_passed)
    call test_coordinate_mapping(ctx, 10.0_wp, 10.0_wp, "top-right", test_passed)
    call test_coordinate_mapping(ctx, 5.0_wp, 5.0_wp, "center", test_passed)

    ! Now test different aspect ratio figure
    write(*, '(A)') ""
    write(*, '(A)') "Test: Wide figure (1200x600) coordinate accuracy"
    ctx = create_pdf_canvas(1200, 600)
    ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
    ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp

    write(*, '(A)') "Plot area details:"
    write(*, '(A, I0, A, I0, A, I0, A, I0)') &
        "  Left: ", ctx%plot_area%left, ", Bottom: ", ctx%plot_area%bottom, &
        ", Width: ", ctx%plot_area%width, ", Height: ", ctx%plot_area%height
    write(*, '(A, I0, A, I0)') "  Canvas: ", ctx%width, " x ", ctx%height

    call test_coordinate_mapping(ctx, 0.0_wp, 0.0_wp, "wide bottom-left", test_passed)
    call test_coordinate_mapping(ctx, 10.0_wp, 10.0_wp, "wide top-right", test_passed)
    call test_coordinate_mapping(ctx, 5.0_wp, 5.0_wp, "wide center", test_passed)

    ! Test aspect ratio preservation
    write(*, '(A)') ""
    write(*, '(A)') "Testing aspect ratio preservation:"
    call test_aspect_ratio_preservation(ctx, test_passed)

    if (test_passed) then
        write(*, '(A)') "=== All coordinate accuracy tests PASSED ==="
    else
        write(*, '(A)') "=== Coordinate accuracy tests FAILED - PDF stretching confirmed ==="
        error stop 1
    end if

contains

    subroutine test_coordinate_mapping(ctx, x, y, label, passed)
        type(pdf_context), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: label
        logical, intent(inout) :: passed
        
        real(wp) :: pdf_x, pdf_y
        type(pdf_context_handle) :: ctx_handle
        
        ! Set up context handle for coordinate transformation
        ctx_handle%x_min = ctx%x_min
        ctx_handle%x_max = ctx%x_max
        ctx_handle%y_min = ctx%y_min
        ctx_handle%y_max = ctx%y_max
        ctx_handle%plot_area = ctx%plot_area
        ctx_handle%width = ctx%width
        ctx_handle%height = ctx%height
        
        ! Use actual coordinate transformation function
        call normalize_to_pdf_coords(ctx_handle, x, y, pdf_x, pdf_y)
        
        ! Test that coordinates are within expected bounds (PDF Y=0 at bottom)
        if (pdf_x < real(ctx%plot_area%left, wp) - 1.0_wp .or. &
            pdf_x > real(ctx%plot_area%left + ctx%plot_area%width, wp) + 1.0_wp .or. &
            pdf_y < real(ctx%plot_area%bottom, wp) - 1.0_wp .or. &
            pdf_y > real(ctx%plot_area%bottom + ctx%plot_area%height, wp) + 1.0_wp) then
            write(*, '(A, A, A)') "  ERROR: ", label, " coordinates out of bounds"
            passed = .false.
        else
            write(*, '(A, A, A, F6.1, A, F6.1, A, F8.2, A, F8.2, A)') &
                "  ", label, ": (", x, ", ", y, ") -> (", pdf_x, ", ", pdf_y, ")"
        end if
    end subroutine test_coordinate_mapping

    subroutine test_aspect_ratio_preservation(ctx, passed)
        type(pdf_context), intent(inout) :: ctx
        logical, intent(inout) :: passed
        
        real(wp) :: x_scale, y_scale, canvas_aspect, data_aspect, plot_aspect
        
        ! Calculate different aspect ratios
        canvas_aspect = real(ctx%width, wp) / real(ctx%height, wp)
        data_aspect = (ctx%x_max - ctx%x_min) / (ctx%y_max - ctx%y_min)
        plot_aspect = real(ctx%plot_area%width, wp) / real(ctx%plot_area%height, wp)
        
        x_scale = real(ctx%plot_area%width, wp) / (ctx%x_max - ctx%x_min)
        y_scale = real(ctx%plot_area%height, wp) / (ctx%y_max - ctx%y_min)
        
        write(*, '(A, F6.3)') "  Canvas aspect ratio: ", canvas_aspect
        write(*, '(A, F6.3)') "  Data aspect ratio: ", data_aspect  
        write(*, '(A, F6.3)') "  Plot area aspect ratio: ", plot_aspect
        write(*, '(A, F6.1)') "  X scale (pixels per data unit): ", x_scale
        write(*, '(A, F6.1)') "  Y scale (pixels per data unit): ", y_scale
        write(*, '(A, F6.3)') "  Scale ratio (X/Y): ", x_scale / y_scale
        
        ! For square data (aspect=1), if plot has different aspect ratio,
        ! the scale ratio should NOT be 1.0 - this would indicate stretching
        if (abs(data_aspect - 1.0_wp) < TOLERANCE) then
            ! Square data should maintain aspect ratio via different X/Y scales
            if (abs(plot_aspect - 1.0_wp) > TOLERANCE .and. &
                abs(x_scale / y_scale - 1.0_wp) < TOLERANCE) then
                write(*, '(A)') "  WARNING: Square data mapped to non-square plot with equal scales"
                write(*, '(A)') "  This may cause stretching!"
                passed = .false.
            end if
        end if
    end subroutine test_aspect_ratio_preservation

end program test_pdf_coordinate_accuracy