program test_text_annotations
    !! Comprehensive TDD RED phase test suite for text annotation support (Issue #55)
    !! Tests define exact API and behavior expectations for text annotation implementation
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none

    call test_text_annotation_type_definition()
    call test_coordinate_system_constants()
    call test_basic_text_annotation_creation()
    call test_data_coordinate_positioning()
    call test_figure_coordinate_positioning()
    call test_axis_coordinate_positioning()
    call test_figure_text_method_api()
    call test_figure_annotate_method_api()
    call test_global_text_function_api()
    call test_global_annotate_function_api()
    call test_typography_font_size()
    call test_typography_alignment()
    call test_typography_rotation()
    call test_png_backend_text_rendering()
    call test_pdf_backend_text_rendering()
    call test_ascii_backend_text_rendering()
    call test_coordinate_transformation_accuracy()
    call test_text_metrics_calculation()
    call test_multiple_annotation_handling()
    call test_annotation_clipping_bounds()
    call test_error_handling_invalid_coordinates()
    call test_error_handling_invalid_text_parameters()
    call test_performance_multiple_annotations()
    call test_background_box_rendering()
    call test_arrow_annotation_support()
    call test_backend_consistency_validation()
    call test_memory_management()

contains

    subroutine test_text_annotation_type_definition()
        !! GIVEN: Need for text annotation data structure
        !! WHEN: Using text_annotation_t type
        !! THEN: Type provides all required fields for positioning, styling, and content
        use fortplot_annotations, only: text_annotation_t
        
        type(text_annotation_t) :: annotation
        
        ! Text content field
        annotation%text = "Test annotation"
        
        ! Position coordinates
        annotation%x = 1.5_wp
        annotation%y = 2.5_wp
        annotation%coord_type = COORD_DATA
        
        ! Typography fields
        annotation%font_size = 12.0_wp
        annotation%rotation = 45.0_wp
        annotation%alignment = 'center'
        
        ! Background box fields
        annotation%has_bbox = .true.
        annotation%bbox_color%r = 1.0_wp
        annotation%bbox_color%g = 1.0_wp
        annotation%bbox_color%b = 1.0_wp
        
        ! Arrow annotation fields
        annotation%has_arrow = .true.
        annotation%xytext_x = 0.5_wp
        annotation%xytext_y = 1.0_wp
        annotation%xytext_coord_type = COORD_DATA
        
        print *, "PASS: text_annotation_t type definition test"
    end subroutine test_text_annotation_type_definition

    subroutine test_coordinate_system_constants()
        !! GIVEN: Need for coordinate system identification
        !! WHEN: Using coordinate type constants
        !! THEN: Constants are defined and have distinct values
        use fortplot_annotations, only: COORD_DATA, COORD_FIGURE, COORD_AXIS
        
        if (COORD_DATA == COORD_FIGURE .or. COORD_DATA == COORD_AXIS .or. COORD_FIGURE == COORD_AXIS) then
            error stop "FAIL: Coordinate constants must have distinct values"
        end if
        
        print *, "PASS: Coordinate system constants test"
    end subroutine test_coordinate_system_constants

    subroutine test_basic_text_annotation_creation()
        !! GIVEN: Text content and position
        !! WHEN: Creating basic text annotation
        !! THEN: Annotation is created with specified properties
        use fortplot_annotations, only: create_text_annotation, text_annotation_t
        
        type(text_annotation_t) :: annotation
        
        annotation = create_text_annotation("Sample text", 1.0_wp, 2.0_wp)
        
        if (annotation%text /= "Sample text") then
            error stop "FAIL: Text content not set correctly"
        end if
        
        if (abs(annotation%x - 1.0_wp) > epsilon(1.0_wp)) then
            error stop "FAIL: X coordinate not set correctly"
        end if
        
        if (abs(annotation%y - 2.0_wp) > epsilon(1.0_wp)) then
            error stop "FAIL: Y coordinate not set correctly"
        end if
        
        print *, "PASS: Basic text annotation creation test"
    end subroutine test_basic_text_annotation_creation

    subroutine test_data_coordinate_positioning()
        !! GIVEN: Text annotation in data coordinates
        !! WHEN: Transforming to pixel coordinates
        !! THEN: Transformation uses data bounds and plot area
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t, COORD_DATA
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: plot_area(4) = [100.0_wp, 100.0_wp, 400.0_wp, 300.0_wp]  ! x, y, width, height
        real(wp) :: data_bounds(4) = [0.0_wp, 10.0_wp, 0.0_wp, 5.0_wp]       ! xmin, xmax, ymin, ymax
        
        annotation%x = 5.0_wp  ! Middle of data range
        annotation%y = 2.5_wp  ! Middle of data range
        annotation%coord_type = COORD_DATA
        
        call transform_annotation_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        
        ! Middle of data should map to middle of plot area
        if (abs(pixel_x - 300.0_wp) > 1.0_wp) then  ! 100 + 400/2
            error stop "FAIL: Data coordinate X transformation incorrect"
        end if
        
        if (abs(pixel_y - 250.0_wp) > 1.0_wp) then  ! 100 + 300/2
            error stop "FAIL: Data coordinate Y transformation incorrect"
        end if
        
        print *, "PASS: Data coordinate positioning test"
    end subroutine test_data_coordinate_positioning

    subroutine test_figure_coordinate_positioning()
        !! GIVEN: Text annotation in figure coordinates (0-1 normalized)
        !! WHEN: Transforming to pixel coordinates
        !! THEN: Transformation uses figure dimensions
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t, COORD_FIGURE
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: figure_size(2) = [800.0_wp, 600.0_wp]  ! width, height
        
        annotation%x = 0.5_wp  ! Middle of figure
        annotation%y = 0.25_wp ! Quarter from bottom
        annotation%coord_type = COORD_FIGURE
        
        call transform_annotation_coordinates(annotation, figure_size, pixel_x, pixel_y)
        
        if (abs(pixel_x - 400.0_wp) > 1.0_wp) then  ! 800 * 0.5
            error stop "FAIL: Figure coordinate X transformation incorrect"
        end if
        
        if (abs(pixel_y - 150.0_wp) > 1.0_wp) then  ! 600 * 0.25
            error stop "FAIL: Figure coordinate Y transformation incorrect"
        end if
        
        print *, "PASS: Figure coordinate positioning test"
    end subroutine test_figure_coordinate_positioning

    subroutine test_axis_coordinate_positioning()
        !! GIVEN: Text annotation in axis coordinates (0-1 normalized to plot area)
        !! WHEN: Transforming to pixel coordinates
        !! THEN: Transformation uses plot area dimensions
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t, COORD_AXIS
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: plot_area(4) = [100.0_wp, 100.0_wp, 400.0_wp, 300.0_wp]  ! x, y, width, height
        
        annotation%x = 0.75_wp  ! 3/4 across plot area
        annotation%y = 0.33_wp  ! 1/3 up plot area
        annotation%coord_type = COORD_AXIS
        
        call transform_annotation_coordinates(annotation, plot_area, pixel_x, pixel_y)
        
        if (abs(pixel_x - 400.0_wp) > 1.0_wp) then  ! 100 + 400 * 0.75
            error stop "FAIL: Axis coordinate X transformation incorrect"
        end if
        
        if (abs(pixel_y - 199.0_wp) > 1.0_wp) then  ! 100 + 300 * 0.33
            error stop "FAIL: Axis coordinate Y transformation incorrect"
        end if
        
        print *, "PASS: Axis coordinate positioning test"
    end subroutine test_axis_coordinate_positioning

    subroutine test_figure_text_method_api()
        !! GIVEN: Figure instance
        !! WHEN: Using text method for annotation
        !! THEN: Method accepts position, text, and optional parameters
        type(figure_t) :: fig
        
        call fig%initialize(800, 600)
        
        ! Basic text placement
        call fig%text(1.5_wp, 2.5_wp, "Basic text")
        
        ! Text with coordinate type specification
        call fig%text(0.5_wp, 0.8_wp, "Figure coords", coord_type=COORD_FIGURE)
        
        ! Text with typography parameters
        call fig%text(1.0_wp, 3.0_wp, "Styled text", font_size=14.0_wp, rotation=30.0_wp, alignment='center')
        
        ! Text with background box
        call fig%text(2.0_wp, 1.0_wp, "Boxed text", has_bbox=.true.)
        
        print *, "PASS: Figure text method API test"
    end subroutine test_figure_text_method_api

    subroutine test_figure_annotate_method_api()
        !! GIVEN: Figure instance
        !! WHEN: Using annotate method for arrow annotation
        !! THEN: Method accepts text, xy position, xytext position, and optional parameters
        type(figure_t) :: fig
        
        call fig%initialize(800, 600)
        
        ! Basic arrow annotation
        call fig%annotate("Point of interest", [1.5_wp, 2.5_wp], [0.5_wp, 1.0_wp])
        
        ! Annotation with mixed coordinate types
        call fig%annotate("Mixed coords", [1.0_wp, 2.0_wp], [0.2_wp, 0.8_wp], &
                         xy_coord_type=COORD_DATA, xytext_coord_type=COORD_FIGURE)
        
        ! Annotation with styling
        call fig%annotate("Styled annotation", [2.0_wp, 3.0_wp], [1.0_wp, 1.5_wp], &
                         font_size=12.0_wp, alignment='center', has_bbox=.true.)
        
        print *, "PASS: Figure annotate method API test"
    end subroutine test_figure_annotate_method_api

    subroutine test_global_text_function_api()
        !! GIVEN: Global text function
        !! WHEN: Using text function without figure instance
        !! THEN: Function operates on current figure context
        
        ! Basic global text placement
        call text(1.5_wp, 2.5_wp, "Global text")
        
        ! Global text with parameters
        call text(0.5_wp, 0.8_wp, "Global styled", font_size=16.0_wp, rotation=45.0_wp)
        
        print *, "PASS: Global text function API test"
    end subroutine test_global_text_function_api

    subroutine test_global_annotate_function_api()
        !! GIVEN: Global annotate function
        !! WHEN: Using annotate function without figure instance
        !! THEN: Function operates on current figure context
        
        ! Basic global annotation
        call annotate("Global annotation", [1.5_wp, 2.5_wp], [0.5_wp, 1.0_wp])
        
        print *, "PASS: Global annotate function API test"
    end subroutine test_global_annotate_function_api

    subroutine test_typography_font_size()
        !! GIVEN: Text annotation with font size specification
        !! WHEN: Rendering text
        !! THEN: Text appears at specified size
        use fortplot_annotations, only: text_annotation_t
        
        type(text_annotation_t) :: annotation
        real(wp) :: calculated_width, calculated_height
        
        annotation%text = "Test"
        annotation%font_size = 24.0_wp
        
        call calculate_annotation_metrics(annotation, calculated_width, calculated_height)
        
        ! Larger font size should produce larger text dimensions
        if (calculated_width <= 0.0_wp .or. calculated_height <= 0.0_wp) then
            error stop "FAIL: Font size calculation produces invalid dimensions"
        end if
        
        print *, "PASS: Typography font size test"
    end subroutine test_typography_font_size

    subroutine test_typography_alignment()
        !! GIVEN: Text annotation with alignment specification
        !! WHEN: Calculating text position
        !! THEN: Text is positioned according to alignment
        use fortplot_annotations, only: text_annotation_t, calculate_aligned_position
        
        type(text_annotation_t) :: annotation
        real(wp) :: adjusted_x, adjusted_y
        real(wp) :: text_width = 100.0_wp, text_height = 20.0_wp
        
        annotation%x = 200.0_wp
        annotation%y = 150.0_wp
        annotation%alignment = 'center'
        
        call calculate_aligned_position(annotation, text_width, text_height, adjusted_x, adjusted_y)
        
        ! Center alignment should offset by half text width
        if (abs(adjusted_x - (200.0_wp - text_width/2)) > 1.0_wp) then
            error stop "FAIL: Center alignment X position incorrect"
        end if
        
        print *, "PASS: Typography alignment test"
    end subroutine test_typography_alignment

    subroutine test_typography_rotation()
        !! GIVEN: Text annotation with rotation angle
        !! WHEN: Rendering rotated text
        !! THEN: Text appears at specified rotation
        use fortplot_annotations, only: text_annotation_t, calculate_rotated_bounds
        
        type(text_annotation_t) :: annotation
        real(wp) :: rotated_bounds(4)  ! xmin, xmax, ymin, ymax
        
        annotation%text = "Rotated text"
        annotation%rotation = 45.0_wp
        
        call calculate_rotated_bounds(annotation, rotated_bounds)
        
        ! Rotated text should have non-zero bounding box
        if (rotated_bounds(2) <= rotated_bounds(1) .or. rotated_bounds(4) <= rotated_bounds(3)) then
            error stop "FAIL: Rotated text bounds calculation invalid"
        end if
        
        print *, "PASS: Typography rotation test"
    end subroutine test_typography_rotation

    subroutine test_png_backend_text_rendering()
        !! GIVEN: Text annotation for PNG backend
        !! WHEN: Rendering to PNG format
        !! THEN: Text appears correctly in PNG output
        type(figure_t) :: fig
        
        call fig%initialize(400, 300)
        call fig%text(1.0_wp, 1.0_wp, "PNG test text")
        call fig%savefig("test_annotation_png.png")
        
        ! Verify PNG file was created and contains text data
        call verify_png_contains_text("test_annotation_png.png", "PNG test text")
        
        print *, "PASS: PNG backend text rendering test"
    end subroutine test_png_backend_text_rendering

    subroutine test_pdf_backend_text_rendering()
        !! GIVEN: Text annotation for PDF backend
        !! WHEN: Rendering to PDF format
        !! THEN: Text appears correctly in PDF output
        type(figure_t) :: fig
        
        call fig%initialize(400, 300)
        call fig%text(1.0_wp, 1.0_wp, "PDF test text")
        call fig%savefig("test_annotation_pdf.pdf")
        
        ! Verify PDF file was created and contains text data
        call verify_pdf_contains_text("test_annotation_pdf.pdf", "PDF test text")
        
        print *, "PASS: PDF backend text rendering test"
    end subroutine test_pdf_backend_text_rendering

    subroutine test_ascii_backend_text_rendering()
        !! GIVEN: Text annotation for ASCII backend
        !! WHEN: Rendering to ASCII format
        !! THEN: Text appears correctly in ASCII output
        type(figure_t) :: fig
        
        call fig%initialize(40, 20)
        call fig%text(1.0_wp, 1.0_wp, "ASCII test")
        call fig%savefig("test_annotation_ascii.txt")
        
        ! Verify ASCII file was created and contains text data
        call verify_ascii_contains_text("test_annotation_ascii.txt", "ASCII test")
        
        print *, "PASS: ASCII backend text rendering test"
    end subroutine test_ascii_backend_text_rendering

    subroutine test_coordinate_transformation_accuracy()
        !! GIVEN: Known coordinate values and plot configuration
        !! WHEN: Transforming coordinates
        !! THEN: Transformations are mathematically accurate
        use fortplot_annotations, only: transform_annotation_coordinates, text_annotation_t, COORD_DATA
        
        type(text_annotation_t) :: annotation
        real(wp) :: pixel_x, pixel_y
        real(wp) :: plot_area(4) = [50.0_wp, 50.0_wp, 300.0_wp, 200.0_wp]
        real(wp) :: data_bounds(4) = [0.0_wp, 10.0_wp, 0.0_wp, 5.0_wp]
        
        ! Test multiple coordinate points
        annotation%coord_type = COORD_DATA
        
        ! Test minimum data point
        annotation%x = 0.0_wp
        annotation%y = 0.0_wp
        call transform_annotation_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        
        if (abs(pixel_x - 50.0_wp) > 0.1_wp) then
            error stop "FAIL: Minimum X coordinate transformation inaccurate"
        end if
        
        if (abs(pixel_y - 50.0_wp) > 0.1_wp) then
            error stop "FAIL: Minimum Y coordinate transformation inaccurate"
        end if
        
        ! Test maximum data point
        annotation%x = 10.0_wp
        annotation%y = 5.0_wp
        call transform_annotation_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        
        if (abs(pixel_x - 350.0_wp) > 0.1_wp) then
            error stop "FAIL: Maximum X coordinate transformation inaccurate"
        end if
        
        if (abs(pixel_y - 250.0_wp) > 0.1_wp) then
            error stop "FAIL: Maximum Y coordinate transformation inaccurate"
        end if
        
        print *, "PASS: Coordinate transformation accuracy test"
    end subroutine test_coordinate_transformation_accuracy

    subroutine test_text_metrics_calculation()
        !! GIVEN: Text annotation with known content
        !! WHEN: Calculating text dimensions
        !! THEN: Dimensions are consistent and reasonable
        use fortplot_annotations, only: text_annotation_t
        
        type(text_annotation_t) :: annotation
        real(wp) :: width1, height1, width2, height2
        
        annotation%text = "Test"
        annotation%font_size = 16.0_wp
        call calculate_annotation_metrics(annotation, width1, height1)
        
        annotation%text = "Test Test"  ! Longer text
        call calculate_annotation_metrics(annotation, width2, height2)
        
        ! Longer text should be wider but same height
        if (width2 <= width1) then
            error stop "FAIL: Longer text should be wider"
        end if
        
        if (abs(height2 - height1) > 1.0_wp) then
            error stop "FAIL: Text height should be consistent for same font size"
        end if
        
        print *, "PASS: Text metrics calculation test"
    end subroutine test_text_metrics_calculation

    subroutine test_multiple_annotation_handling()
        !! GIVEN: Figure with multiple text annotations
        !! WHEN: Rendering all annotations
        !! THEN: All annotations appear correctly without interference
        type(figure_t) :: fig
        
        call fig%initialize(600, 400)
        
        ! Add multiple annotations with different properties
        call fig%text(1.0_wp, 1.0_wp, "Annotation 1")
        call fig%text(2.0_wp, 2.0_wp, "Annotation 2", font_size=20.0_wp)
        call fig%text(0.5_wp, 0.5_wp, "Figure coords", coord_type=COORD_FIGURE)
        call fig%annotate("Arrow annotation", [3.0_wp, 3.0_wp], [2.5_wp, 2.5_wp])
        
        call fig%savefig("test_multiple_annotations.png")
        
        ! Verify all annotations are present
        call verify_png_contains_text("test_multiple_annotations.png", "Annotation 1")
        call verify_png_contains_text("test_multiple_annotations.png", "Annotation 2")
        
        print *, "PASS: Multiple annotation handling test"
    end subroutine test_multiple_annotation_handling

    subroutine test_annotation_clipping_bounds()
        !! GIVEN: Text annotation positioned outside plot area
        !! WHEN: Rendering annotation
        !! THEN: Annotation is clipped or handled gracefully
        use fortplot_annotations, only: text_annotation_t, is_annotation_visible
        
        type(text_annotation_t) :: annotation
        real(wp) :: plot_area(4) = [100.0_wp, 100.0_wp, 300.0_wp, 200.0_wp]
        logical :: visible
        
        ! Annotation outside plot area
        annotation%x = -1000.0_wp
        annotation%y = -1000.0_wp
        annotation%coord_type = COORD_DATA
        
        visible = is_annotation_visible(annotation, plot_area)
        
        if (visible) then
            error stop "FAIL: Annotation outside plot area should not be visible"
        end if
        
        print *, "PASS: Annotation clipping bounds test"
    end subroutine test_annotation_clipping_bounds

    subroutine test_error_handling_invalid_coordinates()
        !! GIVEN: Text annotation with invalid coordinate values
        !! WHEN: Processing annotation
        !! THEN: Error is handled gracefully with informative message
        use fortplot_annotations, only: text_annotation_t, validate_annotation_coordinates
        
        type(text_annotation_t) :: annotation
        logical :: valid
        character(len=256) :: error_message
        
        ! Test NaN coordinates
        annotation%x = 0.0_wp / 0.0_wp  ! NaN
        annotation%y = 1.0_wp
        annotation%coord_type = COORD_DATA
        
        call validate_annotation_coordinates(annotation, valid, error_message)
        
        if (valid) then
            error stop "FAIL: NaN coordinates should be invalid"
        end if
        
        if (len_trim(error_message) == 0) then
            error stop "FAIL: Error message should be provided for invalid coordinates"
        end if
        
        print *, "PASS: Error handling invalid coordinates test"
    end subroutine test_error_handling_invalid_coordinates

    subroutine test_error_handling_invalid_text_parameters()
        !! GIVEN: Text annotation with invalid parameters
        !! WHEN: Processing annotation
        !! THEN: Error is handled gracefully with informative message
        use fortplot_annotations, only: text_annotation_t, validate_annotation_parameters
        
        type(text_annotation_t) :: annotation
        logical :: valid
        character(len=256) :: error_message
        
        ! Test negative font size
        annotation%text = "Valid text"
        annotation%font_size = -5.0_wp
        annotation%alignment = 'center'
        
        call validate_annotation_parameters(annotation, valid, error_message)
        
        if (valid) then
            error stop "FAIL: Negative font size should be invalid"
        end if
        
        if (len_trim(error_message) == 0) then
            error stop "FAIL: Error message should be provided for invalid parameters"
        end if
        
        print *, "PASS: Error handling invalid text parameters test"
    end subroutine test_error_handling_invalid_text_parameters

    subroutine test_performance_multiple_annotations()
        !! GIVEN: Large number of text annotations
        !! WHEN: Rendering all annotations
        !! THEN: Performance is acceptable and memory usage is reasonable
        type(figure_t) :: fig
        integer :: i
        real(wp) :: start_time, end_time
        
        call fig%initialize(800, 600)
        
        call cpu_time(start_time)
        
        ! Add many annotations to test performance
        do i = 1, 1000
            call fig%text(real(i, wp) * 0.01_wp, real(i, wp) * 0.01_wp, "Annotation")
        end do
        
        call fig%savefig("test_performance_annotations.png")
        
        call cpu_time(end_time)
        
        ! Performance should be reasonable (less than 5 seconds for 1000 annotations)
        if (end_time - start_time > 5.0_wp) then
            error stop "FAIL: Performance test exceeded 5 second limit"
        end if
        
        print *, "PASS: Performance multiple annotations test"
    end subroutine test_performance_multiple_annotations

    subroutine test_background_box_rendering()
        !! GIVEN: Text annotation with background box
        !! WHEN: Rendering annotation
        !! THEN: Background box appears with correct styling
        type(figure_t) :: fig
        
        call fig%initialize(400, 300)
        call fig%text(1.0_wp, 1.0_wp, "Boxed text", has_bbox=.true.)
        call fig%savefig("test_background_box.png")
        
        ! Verify background box is rendered
        call verify_png_has_background_box("test_background_box.png")
        
        print *, "PASS: Background box rendering test"
    end subroutine test_background_box_rendering

    subroutine test_arrow_annotation_support()
        !! GIVEN: Arrow annotation with text and arrow
        !! WHEN: Rendering annotation
        !! THEN: Both text and arrow appear correctly
        type(figure_t) :: fig
        
        call fig%initialize(400, 300)
        call fig%annotate("Arrow test", [2.0_wp, 2.0_wp], [1.0_wp, 1.0_wp])
        call fig%savefig("test_arrow_annotation.png")
        
        ! Verify both text and arrow are rendered
        call verify_png_contains_text("test_arrow_annotation.png", "Arrow test")
        call verify_png_has_arrow("test_arrow_annotation.png")
        
        print *, "PASS: Arrow annotation support test"
    end subroutine test_arrow_annotation_support

    subroutine test_backend_consistency_validation()
        !! GIVEN: Same annotation rendered on different backends
        !! WHEN: Comparing outputs
        !! THEN: Text positioning and appearance are consistent
        type(figure_t) :: fig
        
        call fig%initialize(400, 300)
        call fig%text(1.0_wp, 1.0_wp, "Consistency test", font_size=16.0_wp)
        
        call fig%savefig("test_consistency.png")
        call fig%savefig("test_consistency.pdf")
        call fig%savefig("test_consistency.txt")
        
        ! Verify consistent text positioning across backends
        call verify_backend_consistency("test_consistency")
        
        print *, "PASS: Backend consistency validation test"
    end subroutine test_backend_consistency_validation

    subroutine test_memory_management()
        !! GIVEN: Text annotations with dynamic string allocation
        !! WHEN: Creating and destroying annotations
        !! THEN: Memory is managed correctly without leaks
        use fortplot_annotations, only: text_annotation_t, create_text_annotation, destroy_text_annotation
        
        type(text_annotation_t) :: annotation
        integer :: i
        
        ! Test repeated allocation/deallocation
        do i = 1, 100
            annotation = create_text_annotation("Dynamic text", 1.0_wp, 1.0_wp)
            call destroy_text_annotation(annotation)
        end do
        
        print *, "PASS: Memory management test"
    end subroutine test_memory_management

    ! Helper subroutines (these would fail initially as they require implementation)
    
    subroutine calculate_annotation_metrics(annotation, width, height)
        use fortplot_annotations, only: text_annotation_t
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(out) :: width, height
        
        ! This should use the text metrics calculation system
        error stop "Helper subroutine calculate_annotation_metrics not implemented"
    end subroutine calculate_annotation_metrics

    subroutine verify_png_contains_text(filename, expected_text)
        character(len=*), intent(in) :: filename, expected_text
        
        ! This should verify PNG contains specified text
        error stop "Helper subroutine verify_png_contains_text not implemented"
    end subroutine verify_png_contains_text

    subroutine verify_pdf_contains_text(filename, expected_text)
        character(len=*), intent(in) :: filename, expected_text
        
        ! This should verify PDF contains specified text
        error stop "Helper subroutine verify_pdf_contains_text not implemented"
    end subroutine verify_pdf_contains_text

    subroutine verify_ascii_contains_text(filename, expected_text)
        character(len=*), intent(in) :: filename, expected_text
        
        ! This should verify ASCII contains specified text
        error stop "Helper subroutine verify_ascii_contains_text not implemented"
    end subroutine verify_ascii_contains_text

    subroutine verify_png_has_background_box(filename)
        character(len=*), intent(in) :: filename
        
        ! This should verify PNG has background box rendered
        error stop "Helper subroutine verify_png_has_background_box not implemented"
    end subroutine verify_png_has_background_box

    subroutine verify_png_has_arrow(filename)
        character(len=*), intent(in) :: filename
        
        ! This should verify PNG has arrow rendered
        error stop "Helper subroutine verify_png_has_arrow not implemented"
    end subroutine verify_png_has_arrow

    subroutine verify_backend_consistency(base_filename)
        character(len=*), intent(in) :: base_filename
        
        ! This should verify consistent positioning across backends
        error stop "Helper subroutine verify_backend_consistency not implemented"
    end subroutine verify_backend_consistency

end program test_text_annotations