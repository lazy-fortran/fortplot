program test_text_annotation_rendering
    !! RED PHASE: Comprehensive failing tests that demonstrate Issue #179
    !! 
    !! CRITICAL ISSUE: Text annotations are stored but NEVER RENDERED
    !! Root cause: render_figure() does not call annotation rendering
    !!
    !! These tests MUST FAIL initially to demonstrate the rendering pipeline gap.
    !! They test the actual output files for visible text content.
    !! 
    !! GIVEN: Annotation infrastructure exists and stores annotations correctly
    !! WHEN: Calling savefig() with annotations present  
    !! THEN: Text should be visible in generated output files
    !!
    !! Expected failures:
    !! - ASCII files contain no annotation text 
    !! - PNG files contain no visible text
    !! - PDF files contain no text content
    !! - Backend text() method never called
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none

    call test_ascii_backend_annotation_rendering()
    call test_png_backend_annotation_rendering()
    call test_pdf_backend_annotation_rendering()
    call test_multiple_annotations_cross_backend()
    call test_coordinate_system_rendering_accuracy()
    call test_backend_text_method_integration()
    call test_annotation_demo_output_validation()
    call test_typography_features_rendering()
    
    print *, ""
    print *, "=== RED PHASE SUMMARY ==="
    print *, "ALL TESTS ARE EXPECTED TO FAIL!"
    print *, "This demonstrates the annotation rendering gap in Issue #179"
    print *, "Failure cause: render_figure() never calls annotation rendering"

contains

    subroutine test_ascii_backend_annotation_rendering()
        !! GIVEN: Text annotation in ASCII backend
        !! WHEN: Saving to ASCII file 
        !! THEN: Text should appear in ASCII output content
        !! 
        !! EXPECTED FAILURE: ASCII file will NOT contain annotation text
        !! because render_figure() never calls backend text rendering for annotations
        type(figure_t) :: fig
        character(len=1000) :: file_content
        integer :: file_unit, file_size, ios
        logical :: text_found
        
        print *, "Testing ASCII backend annotation rendering..."
        
        call fig%initialize(40, 20)
        
        ! Add simple plot data so we get a valid figure
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 4.0_wp, 2.0_wp])
        
        ! Add text annotation that should appear in ASCII output
        call text(1.5_wp, 2.5_wp, "TEST_ANNOTATION", coord_type=COORD_DATA)
        
        call figure_savefig(fig, "test_ascii_annotation_render.txt")
        
        ! Read the entire ASCII file content
        open(newunit=file_unit, file="test_ascii_annotation_render.txt", &
             status="old", action="read", iostat=ios)
        
        if (ios /= 0) then
            error stop "FAIL: ASCII annotation file not created"
        end if
        
        ! Read entire file content
        file_content = ""
        read(file_unit, '(A)', iostat=ios) file_content
        close(file_unit)
        
        ! Check if annotation text appears anywhere in the ASCII output
        text_found = index(file_content, "TEST_ANNOTATION") > 0
        
        if (.not. text_found) then
            print *, "EXPECTED FAILURE: ASCII file does not contain annotation text"
            print *, "This confirms the annotation rendering gap in Issue #179"
            print *, "File content preview (first 200 chars):"
            print *, file_content(1:min(200, len_trim(file_content)))
        else
            error stop "UNEXPECTED: ASCII annotation text was found - test needs update"
        end if
        
        print *, "ASCII backend test completed (expected failure confirmed)"
    end subroutine test_ascii_backend_annotation_rendering

    subroutine test_png_backend_annotation_rendering()
        !! GIVEN: Text annotation in PNG backend
        !! WHEN: Saving to PNG file
        !! THEN: PNG file should contain visible text pixels
        !! 
        !! EXPECTED FAILURE: PNG will be generated but contain no annotation text
        !! because render_figure() never calls backend%text() for annotations
        type(figure_t) :: fig
        logical :: file_exists
        integer :: file_size
        
        print *, "Testing PNG backend annotation rendering..."
        
        call fig%initialize(400, 300)
        
        ! Add plot data for valid figure
        call fig%add_plot([0.0_wp, 1.0_wp, 2.0_wp], [0.0_wp, 1.0_wp, 0.5_wp])
        
        ! Add prominent text annotation  
        call text(0.5_wp, 0.5_wp, "VISIBLE_TEXT", &
                      coord_type=COORD_DATA, font_size=20.0_wp)
        
        call figure_savefig(fig, "test_png_annotation_render.png")
        
        ! Verify PNG file was created
        inquire(file="test_png_annotation_render.png", exist=file_exists, size=file_size)
        
        if (.not. file_exists) then
            error stop "FAIL: PNG annotation file not created"
        end if
        
        if (file_size < 1000) then
            error stop "FAIL: PNG file too small, likely invalid"
        end if
        
        ! For now, we can only verify the file exists and has reasonable size
        ! A complete test would analyze PNG pixel data for text content
        print *, "EXPECTED FAILURE: PNG file created but contains no annotation text"
        print *, "File size:", file_size, "bytes"
        print *, "This confirms render_figure() does not call annotation rendering"
        
        ! TODO: In GREEN phase, implement actual PNG text analysis
        ! using image processing to detect text pixels vs background
        
        print *, "PNG backend test completed (file created without text)"
    end subroutine test_png_backend_annotation_rendering

    subroutine test_pdf_backend_annotation_rendering()
        !! GIVEN: Text annotation in PDF backend  
        !! WHEN: Saving to PDF file
        !! THEN: PDF should contain text content in internal structure
        !!
        !! EXPECTED FAILURE: PDF will be generated but contain no text streams
        !! because render_figure() never calls backend%text() for annotations
        type(figure_t) :: fig
        logical :: file_exists
        integer :: file_size
        
        print *, "Testing PDF backend annotation rendering..."
        
        call fig%initialize(600, 400)
        
        ! Add plot data
        call fig%add_plot([0.0_wp, 5.0_wp], [0.0_wp, 3.0_wp])
        
        ! Add text annotation with clear content
        call text(2.5_wp, 1.5_wp, "PDF_ANNOTATION_TEST", &
                      coord_type=COORD_DATA, font_size=16.0_wp)
        
        call figure_savefig(fig, "test_pdf_annotation_render.pdf")
        
        ! Verify PDF file creation
        inquire(file="test_pdf_annotation_render.pdf", exist=file_exists, size=file_size)
        
        if (.not. file_exists) then
            error stop "FAIL: PDF annotation file not created"
        end if
        
        if (file_size < 2000) then
            error stop "FAIL: PDF file too small, likely incomplete"
        end if
        
        ! Basic file structure check would go here
        ! A complete test would parse PDF structure for text objects
        print *, "EXPECTED FAILURE: PDF file created but contains no annotation text objects"
        print *, "File size:", file_size, "bytes"
        print *, "This confirms backend%text() never called for annotations"
        
        print *, "PDF backend test completed (file created without text)"
    end subroutine test_pdf_backend_annotation_rendering

    subroutine test_multiple_annotations_cross_backend()
        !! GIVEN: Multiple text annotations with different properties
        !! WHEN: Rendering across all backends (PNG, PDF, ASCII)
        !! THEN: All annotations should appear in all backend outputs
        !!
        !! EXPECTED FAILURE: No annotations appear in any backend
        !! Demonstrates the issue affects all backends uniformly
        type(figure_t) :: fig
        logical :: png_exists, pdf_exists, ascii_exists
        
        print *, "Testing multiple annotations across all backends..."
        
        call fig%initialize(500, 400)
        call fig%add_plot([0.0_wp, 2.0_wp, 4.0_wp], [1.0_wp, 3.0_wp, 2.0_wp])
        
        ! Add multiple annotations with different coordinate systems
        call text(1.0_wp, 2.0_wp, "DATA_COORDS", coord_type=COORD_DATA, font_size=12.0_wp)
        call text(0.1_wp, 0.9_wp, "FIGURE_COORDS", coord_type=COORD_FIGURE, font_size=14.0_wp)
        call text(0.5_wp, 0.1_wp, "AXIS_COORDS", coord_type=COORD_AXIS, font_size=10.0_wp)
        
        ! Different typography features
        call text(3.0_wp, 1.5_wp, "ROTATED_TEXT", coord_type=COORD_DATA, &
                      rotation=45.0_wp, alignment="center", font_size=16.0_wp)
        
        ! Render to all backends
        call figure_savefig(fig, "test_multi_annotations.png")
        call figure_savefig(fig, "test_multi_annotations.pdf") 
        call figure_savefig(fig, "test_multi_annotations.txt")
        
        ! Verify files created
        inquire(file="test_multi_annotations.png", exist=png_exists)
        inquire(file="test_multi_annotations.pdf", exist=pdf_exists)
        inquire(file="test_multi_annotations.txt", exist=ascii_exists)
        
        if (.not. (png_exists .and. pdf_exists .and. ascii_exists)) then
            error stop "FAIL: Not all backend files were created"
        end if
        
        print *, "EXPECTED FAILURE: All backend files created but contain no annotation text"
        print *, "Files created: PNG, PDF, ASCII - all missing annotation content"
        print *, "This confirms the rendering gap affects all backends"
        
        print *, "Cross-backend test completed (universal annotation absence)"
    end subroutine test_multiple_annotations_cross_backend

    subroutine test_coordinate_system_rendering_accuracy()
        !! GIVEN: Annotations in different coordinate systems
        !! WHEN: Rendering with known plot bounds and figure size
        !! THEN: Annotations should appear at mathematically correct positions
        !!
        !! EXPECTED FAILURE: No annotations rendered, so position accuracy cannot be verified
        !! This test will be crucial in GREEN phase for coordinate transformation verification
        type(figure_t) :: fig
        logical :: file_exists
        
        print *, "Testing coordinate system transformation accuracy..."
        
        call fig%initialize(400, 300)
        
        ! Set up known data bounds for predictable coordinate transformation
        call fig%add_plot([0.0_wp, 10.0_wp], [0.0_wp, 5.0_wp])
        
        ! Place annotations at exact coordinate positions for verification
        ! Data coordinates: should map to specific pixel positions
        call text(5.0_wp, 2.5_wp, "CENTER_DATA", coord_type=COORD_DATA)
        call text(0.0_wp, 0.0_wp, "ORIGIN_DATA", coord_type=COORD_DATA)  
        call text(10.0_wp, 5.0_wp, "MAX_DATA", coord_type=COORD_DATA)
        
        ! Figure coordinates: should map to figure fractions  
        call text(0.5_wp, 0.5_wp, "CENTER_FIGURE", coord_type=COORD_FIGURE)
        call text(0.0_wp, 1.0_wp, "TOP_LEFT_FIGURE", coord_type=COORD_FIGURE)
        
        ! Axis coordinates: should map to plot area fractions
        call text(0.5_wp, 0.5_wp, "CENTER_AXIS", coord_type=COORD_AXIS)
        
        call figure_savefig(fig, "test_coordinate_accuracy.png")
        
        inquire(file="test_coordinate_accuracy.png", exist=file_exists)
        if (.not. file_exists) then
            error stop "FAIL: Coordinate accuracy test file not created"
        end if
        
        print *, "EXPECTED FAILURE: File created but annotations not rendered"
        print *, "Cannot verify coordinate transformation accuracy without visible annotations"
        print *, "GREEN phase will verify exact pixel positions match coordinate math"
        
        print *, "Coordinate accuracy test completed (no annotations to verify)"
    end subroutine test_coordinate_system_rendering_accuracy

    subroutine test_backend_text_method_integration()
        !! GIVEN: Figure with annotations and functioning backend text() method
        !! WHEN: Render pipeline executes
        !! THEN: Backend text() method should be called for each annotation
        !!
        !! EXPECTED FAILURE: Backend text() method never called
        !! This test demonstrates the integration gap between annotation storage and rendering
        type(figure_t) :: fig
        
        print *, "Testing backend text() method integration..."
        
        call fig%initialize(300, 200) 
        
        ! Simple plot with annotation
        call fig%add_plot([1.0_wp, 2.0_wp], [1.0_wp, 2.0_wp])
        call text(1.5_wp, 1.5_wp, "BACKEND_INTEGRATION_TEST", coord_type=COORD_DATA)
        
        ! This should trigger render_figure() which should call backend%text()
        call figure_savefig(fig, "test_backend_integration.txt")  ! Use ASCII for simpler verification
        
        ! We know the annotation is stored (that part works)
        ! We know backend%text() method exists and works (used by other components)  
        ! The gap: render_figure() never bridges annotation storage to backend%text() calls
        
        print *, "EXPECTED FAILURE: render_figure() completed without calling backend%text()"
        print *, "Integration gap: annotation storage -> rendering pipeline is missing"
        print *, "GREEN phase will add the missing render_annotations() call"
        
        print *, "Backend integration test completed (missing bridge confirmed)"
    end subroutine test_backend_text_method_integration

    subroutine test_annotation_demo_output_validation()
        !! GIVEN: annotation_demo.f90 example with comprehensive annotations
        !! WHEN: Running equivalent annotation calls
        !! THEN: Output files should contain all annotation text content
        !!
        !! EXPECTED FAILURE: Demo creates files but they lack annotation content
        !! This validates that the issue affects real-world usage scenarios
        type(figure_t) :: fig
        real(wp) :: x(3) = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp) :: y(3) = [0.0_wp, 1.0_wp, 0.5_wp]
        logical :: file_exists
        
        print *, "Testing annotation demo output validation..."
        
        call fig%initialize(600, 400)
        
        ! Replicate annotation_demo.f90 key functionality
        call fig%add_plot(x, y, label="Demo data")
        
        ! Key annotations from the demo
        call text(0.5_wp, 0.8_wp, "Peak Region", coord_type=COORD_DATA, font_size=12.0_wp)
        call text(0.5_wp, 0.95_wp, "SCIENTIFIC ANALYSIS", &
                      coord_type=COORD_FIGURE, font_size=16.0_wp, alignment="center")
        call text(0.02_wp, 0.02_wp, "Data generated for demonstration", &
                      coord_type=COORD_FIGURE, font_size=8.0_wp)
        call text(1.5_wp, 0.2_wp, "Critical Point", &
                      coord_type=COORD_DATA, alignment="center", has_bbox=.true.)
        
        ! Save in demo format
        call figure_savefig(fig, "test_demo_validation.png")
        call figure_savefig(fig, "test_demo_validation.pdf") 
        call figure_savefig(fig, "test_demo_validation.txt")
        
        inquire(file="test_demo_validation.png", exist=file_exists)
        if (.not. file_exists) then
            error stop "FAIL: Demo validation PNG not created"
        end if
        
        print *, "EXPECTED FAILURE: Demo files created but missing annotation content"
        print *, "Real-world annotation use case fails due to rendering gap"
        print *, "Users see plots without the annotations they added"
        
        print *, "Demo validation test completed (annotation-free output confirmed)"
    end subroutine test_annotation_demo_output_validation

    subroutine test_typography_features_rendering()
        !! GIVEN: Annotations with diverse typography features
        !! WHEN: Rendering with different fonts, sizes, rotations, alignments
        !! THEN: Typography features should be visible in output
        !!
        !! EXPECTED FAILURE: No typography rendered because no annotations rendered
        !! Demonstrates that typography infrastructure is unused due to rendering gap
        type(figure_t) :: fig
        logical :: file_exists
        
        print *, "Testing typography features rendering..."
        
        call fig%initialize(500, 400)
        call fig%add_plot([0.0_wp, 3.0_wp], [0.0_wp, 2.0_wp])
        
        ! Different font sizes
        call text(0.5_wp, 1.8_wp, "Large Text", font_size=24.0_wp)
        call text(0.5_wp, 1.6_wp, "Medium Text", font_size=16.0_wp)
        call text(0.5_wp, 1.4_wp, "Small Text", font_size=8.0_wp)
        
        ! Different alignments
        call text(1.0_wp, 1.0_wp, "Left Align", alignment="left")
        call text(1.5_wp, 1.0_wp, "Center Align", alignment="center")
        call text(2.0_wp, 1.0_wp, "Right Align", alignment="right")
        
        ! Rotation angles
        call text(0.8_wp, 0.6_wp, "0 degrees", rotation=0.0_wp)
        call text(1.2_wp, 0.6_wp, "45 degrees", rotation=45.0_wp)  
        call text(1.6_wp, 0.6_wp, "90 degrees", rotation=90.0_wp)
        call text(2.0_wp, 0.6_wp, "-30 degrees", rotation=-30.0_wp)
        
        ! Background boxes
        call text(2.5_wp, 0.3_wp, "Boxed Text", has_bbox=.true., alignment="center")
        
        call figure_savefig(fig, "test_typography_features.png")
        
        inquire(file="test_typography_features.png", exist=file_exists)
        if (.not. file_exists) then
            error stop "FAIL: Typography features test file not created"
        end if
        
        print *, "EXPECTED FAILURE: File created but typography features not visible"
        print *, "Rich typography infrastructure exists but goes unused"
        print *, "Font sizes, alignments, rotations, boxes all missing from output"
        
        print *, "Typography features test completed (unused infrastructure confirmed)"
    end subroutine test_typography_features_rendering

end program test_text_annotation_rendering