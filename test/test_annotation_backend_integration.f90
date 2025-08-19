program test_annotation_backend_integration
    !! Backend-specific text annotation rendering tests
    !! Tests PNG, PDF, and ASCII backend text rendering capabilities
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none

    call test_png_backend_text_quality()
    call test_png_backend_rotation_rendering()
    call test_png_backend_font_sizing()
    call test_pdf_backend_text_quality()
    call test_pdf_backend_font_embedding()
    call test_pdf_backend_special_characters()
    call test_ascii_backend_character_mapping()
    call test_ascii_backend_alignment_approximation()
    call test_ascii_backend_box_drawing()
    call test_backend_coordinate_consistency()
    call test_backend_performance_comparison()
    call test_backend_memory_usage()

contains

    subroutine test_png_backend_text_quality()
        !! GIVEN: PNG backend text rendering
        !! WHEN: Rendering text at various sizes and styles
        !! THEN: Text quality meets visual standards
        type(figure_t) :: fig
        
        call fig%initialize(600, 400)
        
        ! Test various font sizes
        call fig%text(0.1_wp, 0.8_wp, "Small text", font_size=8.0_wp)
        call fig%text(0.1_wp, 0.6_wp, "Medium text", font_size=16.0_wp)
        call fig%text(0.1_wp, 0.4_wp, "Large text", font_size=24.0_wp)
        call fig%text(0.1_wp, 0.2_wp, "Extra large text", font_size=32.0_wp)
        
        call fig%savefig("test_png_text_quality.png")
        
        ! Verify PNG text quality
        call verify_png_text_clarity("test_png_text_quality.png")
        call verify_png_text_sizing("test_png_text_quality.png")
        
        print *, "PASS: PNG backend text quality test"
    end subroutine test_png_backend_text_quality

    subroutine test_png_backend_rotation_rendering()
        !! GIVEN: PNG backend with rotated text
        !! WHEN: Rendering text at various rotation angles
        !! THEN: Rotated text renders correctly with proper positioning
        type(figure_t) :: fig
        
        call fig%initialize(500, 500)
        
        ! Test various rotation angles
        call fig%text(0.5_wp, 0.5_wp, "0 degrees", rotation=0.0_wp, alignment='center')
        call fig%text(0.3_wp, 0.7_wp, "45 degrees", rotation=45.0_wp, alignment='center')
        call fig%text(0.2_wp, 0.5_wp, "90 degrees", rotation=90.0_wp, alignment='center')
        call fig%text(0.3_wp, 0.3_wp, "-45 degrees", rotation=-45.0_wp, alignment='center')
        call fig%text(0.7_wp, 0.3_wp, "135 degrees", rotation=135.0_wp, alignment='center')
        
        call fig%savefig("test_png_rotation.png")
        
        ! Verify rotation accuracy
        call verify_png_rotation_accuracy("test_png_rotation.png")
        
        print *, "PASS: PNG backend rotation rendering test"
    end subroutine test_png_backend_rotation_rendering

    subroutine test_png_backend_font_sizing()
        !! GIVEN: PNG backend with font size specifications
        !! WHEN: Rendering text at different sizes
        !! THEN: Font sizes are accurate and proportional
        type(figure_t) :: fig
        real(wp) :: sizes(5) = [8.0_wp, 12.0_wp, 16.0_wp, 20.0_wp, 24.0_wp]
        integer :: i
        
        call fig%initialize(400, 600)
        
        do i = 1, size(sizes)
            call fig%text(0.1_wp, 0.9_wp - real(i-1, wp) * 0.15_wp, &
                         "Font size test", font_size=sizes(i))
        end do
        
        call fig%savefig("test_png_font_sizing.png")
        
        ! Verify progressive font sizing
        call verify_png_font_progression("test_png_font_sizing.png", sizes)
        
        print *, "PASS: PNG backend font sizing test"
    end subroutine test_png_backend_font_sizing

    subroutine test_pdf_backend_text_quality()
        !! GIVEN: PDF backend text rendering
        !! WHEN: Rendering text with typography features
        !! THEN: PDF text is vector-based and scalable
        type(figure_t) :: fig
        
        call fig%initialize(600, 400)
        
        call fig%text(0.1_wp, 0.8_wp, "PDF vector text", font_size=16.0_wp)
        call fig%text(0.1_wp, 0.6_wp, "Scalable typography", font_size=20.0_wp)
        call fig%text(0.1_wp, 0.4_wp, "High resolution", font_size=14.0_wp)
        
        call fig%savefig("test_pdf_text_quality.pdf")
        
        ! Verify PDF text is vector-based
        call verify_pdf_vector_text("test_pdf_text_quality.pdf")
        
        print *, "PASS: PDF backend text quality test"
    end subroutine test_pdf_backend_text_quality

    subroutine test_pdf_backend_font_embedding()
        !! GIVEN: PDF backend with custom fonts
        !! WHEN: Rendering text with font specifications
        !! THEN: Fonts are properly embedded or substituted
        type(figure_t) :: fig
        
        call fig%initialize(500, 300)
        
        call fig%text(0.1_wp, 0.7_wp, "Default font text")
        call fig%text(0.1_wp, 0.5_wp, "System font text", font_family='Arial')
        call fig%text(0.1_wp, 0.3_wp, "Fallback font text", font_family='NonExistentFont')
        
        call fig%savefig("test_pdf_fonts.pdf")
        
        ! Verify font handling
        call verify_pdf_font_embedding("test_pdf_fonts.pdf")
        
        print *, "PASS: PDF backend font embedding test"
    end subroutine test_pdf_backend_font_embedding

    subroutine test_pdf_backend_special_characters()
        !! GIVEN: PDF backend with special characters
        !! WHEN: Rendering Unicode and special symbols
        !! THEN: Characters are rendered correctly
        type(figure_t) :: fig
        
        call fig%initialize(500, 400)
        
        call fig%text(0.1_wp, 0.8_wp, "Unicode: αβγδε")
        call fig%text(0.1_wp, 0.6_wp, "Math: ∫∑∏√∞")
        call fig%text(0.1_wp, 0.4_wp, "Symbols: ±×÷≠≤≥")
        call fig%text(0.1_wp, 0.2_wp, "Special: quotes-dashes")
        
        call fig%savefig("test_pdf_unicode.pdf")
        
        ! Verify Unicode support
        call verify_pdf_unicode_support("test_pdf_unicode.pdf")
        
        print *, "PASS: PDF backend special characters test"
    end subroutine test_pdf_backend_special_characters

    subroutine test_ascii_backend_character_mapping()
        !! GIVEN: ASCII backend text rendering
        !! WHEN: Rendering text to character grid
        !! THEN: Text maps correctly to ASCII characters
        type(figure_t) :: fig
        
        call fig%initialize(40, 20)  ! Small ASCII grid
        
        call fig%text(0.2_wp, 0.8_wp, "ASCII")
        call fig%text(0.2_wp, 0.6_wp, "TEXT")
        call fig%text(0.2_wp, 0.4_wp, "DEMO")
        
        call fig%savefig("test_ascii_mapping.txt")
        
        ! Verify ASCII character mapping
        call verify_ascii_character_placement("test_ascii_mapping.txt")
        
        print *, "PASS: ASCII backend character mapping test"
    end subroutine test_ascii_backend_character_mapping

    subroutine test_ascii_backend_alignment_approximation()
        !! GIVEN: ASCII backend with text alignment
        !! WHEN: Rendering aligned text
        !! THEN: Alignment is approximated correctly in character grid
        type(figure_t) :: fig
        
        call fig%initialize(60, 15)
        
        call fig%text(0.5_wp, 0.8_wp, "CENTER", alignment='center')
        call fig%text(0.1_wp, 0.6_wp, "LEFT", alignment='left')
        call fig%text(0.9_wp, 0.4_wp, "RIGHT", alignment='right')
        
        call fig%savefig("test_ascii_alignment.txt")
        
        ! Verify ASCII alignment approximation
        call verify_ascii_alignment("test_ascii_alignment.txt")
        
        print *, "PASS: ASCII backend alignment approximation test"
    end subroutine test_ascii_backend_alignment_approximation

    subroutine test_ascii_backend_box_drawing()
        !! GIVEN: ASCII backend with background boxes
        !! WHEN: Rendering text with background
        !! THEN: ASCII box characters are used appropriately
        type(figure_t) :: fig
        
        call fig%initialize(50, 10)
        
        call fig%text(0.3_wp, 0.6_wp, "BOXED", has_bbox=.true.)
        
        call fig%savefig("test_ascii_boxes.txt")
        
        ! Verify ASCII box drawing
        call verify_ascii_box_characters("test_ascii_boxes.txt")
        
        print *, "PASS: ASCII backend box drawing test"
    end subroutine test_ascii_backend_box_drawing

    subroutine test_backend_coordinate_consistency()
        !! GIVEN: Same annotation coordinates for all backends
        !! WHEN: Rendering on PNG, PDF, and ASCII
        !! THEN: Text appears at equivalent positions
        type(figure_t) :: fig
        
        call fig%initialize(400, 300)
        call fig%add_plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
        call fig%text(0.5_wp, 0.5_wp, "Center", coord_type=COORD_DATA)
        call fig%text(0.1_wp, 0.9_wp, "Corner", coord_type=COORD_DATA)
        
        call fig%savefig("test_backend_consistency.png")
        call fig%savefig("test_backend_consistency.pdf")
        call fig%savefig("test_backend_consistency.txt")
        
        ! Verify coordinate consistency across backends
        call verify_cross_backend_positioning("test_backend_consistency")
        
        print *, "PASS: Backend coordinate consistency test"
    end subroutine test_backend_coordinate_consistency

    subroutine test_backend_performance_comparison()
        !! GIVEN: Multiple text annotations for different backends
        !! WHEN: Rendering performance is measured
        !! THEN: Performance differences are within acceptable ranges
        type(figure_t) :: fig
        real(wp) :: start_time, end_time, png_time, pdf_time, ascii_time
        integer :: i
        
        ! PNG performance test
        call fig%initialize(600, 400)
        do i = 1, 100
            call fig%text(real(i, wp) * 0.005_wp, 0.5_wp, "Performance test")
        end do
        
        call cpu_time(start_time)
        call fig%savefig("test_png_performance.png")
        call cpu_time(end_time)
        png_time = end_time - start_time
        
        ! PDF performance test
        call fig%initialize(600, 400)
        do i = 1, 100
            call fig%text(real(i, wp) * 0.005_wp, 0.5_wp, "Performance test")
        end do
        
        call cpu_time(start_time)
        call fig%savefig("test_pdf_performance.pdf")
        call cpu_time(end_time)
        pdf_time = end_time - start_time
        
        ! ASCII performance test
        call fig%initialize(80, 25)
        do i = 1, 100
            call fig%text(real(i, wp) * 0.01_wp, 0.5_wp, "Perf")
        end do
        
        call cpu_time(start_time)
        call fig%savefig("test_ascii_performance.txt")
        call cpu_time(end_time)
        ascii_time = end_time - start_time
        
        ! Verify reasonable performance differences
        if (png_time > 10.0_wp .or. pdf_time > 10.0_wp .or. ascii_time > 10.0_wp) then
            error stop "FAIL: Backend performance exceeds 10 second limit"
        end if
        
        print *, "PASS: Backend performance comparison test"
    end subroutine test_backend_performance_comparison

    subroutine test_backend_memory_usage()
        !! GIVEN: Text annotations with different backends
        !! WHEN: Creating and destroying figures
        !! THEN: Memory usage is reasonable and stable
        type(figure_t) :: fig
        integer :: i
        
        ! Test memory stability with repeated operations
        do i = 1, 50
            call fig%initialize(400, 300)
            call fig%text(0.5_wp, 0.5_wp, "Memory test")
            call fig%savefig("test_memory_png.png")
            call fig%savefig("test_memory_pdf.pdf")
            call fig%savefig("test_memory_ascii.txt")
            ! Figure should be automatically cleaned up
        end do
        
        print *, "PASS: Backend memory usage test"
    end subroutine test_backend_memory_usage

    ! Helper subroutines (these would fail initially as they require implementation)
    
    subroutine verify_png_text_clarity(filename)
        character(len=*), intent(in) :: filename
        error stop "Helper subroutine verify_png_text_clarity not implemented"
    end subroutine verify_png_text_clarity

    subroutine verify_png_text_sizing(filename)
        character(len=*), intent(in) :: filename
        error stop "Helper subroutine verify_png_text_sizing not implemented"
    end subroutine verify_png_text_sizing

    subroutine verify_png_rotation_accuracy(filename)
        character(len=*), intent(in) :: filename
        error stop "Helper subroutine verify_png_rotation_accuracy not implemented"
    end subroutine verify_png_rotation_accuracy

    subroutine verify_png_font_progression(filename, sizes)
        character(len=*), intent(in) :: filename
        real(wp), intent(in) :: sizes(:)
        error stop "Helper subroutine verify_png_font_progression not implemented"
    end subroutine verify_png_font_progression

    subroutine verify_pdf_vector_text(filename)
        character(len=*), intent(in) :: filename
        error stop "Helper subroutine verify_pdf_vector_text not implemented"
    end subroutine verify_pdf_vector_text

    subroutine verify_pdf_font_embedding(filename)
        character(len=*), intent(in) :: filename
        error stop "Helper subroutine verify_pdf_font_embedding not implemented"
    end subroutine verify_pdf_font_embedding

    subroutine verify_pdf_unicode_support(filename)
        character(len=*), intent(in) :: filename
        error stop "Helper subroutine verify_pdf_unicode_support not implemented"
    end subroutine verify_pdf_unicode_support

    subroutine verify_ascii_character_placement(filename)
        character(len=*), intent(in) :: filename
        error stop "Helper subroutine verify_ascii_character_placement not implemented"
    end subroutine verify_ascii_character_placement

    subroutine verify_ascii_alignment(filename)
        character(len=*), intent(in) :: filename
        error stop "Helper subroutine verify_ascii_alignment not implemented"
    end subroutine verify_ascii_alignment

    subroutine verify_ascii_box_characters(filename)
        character(len=*), intent(in) :: filename
        error stop "Helper subroutine verify_ascii_box_characters not implemented"
    end subroutine verify_ascii_box_characters

    subroutine verify_cross_backend_positioning(base_filename)
        character(len=*), intent(in) :: base_filename
        error stop "Helper subroutine verify_cross_backend_positioning not implemented"
    end subroutine verify_cross_backend_positioning

end program test_annotation_backend_integration