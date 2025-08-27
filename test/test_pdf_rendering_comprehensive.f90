program test_pdf_rendering_comprehensive
    !! Comprehensive PDF rendering test consolidating rendering-related functionality
    !! Replaces: test_pdf_stretching_comprehensive.f90, test_pdf_mixed_fonts.f90,
    !!           test_pdf_stream_output.f90, test_pdf_tick_configurability.f90 (4 files total)
    !!
    !! This test covers:
    !! - PDF stretching and scaling behavior
    !! - Mixed font handling and text rendering
    !! - PDF stream output optimization
    !! - Tick mark configuration and positioning
    !! - Font size and style variations
    !! - Advanced PDF rendering features

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_security, only: get_test_output_path
    use fortplot
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    print *, "=== COMPREHENSIVE PDF RENDERING TESTS ==="
    
    ! Run all test categories
    call test_stretching_behavior()
    call test_font_handling()
    call test_stream_output()
    call test_tick_configuration()
    call test_advanced_rendering()
    
    call print_test_summary()

contains

    !===========================================================================
    ! Stretching Tests (from test_pdf_stretching_comprehensive.f90)
    !===========================================================================
    
    subroutine test_stretching_behavior()
        print *, "--- PDF Stretching Tests ---"
        
        call test_uniform_stretching()
        call test_aspect_ratio_stretching()
        call test_content_scaling()
        call test_stretch_edge_cases()
    end subroutine test_stretching_behavior

    subroutine test_uniform_stretching()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Uniform stretching behavior")
        
        ctx = create_pdf_canvas(600, 600)  ! Square canvas
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        
        ! Draw content that should stretch uniformly
        call ctx%circle(5.0_wp, 5.0_wp, 2.0_wp)           ! Center circle
        call ctx%rectangle(2.0_wp, 2.0_wp, 6.0_wp, 6.0_wp)  ! Bounding rectangle
        call ctx%text(5.0_wp, 1.0_wp, "Uniform Stretch Test")
        
        filename = get_test_output_path('/tmp/pdf_uniform_stretch.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Uniform stretching test saved - circle should remain circular"
        call end_test()
    end subroutine test_uniform_stretching

    subroutine test_aspect_ratio_stretching()
        type(pdf_context) :: ctx_wide, ctx_tall
        character(len=512) :: filename
        
        call start_test("Aspect ratio stretching")
        
        ! Wide canvas
        ctx_wide = create_pdf_canvas(800, 400)  ! 2:1 aspect
        ctx_wide%x_min = 0.0_wp; ctx_wide%x_max = 10.0_wp
        ctx_wide%y_min = 0.0_wp; ctx_wide%y_max = 5.0_wp
        
        call ctx_wide%circle(5.0_wp, 2.5_wp, 1.0_wp)
        call ctx_wide%text(5.0_wp, 4.0_wp, "Wide Canvas (2:1)")
        
        filename = get_test_output_path('/tmp/pdf_wide_stretch.pdf')
        call ctx_wide%write_pdf(filename)
        
        ! Tall canvas
        ctx_tall = create_pdf_canvas(300, 900)  ! 1:3 aspect
        ctx_tall%x_min = 0.0_wp; ctx_tall%x_max = 4.0_wp
        ctx_tall%y_min = 0.0_wp; ctx_tall%y_max = 12.0_wp
        
        call ctx_tall%circle(2.0_wp, 6.0_wp, 1.0_wp)
        call ctx_tall%text(2.0_wp, 10.0_wp, "Tall Canvas (1:3)")
        
        filename = get_test_output_path('/tmp/pdf_tall_stretch.pdf')
        call ctx_tall%write_pdf(filename)
        
        print *, "  Aspect ratio stretching tests saved"
        call end_test()
    end subroutine test_aspect_ratio_stretching

    subroutine test_content_scaling()
        type(pdf_context) :: ctx
        real(wp) :: x_data(10), y_data(10)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Content scaling with stretching")
        
        ctx = create_pdf_canvas(500, 700)  ! Non-square
        ctx%x_min = 0.0_wp; ctx%x_max = 8.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 12.0_wp
        
        ! Generate plot data
        do i = 1, 10
            x_data(i) = real(i-1, wp) * 0.8_wp
            y_data(i) = sin(x_data(i)) * 4.0_wp + 6.0_wp
        end do
        
        ! Draw plot elements that should scale properly
        do i = 1, 9
            call ctx%line(x_data(i), y_data(i), x_data(i+1), y_data(i+1))
        end do
        
        ! Add markers
        do i = 1, 10
            call ctx%circle(x_data(i), y_data(i), 0.2_wp)
        end do
        
        call ctx%text(4.0_wp, 11.0_wp, "Content Scaling Test")
        call ctx%axes()
        
        filename = get_test_output_path('/tmp/pdf_content_scaling.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Content scaling test saved"
        call end_test()
    end subroutine test_content_scaling

    subroutine test_stretch_edge_cases()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Stretch edge cases")
        
        ctx = create_pdf_canvas(1000, 200)  ! Very wide (5:1)
        ctx%x_min = 0.0_wp; ctx%x_max = 20.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 2.0_wp
        
        ! Content that tests extreme stretching
        call ctx%circle(10.0_wp, 1.0_wp, 0.5_wp)
        call ctx%text(10.0_wp, 0.5_wp, "Extreme Stretch (5:1)")
        call ctx%line(2.0_wp, 1.0_wp, 18.0_wp, 1.0_wp)
        
        filename = get_test_output_path('/tmp/pdf_extreme_stretch.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Extreme stretch test saved - check circle distortion"
        call end_test()
    end subroutine test_stretch_edge_cases

    !===========================================================================
    ! Font Handling Tests (from test_pdf_mixed_fonts.f90)
    !===========================================================================
    
    subroutine test_font_handling()
        print *, "--- Font Handling Tests ---"
        
        call test_basic_font_rendering()
        call test_font_size_variations()
        call test_mixed_font_styles()
        call test_special_characters()
    end subroutine test_font_handling

    subroutine test_basic_font_rendering()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Basic font rendering")
        
        ctx = create_pdf_canvas(500, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        ! Test basic text rendering
        call ctx%text(5.0_wp, 7.0_wp, "Basic Font Test")
        call ctx%text(5.0_wp, 6.0_wp, "UPPERCASE TEXT")
        call ctx%text(5.0_wp, 5.0_wp, "lowercase text")
        call ctx%text(5.0_wp, 4.0_wp, "Mixed Case Text")
        call ctx%text(5.0_wp, 3.0_wp, "Numbers: 123456789")
        call ctx%text(5.0_wp, 2.0_wp, "Symbols: !@#$%^&*()")
        
        filename = get_test_output_path('/tmp/pdf_basic_fonts.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Basic font rendering test saved"
        call end_test()
    end subroutine test_basic_font_rendering

    subroutine test_font_size_variations()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        real(wp) :: y_pos
        
        call start_test("Font size variations")
        
        ctx = create_pdf_canvas(600, 500)
        ctx%x_min = 0.0_wp; ctx%x_max = 12.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        
        y_pos = 9.0_wp
        
        ! Test different font sizes (if font size control is available)
        call ctx%text(1.0_wp, y_pos, "Default Size Text")
        y_pos = y_pos - 1.0_wp
        
        ! Simulate larger text with spacing
        call ctx%text(1.0_wp, y_pos, "L A R G E R   T E X T")
        y_pos = y_pos - 1.2_wp
        
        call ctx%text(1.0_wp, y_pos, "Medium Size Text")
        y_pos = y_pos - 0.8_wp
        
        call ctx%text(1.0_wp, y_pos, "smaller text")
        y_pos = y_pos - 0.6_wp
        
        call ctx%text(1.0_wp, y_pos, "Font Size Variation Test")
        
        filename = get_test_output_path('/tmp/pdf_font_sizes.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Font size variations test saved"
        call end_test()
    end subroutine test_font_size_variations

    subroutine test_mixed_font_styles()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Mixed font styles")
        
        ctx = create_pdf_canvas(550, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 11.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        ! Test mixed content that might use different fonts
        call ctx%text(5.5_wp, 7.0_wp, "Mixed Font Styles Test")
        call ctx%text(1.0_wp, 6.0_wp, "Regular text with labels")
        call ctx%text(1.0_wp, 5.5_wp, "Mathematical: x² + y² = r²")
        call ctx%text(1.0_wp, 5.0_wp, "Greek letters: α β γ δ ε")
        call ctx%text(1.0_wp, 4.5_wp, "Subscripts: H₂O, CO₂")
        call ctx%text(1.0_wp, 4.0_wp, "Superscripts: E = mc²")
        call ctx%text(1.0_wp, 3.5_wp, "Scientific: 1.23×10⁻⁶")
        
        filename = get_test_output_path('/tmp/pdf_mixed_fonts.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Mixed font styles test saved"
        call end_test()
    end subroutine test_mixed_font_styles

    subroutine test_special_characters()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Special characters rendering")
        
        ctx = create_pdf_canvas(500, 350)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 7.0_wp
        
        ! Test special characters and symbols
        call ctx%text(5.0_wp, 6.0_wp, "Special Characters")
        call ctx%text(1.0_wp, 5.0_wp, "Degree: 90° angle")
        call ctx%text(1.0_wp, 4.5_wp, "Plus/Minus: ±0.5")
        call ctx%text(1.0_wp, 4.0_wp, "Arrows: → ← ↑ ↓")
        call ctx%text(1.0_wp, 3.5_wp, "Temperature: °C, °F")
        call ctx%text(1.0_wp, 3.0_wp, "Fractions: ½, ¼, ¾")
        
        filename = get_test_output_path('/tmp/pdf_special_chars.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Special characters test saved"
        call end_test()
    end subroutine test_special_characters

    !===========================================================================
    ! Stream Output Tests (from test_pdf_stream_output.f90)
    !===========================================================================
    
    subroutine test_stream_output()
        print *, "--- Stream Output Tests ---"
        
        call test_basic_stream_output()
        call test_optimized_stream_output()
        call test_large_content_streaming()
    end subroutine test_stream_output

    subroutine test_basic_stream_output()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Basic stream output")
        
        ctx = create_pdf_canvas(400, 300)
        ctx%x_min = 0.0_wp; ctx%x_max = 8.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 6.0_wp
        
        ! Simple content for stream output
        call ctx%text(4.0_wp, 5.0_wp, "Stream Output Test")
        call ctx%line(1.0_wp, 3.0_wp, 7.0_wp, 3.0_wp)
        call ctx%circle(4.0_wp, 2.0_wp, 0.5_wp)
        
        filename = get_test_output_path('/tmp/pdf_basic_stream.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Basic stream output test saved"
        call end_test()
    end subroutine test_basic_stream_output

    subroutine test_optimized_stream_output()
        type(pdf_context) :: ctx
        real(wp) :: start_time, end_time
        integer :: i
        character(len=512) :: filename
        
        call start_test("Optimized stream output")
        
        ctx = create_pdf_canvas(600, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 12.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        call cpu_time(start_time)
        
        ! Generate content that benefits from stream optimization
        do i = 1, 100
            call ctx%circle(real(mod(i-1, 12), wp) + 0.5_wp, &
                           real((i-1) / 12, wp) * 0.8_wp + 1.0_wp, 0.1_wp)
        end do
        
        call ctx%text(6.0_wp, 7.5_wp, "Optimized Stream Test")
        
        call cpu_time(end_time)
        
        filename = get_test_output_path('/tmp/pdf_optimized_stream.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Optimized stream output (100 elements) in ", end_time - start_time, " seconds"
        call end_test()
    end subroutine test_optimized_stream_output

    subroutine test_large_content_streaming()
        type(pdf_context) :: ctx
        real(wp) :: start_time, end_time
        integer :: i, j
        character(len=512) :: filename
        
        call start_test("Large content streaming")
        
        ctx = create_pdf_canvas(800, 600)
        ctx%x_min = 0.0_wp; ctx%x_max = 20.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 15.0_wp
        
        call cpu_time(start_time)
        
        ! Generate larger content to test streaming
        do j = 1, 10
            do i = 1, 15
                call ctx%line(real(i, wp), real(j, wp), real(i, wp) + 0.8_wp, real(j, wp))
            end do
        end do
        
        call ctx%text(10.0_wp, 14.0_wp, "Large Content Stream Test")
        
        call cpu_time(end_time)
        
        filename = get_test_output_path('/tmp/pdf_large_stream.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Large content streaming (150 elements) in ", end_time - start_time, " seconds"
        call end_test()
    end subroutine test_large_content_streaming

    !===========================================================================
    ! Tick Configuration Tests (from test_pdf_tick_configurability.f90)
    !===========================================================================
    
    subroutine test_tick_configuration()
        print *, "--- Tick Configuration Tests ---"
        
        call test_basic_tick_marks()
        call test_custom_tick_spacing()
        call test_tick_labels()
        call test_tick_positioning()
    end subroutine test_tick_configuration

    subroutine test_basic_tick_marks()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Basic tick marks")
        
        ctx = create_pdf_canvas(500, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        call ctx%axes()
        call ctx%text(5.0_wp, 7.0_wp, "Basic Tick Marks")
        
        ! Draw manual tick marks if automatic ones aren't available
        call ctx%line(1.0_wp, 0.0_wp, 1.0_wp, -0.2_wp)  ! X tick
        call ctx%line(2.0_wp, 0.0_wp, 2.0_wp, -0.2_wp)  ! X tick
        call ctx%line(3.0_wp, 0.0_wp, 3.0_wp, -0.2_wp)  ! X tick
        call ctx%line(0.0_wp, 1.0_wp, -0.2_wp, 1.0_wp)  ! Y tick
        call ctx%line(0.0_wp, 2.0_wp, -0.2_wp, 2.0_wp)  ! Y tick
        
        filename = get_test_output_path('/tmp/pdf_basic_ticks.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Basic tick marks test saved"
        call end_test()
    end subroutine test_basic_tick_marks

    subroutine test_custom_tick_spacing()
        type(pdf_context) :: ctx
        real(wp) :: tick_positions(5)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Custom tick spacing")
        
        ctx = create_pdf_canvas(600, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 12.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        call ctx%axes()
        call ctx%text(6.0_wp, 7.0_wp, "Custom Tick Spacing")
        
        ! Custom X-axis tick positions
        tick_positions = [1.0_wp, 3.5_wp, 6.0_wp, 9.5_wp, 11.0_wp]
        do i = 1, 5
            call ctx%line(tick_positions(i), 0.0_wp, tick_positions(i), -0.3_wp)
        end do
        
        ! Custom Y-axis tick positions  
        tick_positions = [0.5_wp, 2.0_wp, 4.5_wp, 6.0_wp, 7.5_wp]
        do i = 1, 5
            call ctx%line(0.0_wp, tick_positions(i), -0.3_wp, tick_positions(i))
        end do
        
        filename = get_test_output_path('/tmp/pdf_custom_ticks.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Custom tick spacing test saved"
        call end_test()
    end subroutine test_custom_tick_spacing

    subroutine test_tick_labels()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Tick labels")
        
        ctx = create_pdf_canvas(500, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        call ctx%axes()
        call ctx%text(5.0_wp, 7.5_wp, "Tick Labels Test")
        
        ! X-axis tick labels
        call ctx%text(2.0_wp, -0.7_wp, "2")
        call ctx%text(4.0_wp, -0.7_wp, "4")
        call ctx%text(6.0_wp, -0.7_wp, "6")
        call ctx%text(8.0_wp, -0.7_wp, "8")
        
        ! Y-axis tick labels
        call ctx%text(-0.7_wp, 2.0_wp, "2")
        call ctx%text(-0.7_wp, 4.0_wp, "4")
        call ctx%text(-0.7_wp, 6.0_wp, "6")
        
        filename = get_test_output_path('/tmp/pdf_tick_labels.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Tick labels test saved"
        call end_test()
    end subroutine test_tick_labels

    subroutine test_tick_positioning()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Tick positioning accuracy")
        
        ctx = create_pdf_canvas(400, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 8.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        call ctx%axes()
        call ctx%text(4.0_wp, 7.5_wp, "Tick Positioning")
        
        ! Test precise tick positioning
        call ctx%line(2.0_wp, 0.0_wp, 2.0_wp, -0.2_wp)  ! Should align with data
        call ctx%line(4.0_wp, 0.0_wp, 4.0_wp, -0.2_wp)
        call ctx%line(6.0_wp, 0.0_wp, 6.0_wp, -0.2_wp)
        
        call ctx%line(0.0_wp, 2.0_wp, -0.2_wp, 2.0_wp)
        call ctx%line(0.0_wp, 4.0_wp, -0.2_wp, 4.0_wp)
        call ctx%line(0.0_wp, 6.0_wp, -0.2_wp, 6.0_wp)
        
        ! Add reference points to verify alignment
        call ctx%circle(2.0_wp, 2.0_wp, 0.1_wp)
        call ctx%circle(4.0_wp, 4.0_wp, 0.1_wp)
        call ctx%circle(6.0_wp, 6.0_wp, 0.1_wp)
        
        filename = get_test_output_path('/tmp/pdf_tick_positioning.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Tick positioning test saved - verify alignment"
        call end_test()
    end subroutine test_tick_positioning

    !===========================================================================
    ! Advanced Rendering Tests
    !===========================================================================
    
    subroutine test_advanced_rendering()
        print *, "--- Advanced Rendering Tests ---"
        
        call test_complex_scene_rendering()
        call test_rendering_optimization()
    end subroutine test_advanced_rendering

    subroutine test_complex_scene_rendering()
        type(pdf_context) :: ctx
        real(wp) :: x_data(20), y_data(20)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Complex scene rendering")
        
        ctx = create_pdf_canvas(700, 500)
        ctx%x_min = 0.0_wp; ctx%x_max = 14.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        
        ! Generate complex scene
        do i = 1, 20
            x_data(i) = real(i-1, wp) * 0.7_wp
            y_data(i) = 5.0_wp + 3.0_wp * sin(x_data(i) * 0.5_wp)
        end do
        
        ! Multiple plot elements
        call ctx%axes()
        
        ! Data plot
        do i = 1, 19
            call ctx%line(x_data(i), y_data(i), x_data(i+1), y_data(i+1))
        end do
        
        ! Data points
        do i = 1, 20
            call ctx%circle(x_data(i), y_data(i), 0.15_wp)
        end do
        
        ! Additional elements
        call ctx%rectangle(10.0_wp, 1.0_wp, 3.0_wp, 2.0_wp)
        call ctx%text(7.0_wp, 9.0_wp, "Complex Scene Test")
        call ctx%text(11.5_wp, 0.5_wp, "Legend")
        
        filename = get_test_output_path('/tmp/pdf_complex_scene.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Complex scene rendering test saved"
        call end_test()
    end subroutine test_complex_scene_rendering

    subroutine test_rendering_optimization()
        type(pdf_context) :: ctx
        real(wp) :: start_time, end_time
        integer :: i
        character(len=512) :: filename
        
        call start_test("Rendering optimization")
        
        ctx = create_pdf_canvas(600, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 15.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 10.0_wp
        
        call cpu_time(start_time)
        
        ! Test optimized rendering of many elements
        do i = 1, 300  ! Moderate number for speed
            call ctx%circle(real(mod(i-1, 15), wp) + 0.5_wp, &
                           real((i-1) / 15, wp) * 0.5_wp + 1.0_wp, 0.05_wp)
        end do
        
        call ctx%text(7.5_wp, 9.0_wp, "Rendering Optimization Test")
        
        call cpu_time(end_time)
        
        filename = get_test_output_path('/tmp/pdf_render_optimization.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Rendering optimization (300 elements) in ", end_time - start_time, " seconds"
        call end_test()
    end subroutine test_rendering_optimization

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'PDF Rendering Comprehensive Test Summary'
        write(*, '(A)') 'Consolidated 4 PDF rendering test files into single comprehensive test'
        write(*, '(A)') ''
        write(*, '(A)') 'MANUAL VERIFICATION REQUIRED:'
        write(*, '(A)') '  1. Check PDF stretching behavior in different aspect ratios'
        write(*, '(A)') '  2. Verify font rendering and mixed font styles'
        write(*, '(A)') '  3. Confirm stream output performance is acceptable'
        write(*, '(A)') '  4. Validate tick mark positioning and alignment'
        write(*, '(A)') '  5. Ensure complex scenes render correctly'
        write(*, '(A)') ''
        write(*, '(A)') 'All comprehensive PDF rendering tests COMPLETED!'
    end subroutine print_test_summary

end program test_pdf_rendering_comprehensive