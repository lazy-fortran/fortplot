program test_pdf_stream_rendering
    !! PDF stream output and advanced rendering tests
    !! Extracted from test_pdf_rendering_comprehensive.f90
    !! 
    !! This test covers:
    !! - PDF stream output optimization
    !! - Tick mark configuration and positioning
    !! - Advanced PDF rendering features
    !! - Complex scene rendering and optimization

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_security, only: get_test_output_path
    use fortplot
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0

    print *, "=== PDF STREAM AND RENDERING TESTS ==="
    
    call test_stream_output()
    call test_tick_configuration()
    call test_advanced_rendering()
    call print_test_summary()

contains

    !===========================================================================
    ! Stream Output Tests
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
    ! Tick Configuration Tests
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
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') 'Test ', test_count, ': ', test_name
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') '  PASS'
        write(*, *)
    end subroutine end_test

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'PDF Stream and Rendering Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        write(*, '(A)') 'Stream output and rendering tests COMPLETED!'
        write(*, '(A)') ''
        write(*, '(A)') 'MANUAL VERIFICATION REQUIRED:'
        write(*, '(A)') '  1. Check stream output performance is acceptable'
        write(*, '(A)') '  2. Validate tick mark positioning and alignment'
        write(*, '(A)') '  3. Ensure complex scenes render correctly'
        write(*, '(A)') '  4. Verify rendering optimization effectiveness'
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_pdf_stream_rendering