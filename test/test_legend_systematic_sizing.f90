program test_legend_systematic_sizing
    !! TDD test for systematic legend sizing using text system metrics
    !! This test defines the expected behavior for proper content-based sizing
    use fortplot
    use fortplot_text, only: calculate_text_width, calculate_text_height, init_text_system
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_text_system_measurement()
    call test_legend_content_based_sizing()
    call test_thin_border_styling()
    call test_matplotlib_reference_comparison()
    print *, "All systematic legend sizing tests completed!"
    
contains

    subroutine test_text_system_measurement()
        !! Test that we can measure text width and height accurately using text system
        integer :: width_sin, width_cos, width_long
        integer :: height_sin, height_cos, height_long
        logical :: success
        
        ! Initialize text system if needed
        success = init_text_system()
        if (.not. success) then
            print *, "SKIP: Text system not available for text measurement"
            return
        end if
        
        ! Measure actual text dimensions
        width_sin = calculate_text_width("sin(x)")
        width_cos = calculate_text_width("cos(x)")  
        width_long = calculate_text_width("Very Long Label Name")
        
        height_sin = calculate_text_height("sin(x)")
        height_cos = calculate_text_height("cos(x)")  
        height_long = calculate_text_height("Very Long Label Name")
        
        print *, "Text dimension measurements:"
        print *, "  sin(x):", width_sin, "x", height_sin, "pixels"
        print *, "  cos(x):", width_cos, "x", height_cos, "pixels"
        print *, "  Very Long Label Name:", width_long, "x", height_long, "pixels"
        
        ! Verify measurements are reasonable
        if (width_sin <= 0 .or. width_cos <= 0 .or. width_long <= 0) then
            print *, "FAIL: Invalid text width measurements"
            stop 1
        end if
        
        if (height_sin <= 0 .or. height_cos <= 0 .or. height_long <= 0) then
            print *, "FAIL: Invalid text height measurements"
            stop 1
        end if
        
        if (width_long <= width_sin) then
            print *, "FAIL: Long text should be wider than short text"
            stop 1
        end if
        
        print *, "PASS: Text system measurement working correctly"
    end subroutine test_text_system_measurement

    subroutine test_legend_content_based_sizing()
        !! Test that legend box is sized based on actual content dimensions
        type(figure_t) :: fig
        real(wp), dimension(10) :: x, y1, y2, y3
        integer :: i
        
        x = [(real(i, wp), i=1, 10)]
        y1 = x
        y2 = x * 2.0_wp 
        y3 = x * 3.0_wp
        
        call fig%initialize(640, 480)
        call fig%add_plot(x, y1, label="Short")
        call fig%add_plot(x, y2, label="Medium Label")
        call fig%add_plot(x, y3, label="Very Long Label Name")
        
        call fig%legend()
        
        ! This should produce a legend box that:
        ! 1. Fits all text content without clipping
        ! 2. Has minimal excess space
        ! 3. Uses actual text width measurements
        call fig%savefig('test/test_content_based_sizing.png')
        
        print *, "PASS: Content-based sizing test created"
    end subroutine test_legend_content_based_sizing

    subroutine test_thin_border_styling()
        !! Test that legend border matches axes frame thickness
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y
        integer :: i
        
        x = [(real(i, wp), i=1, 5)]
        y = x**2
        
        call fig%initialize(640, 480)
        call fig%add_plot(x, y, label="y = x²")
        call fig%legend()
        
        ! This should produce a legend with:
        ! 1. Border thickness matching axes frame
        ! 2. Professional matplotlib-style appearance
        call fig%savefig('test/test_thin_border_styling.png')
        
        print *, "PASS: Thin border styling test created"
    end subroutine test_thin_border_styling

    subroutine test_matplotlib_reference_comparison()
        !! Test that replicates the basic_plots multi_line example for comparison
        type(figure_t) :: fig
        real(wp), dimension(100) :: x, sx, cx
        integer :: i
        
        x = [(real(i, wp), i=0, size(x) - 1)]/5.0_wp
        sx = sin(x)
        cx = cos(x)
        
        call fig%initialize(640, 480)
        call fig%set_xlabel("x")
        call fig%set_ylabel("y")
        call fig%set_title("Sine and Cosine Functions")
        call fig%add_plot(x, sx, label="sin(x)")
        call fig%add_plot(x, cx, label="cos(x)")
        call fig%legend()
        
        ! This should match the matplotlib reference plot closely
        call fig%savefig('test/test_matplotlib_comparison.png')
        
        print *, "PASS: Matplotlib comparison test created"
        print *, "Compare test_matplotlib_comparison.png with multi_line_ref.png"
    end subroutine test_matplotlib_reference_comparison

end program test_legend_systematic_sizing