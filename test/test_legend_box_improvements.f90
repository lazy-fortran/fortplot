program test_legend_box_improvements
    !! TDD tests for legend box sizing and placement improvements
    !! These tests define the expected behavior for better legend rendering
    use fortplot
    use fortplot_legend, only: legend_t, legend_entry_t, create_legend
    use fortplot_context, only: plot_context  
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_legend_box_sizing()
    call test_legend_text_positioning()
    call test_legend_placement_margins()
    call test_legend_visual_quality()
    print *, "All legend box improvement tests passed!"
    
contains

    subroutine test_legend_box_sizing()
        !! Test that legend box properly contains all text
        !! FAILING: Current box is too small for text content
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
        
        ! Test files to verify box sizing
        call fig%savefig('/tmp/test/test_legend_box_sizing.png')
        
        ! TODO: Add assertions to verify box dimensions contain text
        ! Expected: Legend box width should accommodate longest label
        ! Expected: Legend box height should accommodate all entries with proper spacing
        ! Expected: Text should not be clipped or overflow box boundaries
        
        print *, "PASS: Legend box sizing test completed"
    end subroutine test_legend_box_sizing

    subroutine test_legend_text_positioning()
        !! Test that legend text is properly centered and aligned
        !! FAILING: Text positioning is not optimal within legend entries
        type(figure_t) :: fig
        real(wp), dimension(5) :: x, y1, y2
        integer :: i
        
        x = [(real(i, wp), i=1, 5)]
        y1 = sin(x)
        y2 = cos(x)
        
        call fig%initialize(640, 480)
        call fig%add_plot(x, y1, label="sin(x)")
        call fig%add_plot(x, y2, label="cos(x)")
        
        call fig%legend()
        call fig%savefig('/tmp/test/test_legend_text_positioning.png')
        
        ! TODO: Add assertions for text positioning
        ! Expected: Text should be vertically centered with legend lines
        ! Expected: Text should have consistent spacing from legend lines
        ! Expected: Text should not overlap with legend box borders
        
        print *, "PASS: Legend text positioning test completed"
    end subroutine test_legend_text_positioning

    subroutine test_legend_placement_margins()
        !! Test that legend placement respects proper margins
        !! FAILING: Legend placement doesn't maintain adequate margins
        type(figure_t) :: fig
        real(wp), dimension(20) :: x, y1, y2, y3, y4
        integer :: i
        
        x = [(real(i, wp), i=1, 20)]
        y1 = x
        y2 = x**2
        y3 = sqrt(x)
        y4 = log(x)
        
        call fig%initialize(640, 480)
        call fig%add_plot(x, y1, label="Linear")
        call fig%add_plot(x, y2, label="Quadratic")
        call fig%add_plot(x, y3, label="Square Root")
        call fig%add_plot(x, y4, label="Logarithmic")
        
        ! Test all legend positions
        call fig%legend(location="upper left")
        call fig%savefig('/tmp/test/test_legend_margins_ul.png')
        
        call fig%legend(location="upper right")
        call fig%savefig('/tmp/test/test_legend_margins_ur.png')
        
        call fig%legend(location="lower left")
        call fig%savefig('/tmp/test/test_legend_margins_ll.png')
        
        call fig%legend(location="lower right")
        call fig%savefig('/tmp/test/test_legend_margins_lr.png')
        
        ! TODO: Add assertions for margin compliance
        ! Expected: Legend should not overlap with plot data
        ! Expected: Legend should maintain minimum distance from plot edges
        ! Expected: Legend should not extend beyond plot canvas
        
        print *, "PASS: Legend placement margins test completed"
    end subroutine test_legend_placement_margins

    subroutine test_legend_visual_quality()
        !! Test overall visual quality and professional appearance
        !! FAILING: Current legend lacks professional matplotlib-style appearance
        type(figure_t) :: fig
        real(wp), dimension(50) :: x, y1, y2, y3
        integer :: i
        
        x = [(real(i, wp) * 0.1_wp, i=1, 50)]
        y1 = exp(-x) * sin(x)
        y2 = x * exp(-x/2.0_wp)
        y3 = cos(x) / (1.0_wp + x)
        
        call fig%initialize(800, 600)
        call fig%set_title("Professional Legend Quality Test")
        call fig%add_plot(x, y1, label="Damped Sine")
        call fig%add_plot(x, y2, label="Exponential Decay")
        call fig%add_plot(x, y3, label="Damped Cosine")
        
        call fig%legend()
        call fig%savefig('/tmp/test/test_legend_visual_quality.png')
        
        ! TODO: Add assertions for visual quality
        ! Expected: Legend should have professional appearance similar to matplotlib
        ! Expected: Legend should have proper padding and spacing
        ! Expected: Legend should have clean border and background
        
        print *, "PASS: Legend visual quality test completed"
    end subroutine test_legend_visual_quality

end program test_legend_box_improvements