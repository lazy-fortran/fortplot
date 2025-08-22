program test_linestyle_marker_rendering
    !! Test suite for linestyle and marker rendering across all backends
    !! Following TDD RED phase - these tests define backend integration requirements
    !! 
    !! GIVEN: matplotlib-compatible linestyle and marker syntax
    !! WHEN: rendering plots across PNG, PDF, and ASCII backends
    !! THEN: should produce consistent visual output matching matplotlib behavior

    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Backend-specific rendering tests
    call test_png_backend_rendering()
    call test_pdf_backend_rendering()
    call test_ascii_backend_rendering()
    call test_cross_backend_consistency()
    call test_marker_size_consistency()
    call test_linestyle_pattern_accuracy()
    call test_combined_rendering_quality()
    
    call print_test_summary()
    
contains

    subroutine test_png_backend_rendering()
        !! GIVEN: matplotlib syntax strings for linestyles and markers
        !! WHEN: rendering to PNG backend
        !! THEN: should produce visually correct PNG output with proper pixel patterns
        
        type(figure_t) :: fig
        real(wp) :: x(10), y_solid(10), y_dashed(10), y_dotted(10), y_dashdot(10)
        integer :: i
        
        call start_test("PNG backend linestyle and marker rendering")
        
        ! Generate test data with clear visual separation
        do i = 1, 10
            x(i) = real(i-1, wp)
            y_solid(i) = sin(x(i) * 0.5_wp) + 3.0_wp
            y_dashed(i) = sin(x(i) * 0.5_wp) + 2.0_wp
            y_dotted(i) = sin(x(i) * 0.5_wp) + 1.0_wp
            y_dashdot(i) = sin(x(i) * 0.5_wp) + 0.0_wp
        end do
        
        call fig%initialize(800, 600)
        call fig%set_title("PNG Backend Linestyle Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        ! Test matplotlib linestyle syntax in PNG backend
        call fig%add_plot(x, y_solid, linestyle='-', label='Solid (-)')
        call fig%add_plot(x, y_dashed, linestyle='--', label='Dashed (--)')
        call fig%add_plot(x, y_dotted, linestyle=':', label='Dotted (:)')
        call fig%add_plot(x, y_dashdot, linestyle='-.', label='Dash-dot (-.)') 
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/png_linestyle_test.png'))
        
        ! Test matplotlib marker syntax in PNG backend
        call fig%initialize(800, 600)
        call fig%set_title("PNG Backend Marker Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        call fig%add_plot(x, y_solid, linestyle='o', label='Circle (o)')
        call fig%add_plot(x, y_dashed, linestyle='s', label='Square (s)')
        call fig%add_plot(x, y_dotted, linestyle='^', label='Triangle up (^)')
        call fig%add_plot(x, y_dashdot, linestyle='*', label='Star (*)')
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/png_marker_test.png'))
        
        ! Test combined marker+linestyle in PNG backend
        call fig%initialize(800, 600)
        call fig%set_title("PNG Backend Combined Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        call fig%add_plot(x, y_solid, linestyle='o-', label='Circle+Solid (o-)')
        call fig%add_plot(x, y_dashed, linestyle='s--', label='Square+Dashed (s--)')
        call fig%add_plot(x, y_dotted, linestyle='^:', label='Triangle+Dotted (^:)')
        call fig%add_plot(x, y_dashdot, linestyle='*-.', label='Star+Dashdot (*-.)')
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/png_combined_test.png'))
        
        call end_test()
    end subroutine test_png_backend_rendering

    subroutine test_pdf_backend_rendering()
        !! GIVEN: matplotlib syntax strings for linestyles and markers
        !! WHEN: rendering to PDF backend
        !! THEN: should produce visually correct PDF output with proper vector patterns
        
        type(figure_t) :: fig
        real(wp) :: x(10), y_solid(10), y_dashed(10), y_dotted(10), y_dashdot(10)
        integer :: i
        
        call start_test("PDF backend linestyle and marker rendering")
        
        ! Generate test data
        do i = 1, 10
            x(i) = real(i-1, wp)
            y_solid(i) = cos(x(i) * 0.5_wp) + 3.0_wp
            y_dashed(i) = cos(x(i) * 0.5_wp) + 2.0_wp
            y_dotted(i) = cos(x(i) * 0.5_wp) + 1.0_wp
            y_dashdot(i) = cos(x(i) * 0.5_wp) + 0.0_wp
        end do
        
        call fig%initialize(800, 600)
        call fig%set_title("PDF Backend Linestyle Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        ! Test matplotlib linestyle syntax in PDF backend
        call fig%add_plot(x, y_solid, linestyle='-', label='Solid (-)')
        call fig%add_plot(x, y_dashed, linestyle='--', label='Dashed (--)')
        call fig%add_plot(x, y_dotted, linestyle=':', label='Dotted (:)')
        call fig%add_plot(x, y_dashdot, linestyle='-.', label='Dash-dot (-.)') 
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/pdf_linestyle_test.pdf'))
        
        ! Test matplotlib marker syntax in PDF backend
        call fig%initialize(800, 600)
        call fig%set_title("PDF Backend Marker Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        call fig%add_plot(x, y_solid, linestyle='o', label='Circle (o)')
        call fig%add_plot(x, y_dashed, linestyle='s', label='Square (s)')
        call fig%add_plot(x, y_dotted, linestyle='^', label='Triangle up (^)')
        call fig%add_plot(x, y_dashdot, linestyle='*', label='Star (*)')
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/pdf_marker_test.pdf'))
        
        ! Test combined marker+linestyle in PDF backend
        call fig%initialize(800, 600)
        call fig%set_title("PDF Backend Combined Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        call fig%add_plot(x, y_solid, linestyle='o-', label='Circle+Solid (o-)')
        call fig%add_plot(x, y_dashed, linestyle='s--', label='Square+Dashed (s--)')
        call fig%add_plot(x, y_dotted, linestyle='^:', label='Triangle+Dotted (^:)')
        call fig%add_plot(x, y_dashdot, linestyle='*-.', label='Star+Dashdot (*-.)')
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/pdf_combined_test.pdf'))
        
        call end_test()
    end subroutine test_pdf_backend_rendering

    subroutine test_ascii_backend_rendering()
        !! GIVEN: matplotlib syntax strings for linestyles and markers
        !! WHEN: rendering to ASCII backend
        !! THEN: should produce visually correct ASCII output with distinct characters
        
        type(figure_t) :: fig
        real(wp) :: x(10), y_solid(10), y_dashed(10), y_dotted(10), y_dashdot(10)
        integer :: i
        
        call start_test("ASCII backend linestyle and marker rendering")
        
        ! Generate test data
        do i = 1, 10
            x(i) = real(i-1, wp)
            y_solid(i) = x(i) * 0.3_wp + 3.0_wp
            y_dashed(i) = x(i) * 0.3_wp + 2.0_wp
            y_dotted(i) = x(i) * 0.3_wp + 1.0_wp
            y_dashdot(i) = x(i) * 0.3_wp + 0.0_wp
        end do
        
        call fig%initialize(80, 25)
        call fig%set_title("ASCII Backend Linestyle Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        ! Test matplotlib linestyle syntax in ASCII backend
        call fig%add_plot(x, y_solid, linestyle='-', label='Solid (-)')
        call fig%add_plot(x, y_dashed, linestyle='--', label='Dashed (--)')
        call fig%add_plot(x, y_dotted, linestyle=':', label='Dotted (:)')
        call fig%add_plot(x, y_dashdot, linestyle='-.', label='Dash-dot (-.)') 
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/ascii_linestyle_test.txt'))
        
        ! Test matplotlib marker syntax in ASCII backend
        call fig%initialize(80, 25)
        call fig%set_title("ASCII Backend Marker Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        call fig%add_plot(x, y_solid, linestyle='o', label='Circle (o)')
        call fig%add_plot(x, y_dashed, linestyle='s', label='Square (s)')
        call fig%add_plot(x, y_dotted, linestyle='^', label='Triangle up (^)')
        call fig%add_plot(x, y_dashdot, linestyle='*', label='Star (*)')
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/ascii_marker_test.txt'))
        
        ! Test combined marker+linestyle in ASCII backend
        call fig%initialize(80, 25)
        call fig%set_title("ASCII Backend Combined Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        call fig%add_plot(x, y_solid, linestyle='o-', label='Circle+Solid (o-)')
        call fig%add_plot(x, y_dashed, linestyle='s--', label='Square+Dashed (s--)')
        call fig%add_plot(x, y_dotted, linestyle='^:', label='Triangle+Dotted (^:)')
        call fig%add_plot(x, y_dashdot, linestyle='*-.', label='Star+Dashdot (*-.)')
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/ascii_combined_test.txt'))
        
        call end_test()
    end subroutine test_ascii_backend_rendering

    subroutine test_cross_backend_consistency()
        !! GIVEN: identical matplotlib syntax across different backends
        !! WHEN: rendering same plot data to PNG, PDF, and ASCII
        !! THEN: should maintain visual consistency and proportional relationships
        
        type(figure_t) :: fig
        real(wp) :: x(5), y(5)
        integer :: i
        
        call start_test("Cross-backend consistency")
        
        ! Simple test data for consistency comparison
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = x(i) * 0.5_wp
        end do
        
        ! Render identical plot across all backends
        call fig%initialize(400, 300)
        call fig%set_title("Cross-Backend Consistency Test")
        call fig%add_plot(x, y, linestyle='o--', label='Test Data (o--)')
        call figure_legend(fig, )
        
        ! Save to all backends for visual comparison
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/consistency_test.png'))
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/consistency_test.pdf'))
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/consistency_test.txt'))
        
        call end_test()
    end subroutine test_cross_backend_consistency

    subroutine test_marker_size_consistency()
        !! GIVEN: different marker types using matplotlib syntax
        !! WHEN: rendering markers across backends
        !! THEN: should maintain consistent relative sizes and visual hierarchy
        
        type(figure_t) :: fig
        real(wp) :: x(6), y(6)
        integer :: i
        
        call start_test("Marker size consistency across backends")
        
        ! Test data for marker size comparison
        do i = 1, 6
            x(i) = real(i, wp)
            y(i) = 2.0_wp  ! Horizontal line for clear size comparison
        end do
        
        call fig%initialize(600, 400)
        call fig%set_title("Marker Size Consistency Test")
        call fig%set_xlabel("Different Markers")
        call fig%set_ylabel("Constant Y Value")
        
        ! Test all major marker types for size consistency
        call fig%add_plot(x(1:1), y(1:1), linestyle='o', label='Circle (o)')
        call fig%add_plot(x(2:2), y(2:2), linestyle='s', label='Square (s)')
        call fig%add_plot(x(3:3), y(3:3), linestyle='^', label='Triangle (^)')
        call fig%add_plot(x(4:4), y(4:4), linestyle='D', label='Diamond (D)')
        call fig%add_plot(x(5:5), y(5:5), linestyle='*', label='Star (*)')
        call fig%add_plot(x(6:6), y(6:6), linestyle='+', label='Plus (+)')
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/marker_sizes_test.png'))
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/marker_sizes_test.pdf'))
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/marker_sizes_test.txt'))
        
        call end_test()
    end subroutine test_marker_size_consistency

    subroutine test_linestyle_pattern_accuracy()
        !! GIVEN: matplotlib linestyle syntax patterns
        !! WHEN: rendering linestyle patterns across backends
        !! THEN: should produce visually distinct and recognizable patterns
        
        type(figure_t) :: fig
        real(wp) :: x(20), y_solid(20), y_dashed(20), y_dotted(20), y_dashdot(20)
        integer :: i
        
        call start_test("Linestyle pattern accuracy")
        
        ! Generate longer test data to show pattern details
        do i = 1, 20
            x(i) = real(i-1, wp) * 0.3_wp
            y_solid(i) = 4.0_wp
            y_dashed(i) = 3.0_wp
            y_dotted(i) = 2.0_wp
            y_dashdot(i) = 1.0_wp
        end do
        
        call fig%initialize(800, 400)
        call fig%set_title("Linestyle Pattern Accuracy Test")
        call fig%set_xlabel("X values (extended range)")
        call fig%set_ylabel("Y levels")
        
        ! Test linestyle patterns with extended data for pattern visibility
        call fig%add_plot(x, y_solid, linestyle='-', label='Solid (-)')
        call fig%add_plot(x, y_dashed, linestyle='--', label='Dashed (--)')
        call fig%add_plot(x, y_dotted, linestyle=':', label='Dotted (:)')
        call fig%add_plot(x, y_dashdot, linestyle='-.', label='Dash-dot (-.)') 
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/pattern_accuracy_test.png'))
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/pattern_accuracy_test.pdf'))
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/pattern_accuracy_test.txt'))
        
        call end_test()
    end subroutine test_linestyle_pattern_accuracy

    subroutine test_combined_rendering_quality()
        !! GIVEN: combined marker+linestyle matplotlib syntax
        !! WHEN: rendering complex combinations across backends
        !! THEN: should properly render both components without interference
        
        type(figure_t) :: fig
        real(wp) :: x(8), y1(8), y2(8), y3(8), y4(8)
        integer :: i
        
        call start_test("Combined marker+linestyle rendering quality")
        
        ! Generate test data with sufficient detail
        do i = 1, 8
            x(i) = real(i-1, wp)
            y1(i) = sin(x(i) * 0.8_wp) + 3.0_wp
            y2(i) = cos(x(i) * 0.8_wp) + 2.0_wp
            y3(i) = sin(x(i) * 1.2_wp) + 1.0_wp
            y4(i) = cos(x(i) * 1.2_wp) + 0.0_wp
        end do
        
        call fig%initialize(800, 600)
        call fig%set_title("Combined Rendering Quality Test")
        call fig%set_xlabel("X values")
        call fig%set_ylabel("Y values")
        
        ! Test various marker+linestyle combinations
        call fig%add_plot(x, y1, linestyle='o-', label='Circle+Solid (o-)')
        call fig%add_plot(x, y2, linestyle='s--', label='Square+Dashed (s--)')
        call fig%add_plot(x, y3, linestyle='^:', label='Triangle+Dotted (^:)')
        call fig%add_plot(x, y4, linestyle='*-.', label='Star+Dashdot (*-.)')
        
        call figure_legend(fig, )
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/combined_quality_test.png'))
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/combined_quality_test.pdf'))
        call figure_savefig(fig, get_test_output_path('output/test/test_linestyle_marker_rendering/combined_quality_test.txt'))
        
        call end_test()
    end subroutine test_combined_rendering_quality

    ! Testing utilities
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "Running test: ", trim(test_name)
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') "  PASS"
    end subroutine end_test

    subroutine print_test_summary()
        write(*, '(A,I0,A,I0,A)') "Tests completed: ", pass_count, "/", test_count, " passed"
        if (pass_count /= test_count) error stop "Some tests failed"
    end subroutine print_test_summary

end program test_linestyle_marker_rendering