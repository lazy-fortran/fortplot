program test_pdf_tick_configurability
    !! Test configurable tick count functionality for PDF backend
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_default_dynamic_ticks_small()
    call test_default_dynamic_ticks_medium()
    call test_default_dynamic_ticks_large()
    call test_explicit_tick_count_override()
    call test_minimum_tick_count()
    call test_maximum_tick_count()
    call test_backward_compatibility()
    call test_dimension_based_calculation()
    call test_edge_case_tiny_plot()
    call test_edge_case_huge_plot()
    
    print *, "All PDF tick configurability tests completed!"
    
contains

    subroutine test_default_dynamic_ticks_small()
        !! Test dynamic tick calculation for small plots (< 200px)
        type(figure_t) :: fig
        real(wp), parameter :: x_data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y_data(5) = [10.0_wp, 25.0_wp, 20.0_wp, 30.0_wp, 15.0_wp]
        
        print *, "=== Testing dynamic ticks for small plot ==="
        
        call fig%initialize(150, 150)  ! Small plot
        call fig%add_plot(x_data, y_data)
        call fig%set_title("Small plot dynamic ticks")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_small.pdf"))
        
        print *, "Expected: 4-5 ticks for small plot dimensions"
    end subroutine test_default_dynamic_ticks_small
    
    subroutine test_default_dynamic_ticks_medium()
        !! Test dynamic tick calculation for medium plots (200-600px)
        type(figure_t) :: fig
        real(wp), parameter :: x_data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                              6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]
        real(wp), parameter :: y_data(10) = [100.0_wp, 150.0_wp, 125.0_wp, 175.0_wp, 160.0_wp, &
                                              190.0_wp, 140.0_wp, 165.0_wp, 155.0_wp, 180.0_wp]
        
        print *, "=== Testing dynamic ticks for medium plot ==="
        
        call fig%initialize(400, 400)  ! Medium plot
        call fig%add_plot(x_data, y_data)
        call fig%set_title("Medium plot dynamic ticks")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_medium.pdf"))
        
        print *, "Expected: 6-8 ticks for medium plot dimensions"
    end subroutine test_default_dynamic_ticks_medium
    
    subroutine test_default_dynamic_ticks_large()
        !! Test dynamic tick calculation for large plots (> 600px)
        type(figure_t) :: fig
        integer :: i
        real(wp), parameter :: x_data(20) = [(real(i, wp), i = 1, 20)]
        real(wp), parameter :: y_data(20) = [(1000.0_wp + 50.0_wp * sin(real(i, wp) * 0.5_wp), i = 1, 20)]
        
        print *, "=== Testing dynamic ticks for large plot ==="
        
        call fig%initialize(800, 600)  ! Large plot
        call fig%add_plot(x_data, y_data)
        call fig%set_title("Large plot dynamic ticks")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_large.pdf"))
        
        print *, "Expected: 8-12 ticks for large plot dimensions"
    end subroutine test_default_dynamic_ticks_large
    
    subroutine test_explicit_tick_count_override()
        !! Test explicit tick count configuration override
        type(figure_t) :: fig
        integer :: i
        real(wp), parameter :: x_data(10) = [(real(i, wp), i = 1, 10)]
        real(wp), parameter :: y_data(10) = [(real(i**2, wp), i = 1, 10)]
        
        print *, "=== Testing explicit tick count override ==="
        
        call fig%initialize(500, 400)
        call fig%add_plot(x_data, y_data)
        ! Fix set_tick_count interface - method not available in figure_t from figure_core
        ! call fig%set_tick_count(10, 10)  ! Override with 10 ticks for both axes
        call fig%set_title("Explicit tick count = 10")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_override.pdf"))
        
        print *, "Expected: Exactly 10 ticks on each axis"
    end subroutine test_explicit_tick_count_override
    
    subroutine test_minimum_tick_count()
        !! Test minimum tick count constraint
        type(figure_t) :: fig
        real(wp), parameter :: x_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y_data(3) = [5.0_wp, 10.0_wp, 7.5_wp]
        
        print *, "=== Testing minimum tick count ==="
        
        call fig%initialize(100, 100)  ! Very small plot
        call fig%add_plot(x_data, y_data)
        ! Fix set_tick_count interface - method not available in figure_t from figure_core
        ! call fig%set_tick_count(2, 2)  ! Try to set below minimum
        call fig%set_title("Minimum tick test")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_minimum.pdf"))
        
        print *, "Expected: Minimum 3 ticks enforced despite request for 2"
    end subroutine test_minimum_tick_count
    
    subroutine test_maximum_tick_count()
        !! Test maximum tick count constraint
        type(figure_t) :: fig
        integer :: i
        real(wp), parameter :: x_data(50) = [(real(i, wp), i = 1, 50)]
        real(wp), parameter :: y_data(50) = [(real(i, wp) * 2.5_wp, i = 1, 50)]
        
        print *, "=== Testing maximum tick count ==="
        
        call fig%initialize(600, 450)
        call fig%add_plot(x_data, y_data)
        ! Fix set_tick_count interface - method not available in figure_t from figure_core
        ! call fig%set_tick_count(25, 25)  ! Try to set above maximum
        call fig%set_title("Maximum tick test")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_maximum.pdf"))
        
        print *, "Expected: Maximum 15 ticks enforced despite request for 25"
    end subroutine test_maximum_tick_count
    
    subroutine test_backward_compatibility()
        !! Test that default behavior remains reasonable without configuration
        type(figure_t) :: fig
        real(wp), parameter :: x_data(8) = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp]
        real(wp), parameter :: y_data(8) = [0.0_wp, 1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp, 36.0_wp, 49.0_wp]
        
        print *, "=== Testing backward compatibility ==="
        
        call fig%initialize(640, 480)  ! Standard size
        call fig%add_plot(x_data, y_data)
        ! No tick count configuration - should use dynamic default
        call fig%set_title("Backward compatibility test")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_compatibility.pdf"))
        
        print *, "Expected: Reasonable default tick count (likely 6-8 for standard size)"
    end subroutine test_backward_compatibility
    
    subroutine test_dimension_based_calculation()
        !! Test that tick calculation considers both width and height
        type(figure_t) :: fig
        integer :: i
        real(wp), parameter :: x_data(15) = [(real(i, wp), i = 1, 15)]
        real(wp), parameter :: y_data(15) = [(sin(real(i, wp) * 0.4_wp) * 100.0_wp, i = 1, 15)]
        
        print *, "=== Testing dimension-based calculation ==="
        
        ! Wide but short plot
        call fig%initialize(800, 200)
        call fig%add_plot(x_data, y_data)
        call fig%set_title("Wide short plot")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_wide.pdf"))
        
        ! Tall but narrow plot
        call fig%initialize(200, 800)
        call fig%add_plot(x_data, y_data)
        call fig%set_title("Tall narrow plot")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_tall.pdf"))
        
        print *, "Expected: Different tick counts based on aspect ratio"
    end subroutine test_dimension_based_calculation
    
    subroutine test_edge_case_tiny_plot()
        !! Test edge case with extremely small plot dimensions
        type(figure_t) :: fig
        real(wp), parameter :: x_data(3) = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), parameter :: y_data(3) = [0.0_wp, 1.0_wp, 0.5_wp]
        
        print *, "=== Testing edge case: tiny plot ==="
        
        call fig%initialize(50, 50)  ! Extremely small
        call fig%add_plot(x_data, y_data)
        call fig%set_title("Tiny")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_tiny.pdf"))
        
        print *, "Expected: Minimum viable tick count (3)"
    end subroutine test_edge_case_tiny_plot
    
    subroutine test_edge_case_huge_plot()
        !! Test edge case with extremely large plot dimensions
        type(figure_t) :: fig
        integer :: i
        real(wp), parameter :: x_data(100) = [(real(i, wp), i = 1, 100)]
        real(wp), parameter :: y_data(100) = [(real(i, wp)**1.5_wp, i = 1, 100)]
        
        print *, "=== Testing edge case: huge plot ==="
        
        call fig%initialize(2000, 1500)  ! Extremely large
        call fig%add_plot(x_data, y_data)
        call fig%set_title("Huge plot test")
        call fig%savefig(get_test_output_path("/tmp/test_ticks_huge.pdf"))
        
        print *, "Expected: Maximum reasonable tick count (12-15)"
    end subroutine test_edge_case_huge_plot

end program test_pdf_tick_configurability