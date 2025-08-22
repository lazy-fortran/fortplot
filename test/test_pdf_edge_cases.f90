program test_pdf_edge_cases
    !! Stress test edge cases for PDF Y-axis label overlap fix
    use fortplot
    use fortplot_security, only: get_test_output_path
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_extreme_tight_range()
    call test_many_tick_candidates()
    call test_zero_crossing_precision()
    call test_negative_small_values()
    call test_mixed_scale_scenarios()
    
    print *, "All PDF edge case tests completed!"
    
contains

    subroutine test_extreme_tight_range()
        !! Test extremely tight Y-axis range that would create many overlapping labels
        type(figure_t) :: fig
        real(wp), parameter :: x_data(3) = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), parameter :: y_data(3) = [-0.001_wp, 0.0_wp, 0.001_wp]
        
        print *, "=== Testing extreme tight range ==="
        
        call fig%initialize(640, 480)
        call fig%add_plot(x_data, y_data)
        call fig%set_ylim(-0.0012_wp, 0.0012_wp)  ! Extremely tight
        call fig%set_title("Extreme tight range test")
        call fig%set_ylabel("Micro values")
        call figure_savefig(fig, get_test_output_path("/tmp/test_extreme_tight.pdf"))
        
        print *, "Expected: Maximum label filtering, only essential labels shown"
    end subroutine test_extreme_tight_range
    
    subroutine test_many_tick_candidates()
        !! Test scenario that would generate many candidate tick labels
        type(figure_t) :: fig
        real(wp), parameter :: x_vals(20) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                             6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp, &
                                             11.0_wp, 12.0_wp, 13.0_wp, 14.0_wp, 15.0_wp, &
                                             16.0_wp, 17.0_wp, 18.0_wp, 19.0_wp, 20.0_wp]
        real(wp), parameter :: y_vals(20) = [-0.095_wp, -0.090_wp, -0.085_wp, -0.080_wp, -0.075_wp, &
                                             -0.070_wp, -0.065_wp, -0.060_wp, -0.055_wp, -0.050_wp, &
                                             -0.045_wp, -0.040_wp, -0.035_wp, -0.030_wp, -0.025_wp, &
                                             -0.020_wp, -0.015_wp, -0.010_wp, -0.005_wp, 0.000_wp]
        
        print *, "=== Testing many tick candidates ==="
        
        call fig%initialize(500, 700)  ! Tall figure for more Y-space
        call fig%add_plot(x_vals, y_vals)
        call fig%set_ylim(-0.1_wp, 0.005_wp)
        call fig%set_title("Many tick candidates test")
        call fig%set_ylabel("Dense value range")
        call figure_savefig(fig, get_test_output_path("/tmp/test_many_candidates.pdf"))
        
        print *, "Expected: Intelligent filtering preserves key values, prevents crowding"
    end subroutine test_many_tick_candidates
    
    subroutine test_zero_crossing_precision()
        !! Test precise zero crossing with tiny values on both sides
        type(figure_t) :: fig
        real(wp), parameter :: x_precise(7) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp, 7.0_wp]
        real(wp), parameter :: y_precise(7) = [-0.0003_wp, -0.0001_wp, -0.00005_wp, 0.0_wp, &
                                               0.00005_wp, 0.0001_wp, 0.0003_wp]
        
        print *, "=== Testing zero crossing precision ==="
        
        call fig%initialize(600, 500)
        call fig%add_plot(x_precise, y_precise)
        call fig%set_ylim(-0.00035_wp, 0.00035_wp)
        call fig%set_title("Zero crossing precision test")
        call fig%set_ylabel("Precision values around zero")
        call figure_savefig(fig, get_test_output_path("/tmp/test_zero_precision.pdf"))
        
        print *, "Expected: Zero value preserved, symmetric labeling, no overlap"
    end subroutine test_zero_crossing_precision
    
    subroutine test_negative_small_values()
        !! Test all negative small values near zero
        type(figure_t) :: fig
        real(wp), parameter :: x_neg(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), parameter :: y_neg(5) = [-0.08_wp, -0.06_wp, -0.04_wp, -0.02_wp, -0.01_wp]
        
        print *, "=== Testing negative small values ==="
        
        call fig%initialize(640, 480)
        call fig%add_plot(x_neg, y_neg)
        call fig%set_ylim(-0.09_wp, -0.005_wp)
        call fig%set_title("Negative small values test")
        call fig%set_ylabel("All negative range")
        call figure_savefig(fig, get_test_output_path("/tmp/test_negative_small.pdf"))
        
        print *, "Expected: Proper negative value formatting, no label overlap"
    end subroutine test_negative_small_values
    
    subroutine test_mixed_scale_scenarios()
        !! Test mixed scenarios that combine different scale challenges
        type(figure_t) :: fig
        real(wp), parameter :: x_mixed(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                              6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]
        real(wp), parameter :: y_mixed(10) = [-0.1_wp, -0.05_wp, -0.01_wp, -0.001_wp, 0.0_wp, &
                                              0.001_wp, 0.01_wp, 0.05_wp, 0.1_wp, 0.2_wp]
        
        print *, "=== Testing mixed scale scenarios ==="
        
        call fig%initialize(800, 600)
        call fig%add_plot(x_mixed, y_mixed)
        call fig%set_ylim(-0.12_wp, 0.22_wp)
        call fig%set_title("Mixed scale scenarios test")
        call fig%set_ylabel("Mixed magnitude values")
        call figure_savefig(fig, get_test_output_path("/tmp/test_mixed_scales.pdf"))
        
        print *, "Expected: Balanced label distribution across different magnitudes"
    end subroutine test_mixed_scale_scenarios

end program test_pdf_edge_cases