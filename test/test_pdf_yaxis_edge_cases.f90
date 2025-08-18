program test_pdf_yaxis_edge_cases
    !! Edge case tests for PDF Y-axis label positioning
    !! Given: Challenging data scenarios (negative ranges, small ranges, extreme values)
    !! When: Creating PDF plots with Y-axis labels
    !! Then: Should handle edge cases gracefully without clustering or positioning errors
    !! 
    !! Comprehensive edge case testing to ensure robustness of Issue #34 fix

    use fortplot
    use fortplot_label_positioning, only: calculate_y_tick_label_position_pdf
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_negative_value_ranges()
    call test_very_small_ranges()
    call test_very_large_ranges()
    call test_zero_crossing_ranges()
    call test_extreme_coordinate_values()
    
    print *, "=== PDF Y-axis edge case tests completed ==="

contains

    subroutine test_negative_value_ranges()
        !! Given: Data ranges entirely in negative domain
        !! When: Creating PDF plots with negative Y-values
        !! Then: Labels should be positioned correctly in negative coordinate space
        
        type(figure_t) :: fig
        real(wp), parameter :: PLOT_LEFT = 80.0_wp
        
        ! Test data with various negative ranges
        real(wp) :: x_data(8), y_small_neg(8), y_large_neg(8)
        real(wp) :: test_ticks(4) = [100.0_wp, 150.0_wp, 200.0_wp, 250.0_wp]
        real(wp) :: label_x, label_y
        character(len=10) :: neg_labels(4) = ["-10", "-5 ", "-2 ", "-1 "]
        integer :: i
        
        print *, "=== Test: Negative value ranges ==="
        
        ! Create test data with negative ranges
        do i = 1, 8
            x_data(i) = real(i, wp)
            y_small_neg(i) = -real(i, wp) * 0.5_wp     ! Range: [-0.5, -4.0]
            y_large_neg(i) = -real(i, wp) * 100.0_wp   ! Range: [-100, -800]  
        end do
        
        print *, "Small negative range: [", minval(y_small_neg), ",", maxval(y_small_neg), "]"
        print *, "Large negative range: [", minval(y_large_neg), ",", maxval(y_large_neg), "]"
        
        ! Test label positioning for negative values
        print *, "Testing label positioning for negative values:"
        do i = 1, 4
            call calculate_y_tick_label_position_pdf(test_ticks(i), PLOT_LEFT, &
                                                   trim(neg_labels(i)), label_x, label_y)
            print *, "Negative label '", trim(neg_labels(i)), "': tick Y=", test_ticks(i), &
                    "-> label Y=", label_y
            
            ! Basic validation
            if (label_x >= PLOT_LEFT) then
                print *, "  FAIL: Negative value label X should be left of plot"
            else
                print *, "  PASS: Negative value label X positioned correctly"
            end if
        end do
        
        ! Create visual test plots
        call fig%initialize(400, 300)
        call fig%add_plot(x_data, y_small_neg, label="small_negative")
        call fig%savefig("test_pdf_small_negative.pdf")
        print *, "Created test_pdf_small_negative.pdf"
        
        call fig%initialize(400, 300)
        call fig%add_plot(x_data, y_large_neg, label="large_negative")
        call fig%savefig("test_pdf_large_negative.pdf")
        print *, "Created test_pdf_large_negative.pdf"
        print *, ""
        
    end subroutine test_negative_value_ranges

    subroutine test_very_small_ranges()
        !! Given: Data with very small value ranges (near numerical precision limits)
        !! When: Creating PDF plots with tiny Y-ranges
        !! Then: Should handle small ranges without clustering or precision issues
        
        type(figure_t) :: fig
        real(wp) :: x_tiny(6), y_tiny(6)
        real(wp) :: x_micro(6), y_micro(6)
        integer :: i
        
        print *, "=== Test: Very small ranges ==="
        
        ! Create tiny range data (around 1e-3)
        do i = 1, 6
            x_tiny(i) = real(i, wp)
            y_tiny(i) = 1.0e-3_wp + real(i-3, wp) * 1.0e-4_wp  ! Range ≈ [-2e-4, 3e-3]
        end do
        
        ! Create micro range data (around 1e-6)  
        do i = 1, 6
            x_micro(i) = real(i, wp)
            y_micro(i) = 1.0e-6_wp + real(i-3, wp) * 1.0e-7_wp  ! Range ≈ [-2e-7, 3e-6]
        end do
        
        print *, "Tiny range: [", minval(y_tiny), ",", maxval(y_tiny), "]"
        print *, "Micro range: [", minval(y_micro), ",", maxval(y_micro), "]"
        
        ! Test with tiny range
        call fig%initialize(400, 300)
        call fig%add_plot(x_tiny, y_tiny, label="tiny_range")
        call fig%savefig("test_pdf_tiny_range.pdf")
        print *, "Created test_pdf_tiny_range.pdf"
        
        ! Test with micro range  
        call fig%initialize(400, 300)
        call fig%add_plot(x_micro, y_micro, label="micro_range")
        call fig%savefig("test_pdf_micro_range.pdf")
        print *, "Created test_pdf_micro_range.pdf"
        
        print *, "Small range tests completed - check for label clustering"
        print *, ""
        
    end subroutine test_very_small_ranges

    subroutine test_very_large_ranges()
        !! Given: Data with very large value ranges
        !! When: Creating PDF plots with huge Y-ranges
        !! Then: Should handle large ranges without coordinate overflow or clustering
        
        type(figure_t) :: fig
        real(wp) :: x_large(6), y_large(6)
        real(wp) :: x_huge(6), y_huge(6)
        integer :: i
        
        print *, "=== Test: Very large ranges ==="
        
        ! Create large range data (around 1e6)
        do i = 1, 6
            x_large(i) = real(i, wp)
            y_large(i) = real(i-3, wp) * 1.0e6_wp  ! Range: [-2e6, 3e6]
        end do
        
        ! Create huge range data (around 1e9)
        do i = 1, 6
            x_huge(i) = real(i, wp)  
            y_huge(i) = real(i-3, wp) * 1.0e9_wp   ! Range: [-2e9, 3e9]
        end do
        
        print *, "Large range: [", minval(y_large), ",", maxval(y_large), "]"
        print *, "Huge range: [", minval(y_huge), ",", maxval(y_huge), "]"
        
        ! Test with large range
        call fig%initialize(400, 300)
        call fig%add_plot(x_large, y_large, label="large_range")
        call fig%savefig("test_pdf_large_range.pdf")
        print *, "Created test_pdf_large_range.pdf"
        
        ! Test with huge range
        call fig%initialize(400, 300)
        call fig%add_plot(x_huge, y_huge, label="huge_range")
        call fig%savefig("test_pdf_huge_range.pdf")
        print *, "Created test_pdf_huge_range.pdf"
        
        print *, "Large range tests completed - check for coordinate handling"
        print *, ""
        
    end subroutine test_very_large_ranges

    subroutine test_zero_crossing_ranges()
        !! Given: Data ranges that cross zero with various symmetries
        !! When: Creating PDF plots with zero-crossing data
        !! Then: Should handle zero-crossing without clustering at origin
        
        type(figure_t) :: fig
        real(wp) :: x_data(10)
        real(wp) :: y_symmetric(10), y_asymmetric_pos(10), y_asymmetric_neg(10)
        integer :: i
        
        print *, "=== Test: Zero-crossing ranges ==="
        
        do i = 1, 10
            x_data(i) = real(i, wp)
            
            ! Symmetric around zero
            y_symmetric(i) = real(i-5, wp) * 2.0_wp     ! Range: [-8, 10]
            
            ! Asymmetric toward positive  
            y_asymmetric_pos(i) = real(i-2, wp) * 3.0_wp  ! Range: [-3, 24]
            
            ! Asymmetric toward negative
            y_asymmetric_neg(i) = real(i-8, wp) * 2.0_wp  ! Range: [-14, 4]
        end do
        
        print *, "Symmetric range: [", minval(y_symmetric), ",", maxval(y_symmetric), "]"
        print *, "Asymmetric positive: [", minval(y_asymmetric_pos), ",", maxval(y_asymmetric_pos), "]"
        print *, "Asymmetric negative: [", minval(y_asymmetric_neg), ",", maxval(y_asymmetric_neg), "]"
        
        ! Test symmetric zero-crossing
        call fig%initialize(400, 300)
        call fig%add_plot(x_data, y_symmetric, label="symmetric_zero")
        call fig%savefig("test_pdf_symmetric_zero.pdf")
        print *, "Created test_pdf_symmetric_zero.pdf"
        
        ! Test asymmetric positive
        call fig%initialize(400, 300)
        call fig%add_plot(x_data, y_asymmetric_pos, label="asymmetric_pos")
        call fig%savefig("test_pdf_asymmetric_pos.pdf")
        print *, "Created test_pdf_asymmetric_pos.pdf"
        
        ! Test asymmetric negative  
        call fig%initialize(400, 300)
        call fig%add_plot(x_data, y_asymmetric_neg, label="asymmetric_neg")
        call fig%savefig("test_pdf_asymmetric_neg.pdf")
        print *, "Created test_pdf_asymmetric_neg.pdf"
        
        print *, "Zero-crossing tests completed - verify no origin clustering"
        print *, ""
        
    end subroutine test_zero_crossing_ranges

    subroutine test_extreme_coordinate_values()
        !! Given: Extreme coordinate values that might trigger edge conditions
        !! When: Using calculate_y_tick_label_position_pdf with extreme inputs
        !! Then: Should handle extremes gracefully without errors or invalid results
        
        real(wp), parameter :: PLOT_LEFT = 80.0_wp
        
        real(wp) :: extreme_ticks(6) = [0.1_wp, 1.0_wp, 10.0_wp, 1000.0_wp, 9999.0_wp, 1.0e5_wp]
        real(wp) :: label_x, label_y
        character(len=15) :: extreme_labels(6) = ["-1e10", "-1000", "0    ", "1000 ", "1e6  ", "1e10 "]
        logical :: extreme_failed
        integer :: i
        
        print *, "=== Test: Extreme coordinate values ==="
        
        extreme_failed = .false.
        do i = 1, 6
            call calculate_y_tick_label_position_pdf(extreme_ticks(i), PLOT_LEFT, &
                                                   trim(extreme_labels(i)), label_x, label_y)
            
            print *, "Extreme test", i, ": tick Y=", extreme_ticks(i), &
                    "-> label at (", label_x, ",", label_y, ")"
            
            ! Check for reasonable positioning (should not be wildly out of range)
            if (label_y < -1000.0_wp .or. label_y > 10000.0_wp) then
                extreme_failed = .true.
                print *, "  FAIL: Extreme label Y position suggests coordinate error"
            else
                print *, "  PASS: Extreme label Y position reasonable"
            end if
            
            ! Check X positioning
            if (label_x >= PLOT_LEFT) then
                extreme_failed = .true.
                print *, "  FAIL: Extreme label X should be left of plot"
            else
                print *, "  PASS: Extreme label X positioned correctly"
            end if
            
            ! Check for NaN or infinity
            if (label_x /= label_x .or. label_y /= label_y) then  ! NaN check
                extreme_failed = .true.
                print *, "  FAIL: NaN detected in extreme coordinate calculation"
            end if
        end do
        
        if (extreme_failed) then
            print *, "FAIL: Extreme coordinate value handling issues detected"
        else
            print *, "PASS: Extreme coordinates handled properly"
        end if
        print *, ""
        
    end subroutine test_extreme_coordinate_values

end program test_pdf_yaxis_edge_cases