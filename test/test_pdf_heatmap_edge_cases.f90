program test_pdf_heatmap_edge_cases
    !! Test edge cases for PDF heatmap/pcolormesh rendering
    !! Validates RGB clamping and edge case handling in PDF output
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic
    use fortplot
    use fortplot_validation, only: validate_pdf_format, validation_result_t
    implicit none
    
    logical :: all_passed = .true.
    integer :: test_count = 0
    
    print *, "=========================================="
    print *, "PDF HEATMAP EDGE CASE TESTS (Issue #323)"
    print *, "=========================================="
    print *, ""
    
    ! Run comprehensive edge case tests
    call test_constant_field()
    call test_nan_handling()
    call test_infinity_handling()
    call test_extreme_ranges()
    call test_epsilon_precision()
    
    ! Report results
    print *, ""
    print *, "=========================================="
    if (all_passed) then
        print '(A,I0,A)', "✅ ALL ", test_count, " TESTS PASSED!"
        print *, "PDF heatmap edge cases handled correctly"
    else
        print *, "❌ SOME TESTS FAILED"
        stop 1
    end if
    
contains
    
    subroutine test_constant_field()
        !! Test constant field (z_max == z_min)
        !! Should produce valid grayscale output without division errors
        
        real(wp) :: x(3), y(3), z(2,2)
        type(figure_t) :: fig
        type(validation_result_t) :: validation
        
        test_count = test_count + 1
        print *, "Test 1: Constant field (z_max == z_min)"
        
        x = [0.0_wp, 1.0_wp, 2.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp]
        z = 5.0_wp  ! Constant value
        
        ! Create figure with PDF backend for proper PDF generation
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x, y, z)
        
        ! Save as PDF
        call fig%savefig('test_constant.pdf')
        
        ! Validate PDF format
        validation = validate_pdf_format('test_constant.pdf')
        
        if (validation%passed) then
            print *, "  ✅ Passed: Constant field handled correctly, valid PDF generated"
        else
            print *, "  ❌ Failed: ", trim(validation%message)
            all_passed = .false.
        end if
    end subroutine test_constant_field
    
    subroutine test_nan_handling()
        !! Test NaN values in z_data
        !! Should handle gracefully without producing invalid PDF
        
        real(wp) :: x(4), y(4), z(3,3)
        real(wp) :: nan_val
        type(figure_t) :: fig
        type(validation_result_t) :: validation
        
        test_count = test_count + 1
        print *, "Test 2: NaN value handling"
        
        nan_val = ieee_value(0.0_wp, ieee_quiet_nan)
        
        x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        
        ! Mix normal and NaN values
        z(1,:) = [1.0_wp, nan_val, 3.0_wp]
        z(2,:) = [nan_val, 5.0_wp, nan_val]
        z(3,:) = [7.0_wp, 8.0_wp, nan_val]
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x, y, z)
        
        call fig%savefig('test_nan.pdf')
        
        ! Validate PDF format
        validation = validate_pdf_format('test_nan.pdf')
        
        if (validation%passed) then
            print *, "  ✅ Passed: NaN values handled correctly, valid PDF generated"
        else
            print *, "  ❌ Failed: ", trim(validation%message)
            all_passed = .false.
        end if
    end subroutine test_nan_handling
    
    subroutine test_infinity_handling()
        !! Test positive and negative infinity
        !! Should clamp to valid range
        
        real(wp) :: x(3), y(3), z(2,2)
        real(wp) :: pos_inf, neg_inf
        type(figure_t) :: fig
        type(validation_result_t) :: validation
        
        test_count = test_count + 1
        print *, "Test 3: Infinity value handling"
        
        pos_inf = ieee_value(0.0_wp, ieee_positive_inf)
        neg_inf = ieee_value(0.0_wp, ieee_negative_inf)
        
        x = [0.0_wp, 1.0_wp, 2.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp]
        
        z(1,:) = [pos_inf, 5.0_wp]
        z(2,:) = [neg_inf, 10.0_wp]
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x, y, z)
        
        call fig%savefig('test_inf.pdf')
        
        ! Validate PDF format
        validation = validate_pdf_format('test_inf.pdf')
        
        if (validation%passed) then
            print *, "  ✅ Passed: Infinity values handled correctly, valid PDF generated"
        else
            print *, "  ❌ Failed: ", trim(validation%message)
            all_passed = .false.
        end if
    end subroutine test_infinity_handling
    
    subroutine test_extreme_ranges()
        !! Test very large and very small value ranges
        !! Should not produce NaN during normalization
        
        real(wp) :: x(3), y(3), z(2,2)
        type(figure_t) :: fig
        type(validation_result_t) :: validation
        
        test_count = test_count + 1
        print *, "Test 4: Extreme value ranges"
        
        x = [0.0_wp, 1.0_wp, 2.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp]
        
        ! Very large range (1e40 span)
        z(1,:) = [1.0e-20_wp, 1.0e20_wp]
        z(2,:) = [5.0e19_wp, 9.0e19_wp]
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x, y, z)
        
        call fig%savefig('test_large_range.pdf')
        
        ! Validate PDF format for large range
        validation = validate_pdf_format('test_large_range.pdf')
        
        if (.not. validation%passed) then
            print *, "  ❌ Failed (large range): ", trim(validation%message)
            all_passed = .false.
        end if
        
        ! Very small range (near epsilon)
        z = 1.0_wp
        z(1,1) = 1.0_wp + 1.0e-15_wp
        z(2,2) = 1.0_wp - 1.0e-15_wp
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x, y, z)
        
        call fig%savefig('test_small_range.pdf')
        
        ! Validate PDF format for small range
        validation = validate_pdf_format('test_small_range.pdf')
        
        if (validation%passed) then
            print *, "  ✅ Passed: Extreme ranges handled correctly, valid PDFs generated"
        else
            print *, "  ❌ Failed (small range): ", trim(validation%message)
            all_passed = .false.
        end if
    end subroutine test_extreme_ranges
    
    subroutine test_epsilon_precision()
        !! Test values near epsilon threshold (1e-10)
        !! Should handle gracefully without division errors
        
        real(wp) :: x(3), y(3), z(2,2)
        type(figure_t) :: fig
        type(validation_result_t) :: validation
        
        test_count = test_count + 1
        print *, "Test 5: Numerical precision near epsilon"
        
        x = [0.0_wp, 1.0_wp, 2.0_wp]
        y = [0.0_wp, 1.0_wp, 2.0_wp]
        
        ! Values just above and below epsilon threshold
        z(1,:) = [1.0_wp, 1.0_wp + 5e-11_wp]
        z(2,:) = [1.0_wp - 5e-11_wp, 1.0_wp + 1e-10_wp]
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x, y, z)
        
        call fig%savefig('test_epsilon.pdf')
        
        ! Validate PDF format
        validation = validate_pdf_format('test_epsilon.pdf')
        
        if (validation%passed) then
            print *, "  ✅ Passed: Epsilon precision handled correctly, valid PDF generated"
        else
            print *, "  ❌ Failed: ", trim(validation%message)
            all_passed = .false.
        end if
    end subroutine test_epsilon_precision
    
end program test_pdf_heatmap_edge_cases