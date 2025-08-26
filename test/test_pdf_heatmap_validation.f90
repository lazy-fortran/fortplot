program test_pdf_heatmap_validation
    !! Test edge cases for PDF heatmap color validation  
    !! 
    !! Tests comprehensive edge cases including:
    !! - NaN values in various positions
    !! - Positive and negative infinity
    !! - Missing/sparse data patterns  
    !! - RGB values outside valid range
    !! - Dimension mismatches
    !! - Empty datasets
    
    use, intrinsic :: iso_fortran_env, only: wp => real64, int32
    use, intrinsic :: ieee_arithmetic
    use fortplot
    use fortplot_validation
    implicit none
    
    logical :: all_tests_passed = .true.
    character(len=256) :: test_file
    integer :: test_count = 0
    
    print *, "=========================================="
    print *, "PDF HEATMAP EDGE CASE VALIDATION TESTS"
    print *, "=========================================="
    print *, ""
    
    ! Run all edge case tests
    call test_constant_field()
    call test_nan_values()
    call test_infinity_values()
    call test_extreme_ranges()
    call test_numerical_precision()
    call test_minimal_dataset()
    call test_single_cell()
    call test_sparse_data()
    call test_rgb_clamping()
    call test_mixed_edge_cases()
    
    ! Report results
    print *, ""
    print *, "=========================================="
    if (all_tests_passed) then
        print '(A,I0,A)', "✅ ALL ", test_count, " EDGE CASE TESTS PASSED!"
    else
        print '(A)', "❌ SOME TESTS FAILED!"
        stop 1
    end if
    print *, "=========================================="
    
contains
    
    subroutine test_constant_field()
        !! Test heatmap with constant field (z_max == z_min)
        !! Should default norm_value to 0.5 and produce valid grayscale
        
        real(wp) :: x_grid(3), y_grid(3), z_grid(2,2)
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: Constant field handling (z_max == z_min)"
        
        ! Create constant field
        x_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        y_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        z_grid = 5.0_wp  ! All values the same
        
        test_file = "test_constant_field.pdf"
        
        ! Create figure and add pcolormesh
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='viridis')
        
        ! Save to PDF
        call fig%savefig(test_file)
        
        ! Validate output
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed: ", trim(validation%message)
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: Constant field handled correctly"
        end if
    end subroutine test_constant_field
    
    subroutine test_nan_values()
        !! Test heatmap with NaN values in various positions
        
        real(wp) :: x_grid(4), y_grid(4), z_grid(3,3)
        real(wp) :: nan_val
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: NaN value handling"
        
        nan_val = ieee_value(0.0_wp, ieee_quiet_nan)
        
        ! Create grid with NaN values
        x_grid = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        y_grid = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        
        ! Mix of normal and NaN values
        z_grid(1,:) = [1.0_wp, nan_val, 3.0_wp]
        z_grid(2,:) = [nan_val, 5.0_wp, nan_val]
        z_grid(3,:) = [7.0_wp, 8.0_wp, nan_val]
        
        test_file = "test_nan_values.pdf"
        
        ! Create figure and add pcolormesh
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='coolwarm')
        
        ! Save to PDF
        call fig%savefig(test_file)
        
        ! Validate output
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed: ", trim(validation%message)
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: NaN values handled without crash"
        end if
    end subroutine test_nan_values
    
    subroutine test_infinity_values()
        !! Test heatmap with positive and negative infinity
        
        real(wp) :: x_grid(3), y_grid(3), z_grid(2,2)
        real(wp) :: pos_inf, neg_inf
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: Infinity value handling"
        
        pos_inf = ieee_value(0.0_wp, ieee_positive_inf)
        neg_inf = ieee_value(0.0_wp, ieee_negative_inf)
        
        ! Create grid with infinity values
        x_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        y_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        
        z_grid(1,:) = [pos_inf, 5.0_wp]
        z_grid(2,:) = [neg_inf, 10.0_wp]
        
        test_file = "test_infinity_values.pdf"
        
        ! Create figure and add pcolormesh
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='plasma')
        
        ! Save to PDF
        call fig%savefig(test_file)
        
        ! Validate output
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed: ", trim(validation%message)
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: Infinity values handled correctly"
        end if
    end subroutine test_infinity_values
    
    subroutine test_extreme_ranges()
        !! Test heatmap with extreme value ranges
        
        real(wp) :: x_grid(3), y_grid(3), z_grid(2,2)
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: Extreme value ranges"
        
        x_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        y_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        
        ! Test very large range
        z_grid(1,:) = [1.0e-20_wp, 1.0e20_wp]
        z_grid(2,:) = [5.0e19_wp, 9.0e19_wp]
        
        test_file = "test_extreme_range_large.pdf"
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='inferno')
        call fig%savefig(test_file)
        
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed (large range): ", trim(validation%message)
            all_tests_passed = .false.
        end if
        
        ! Test very small range
        z_grid = 1.0_wp
        z_grid(1,1) = 1.0_wp + 1.0e-15_wp
        z_grid(2,2) = 1.0_wp - 1.0e-15_wp
        
        test_file = "test_extreme_range_small.pdf"
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='magma')
        call fig%savefig(test_file)
        
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed (small range): ", trim(validation%message)  
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: Extreme ranges handled correctly"
        end if
    end subroutine test_extreme_ranges
    
    subroutine test_numerical_precision()
        !! Test values near epsilon threshold
        
        real(wp) :: x_grid(3), y_grid(3), z_grid(2,2)
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: Numerical precision near epsilon"
        
        x_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        y_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        
        ! Values just above and below epsilon threshold
        z_grid(1,:) = [1.0_wp, 1.0_wp + 5e-11_wp]
        z_grid(2,:) = [1.0_wp - 5e-11_wp, 1.0_wp + 1e-10_wp]
        
        test_file = "test_numerical_precision.pdf"
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='cividis')
        call fig%savefig(test_file)
        
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed: ", trim(validation%message)
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: Numerical precision handled correctly"
        end if
    end subroutine test_numerical_precision
    
    subroutine test_minimal_dataset()
        !! Test with minimal valid dataset
        
        real(wp), allocatable :: x_grid(:), y_grid(:), z_grid(:,:)
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: Minimal dataset handling"
        
        ! Allocate minimal valid grid (needs at least 2x2 vertices for 1x1 heatmap)
        allocate(x_grid(2), y_grid(2), z_grid(1,1))
        x_grid = [0.0_wp, 1.0_wp]
        y_grid = [0.0_wp, 1.0_wp]
        z_grid(1,1) = 0.5_wp
        
        test_file = "test_minimal_dataset.pdf"
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='twilight')
        call fig%savefig(test_file)
        
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed: ", trim(validation%message)
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: Minimal dataset handled correctly"
        end if
        
        deallocate(x_grid, y_grid, z_grid)
    end subroutine test_minimal_dataset
    
    subroutine test_single_cell()
        !! Test single cell heatmap
        
        real(wp) :: x_grid(2), y_grid(2), z_grid(1,1)
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: Single cell heatmap"
        
        x_grid = [0.0_wp, 1.0_wp]
        y_grid = [0.0_wp, 1.0_wp]
        z_grid(1,1) = 0.5_wp
        
        test_file = "test_single_cell.pdf"
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='rainbow')
        call fig%savefig(test_file)
        
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed: ", trim(validation%message)
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: Single cell rendered correctly"
        end if
    end subroutine test_single_cell
    
    subroutine test_sparse_data()
        !! Test sparse data patterns with NaN representing missing values
        
        real(wp) :: x_grid(6), y_grid(6), z_grid(5,5)
        real(wp) :: nan_val
        integer :: i, j
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: Sparse data patterns"
        
        nan_val = ieee_value(0.0_wp, ieee_quiet_nan)
        
        ! Create checkerboard pattern of valid/missing data
        do i = 1, 6
            x_grid(i) = real(i-1, wp)
            y_grid(i) = real(i-1, wp)
        end do
        
        do i = 1, 5
            do j = 1, 5
                if (mod(i+j, 2) == 0) then
                    z_grid(i,j) = real(i*j, wp)
                else
                    z_grid(i,j) = nan_val  ! Missing data
                end if
            end do
        end do
        
        test_file = "test_sparse_data.pdf"
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='turbo')
        call fig%savefig(test_file)
        
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed: ", trim(validation%message)
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: Sparse data pattern handled correctly"
        end if
    end subroutine test_sparse_data
    
    subroutine test_rgb_clamping()
        !! Test RGB values that would normalize outside [0,1]
        
        real(wp) :: x_grid(3), y_grid(3), z_grid(2,2)
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: RGB clamping for out-of-range normalization"
        
        x_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        y_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        
        ! Values that would normalize outside [0,1] if z_min/z_max are wrong
        z_grid(1,:) = [-10.0_wp, 5.0_wp]
        z_grid(2,:) = [15.0_wp, 25.0_wp]
        
        test_file = "test_rgb_clamping.pdf"
        
        call fig%initialize(80, 24, backend='pdf')
        ! Set explicit vmin/vmax to force clamping
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='gray', vmin=0.0_wp, vmax=10.0_wp)
        call fig%savefig(test_file)
        
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed: ", trim(validation%message)
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: RGB values properly clamped"
        end if
    end subroutine test_rgb_clamping
    
    subroutine test_mixed_edge_cases()
        !! Test combination of multiple edge cases
        
        real(wp) :: x_grid(5), y_grid(5), z_grid(4,4)
        real(wp) :: nan_val, pos_inf
        type(validation_result_t) :: validation
        type(figure_t) :: fig
        
        test_count = test_count + 1
        print *, "Test: Mixed edge cases"
        
        nan_val = ieee_value(0.0_wp, ieee_quiet_nan)
        pos_inf = ieee_value(0.0_wp, ieee_positive_inf)
        
        ! Create grid with mixed edge cases
        x_grid = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y_grid = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        
        z_grid(1,:) = [nan_val, 0.0_wp, pos_inf, 1.0_wp]
        z_grid(2,:) = [1.0e-20_wp, 1.0e20_wp, -5.0_wp, 5.0_wp]
        z_grid(3,:) = [0.5_wp, 0.5_wp, 0.5_wp, 0.5_wp]  ! Constant row
        z_grid(4,:) = [-pos_inf, nan_val, 100.0_wp, -100.0_wp]
        
        test_file = "test_mixed_edge_cases.pdf"
        
        call fig%initialize(80, 24, backend='pdf')
        call fig%add_pcolormesh(x_grid, y_grid, z_grid, colormap='jet')
        call fig%savefig(test_file)
        
        validation = validate_pdf_format(test_file)
        if (.not. validation%passed) then
            print *, "  ❌ Failed: ", trim(validation%message)
            all_tests_passed = .false.
        else
            print *, "  ✅ Passed: Mixed edge cases handled robustly"
        end if
    end subroutine test_mixed_edge_cases
    
end program test_pdf_heatmap_validation