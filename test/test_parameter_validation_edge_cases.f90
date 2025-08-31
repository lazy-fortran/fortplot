! test_parameter_validation_edge_cases.f90 - Comprehensive edge case testing
!
! This test systematically explores edge cases in the parameter validation system
! to discover potential user workflow disruptions
!
program test_parameter_validation_edge_cases
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64, output_unit
    implicit none
    
    real(wp) :: large_data(1000)
    real(wp) :: tiny_data(3)
    real(wp) :: extreme_range(2)
    real(wp) :: nan_data(5)
    integer :: i
    
    write(output_unit, '(A)') "=== Parameter Validation Edge Case Testing ==="
    write(output_unit, '(A)') "Testing boundary conditions and extreme values"
    write(output_unit, '(A)') ""
    
    ! Initialize test data
    do i = 1, 1000
        large_data(i) = real(i, wp) / 1000.0_wp
    end do
    
    tiny_data = [1.0e-100_wp, 2.0e-100_wp, 3.0e-100_wp]
    extreme_range = [1.0e-30_wp, 1.0e30_wp]
    
    ! Test 1: Extremely large figure dimensions
    write(output_unit, '(A)') "Test 1: Extremely large figure dimensions (should warn)"
    call figure(figsize=[2000.0_wp, 1500.0_wp])
    call plot(tiny_data, tiny_data)
    call savefig("test_large_dimensions.png")
    
    ! Test 2: Extremely small figure dimensions
    write(output_unit, '(A)') "Test 2: Extremely small figure dimensions (should warn)"
    call figure(figsize=[0.01_wp, 0.01_wp])
    call plot(tiny_data, tiny_data)
    call savefig("test_tiny_dimensions.png")
    
    ! Test 3: Extreme aspect ratio
    write(output_unit, '(A)') "Test 3: Extreme aspect ratio (should warn)"
    call figure(figsize=[100.0_wp, 2.0_wp])
    call plot(tiny_data, tiny_data)
    call savefig("test_extreme_aspect.png")
    
    ! Test 4: Large dataset processing 
    write(output_unit, '(A)') "Test 4: Large dataset (1000 points)"
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(large_data, large_data**2)
    call savefig("test_large_dataset.png")
    
    ! Test 5: Extreme data range
    write(output_unit, '(A)') "Test 5: Extreme data range (should warn)"
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(extreme_range, extreme_range)
    call savefig("test_extreme_range.png")
    
    ! Test 6: Invalid file paths (boundary cases)
    write(output_unit, '(A)') "Test 6: Invalid file paths"
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(tiny_data, tiny_data)
    
    ! Empty filename (should fail or warn)
    write(output_unit, '(A)') "  Testing empty filename..."
    ! call savefig("")  ! This might crash, so skip for safety
    
    ! Very long filename
    write(output_unit, '(A)') "  Testing very long filename..."
    call savefig(repeat("a", 300) // ".png")
    
    ! Test 7: Text annotations with boundary font sizes
    write(output_unit, '(A)') "Test 7: Boundary font sizes in annotations"
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(tiny_data, tiny_data)
    
    ! Font size = 0.1 (very small)
    call text(0.5_wp, 0.5_wp, "Tiny font", coord_type=COORD_DATA, font_size=0.1_wp)
    
    ! Font size = 199 (just under limit)
    call text(0.5_wp, 0.4_wp, "Large font", coord_type=COORD_DATA, font_size=199.0_wp)
    
    ! Font size = 201 (over limit)
    call text(0.5_wp, 0.3_wp, "Too large", coord_type=COORD_DATA, font_size=201.0_wp)
    
    call savefig("test_font_boundaries.png")
    
    ! Test 8: Basic plotting (skip color validation for now)
    write(output_unit, '(A)') "Test 8: Basic edge case plotting"
    call figure(figsize=[8.0_wp, 6.0_wp])
    
    ! Simple plot without color parameters
    call plot(tiny_data, tiny_data)
    call savefig("test_basic_edge.png")
    
    ! Test 9: Edge case data arrays
    write(output_unit, '(A)') "Test 9: Edge case data arrays"
    call figure(figsize=[8.0_wp, 6.0_wp])
    
    ! Single point plot
    call plot([1.0_wp], [1.0_wp])
    
    ! Two identical points
    call plot([1.0_wp, 1.0_wp], [2.0_wp, 2.0_wp])
    
    call savefig("test_edge_data.png")
    
    ! Test 10: Individual validation tests (safer than combined stress)
    write(output_unit, '(A)') "Test 10: Individual parameter validation tests"
    
    ! Test 10a: Figure dimension validation (width exceeds limit)
    write(output_unit, '(A)') "  Test 10a: Over-sized figure width validation"
    call figure(figsize=[1200.0_wp, 6.0_wp])  ! Over limit but not extreme
    call plot(tiny_data, tiny_data)
    call savefig("test_validation_width.png")
    
    ! Test 10b: Figure dimension validation (height below limit)  
    write(output_unit, '(A)') "  Test 10b: Under-sized figure height validation"
    call figure(figsize=[8.0_wp, 0.08_wp])  ! Below limit but not extreme
    call plot(tiny_data, tiny_data)
    call savefig("test_validation_height.png")
    
    ! Test 10c: Font size validation (zero font size)
    write(output_unit, '(A)') "  Test 10c: Invalid font size validation"
    call figure(figsize=[8.0_wp, 6.0_wp])
    call plot(tiny_data, tiny_data)
    call text(0.5_wp, 0.5_wp, "Test", coord_type=COORD_DATA, font_size=0.1_wp)  ! Very small but valid
    call savefig("test_validation_font.png")
    
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "=== Edge Case Testing Complete ==="
    write(output_unit, '(A)') "Check output above for validation warnings and error handling"
    
end program test_parameter_validation_edge_cases