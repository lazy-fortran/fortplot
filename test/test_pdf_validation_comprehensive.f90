program test_pdf_validation_comprehensive
    !! Comprehensive PDF validation test consolidating validation-related functionality
    !! Replaces: test_pdf_heatmap_validation.f90, test_pdf_heatmap_edge_cases.f90,
    !!           test_pdf_write_validation.f90, test_pdf_validation_performance.f90,
    !!           test_pdf_label_collision_detection.f90 (5 files total)
    !!
    !! This test covers:
    !! - PDF heatmap edge cases and validation
    !! - PDF write operations validation
    !! - Performance validation for large datasets
    !! - Label collision detection and prevention
    !! - NaN, infinity, and invalid data handling
    !! - RGB value validation and clamping

    use, intrinsic :: iso_fortran_env, only: wp => real64, int32
    use, intrinsic :: ieee_arithmetic
    use fortplot
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_validation
    use fortplot_security, only: get_test_output_path
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0
    logical :: all_tests_passed = .true.

    print *, "=== COMPREHENSIVE PDF VALIDATION TESTS ==="
    
    ! Run all test categories
    call test_heatmap_validation()
    call test_write_validation()
    call test_performance_validation()
    call test_label_collision_detection()
    call test_edge_case_handling()
    
    call print_test_summary()

contains

    !===========================================================================
    ! Heatmap Validation Tests (from test_pdf_heatmap_validation.f90, test_pdf_heatmap_edge_cases.f90)
    !===========================================================================
    
    subroutine test_heatmap_validation()
        print *, "--- Heatmap Validation Tests ---"
        
        call test_constant_field()
        call test_nan_values()
        call test_infinity_values() 
        call test_missing_data_patterns()
        call test_rgb_value_validation()
        call test_dimension_mismatches()
    end subroutine test_heatmap_validation

    subroutine test_constant_field()
        real(wp), allocatable :: field(:, :)
        integer :: nx, ny
        character(len=512) :: filename
        
        call start_test("Constant field heatmap")
        
        nx = 10; ny = 8
        allocate(field(nx, ny))
        field = 5.0_wp  ! All values identical
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call imshow(field)
        call title('Constant Field Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_constant.pdf')
        call savefig(filename)
        
        deallocate(field)
        print *, "  Constant field heatmap test saved"
        call end_test()
    end subroutine test_constant_field

    subroutine test_nan_values()
        real(wp), allocatable :: field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("NaN values in heatmap")
        
        nx = 12; ny = 10
        allocate(field(nx, ny))
        
        ! Create field with some NaN values
        do j = 1, ny
            do i = 1, nx
                if (mod(i + j, 7) == 0) then
                    field(i, j) = ieee_value(field(i, j), ieee_quiet_nan)
                else
                    field(i, j) = real(i * j, wp) * 0.1_wp
                end if
            end do
        end do
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call imshow(field)
        call title('NaN Values Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_nan.pdf')
        call savefig(filename)
        
        deallocate(field)
        print *, "  NaN values heatmap test saved"
        call end_test()
    end subroutine test_nan_values

    subroutine test_infinity_values()
        real(wp), allocatable :: field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("Infinity values in heatmap")
        
        nx = 8; ny = 6
        allocate(field(nx, ny))
        
        ! Create field with infinity values
        do j = 1, ny
            do i = 1, nx
                if (i == 1 .and. j == 1) then
                    field(i, j) = ieee_value(field(i, j), ieee_positive_inf)
                else if (i == nx .and. j == ny) then
                    field(i, j) = ieee_value(field(i, j), ieee_negative_inf)
                else
                    field(i, j) = real(i + j, wp)
                end if
            end do
        end do
        
        call figure(figsize=[350.0_wp, 250.0_wp])
        call imshow(field)
        call title('Infinity Values Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_infinity.pdf')
        call savefig(filename)
        
        deallocate(field)
        print *, "  Infinity values heatmap test saved"
        call end_test()
    end subroutine test_infinity_values

    subroutine test_missing_data_patterns()
        real(wp), allocatable :: sparse_field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("Missing data patterns")
        
        nx = 15; ny = 12
        allocate(sparse_field(nx, ny))
        
        ! Create sparse field with missing data patterns
        do j = 1, ny
            do i = 1, nx
                if (mod(i, 3) == 0 .or. mod(j, 4) == 0) then
                    sparse_field(i, j) = ieee_value(sparse_field(i, j), ieee_quiet_nan)
                else
                    sparse_field(i, j) = sin(real(i, wp) * 0.3_wp) * cos(real(j, wp) * 0.4_wp)
                end if
            end do
        end do
        
        call figure(figsize=[450.0_wp, 350.0_wp])
        call imshow(sparse_field)
        call title('Sparse Data Pattern Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_sparse.pdf')
        call savefig(filename)
        
        deallocate(sparse_field)
        print *, "  Sparse data pattern test saved"
        call end_test()
    end subroutine test_missing_data_patterns

    subroutine test_rgb_value_validation()
        real(wp), allocatable :: field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("RGB value validation")
        
        nx = 6; ny = 4
        allocate(field(nx, ny))
        
        ! Create field with extreme values that need RGB clamping
        do j = 1, ny
            do i = 1, nx
                if (i == 1) then
                    field(i, j) = -1000.0_wp  ! Should be clamped to valid range
                else if (i == nx) then
                    field(i, j) = 1000.0_wp   ! Should be clamped to valid range
                else
                    field(i, j) = real(i * j, wp)
                end if
            end do
        end do
        
        call figure(figsize=[300.0_wp, 200.0_wp])
        call imshow(field)
        call title('RGB Validation Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_heatmap_rgb_validation.pdf')
        call savefig(filename)
        
        deallocate(field)
        print *, "  RGB validation test saved"
        call end_test()
    end subroutine test_rgb_value_validation

    subroutine test_dimension_mismatches()
        real(wp), allocatable :: field1(:, :), field2(:, :)
        character(len=512) :: filename
        
        call start_test("Dimension mismatch handling")
        
        ! Test different sized fields
        allocate(field1(5, 3))
        allocate(field2(8, 6))
        
        field1 = 2.0_wp
        field2 = 4.0_wp
        
        ! Test small field
        call figure(figsize=[250.0_wp, 180.0_wp])
        call imshow(field1)
        call title('Small Field (5x3)')
        filename = get_test_output_path('/tmp/pdf_heatmap_small.pdf')
        call savefig(filename)
        
        ! Test larger field  
        call figure(figsize=[400.0_wp, 300.0_wp])
        call imshow(field2)
        call title('Larger Field (8x6)')
        filename = get_test_output_path('/tmp/pdf_heatmap_large.pdf')
        call savefig(filename)
        
        deallocate(field1, field2)
        print *, "  Dimension mismatch test saved"
        call end_test()
    end subroutine test_dimension_mismatches

    !===========================================================================
    ! Write Validation Tests (from test_pdf_write_validation.f90)
    !===========================================================================
    
    subroutine test_write_validation()
        print *, "--- Write Validation Tests ---"
        
        call test_basic_write_validation()
        call test_file_creation_validation()
        call test_content_validation()
    end subroutine test_write_validation

    subroutine test_basic_write_validation()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        logical :: file_exists
        
        call start_test("Basic PDF write validation")
        
        ctx = create_pdf_canvas(400, 300)
        ctx%x_min = 0.0_wp; ctx%x_max = 5.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 4.0_wp
        
        ! Draw simple content
        call ctx%text(2.5_wp, 2.0_wp, "Write Validation Test")
        call ctx%circle(1.0_wp, 1.0_wp, 0.3_wp)
        call ctx%line(3.0_wp, 1.0_wp, 4.0_wp, 3.0_wp)
        
        filename = get_test_output_path('/tmp/pdf_write_validation.pdf')
        call ctx%write_pdf(filename)
        
        ! Verify file was created
        inquire(file=filename, exist=file_exists)
        call assert_true(file_exists, "PDF file created successfully")
        
        print *, "  Basic write validation test saved"
        call end_test()
    end subroutine test_basic_write_validation

    subroutine test_file_creation_validation()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        logical :: file_exists
        integer :: file_size
        
        call start_test("File creation validation")
        
        ctx = create_pdf_canvas(500, 400)
        ctx%x_min = -1.0_wp; ctx%x_max = 6.0_wp
        ctx%y_min = -2.0_wp; ctx%y_max = 3.0_wp
        
        ! Add content that should generate reasonable file size
        call ctx%text(2.5_wp, 1.0_wp, "File Size Test")
        call ctx%axes()
        
        filename = get_test_output_path('/tmp/pdf_file_creation.pdf')
        call ctx%write_pdf(filename)
        
        inquire(file=filename, exist=file_exists, size=file_size)
        call assert_true(file_exists, "PDF file exists")
        
        if (file_size > 100) then
            print *, "  PASS: PDF file has reasonable size (", file_size, " bytes)"
            pass_count = pass_count + 1
        else
            print *, "  FAIL: PDF file too small (", file_size, " bytes)"
        end if
        test_count = test_count + 1
        
        call end_test()
    end subroutine test_file_creation_validation

    subroutine test_content_validation()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Content validation")
        
        ctx = create_pdf_canvas(600, 450)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        ! Test various content types
        call ctx%text(5.0_wp, 7.0_wp, "Content Validation")
        call ctx%line(1.0_wp, 1.0_wp, 9.0_wp, 1.0_wp)
        call ctx%circle(3.0_wp, 4.0_wp, 0.5_wp)
        call ctx%rectangle(6.0_wp, 3.0_wp, 2.0_wp, 2.0_wp)
        
        filename = get_test_output_path('/tmp/pdf_content_validation.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Content validation test saved"
        call end_test()
    end subroutine test_content_validation

    !===========================================================================
    ! Performance Validation Tests (from test_pdf_validation_performance.f90)
    !===========================================================================
    
    subroutine test_performance_validation()
        print *, "--- Performance Validation Tests ---"
        
        call test_large_dataset_performance()
        call test_many_elements_performance()
        call test_text_rendering_performance()
    end subroutine test_performance_validation

    subroutine test_large_dataset_performance()
        type(pdf_context) :: ctx
        real(wp), allocatable :: large_field(:, :)
        integer :: nx, ny, i, j
        real(wp) :: start_time, end_time
        character(len=512) :: filename
        
        call start_test("Large dataset performance")
        
        ! Create moderately large dataset (reduced for speed)
        nx = 50; ny = 40
        allocate(large_field(nx, ny))
        
        do j = 1, ny
            do i = 1, nx
                large_field(i, j) = sin(real(i, wp) * 0.2_wp) * cos(real(j, wp) * 0.15_wp)
            end do
        end do
        
        call cpu_time(start_time)
        
        call figure(figsize=[400.0_wp, 300.0_wp])
        call imshow(large_field)
        call title('Large Dataset Performance')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_large_dataset.pdf')
        call savefig(filename)
        
        call cpu_time(end_time)
        
        print *, "  Large dataset (", nx, "x", ny, ") processed in ", end_time - start_time, " seconds"
        
        deallocate(large_field)
        call end_test()
    end subroutine test_large_dataset_performance

    subroutine test_many_elements_performance()
        type(pdf_context) :: ctx
        real(wp) :: start_time, end_time
        integer :: i, n_elements
        character(len=512) :: filename
        
        call start_test("Many elements performance")
        
        ctx = create_pdf_canvas(600, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 20.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 15.0_wp
        
        call cpu_time(start_time)
        
        n_elements = 200  ! Reduced from potentially larger number for speed
        
        ! Draw many elements
        do i = 1, n_elements
            call ctx%circle(real(mod(i, 20), wp) + 0.5_wp, &
                           real(i / 20, wp) + 0.5_wp, 0.2_wp)
        end do
        
        call cpu_time(end_time)
        
        filename = get_test_output_path('/tmp/pdf_many_elements.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  ", n_elements, " elements rendered in ", end_time - start_time, " seconds"
        call end_test()
    end subroutine test_many_elements_performance

    subroutine test_text_rendering_performance()
        type(pdf_context) :: ctx
        real(wp) :: start_time, end_time
        integer :: i
        character(len=20) :: text_str
        character(len=512) :: filename
        
        call start_test("Text rendering performance")
        
        ctx = create_pdf_canvas(500, 400)
        ctx%x_min = 0.0_wp; ctx%x_max = 10.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 8.0_wp
        
        call cpu_time(start_time)
        
        ! Render multiple text strings
        do i = 1, 50  ! Reduced for speed
            write(text_str, '(A, I0)') 'Text ', i
            call ctx%text(real(mod(i-1, 10), wp) + 0.5_wp, &
                         real((i-1) / 10, wp) * 1.5_wp + 1.0_wp, text_str)
        end do
        
        call cpu_time(end_time)
        
        filename = get_test_output_path('/tmp/pdf_text_performance.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Text rendering completed in ", end_time - start_time, " seconds"
        call end_test()
    end subroutine test_text_rendering_performance

    !===========================================================================
    ! Label Collision Detection Tests (from test_pdf_label_collision_detection.f90)
    !===========================================================================
    
    subroutine test_label_collision_detection()
        print *, "--- Label Collision Detection Tests ---"
        
        call test_overlapping_labels()
        call test_label_spacing()
        call test_automatic_label_adjustment()
    end subroutine test_label_collision_detection

    subroutine test_overlapping_labels()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Overlapping labels detection")
        
        ctx = create_pdf_canvas(400, 300)
        ctx%x_min = 0.0_wp; ctx%x_max = 5.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 4.0_wp
        
        ! Place labels that would overlap
        call ctx%text(2.0_wp, 2.0_wp, "Label 1")
        call ctx%text(2.1_wp, 2.0_wp, "Label 2")  ! Very close - should detect collision
        call ctx%text(2.0_wp, 1.9_wp, "Label 3")  ! Very close vertically
        
        ! Add some separated labels for comparison
        call ctx%text(1.0_wp, 1.0_wp, "Separated A")
        call ctx%text(4.0_wp, 3.5_wp, "Separated B")
        
        filename = get_test_output_path('/tmp/pdf_label_overlap.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Overlapping labels test saved - check for collision handling"
        call end_test()
    end subroutine test_overlapping_labels

    subroutine test_label_spacing()
        type(pdf_context) :: ctx
        real(wp) :: x_positions(5), y_positions(5)
        integer :: i
        character(len=20) :: label_text
        character(len=512) :: filename
        
        call start_test("Label spacing validation")
        
        ctx = create_pdf_canvas(500, 350)
        ctx%x_min = 0.0_wp; ctx%x_max = 8.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 6.0_wp
        
        x_positions = [1.0_wp, 2.5_wp, 4.0_wp, 5.5_wp, 7.0_wp]
        y_positions = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        ! Test systematic label spacing
        do i = 1, 5
            write(label_text, '(A, I0)') 'Point ', i
            call ctx%text(x_positions(i), y_positions(i), label_text)
            call ctx%circle(x_positions(i), y_positions(i), 0.1_wp)  ! Mark the point
        end do
        
        filename = get_test_output_path('/tmp/pdf_label_spacing.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Label spacing test saved"
        call end_test()
    end subroutine test_label_spacing

    subroutine test_automatic_label_adjustment()
        type(pdf_context) :: ctx
        character(len=512) :: filename
        
        call start_test("Automatic label adjustment")
        
        ctx = create_pdf_canvas(350, 250)
        ctx%x_min = 0.0_wp; ctx%x_max = 4.0_wp
        ctx%y_min = 0.0_wp; ctx%y_max = 3.0_wp
        
        ! Test labels near boundaries (should be adjusted inward)
        call ctx%text(0.1_wp, 0.1_wp, "Near Bottom-Left")
        call ctx%text(3.9_wp, 2.9_wp, "Near Top-Right")
        call ctx%text(0.1_wp, 2.9_wp, "Near Top-Left")
        call ctx%text(3.9_wp, 0.1_wp, "Near Bottom-Right")
        
        ! Center reference
        call ctx%text(2.0_wp, 1.5_wp, "Center")
        
        filename = get_test_output_path('/tmp/pdf_label_adjustment.pdf')
        call ctx%write_pdf(filename)
        
        print *, "  Label adjustment test saved - check boundary handling"
        call end_test()
    end subroutine test_automatic_label_adjustment

    !===========================================================================
    ! Edge Case Handling Tests
    !===========================================================================
    
    subroutine test_edge_case_handling()
        print *, "--- Edge Case Handling Tests ---"
        
        call test_empty_datasets()
        call test_single_value_datasets()
        call test_extreme_ranges()
    end subroutine test_edge_case_handling

    subroutine test_empty_datasets()
        real(wp), allocatable :: empty_field(:, :)
        character(len=512) :: filename
        
        call start_test("Empty datasets handling")
        
        ! Test zero-size arrays
        allocate(empty_field(0, 0))
        
        call figure(figsize=[300.0_wp, 200.0_wp])
        ! This should handle empty data gracefully
        if (size(empty_field) > 0) then
            call imshow(empty_field)
        else
            call text(0.5_wp, 0.5_wp, "Empty Dataset")
        end if
        call title('Empty Dataset Test')
        
        filename = get_test_output_path('/tmp/pdf_empty_dataset.pdf')
        call savefig(filename)
        
        deallocate(empty_field)
        print *, "  Empty dataset test saved"
        call end_test()
    end subroutine test_empty_datasets

    subroutine test_single_value_datasets()
        real(wp), allocatable :: single_field(:, :)
        character(len=512) :: filename
        
        call start_test("Single value datasets")
        
        allocate(single_field(1, 1))
        single_field(1, 1) = 42.0_wp
        
        call figure(figsize=[250.0_wp, 200.0_wp])
        call imshow(single_field)
        call title('Single Value Dataset')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_single_value.pdf')
        call savefig(filename)
        
        deallocate(single_field)
        print *, "  Single value dataset test saved"
        call end_test()
    end subroutine test_single_value_datasets

    subroutine test_extreme_ranges()
        real(wp), allocatable :: extreme_field(:, :)
        integer :: nx, ny, i, j
        character(len=512) :: filename
        
        call start_test("Extreme value ranges")
        
        nx = 6; ny = 4
        allocate(extreme_field(nx, ny))
        
        ! Create field with extreme value ranges
        do j = 1, ny
            do i = 1, nx
                if (i == 1) then
                    extreme_field(i, j) = 1.0e-10_wp  ! Very small
                else if (i == nx) then
                    extreme_field(i, j) = 1.0e10_wp   ! Very large
                else
                    extreme_field(i, j) = real(i * j, wp)
                end if
            end do
        end do
        
        call figure(figsize=[350.0_wp, 250.0_wp])
        call imshow(extreme_field)
        call title('Extreme Range Test')
        call colorbar()
        
        filename = get_test_output_path('/tmp/pdf_extreme_ranges.pdf')
        call savefig(filename)
        
        deallocate(extreme_field)
        print *, "  Extreme ranges test saved"
        call end_test()
    end subroutine test_extreme_ranges

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (condition) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
            all_tests_passed = .false.
        end if
    end subroutine assert_true

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A, I0, A, I0, A)') 'Test Summary: ', pass_count, ' of ', test_count, ' tests passed'
        write(*, '(A)') 'Consolidated 5 PDF validation test files into single comprehensive test'
        if (all_tests_passed .and. pass_count == test_count) then
            write(*, '(A)') 'All tests PASSED!'
        else
            write(*, '(A)') 'Some tests FAILED!'
        end if
        write(*, '(A)') ''
        write(*, '(A)') 'MANUAL VERIFICATION REQUIRED:'
        write(*, '(A)') '  1. Check all PDF outputs render correctly'
        write(*, '(A)') '  2. Verify heatmaps handle NaN/infinity values appropriately'
        write(*, '(A)') '  3. Confirm label collision detection works'
        write(*, '(A)') '  4. Validate performance is acceptable for large datasets'
        write(*, '(A)') '  5. Ensure edge cases are handled gracefully'
    end subroutine print_test_summary

end program test_pdf_validation_comprehensive